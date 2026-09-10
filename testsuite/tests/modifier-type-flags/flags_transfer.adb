--  Checks that Gdk_Modifier_Type survives the two paths GObject uses for a
--  G_TYPE_FLAGS value, both of which represent it as a guint:
--
--    * signal arguments, unpacked from a GValue by Gtk.Arguments'
--      Unchecked_To_Gdk_Modifier_Type (which must call g_value_get_flags,
--      not g_value_get_int);
--    * properties, read and written by Gdk.Enums' inherited Get_Property /
--      Set_Property (which must use the guint helpers, not the gulong ones).
--
--  Every mask used below sets a bit in the high half of the guint as well as
--  one in the low half, so a helper that transferred only part of the value
--  would show up as a mismatch rather than as a value that happens to be
--  right.

with Ada.Command_Line;
with System;

with Glib;                    use Glib;
with Glib.Test;               use Glib.Test;
with Glib.Values;             use Glib.Values;

with Gdk.Enums;               use Gdk.Enums;

with Gtk.Cell_Renderer_Accel; use Gtk.Cell_Renderer_Accel;
with Gtk.Keyval_Trigger;      use Gtk.Keyval_Trigger;
with Gtk.Main;

procedure Flags_Transfer is

   Keyval_A : constant Guint := 16#61#;
   --  GDK_KEY_a, which is the Latin-1 code point.

   Mods : constant Gdk_Modifier_Type :=
     Gdk_Meta_Mask or Gdk_Control_Mask or Gdk_Shift_Mask;
   --  16#1000_0005#: two low bits and one bit in the high half.

   function Signal_Lookup (Name : String; IType : GType) return Guint;
   pragma Import (C, Signal_Lookup, "g_signal_lookup");

   procedure Emit_By_Name_V
     (Instance_And_Params : System.Address;
      Signal_Id           : Guint;
      Detail              : GQuark;
      Return_Value        : System.Address);
   pragma Import (C, Emit_By_Name_V, "g_signal_emitv");
   --  g_signal_emitv takes its arguments as an array of GValues rather than
   --  as varargs, so it is the emission entry point a flags argument can be
   --  handed to from Ada without going through a C shim.

   Seen_Mods  : Gdk_Modifier_Type := Gdk_No_Modifier_Mask;
   Seen_Count : Natural := 0;
   --  Written by Accel_Edited_Handler, read back by Test_Signal_Argument.

   procedure Accel_Edited_Handler
     (Self             : access Gtk_Cell_Renderer_Accel_Record'Class;
      Path_String      : UTF8_String;
      Accel_Key        : Guint;
      Accel_Mods       : Gdk_Modifier_Type;
      Hardware_Keycode : Guint);
   --  Handler for "accel-edited". Deliberately not named On_Accel_Edited:
   --  that is the name of the connection primitive called below, and
   --  overloading it here makes the "'Unrestricted_Access" argument of that
   --  call ambiguous -- which some compiler versions reject.

   procedure Test_Signal_Argument with Convention => C;
   procedure Test_Renderer_Property with Convention => C;
   procedure Test_Trigger_Property with Convention => C;

   --------------------------
   -- Accel_Edited_Handler --
   --------------------------

   procedure Accel_Edited_Handler
     (Self             : access Gtk_Cell_Renderer_Accel_Record'Class;
      Path_String      : UTF8_String;
      Accel_Key        : Guint;
      Accel_Mods       : Gdk_Modifier_Type;
      Hardware_Keycode : Guint)
   is
      pragma Unreferenced (Self, Hardware_Keycode);
   begin
      Assert_Cmpstr_Eq (Path_String, "0");
      Assert_Cmpuint_Eq (Accel_Key, Keyval_A);

      Seen_Mods  := Accel_Mods;
      Seen_Count := Seen_Count + 1;
   end Accel_Edited_Handler;

   --------------------------
   -- Test_Signal_Argument --
   --------------------------

   procedure Test_Signal_Argument is
      type GValue_Array is array (0 .. 4) of GValue;
      pragma Convention (C, GValue_Array);

      Renderer : Gtk_Cell_Renderer_Accel;
      Params   : GValue_Array;
   begin
      Gtk_New (Renderer);
      Renderer.On_Accel_Edited (Accel_Edited_Handler'Unrestricted_Access);

      --  The instance comes first, then the four "accel-edited" arguments.
      Init (Params (0), Gtk.Cell_Renderer_Accel.Get_Type);
      Set_Object (Params (0), Renderer);
      Init (Params (1), GType_String);
      Set_String (Params (1), "0");
      Init (Params (2), GType_Uint);
      Set_Uint (Params (2), Keyval_A);
      Init (Params (3), Gdk_Modifier_Type_Get_Type);
      Set_Flags (Params (3), Guint (Mods));
      Init (Params (4), GType_Uint);
      Set_Uint (Params (4), 38);

      Emit_By_Name_V
        (Params (0)'Address,
         Signal_Lookup
           ("accel-edited" & ASCII.NUL, Gtk.Cell_Renderer_Accel.Get_Type),
         0,
         System.Null_Address);

      for Param of Params loop
         Unset (Param);
      end loop;

      Assert_Cmpint_Eq (Gint (Seen_Count), 1);
      Assert_Cmphex_Eq (Guint (Seen_Mods), Guint (Mods));
   end Test_Signal_Argument;

   ----------------------------
   -- Test_Renderer_Property --
   ----------------------------

   procedure Test_Renderer_Property is
      Renderer : Gtk_Cell_Renderer_Accel;
   begin
      Gtk_New (Renderer);

      --  "accel-mods" is read-write, so it exercises both helpers.
      Set_Property (Renderer, Accel_Mods_Property, Mods);
      Assert_Cmphex_Eq
        (Guint (Gdk.Enums.Get_Property (Renderer, Accel_Mods_Property)),
         Guint (Mods));

      Set_Property (Renderer, Accel_Mods_Property, Gdk_No_Modifier_Mask);
      Assert_Cmphex_Eq
        (Guint (Gdk.Enums.Get_Property (Renderer, Accel_Mods_Property)),
         16#0000_0000#);
   end Test_Renderer_Property;

   ---------------------------
   -- Test_Trigger_Property --
   ---------------------------

   procedure Test_Trigger_Property is
      Trigger : constant Gtk_Keyval_Trigger :=
        Gtk_Keyval_Trigger_New (Keyval_A, Mods);
   begin
      --  "modifiers" is construct-only, so only the read helper is used; the
      --  accessor gets the value straight from C and is the reference.
      Assert_Cmphex_Eq (Guint (Trigger.Get_Modifiers), Guint (Mods));
      Assert_Cmphex_Eq
        (Guint (Gdk.Enums.Get_Property (Trigger, Modifiers_Property)),
         Guint (Mods));
   end Test_Trigger_Property;

begin
   Glib.Test.Init;

   --  Cell renderers and shortcut triggers are GTK objects; Gtk.Main.Init
   --  brings the library up.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/modifier-type/signal-argument",
      Test_Signal_Argument'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/modifier-type/renderer-property",
      Test_Renderer_Property'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/modifier-type/trigger-property",
      Test_Trigger_Property'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Flags_Transfer;
