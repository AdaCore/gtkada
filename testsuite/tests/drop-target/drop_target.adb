
with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Glib.Test;           use Glib.Test;
with Glib.Values;         use Glib.Values;
with Gdk.Drag;            use Gdk.Drag;
with Gdk.Drop;            use Gdk.Drop;
with Gtk.Drop_Target;     use Gtk.Drop_Target;
with Gtk.Main;

procedure Drop_Target is

   Dropped : Natural := 0;
   --  Incremented by the "drop" handler

   Last_Int : Gint := 0;
   --  The integer carried by the GValue that reached the last handler

   Last_X, Last_Y : Gdouble := 0.0;

   procedure Emit_Drop
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Value    : System.Address;
      X, Y     : Gdouble;
      Result   : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail of "drop" is (GValue *, double, double, gboolean *)

   procedure Emit_Motion
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      X, Y     : Gdouble;
      Result   : access Drag_Action)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail of "motion" is (double, double, GdkDragAction *)

   function Handle_Motion
     (Self : access Gtk_Drop_Target_Record'Class;
      X, Y : Gdouble) return Drag_Action;

   function Handle_Drop
     (Self  : access Gtk_Drop_Target_Record'Class;
      Value : GValue;
      X, Y  : Gdouble) return Boolean;
   procedure Test_Motion_Signal with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Drop_Signal with Convention => C;

   -------------------
   -- Handle_Motion --
   -------------------

   function Handle_Motion
     (Self : access Gtk_Drop_Target_Record'Class;
      X, Y : Gdouble) return Drag_Action
   is
      pragma Unreferenced (Self, X, Y);
   begin
      return Gdk_Action_Move;
   end Handle_Motion;

   -------------------------
   -- Test_Motion_Signal --
   -------------------------

   procedure Test_Motion_Signal is
      T      : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy or Gdk_Action_Move);
      Result : aliased Drag_Action := Gdk_Action_None;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("motion");
   begin
      T.On_Motion (Handle_Motion'Unrestricted_Access);

      --  A flags return value must make it back through the marshaller.
      Emit_Motion (Get_Object (T), Name, 1.0, 2.0, Result'Access);
      Interfaces.C.Strings.Free (Name);
      Assert_True (Result = Gdk_Action_Move);

      Unref (T);
   end Test_Motion_Signal;

   -----------------
   -- Handle_Drop --
   -----------------

   function Handle_Drop
     (Self  : access Gtk_Drop_Target_Record'Class;
      Value : GValue;
      X, Y  : Gdouble) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Dropped  := Dropped + 1;
      Last_Int := Get_Int (Value);
      Last_X   := X;
      Last_Y   := Y;
      return True;
   end Handle_Drop;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      T : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy or Gdk_Action_Move);
   begin
      Assert_True (T.Get_Actions = (Gdk_Action_Copy or Gdk_Action_Move));
      T.Set_Actions (Gdk_Action_Link);
      Assert_True (T.Get_Actions = Gdk_Action_Link);

      Assert_False (T.Get_Preload);
      T.Set_Preload (True);
      Assert_True (T.Get_Preload);

      --  No drop is in progress: there is nothing to look at.
      Assert_True (T.Get_Current_Drop = null);
      Assert_True (T.Get_Value = null);

      Unref (T);
   end Test_Properties;

   ----------------------
   -- Test_Drop_Signal --
   ----------------------

   procedure Test_Drop_Signal is
      T      : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy);
      Value  : GValue;
      Result : aliased Gboolean := 0;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("drop");
   begin
      T.On_Drop (Handle_Drop'Unrestricted_Access);

      --  The GValue, and the coordinates after it, must reach the handler
      --  intact: this is what the marshaller for "drop" has to get right.
      Init_Set_Int (Value, 42);
      Emit_Drop (Get_Object (T), Name, Value'Address, 3.5, 7.25, Result'Access);
      Unset (Value);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpint_Eq (Gint (Dropped), 1);
      Assert_Cmpint_Eq (Last_Int, 42);
      Assert_True (Last_X = 3.5);
      Assert_True (Last_Y = 7.25);
      Assert_True (Result /= 0);

      Unref (T);
   end Test_Drop_Signal;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/droptarget/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/droptarget/motion-signal", Test_Motion_Signal'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/droptarget/drop-signal", Test_Drop_Signal'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drop_Target;

--  GTK has no C test for GtkDropTarget, so these cases are ours: they cover
--  the surface the Gtk.Drop_Target binding adds, chiefly the marshalling of
--  the GValue parameter of the "drop" signal.
