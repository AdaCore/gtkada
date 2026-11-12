--  Exercise Glib.G_Icon.From_Object_Free with an owned GLib themed icon.
--  No C test covers this GtkAda-only wrapper conversion helper.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Glib;
with Glib.G_Icon;
with Glib.Test;
with Gtkada.Types;
with System;

procedure Main is
   use type Glib.G_Icon.G_Icon;

   --  From_Object_Free releases this temporary wrapper with g_free, so build
   --  its storage with GLib's allocator and view it as a G_Icon access value.
   type G_Icon_Access is access all Glib.G_Icon.G_Icon;
   function To_G_Icon_Access is new Ada.Unchecked_Conversion
     (System.Address, G_Icon_Access);

   function G_Malloc (Size : Glib.Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   function Themed_Icon_New
     (Name : Gtkada.Types.Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, Themed_Icon_New, "g_themed_icon_new");

   procedure G_Object_Unref (Icon : Glib.G_Icon.G_Icon);
   pragma Import (C, G_Object_Unref, "g_object_unref");

   procedure Test_From_Object_Free with Convention => C;

   --  Check that conversion returns the original interface pointer and leaves
   --  its owned GObject reference available for normal unref afterward.
   procedure Test_From_Object_Free is
      Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("document-open");
      Icon : constant Glib.G_Icon.G_Icon := Themed_Icon_New (Name);
      Wrapper : constant G_Icon_Access := To_G_Icon_Access
        (G_Malloc
           (Glib.Gsize (Glib.G_Icon.G_Icon'Size / System.Storage_Unit)));
      Converted : Glib.G_Icon.G_Icon;
   begin
      Gtkada.Types.Free (Name);
      Glib.Test.Assert_True (Icon /= null);

      --  The wrapper owns the single GObject reference consumed by the result.
      Wrapper.all := Icon;
      Converted := Glib.G_Icon.From_Object_Free (Wrapper);
      Glib.Test.Assert_True (Converted = Icon);
      G_Object_Unref (Converted);
   end Test_From_Object_Free;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-g-icon/from-object-free",
      Test_From_Object_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
