--  Tests Glib.Types interface introspection and null conversion behavior.
--  The related accessor-apis.c helper calls g_type_interfaces as an
--  implementation detail, so these tests check the binding's documented API.

with Ada.Command_Line;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Glib.Types;
with Gtk.Box;
with Gtk.Orientable;

procedure Main is
   use type Glib.GType;
   use type Gtk.Orientable.Gtk_Orientable;

   --  GtkBox implements GtkOrientable; GObject itself has no interfaces.
   procedure Test_Interfaces with Convention => C;
   procedure Test_Interfaces is
      Box_Interfaces : constant GType_Array :=
        Glib.Types.Interfaces (Gtk.Box.Get_Type);
      Object_Interfaces : constant GType_Array :=
        Glib.Types.Interfaces (GType_Object);
      Has_Orientable : Boolean := False;
   begin
      for Item of Box_Interfaces loop
         if Item = Gtk.Orientable.Get_Type then
            Has_Orientable := True;
         end if;
      end loop;
      Assert_True (Has_Orientable);
      Assert_True (Object_Interfaces'Length = 0);
   end Test_Interfaces;

   --  An interface GType is recognized, while GtkBox's concrete GType is not.
   procedure Test_Is_Interface with Convention => C;
   procedure Test_Is_Interface is
   begin
      Assert_True (Glib.Types.Is_Interface (Gtk.Orientable.Get_Type));
      Assert_True (not Glib.Types.Is_Interface (Gtk.Box.Get_Type));
   end Test_Is_Interface;

   --  The generated interface conversion must preserve a null object as null.
   procedure Test_Null_Conversion with Convention => C;
   procedure Test_Null_Conversion is
      Converted : constant Gtk.Orientable.Gtk_Orientable :=
        Gtk.Box.Implements_Gtk_Orientable.To_Interface (null);
   begin
      Assert_True (Converted = Gtk.Orientable.Null_Gtk_Orientable);
   end Test_Null_Conversion;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-types/interfaces", Test_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-types/is-interface", Test_Is_Interface'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-types/null-conversion", Test_Null_Conversion'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
