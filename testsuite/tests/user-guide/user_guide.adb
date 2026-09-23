--  Compiles and runs the code samples quoted in the User's Guide
--  (docs/gtkada_ug/), so that they cannot go stale. The samples themselves
--  live in the UG_* packages, between START and END markers that the guide
--  picks up with literalinclude.

with Ada.Command_Line;
with System;
with Interfaces.C.Strings;
with Glib;         use Glib;
with Glib.Object;  use Glib.Object;
with Glib.Test;    use Glib.Test;
with Gtk.Button;   use Gtk.Button;
with Gtk.Main;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Window;   use Gtk.Window;
with UG_Debugging;
with UG_My_Button; use UG_My_Button;
with UG_Signals;   use UG_Signals;

procedure User_Guide is

   procedure Emit_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  gtk_button_clicked is gone in gtk4, and Gtk.Widget.Activate emits
   --  "clicked" only from a timeout, so emit the signal directly.

   procedure Click (Button : Gtk_Button);
   --  Emit "clicked" on Button

   procedure Test_Signals
   with Convention => C;

   procedure Test_My_Button
   with Convention => C;

   procedure Test_Ref_Count
   with Convention => C;

   -----------
   -- Click --
   -----------

   procedure Click (Button : Gtk_Button) is
      Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("clicked");
   begin
      Emit_By_Name (Button.Get_Object, Name);
      Interfaces.C.Strings.Free (Name);
   end Click;

   ------------------
   -- Test_Signals --
   ------------------

   procedure Test_Signals is
      Window : constant Gtk_Window := Gtk_Window_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("OK");
   begin
      Window.Set_Child (Button);
      Connect_Handler (Button);
      Connect_Slot_Handler (Button, Window);

      Click (Button);
      Assert_Cmpuint_Eq (Guint (Clicks_On_Button), 1);
      Assert_Cmpuint_Eq (Guint (Clicks_On_Window), 1);

      Window.Destroy;
   end Test_Signals;

   --------------------
   -- Test_My_Button --
   --------------------

   procedure Test_My_Button is
      Myb : constant My_Button := Create;
   begin
      Assert_Cmpstr_Eq (Myb.Get_Label, "Hello");

      --  Inherited and new primitives are both available
      Myb.My_Primitive_Func;
      Assert_Cmpuint_Eq (Guint (Myb.Count), 1);

      --  The Ada object is found again from the C one
      Assert_True (Get_User_Data_Or_Null (Myb.Get_Object) = GObject (Myb));
   end Test_My_Button;

   --------------------
   -- Test_Ref_Count --
   --------------------

   procedure Test_Ref_Count is
      Window : constant Gtk_Window := Gtk_Window_New;
   begin
      Assert_Cmpuint_Ge (UG_Debugging.Count_Of (Gtk_Widget (Window)), 1);
      Window.Destroy;
   end Test_Ref_Count;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/user-guide/signals", Test_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/user-guide/my-button", Test_My_Button'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/user-guide/ref-count", Test_Ref_Count'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end User_Guide;
