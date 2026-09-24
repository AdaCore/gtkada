--  Compiles and runs the code samples quoted in the User's Guide
--  (docs/gtkada_ug/), so that they cannot go stale. The samples themselves
--  live in the UG_* packages, between START and END markers that the guide
--  picks up with literalinclude.

with Ada.Command_Line;
with System;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded;
with GNAT.Strings;
with Glib;             use Glib;
with Glib.Application; use Glib.Application;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Application;  use Gtk.Application;
with Gtk.Button;       use Gtk.Button;
with Gtk.Main;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with UG_Application;
with UG_Debugging;
with UG_Hello;
pragma Unreferenced (UG_Hello);
--  Only compiled, see ug_hello.adb
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

   procedure Test_Application
   with Convention => C;

   procedure Test_Command_Line
   with Convention => C;

   procedure Click_Quit (Self : access Gapplication_Record'Class);
   --  Connected after UG_Application.On_Activate: click the Quit button of
   --  the window it created, first with unsaved changes (which must keep
   --  the window open), then without (which must end the application).

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

   ----------------
   -- Click_Quit --
   ----------------

   procedure Click_Quit (Self : access Gapplication_Record'Class) is
      App    : constant Gtk_Application := Gtk_Application (Self);
      Window : constant Gtk_Window := App.Get_Active_Window;
      Quit   : constant Gtk_Button := Gtk_Button (Window.Get_Child);
   begin
      UG_Application.Has_Unsaved_Changes := True;
      Click (Quit);
      Assert_True (App.Get_Active_Window = Window);

      UG_Application.Has_Unsaved_Changes := False;
      Click (Quit);
      Assert_True (App.Get_Active_Window = null);
   end Click_Quit;

   ----------------------
   -- Test_Application --
   ----------------------

   procedure Test_Application is
      App  : constant Gtk_Application := Gtk_Application_New
        ("com.adacore.gtkada.user_guide", G_Application_Flags_None);
      Argv : GNAT.Strings.String_List := (1 => new String'("user_guide"));
   begin
      App.On_Activate (UG_Application.On_Activate'Access);
      App.On_Activate (Click_Quit'Unrestricted_Access, After => True);
      Assert_Cmpint_Eq (App.Run (Argv'Length, Argv), 0);
      GNAT.Strings.Free (Argv (1));
      App.Unref;
   end Test_Application;

   -----------------------
   -- Test_Command_Line --
   -----------------------

   procedure Test_Command_Line is
      use Ada.Strings.Unbounded;
      App  : constant Gtk_Application :=
        UG_Application.Create_Command_Line_App;
      Argv : GNAT.Strings.String_List :=
        (new String'("user_guide"),
         new String'("a.txt"),
         new String'("b.txt"));
   begin
      App.On_Activate (Click_Quit'Unrestricted_Access, After => True);
      Assert_Cmpint_Eq (App.Run (Argv'Length, Argv), 0);
      Assert_Cmpstr_Eq
        (To_String (UG_Application.Opened_Files), "a.txt b.txt ");
      for Arg of Argv loop
         GNAT.Strings.Free (Arg);
      end loop;
      App.Unref;
   end Test_Command_Line;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/user-guide/signals", Test_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/user-guide/my-button", Test_My_Button'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/user-guide/ref-count", Test_Ref_Count'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/user-guide/application", Test_Application'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/user-guide/command-line",
      Test_Command_Line'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end User_Guide;
