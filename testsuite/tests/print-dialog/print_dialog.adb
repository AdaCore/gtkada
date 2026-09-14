--  Headless coverage for Gtk.Print_Dialog and Gtk.Print_Setup.
--
--  What is checked here is the synchronous surface: the dialog's properties,
--  including the two that hand a GObject across the binding in both
--  directions (page setup and print settings), and the null Gtk_Print_Setup.
--
--  The asynchronous entry points -- Setup, Print, Print_File and their
--  matching *_Finish -- are deliberately left alone. Every one of them
--  presents a real print dialog and then talks to a print portal on the
--  session bus, so under Xvfb the dialog simply comes up and the callback is
--  never reached; an already-cancelled GCancellable does not short-circuit
--  that, it is only consulted once the dialog is up. Those paths, and the
--  Gtk_Print_Setup that only Setup_Finish can produce, need a test harness
--  with a fake portal, which is out of reach here.

with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Test;          use Glib.Test;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Main;
with Gtk.Page_Setup;     use Gtk.Page_Setup;
with Gtk.Print_Dialog;   use Gtk.Print_Dialog;
with Gtk.Print_Settings; use Gtk.Print_Settings;
with Gtk.Print_Setup;    use Gtk.Print_Setup;

procedure Print_Dialog is

   procedure Test_Properties
   with Convention => C;

   procedure Test_Objects
   with Convention => C;

   procedure Test_Print_Setup
   with Convention => C;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Dialog : constant Gtk_Print_Dialog := Gtk_Print_Dialog_New;
   begin
      Dialog.Set_Title ("Print the thing");
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Print the thing");

      Dialog.Set_Accept_Label ("_Go");
      Assert_Cmpstr_Eq (Dialog.Get_Accept_Label, "_Go");

      Dialog.Set_Modal (False);
      Assert_False (Dialog.Get_Modal);
      Dialog.Set_Modal (True);
      Assert_True (Dialog.Get_Modal);
   end Test_Properties;

   ------------------
   -- Test_Objects --
   ------------------

   procedure Test_Objects is
      Dialog   : constant Gtk_Print_Dialog := Gtk_Print_Dialog_New;
      Setup    : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      --  Both start out unset.
      Assert_True (Dialog.Get_Page_Setup = null);
      Assert_True (Dialog.Get_Print_Settings = null);

      Setup.Set_Orientation (Page_Orientation_Landscape);
      Settings.Set_N_Copies (4);

      Dialog.Set_Page_Setup (Setup);
      Dialog.Set_Print_Settings (Settings);

      --  The very objects that were handed over must come back, not fresh
      --  wrappers around the same C pointers.
      Assert_True (Dialog.Get_Page_Setup = Setup);
      Assert_True (Dialog.Get_Print_Settings = Settings);

      Assert_True
        (Dialog.Get_Page_Setup.Get_Orientation = Page_Orientation_Landscape);
      Assert_Cmpint_Eq (Dialog.Get_Print_Settings.Get_N_Copies, 4);
   end Test_Objects;

   ----------------------
   -- Test_Print_Setup --
   ----------------------

   procedure Test_Print_Setup is
      Setup : constant Gtk_Print_Setup := Null_Gtk_Print_Setup;
   begin
      --  GtkPrintSetup is a reference-counted boxed type; it has a registered
      --  GType even though only Gtk.Print_Dialog.Setup_Finish hands one out.
      Assert_True (Gtk.Print_Setup.Get_Type /= Glib.GType_Invalid);
      Assert_True (Setup.Is_Null);
      Assert_Null (Setup.Get_Object);
   end Test_Print_Setup;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/print-dialog/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-dialog/objects", Test_Objects'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-dialog/print-setup", Test_Print_Setup'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Print_Dialog;
