with Ada.Command_Line;
with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Glib.Test;                  use Glib.Test;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Main;
with Gtk.Page_Setup;             use Gtk.Page_Setup;
with Gtk.Page_Setup_Unix_Dialog;  use Gtk.Page_Setup_Unix_Dialog;
with Gtk.Paper_Size;             use Gtk.Paper_Size;
with Gtk.Print_Settings;         use Gtk.Print_Settings;
with Gtk.Window;                 use Gtk.Window;

procedure Page_Setup_Unix_Dialog is
   procedure Test_Constructors with Convention => C;
   procedure Test_Settings with Convention => C;
   procedure Test_Layout with Convention => C;
   procedure Test_Responses with Convention => C;

   Response_Count : Natural := 0;
   Last_Response : Gtk_Response_Type := Gtk_Response_None;

   procedure Respond
     (Self : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type)
   is
   begin
      --  An inherited signal must preserve the subclass wrapper.
      Assert_True (Self.all in Gtk_Page_Setup_Unix_Dialog_Record'Class);
      Last_Response := Response_Id;
      Response_Count := Response_Count + 1;
   end Respond;

   procedure Test_Constructors is
      Parent : Gtk_Window;
      Dialog : Gtk_Page_Setup_Unix_Dialog;
   begin
      Gtk_New (Parent);
      Gtk_New (Dialog, "Page layout", Parent);
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Page layout");
      Assert_True (Dialog.Get_Transient_For = Parent);
      declare
         Setup : constant Gtk_Page_Setup := Dialog.Get_Page_Setup;
      begin
         Assert_True (Setup /= null);
         Setup.Unref;
      end;
      Dialog.Destroy;
      Parent.Destroy;

      --  Both the title and transient parent are optional in GTK.
      Dialog := Gtk_Page_Setup_Unix_Dialog_New;
      Assert_True (Dialog.Get_Transient_For = null);
      Dialog.Destroy;
   end Test_Constructors;

   procedure Test_Settings is
      Dialog : constant Gtk_Page_Setup_Unix_Dialog :=
        Gtk_Page_Setup_Unix_Dialog_New;
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Replacement : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      Assert_True (Dialog.Get_Print_Settings = null);
      Settings.Set_N_Copies (3);
      Dialog.Set_Print_Settings (Settings);
      Assert_True (Dialog.Get_Print_Settings = Settings);
      Settings.Unref;
      --  The dialog keeps the settings alive after the caller releases them.
      Assert_Cmpint_Eq (Dialog.Get_Print_Settings.Get_N_Copies, 3);

      Replacement.Set_N_Copies (7);
      Dialog.Set_Print_Settings (Replacement);
      Assert_True (Dialog.Get_Print_Settings = Replacement);
      Dialog.Set_Print_Settings (null);
      Assert_True (Dialog.Get_Print_Settings = null);
      Replacement.Unref;
      Dialog.Destroy;
   end Test_Settings;

   procedure Test_Layout is
      Dialog : constant Gtk_Page_Setup_Unix_Dialog :=
        Gtk_Page_Setup_Unix_Dialog_New;
      Setup : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Paper : Gtk_Paper_Size;
      Returned : Gtk_Page_Setup;
   begin
      --  Use a standard size offered by the dialog's paper selector.
      Gtk_New (Paper, Paper_Name_A5);
      Setup.Set_Paper_Size_And_Default_Margins (Paper);
      Paper.Free;
      Setup.Set_Orientation (Page_Orientation_Reverse_Landscape);
      Dialog.Set_Page_Setup (Setup);
      Returned := Dialog.Get_Page_Setup;
      Assert_True
        (Returned.Get_Orientation = Page_Orientation_Reverse_Landscape);
      Assert_Cmpstr_Eq (Returned.Get_Paper_Size.Get_Name, Paper_Name_A5);
      Assert_Cmpfloat_Eq (Returned.Get_Paper_Size.Get_Width (Mm), 148.0);
      Assert_Cmpfloat_Eq (Returned.Get_Paper_Size.Get_Height (Mm), 210.0);
      Assert_Cmpfloat_Eq
        (Returned.Get_Top_Margin (Mm), Setup.Get_Top_Margin (Mm));
      Assert_Cmpfloat_Eq
        (Returned.Get_Bottom_Margin (Mm), Setup.Get_Bottom_Margin (Mm));
      Assert_Cmpfloat_Eq
        (Returned.Get_Left_Margin (Mm), Setup.Get_Left_Margin (Mm));
      Assert_Cmpfloat_Eq
        (Returned.Get_Right_Margin (Mm), Setup.Get_Right_Margin (Mm));
      --  The returned reference survives both the source setup and the dialog.
      Setup.Unref;
      Dialog.Destroy;
      Assert_Cmpstr_Eq (Returned.Get_Paper_Size.Get_Name, Paper_Name_A5);
      Returned.Unref;
   end Test_Layout;

   procedure Test_Responses is
      Dialog : constant Gtk_Page_Setup_Unix_Dialog :=
        Gtk_Page_Setup_Unix_Dialog_New;
   begin
      Dialog.On_Response (Respond'Unrestricted_Access);
      Dialog.Response (Gtk_Response_OK);
      Assert_Cmpint_Eq (Gint (Response_Count), 1);
      Assert_True (Last_Response = Gtk_Response_OK);
      Dialog.Response (Gtk_Response_Cancel);
      Assert_Cmpint_Eq (Gint (Response_Count), 2);
      Assert_True (Last_Response = Gtk_Response_Cancel);
      Dialog.Destroy;
   end Test_Responses;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/page-setup-unix-dialog/constructors",
      Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/page-setup-unix-dialog/settings", Test_Settings'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/page-setup-unix-dialog/layout", Test_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/page-setup-unix-dialog/responses", Test_Responses'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Page_Setup_Unix_Dialog;
