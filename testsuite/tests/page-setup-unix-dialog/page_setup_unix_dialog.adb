--  Gtk.Page_Setup_Unix_Dialog tests. Test_Constructors ports the headless
--  test_page_setup_unix_dialog_basic case from testsuite/c_tests/templates.c;
--  the interactive C show case is not suitable for an unattended test. The
--  remaining cases check the dialog's settings, responses, and inherited
--  accessibility and native-window interfaces.

with Ada.Command_Line;
with Gdk;
with Gsk.Renderer;
with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with Glib.Test;                  use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Main;
with Gtk.Page_Setup;             use Gtk.Page_Setup;
with Gtk.Page_Setup_Unix_Dialog;  use Gtk.Page_Setup_Unix_Dialog;
with Gtk.Paper_Size;             use Gtk.Paper_Size;
with Gtk.Print_Settings;         use Gtk.Print_Settings;
with Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;

procedure Page_Setup_Unix_Dialog is
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Surface;
   use type Gsk.Renderer.Gsk_Renderer;
   use type Glib.Gdouble;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Widget.Gtk_Widget;

   procedure Test_Constructors with Convention => C;
   procedure Test_Settings with Convention => C;
   procedure Test_Layout with Convention => C;
   procedure Test_Responses with Convention => C;
   procedure Test_Accessibility_And_Native with Convention => C;

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

   --  The unattached dialog has no accessible parent, sibling, bounds, or
   --  focus. Present it briefly to exercise GtkNative methods that require a
   --  realized window; nonblocking main-context iterations keep this headless.
   procedure Test_Accessibility_And_Native is
      Dialog : constant Gtk_Page_Setup_Unix_Dialog :=
        Gtk_Page_Setup_Unix_Dialog_New;
      X, Y, Width, Height : Gint;
      Surface_X, Surface_Y : Gdouble;
   begin
      Assert_True
        (Dialog.Get_Accessible_Role /= Gtk.Accessible.Accessible_Role_None);
      Assert_True (Dialog.Get_Accessible_Id'Length > 0);
      Assert_True (Dialog.Get_At_Context = null);
      Assert_False (Dialog.Get_Bounds (X, Y, Width, Height));
      Assert_True
        (Dialog.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Dialog.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_False
        (Dialog.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These accessibility updates are valid on an unrooted widget and do
      --  not require an assistive-technology service to be running.
      Dialog.Announce
        ("Page setup accessibility test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Dialog.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Dialog.Reset_Relation
        (Gtk.Accessible.Accessible_Relation_Labelled_By);
      Dialog.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Dialog.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Dialog.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Dialog.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Dialog.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Dialog.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focused);

      --  The root has no focused child initially; passing null clears focus.
      Assert_True (Dialog.Get_Focus = null);
      Dialog.Set_Focus (null);
      Assert_True (Dialog.Get_Display /= null);

      --  Mapping creates the surface and renderer needed by GtkNative.
      Dialog.Present;
      for Iteration in 1 .. 5 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => False);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      Assert_True (Dialog.Get_Realized);
      Assert_True
        (Dialog.Get_First_Accessible_Child /=
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Dialog.Get_Surface /= null);
      Assert_True (Dialog.Get_Renderer /= null);
      Dialog.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X >= 0.0 and Surface_Y >= 0.0);
      Dialog.Destroy;
   end Test_Accessibility_And_Native;

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
   Glib.Test.Add_Func
     ("/page-setup-unix-dialog/accessibility-and-native",
      Test_Accessibility_And_Native'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Page_Setup_Unix_Dialog;
