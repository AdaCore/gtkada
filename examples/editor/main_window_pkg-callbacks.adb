with Gtk.Main;
with Gtk.Editable; use Gtk.Editable;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with About_Dialog_Pkg; use About_Dialog_Pkg;
with File_Utils; use File_Utils;

package body Main_Window_Pkg.Callbacks is

   Id : Message_Id;

   -----------------------------------
   --  On_Main_Window_Delete_Event  --
   -----------------------------------

   procedure On_Main_Window_Delete_Event
     (Object : access Gtk_Window_Record) is
   begin
      Gtk.Main.Gtk_Exit (0);
   end On_Main_Window_Delete_Event;

   -----------------------
   --  On_New_Activate  --
   -----------------------

   procedure On_New_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      New_File;
   end On_New_Activate;

   ------------------------
   --  On_Open_Activate  --
   ------------------------

   procedure On_Open_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Open_File;
   end On_Open_Activate;

   ------------------------
   --  On_Save_Activate  --
   ------------------------

   procedure On_Save_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      --  If the current document doesn't have a filename yet, we call save_as
      --  to show the file selection dialog.

      if Current_Filename = null then
         Save_As;
      else
         Real_Save_File (Current_Filename.all);
      end if;
   end On_Save_Activate;

   ---------------------------
   --  On_Save_As_Activate  --
   ---------------------------

   procedure On_Save_As_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Save_As;
   end On_Save_As_Activate;

   ------------------------
   --  On_Quit_Activate  --
   ------------------------

   procedure On_Quit_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Main.Gtk_Exit (0);
   end On_Quit_Activate;

   -----------------------
   --  On_Cut_Activate  --
   -----------------------

   procedure On_Cut_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Cut_Clipboard (Gtk_Editable (Main_Window.Text1), 0);

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push
        (Main_Window.Statusbar1, 1, "Text cut to clipboard.");
   end On_Cut_Activate;

   ------------------------
   --  On_Copy_Activate  --
   ------------------------

   procedure On_Copy_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Copy_Clipboard (Gtk_Editable (Main_Window.Text1), 0);

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "Text copied.");
   end On_Copy_Activate;

   -------------------------
   --  On_Paste_Activate  --
   -------------------------

   procedure On_Paste_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Paste_Clipboard (Gtk_Editable (Main_Window.Text1), 0);

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "Text pasted.");
   end On_Paste_Activate;

   --------------------------
   --  On_Delete_Activate  --
   --------------------------

   procedure On_Delete_Activate (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Delete_Selection (Gtk_Editable (Main_Window.Text1));

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "Text deleted.");
   end On_Delete_Activate;

   -------------------------
   --  On_About_Activate  --
   -------------------------

   procedure On_About_Activate (Object : access Gtk_Menu_Item_Record) is
   begin
      if About_Dialog = null then
         Gtk_New (About_Dialog);
      end if;

      Show_All (About_Dialog);
   end On_About_Activate;

   -----------------------------
   --  On_New_Button_Clicked  --
   -----------------------------

   procedure On_New_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      New_File;
   end On_New_Button_Clicked;

   ------------------------------
   --  On_Open_Button_Clicked  --
   ------------------------------

   procedure On_Open_Button_Clicked (Object : access Gtk_Button_Record) is
   begin
      Open_File;
   end On_Open_Button_Clicked;

   ------------------------------
   --  On_Save_Button_Clicked  --
   ------------------------------

   procedure On_Save_Button_Clicked (Object : access Gtk_Button_Record) is
   begin
      if Current_Filename = null then
         Save_As;
      else
         Real_Save_File (Current_Filename.all);
      end if;
   end On_Save_Button_Clicked;

   -----------------------
   --  On_Text_Changed  --
   -----------------------

   procedure On_Text_Changed (Object : access Gtk_Text_Record) is
   begin
      File_Changed := True;
   end On_Text_Changed;

end Main_Window_Pkg.Callbacks;
