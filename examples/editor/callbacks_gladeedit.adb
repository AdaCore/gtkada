with Glib; use Glib;
with Gdk.Window;
with Gdk.Font; use Gdk.Font;
with Gdk.Color; use Gdk.Color;
with Gtk.Editable; use Gtk.Editable;
with Gtk.Main;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Widget; use Gtk.Widget;
with Main_Window_Pkg; use Main_Window_Pkg;
with Open_File_Selection_Pkg; use Open_File_Selection_Pkg;
with Save_File_Selection_Pkg; use Save_File_Selection_Pkg;
with About_Dialog_Pkg; use About_Dialog_Pkg;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Unchecked_Deallocation;

package body Callbacks_Gladeedit is

   procedure Free is new Unchecked_Deallocation
     (Object => String, Name => String_Access);

   --  Complicated dialog boxes can be slow to create, so we create them once
   --  and keep pointers to them in static variables so we can reuse them.

   Open_Filesel : Open_File_Selection_Access;
   Save_Filesel : Save_File_Selection_Access;
   About : About_Dialog_Access;

   --  This is the filename of the file currently being edited.
   Current_Filename : String_Access;

   --  This flag is set to TRUE if the file has been changed. We don't actually
   --  use it here, but the program could be extended to prompt to save
   --  changes.
   File_Changed : Boolean := False;

   --  The size of chunks to read in when opening files.
   Buffer_Size : constant := 8192;

   Id : Message_Id;

   --  Internal procedures

   procedure New_File;
   --  Creates a new document, by clearing the text widget and setting the
   --  current filename to null.

   procedure Open_File;
   --  Shows the file selection dialog to open a file

   procedure Save_As;
   --  Shows the file selection dialog to save a file

   procedure Real_Open_File (Filename : String);
   --  Loads the given file

   procedure Real_Save_File (Filename : String);
   --  Saves the given file

   procedure Set_Window_Title;
   --  Sets the window title according to the current filename.

   procedure New_File is
   begin
      Gtk.Editable.Delete_Text (Gtk_Editable (Main_Window.Text1), 0, -1);
      Current_Filename := null;
      File_Changed := False;
      Set_Window_Title;

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "New file.");
   end New_File;

   procedure Open_File is
   begin
      --  We use the same file selection widget each time, so first
      --  of all we create it if it hasn't already been created.

      if Open_Filesel = null then
         Gtk_New (Open_Filesel);
      end if;

      Show_All (Open_Filesel);
      Gdk.Window.Gdk_Raise (Get_Window (Open_Filesel));
   end Open_File;

   procedure Save_As is
   begin
      if Save_Filesel = null then
         Gtk_New (Save_Filesel);
      end if;

      --  If the current document has a filename we use that as the default

      if Current_Filename /= null then
         Set_Filename (Save_Filesel, Current_Filename.all);
      end if;

      Show_All (Save_Filesel);
      Gdk.Window.Gdk_Raise (Get_Window (Save_Filesel));
   end Save_As;

   procedure Real_Open_File (Filename : String) is
      Fp : File_Descriptor;
      Buffer : String (1 .. Buffer_Size);
      Bytes_Read : Integer;
      File : aliased String := Filename & ASCII.Nul;

   begin
      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Gtk.Text.Freeze (Main_Window.Text1);
      Gtk.Editable.Delete_Text (Gtk_Editable (Main_Window.Text1), 0, -1);
      Free (Current_Filename);
      File_Changed := False;

      Fp := Open_Read (File'Address, Text);

      if Fp = Invalid_FD then
          Gtk.Text.Thaw (Main_Window.Text1);
          Id := Gtk.Status_Bar.Push
            (Main_Window.Statusbar1, 1, "Couldn't open file.");
          return;
      end if;

      loop
         Bytes_Read := Read (Fp, Buffer (1)'Address, Buffer_Size);

         if Bytes_Read > 0 then
           Gtk.Text.Insert
             (Main_Window.Text1, Null_Font, Null_Color, Null_Color, Buffer,
              Gint (Bytes_Read));
         end if;

         exit when Bytes_Read /= Buffer_Size;
      end loop;

      Close (Fp);
      Gtk.Text.Thaw (Main_Window.Text1);

      Current_Filename := new String'(Filename);
      Set_Window_Title;
      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "File opened.");
   end Real_Open_File;

   procedure Real_Save_File (Filename : String) is
   begin
      null;
   end Real_Save_File;

   procedure Set_Window_Title is
   begin
      if Current_Filename = null then
         Set_Title (Main_Window, "Editor: untitled");
      else
         Set_Title (Main_Window, "Editor: " & Current_Filename.all);
      end if;
   end Set_Window_Title;

   --  Callbacks

   procedure On_Main_Window_Delete_Event
     (Object : access Gtk_Window_Record) is
   begin
      Gtk.Main.Gtk_Exit (0);
   end On_Main_Window_Delete_Event;

   procedure On_New_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      New_File;
   end On_New_Activate;

   procedure On_Open_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Open_File;
   end On_Open_Activate;

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

   procedure On_Save_As_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Save_As;
   end On_Save_As_Activate;

   procedure On_Quit_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Main.Gtk_Exit (0);
   end On_Quit_Activate;

   procedure On_Cut_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Cut_Clipboard (Gtk_Editable (Main_Window.Text1), 0);

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push
        (Main_Window.Statusbar1, 1, "Text cut to clipboard.");
   end On_Cut_Activate;

   procedure On_Copy_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Copy_Clipboard (Gtk_Editable (Main_Window.Text1), 0);

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "Text copied.");
   end On_Copy_Activate;

   procedure On_Paste_Activate
     (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Paste_Clipboard (Gtk_Editable (Main_Window.Text1), 0);

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "Text pasted.");
   end On_Paste_Activate;

   procedure On_Delete_Activate (Object : access Gtk_Menu_Item_Record) is
   begin
      Gtk.Editable.Delete_Selection (Gtk_Editable (Main_Window.Text1));

      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Id := Gtk.Status_Bar.Push (Main_Window.Statusbar1, 1, "Text deleted.");
   end On_Delete_Activate;

   procedure On_About_Activate (Object : access Gtk_Menu_Item_Record) is
   begin
      if About = null then
         Gtk_New (About);
      end if;

      Show_All (About);
   end On_About_Activate;

   procedure On_New_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      New_File;
   end On_New_Button_Clicked;

   procedure On_Open_Button_Clicked (Object : access Gtk_Button_Record) is
   begin
      Open_File;
   end On_Open_Button_Clicked;

   procedure On_Save_Button_Clicked (Object : access Gtk_Button_Record) is
   begin
      if Current_Filename = null then
         Save_As;
      else
         Real_Save_File (Current_Filename.all);
      end if;
   end On_Save_Button_Clicked;

   procedure On_Text_Changed (Object : access Gtk_Text_Record) is
   begin
      File_Changed := True;
   end On_Text_Changed;

   procedure On_Open_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record) is
   begin
      Gtk.Widget.Hide (Gtk.Widget.Get_Toplevel (Gtk_Widget (Object)));
   end On_Open_Filesel_Delete_Event;

   procedure On_Open_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Gtk.Widget.Hide (Gtk_Widget (Open_Filesel));
      Free (Current_Filename);
      Current_Filename := new String' (Get_Filename (Open_Filesel));
      Real_Open_File (Current_Filename.all);
   end On_Open_Filesel_Ok_Button_Clicked;

   procedure On_Open_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Gtk.Widget.Hide (Gtk.Widget.Get_Toplevel (Gtk_Widget (Object)));
   end On_Open_Filesel_Cancel_Button_Clicked;

   procedure On_About_Ok_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Hide (About);
   end On_About_Ok_Clicked;

   procedure On_Save_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record) is
   begin
      Gtk.Widget.Hide (Gtk.Widget.Get_Toplevel (Gtk_Widget (Object)));
   end On_Save_Filesel_Delete_Event;

   procedure On_Save_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Gtk.Widget.Hide (Gtk_Widget (Save_Filesel));
      Free (Current_Filename);
      Current_Filename := new String' (Get_Filename (Save_Filesel));
      Real_Open_File (Current_Filename.all);
   end On_Save_Filesel_Ok_Button_Clicked;

   procedure On_Save_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record) is
   begin
      Gtk.Widget.Hide (Gtk.Widget.Get_Toplevel (Gtk_Widget (Object)));
   end On_Save_Filesel_Cancel_Button_Clicked;

end Callbacks_Gladeedit;
