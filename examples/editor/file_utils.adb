with Glib; use Glib;
with Gdk.Window;
with Gdk.Font; use Gdk.Font;
with Gdk.Color; use Gdk.Color;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Text; use Gtk.Text;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Unchecked_Deallocation;
with Main_Window_Pkg; use Main_Window_Pkg;
with Open_File_Selection_Pkg; use Open_File_Selection_Pkg;
with Save_File_Selection_Pkg; use Save_File_Selection_Pkg;
with Ada.Text_IO; use Ada.Text_IO;

package body File_Utils is

   Id : Message_Id;

   procedure New_File is
   begin
      Delete_Text (Main_Window.Text1, 0, -1);
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

      if Open_File_Selection = null then
         Gtk_New (Open_File_Selection);
      end if;

      Show_All (Open_File_Selection);
      Gdk.Window.Gdk_Raise (Get_Window (Open_File_Selection));
   end Open_File;

   procedure Save_As is
   begin
      if Save_File_Selection = null then
         Gtk_New (Save_File_Selection);
      end if;

      --  If the current document has a filename we use that as the default

      if Current_Filename /= null then
         Set_Filename (Save_File_Selection, Current_Filename.all);
      end if;

      Show_All (Save_File_Selection);
      Gdk.Window.Gdk_Raise (Get_Window (Save_File_Selection));
   end Save_As;

   procedure Real_Open_File (Filename : String) is
      Fp : File_Descriptor;
      Buffer : String (1 .. Buffer_Size);
      Bytes_Read : Integer;
      Local_File : aliased String := Filename;
      File       : aliased String := Local_File & ASCII.Nul;

   begin
      Gtk.Status_Bar.Pop (Main_Window.Statusbar1, 1);
      Gtk.Text.Freeze (Main_Window.Text1);
      Delete_Text (Main_Window.Text1, 0, -1);
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

      Free (Current_Filename);
      Current_Filename := new String'(Local_File);
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

end File_Utils;
