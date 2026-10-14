------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2006-2026, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Glib;                use Glib;
with Glib.Cancellable;    use Glib.Cancellable;
with Glib.Error;
with Glib.GFile;
with Glib.List_Store;      use Glib.List_Store;
with Glib.Object;         use Glib.Object;
with Glib.Types;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.File_Dialog;     use Gtk.File_Dialog;
with Gtk.File_Filter;     use Gtk.File_Filter;
with Gtk.File_Launcher;   use Gtk.File_Launcher;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Label;           use Gtk.Label;
with Gtk.Uri_Launcher;    use Gtk.Uri_Launcher;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;

package body Create_File_Chooser is
   use type Glib.GFile.Gfile;
   use type Glib.Error.GError;

   type Operation is (Open_File, Save_File, Select_Folder, Launch_File,
                      Show_Folder, Launch_Uri);
   type Picker_State is record
      Dialog   : Gtk_File_Dialog;
      Launcher : Gtk_File_Launcher;
      Uri      : Gtk_Uri_Launcher;
      Cancel   : Gcancellable;
      File     : Glib.GFile.Gfile := Glib.GFile.Null_Gfile;
      Status   : Gtk_Label;
      Controls : Gtk_Box;
      Stop     : Gtk_Button;
      Address  : Gtk_Entry;
      Current  : Operation := Open_File;
      Busy     : Boolean := False;
   end record;
   type Picker_State_Access is access Picker_State;
   package Picker_Data is new Glib.Object.User_Data (Picker_State_Access);
   package Owners is new Glib.Object.User_Data (GObject);
   procedure Free_State is new Ada.Unchecked_Deallocation
     (Picker_State, Picker_State_Access);

   procedure Release_File (File : Glib.GFile.Gfile) is
   begin
      if File /= Glib.GFile.Null_Gfile then
         Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      end if;
   end Release_File;

   procedure Release_State (State : Picker_State_Access) is
      Owned : Picker_State_Access := State;
   begin
      Release_File (State.File);
      State.Dialog.Unref;
      State.Launcher.Unref;
      State.Uri.Unref;
      State.Cancel.Unref;
      Free_State (Owned);
   end Release_State;

   function Help_Button return String is
   begin
      return "Gtk_File_Dialog asynchronously selects a file, save destination,"
        & " or folder. Save selects a name; it does not write a file."
        & ASCII.LF
        & "Gtk_File_Launcher opens the selected file with a desktop application"
        & " or shows its containing folder. Gtk_Uri_Launcher opens the entered"
        & " URI with its registered application. Launching depends on the"
        & " desktop's installed applications and portal support."
        & ASCII.LF
        & "Cancel stops the pending operation. Leaving this page also cancels"
        & " it. Failed or cancelled picks keep the previous selection.";
   end Help_Button;

   procedure Complete (Source : access GObject_Record'Class;
                       Error : Glib.Error.GError) is
      Owner : constant GObject := Owners.Get (Source);
      State : constant Picker_State_Access := Picker_Data.Get (Owner);
   begin
      if Error /= null then
         if Gtk_Box (Owner).Get_Mapped then
            State.Status.Set_Text (Glib.Error.Get_Message (Error));
         end if;
         Glib.Error.Error_Free (Error);
      end if;
      State.Busy := False;
      State.Controls.Set_Sensitive (True);
      State.Stop.Set_Sensitive (False);
      --  Each request holds the page until its completion, even when the
      --  selector has removed it. It cannot update a replacement page.
      Owner.Unref;
   end Complete;

   procedure On_Picked
     (Source : access GObject_Record'Class; Res : G_Async_Result)
   is
      Owner : constant GObject := Owners.Get (Source);
      State : constant Picker_State_Access := Picker_Data.Get (Owner);
      Error : Glib.Error.GError;
      File  : Glib.GFile.Gfile;
   begin
      case State.Current is
         when Open_File => File := State.Dialog.Open_Finish (Res, Error);
         when Save_File => File := State.Dialog.Save_Finish (Res, Error);
         when Select_Folder =>
            File := State.Dialog.Select_Folder_Finish (Res, Error);
         when others => raise Program_Error;
      end case;
      if Error = null and File /= Glib.GFile.Null_Gfile then
         Release_File (State.File);
         State.File := File;
         State.Launcher.Set_File (File);
         if Gtk_Box (Owner).Get_Mapped then
            State.Status.Set_Text
              ("Selected: " & Glib.GFile.Get_Parse_Name (File));
         end if;
      end if;
      Complete (Source, Error);
   end On_Picked;

   procedure On_Launched
     (Source : access GObject_Record'Class; Res : G_Async_Result)
   is
      State : constant Picker_State_Access :=
        Picker_Data.Get (Owners.Get (Source));
      Error : Glib.Error.GError;
      Success : Boolean;
   begin
      case State.Current is
         when Launch_File =>
            Success := State.Launcher.Launch_Finish (Res, Error);
         when Show_Folder =>
            Success := State.Launcher.Open_Containing_Folder_Finish
              (Res, Error);
         when Launch_Uri => Success := State.Uri.Launch_Finish (Res, Error);
         when others => raise Program_Error;
      end case;
      if Success and Gtk_Box (Owners.Get (Source)).Get_Mapped then
         State.Status.Set_Text ("Launch request completed.");
      end if;
      Complete (Source, Error);
   end On_Launched;

   procedure Start (Object : access GObject_Record'Class; Kind : Operation) is
      State : constant Picker_State_Access := Picker_Data.Get (Object);
      Parent : constant Gtk_Window :=
        Gtk_Window (Gtk_Box (Object).Get_Ancestor (Gtk.Window.Get_Type));
   begin
      if State.Busy then
         return;
      end if;
      if Kind in Launch_File | Show_Folder
        and then State.File = Glib.GFile.Null_Gfile
      then
         State.Status.Set_Text ("Select a file or folder first.");
         return;
      end if;
      State.Current := Kind;
      State.Busy := True;
      State.Cancel.Reset;
      State.Controls.Set_Sensitive (False);
      State.Stop.Set_Sensitive (True);
      State.Status.Set_Text ("Waiting for completion...");
      Object.Ref;
      case Kind is
         when Open_File =>
            State.Dialog.Set_Title ("Open a file");
            State.Dialog.Open (Parent, State.Cancel, On_Picked'Access);
         when Save_File =>
            State.Dialog.Set_Title ("Choose a save destination");
            State.Dialog.Set_Initial_Name ("example.adb");
            State.Dialog.Save (Parent, State.Cancel, On_Picked'Access);
         when Select_Folder =>
            State.Dialog.Set_Title ("Select a folder");
            State.Dialog.Select_Folder
              (Parent, State.Cancel, On_Picked'Access);
         when Launch_File =>
            State.Launcher.Launch (Parent, State.Cancel, On_Launched'Access);
         when Show_Folder =>
            State.Launcher.Open_Containing_Folder
              (Parent, State.Cancel, On_Launched'Access);
         when Launch_Uri =>
            State.Uri.Set_Uri (State.Address.Get_Text);
            State.Uri.Launch (Parent, State.Cancel, On_Launched'Access);
      end case;
   end Start;

   procedure Open_Clicked (Object : access GObject_Record'Class) is
   begin
      Start (Object, Open_File);
   end Open_Clicked;
   procedure Save_Clicked (Object : access GObject_Record'Class) is
   begin
      Start (Object, Save_File);
   end Save_Clicked;
   procedure Folder_Clicked (Object : access GObject_Record'Class) is
   begin
      Start (Object, Select_Folder);
   end Folder_Clicked;
   procedure Launch_Clicked (Object : access GObject_Record'Class) is
   begin
      Start (Object, Launch_File);
   end Launch_Clicked;
   procedure Show_Clicked (Object : access GObject_Record'Class) is
   begin
      Start (Object, Show_Folder);
   end Show_Clicked;
   procedure Uri_Clicked (Object : access GObject_Record'Class) is
   begin
      Start (Object, Launch_Uri);
   end Uri_Clicked;
   procedure Cancel_Clicked (Object : access GObject_Record'Class) is
   begin
      Picker_Data.Get (Object).Cancel.Cancel;
   end Cancel_Clicked;
   procedure On_Unmap (Widget : access Gtk_Widget_Record'Class) is
   begin
      Picker_Data.Get (Widget).Cancel.Cancel;
   end On_Unmap;

   procedure Run_Button (Frame : access Gtk_Frame_Record'Class) is
      Box, Row : Gtk_Box;
      State : constant Picker_State_Access := new Picker_State;
      Filters : Glib.List_Store.Glist_Store;
      All_Files, Ada_Files : Gtk_File_Filter;

      procedure Add_Button
        (Label : String; Call : Gtk.Button.Cb_GObject_Void) is
         Button : Gtk_Button;
      begin
         Gtk_New (Button, Label);
         Button.On_Clicked (Call, Slot => Box);
         Row.Append (Button);
      end Add_Button;
   begin
      Frame.Set_Label ("Files and URIs");
      Gtk_New (Box, Orientation_Vertical, 12);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);
      Gtk_New (State.Dialog);
      Gtk_New (State.Launcher, Glib.GFile.Null_Gfile);
      Gtk_New (State.Uri, "https://www.gtk.org/");
      G_New (State.Cancel);
      Picker_Data.Set (Box, State, On_Destroyed => Release_State'Access);
      Owners.Set (State.Dialog, GObject (Box));
      Owners.Set (State.Launcher, GObject (Box));
      Owners.Set (State.Uri, GObject (Box));
      Box.On_Unmap (On_Unmap'Access);

      Glib.List_Store.G_New (Filters, Gtk.File_Filter.Get_Type);
      Gtk_New (All_Files);
      All_Files.Set_Name ("All files");
      All_Files.Add_Pattern ("*");
      Filters.Append (All_Files);
      Gtk_New (Ada_Files);
      Ada_Files.Set_Name ("Ada sources");
      Ada_Files.Add_Pattern ("*.adb");
      Ada_Files.Add_Pattern ("*.ads");
      Filters.Append (Ada_Files);
      State.Dialog.Set_Filters (+Filters);
      State.Dialog.Set_Default_Filter (All_Files);
      All_Files.Unref;
      Ada_Files.Unref;
      Filters.Unref;

      Gtk_New (State.Controls, Orientation_Vertical, 6);
      Box.Append (State.Controls);
      Gtk_New (Row, Orientation_Horizontal, 6);
      State.Controls.Append (Row);
      Add_Button ("Open file...", Open_Clicked'Access);
      Add_Button ("Save as...", Save_Clicked'Access);
      Add_Button ("Select folder...", Folder_Clicked'Access);
      Gtk_New (Row, Orientation_Horizontal, 6);
      State.Controls.Append (Row);
      Add_Button ("Launch selected file", Launch_Clicked'Access);
      Add_Button ("Show containing folder", Show_Clicked'Access);
      Gtk_New (State.Address);
      State.Address.Set_Text ("https://www.gtk.org/");
      State.Controls.Append (State.Address);
      Gtk_New (Row, Orientation_Horizontal, 6);
      State.Controls.Append (Row);
      Add_Button ("Launch URI", Uri_Clicked'Access);
      Gtk_New (State.Stop, "Cancel operation");
      State.Stop.Set_Sensitive (False);
      State.Stop.On_Clicked (Cancel_Clicked'Access, Slot => Box);
      Box.Append (State.Stop);
      Gtk_New (State.Status, "Select a file, save destination or folder.");
      State.Status.Set_Wrap (True);
      Box.Append (State.Status);
   end Run_Button;
end Create_File_Chooser;
