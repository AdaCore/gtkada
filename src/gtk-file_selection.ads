-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A Gtk_File_Selection is a general widget to interactively select file.
--  It displays a dialog in which the user can navigate through directories,
--  select a file, and even manipulate files with operations like removing,
--  renaming,...
--  Currently, only one file can be selected in the dialog.
--  </description>
--  <c_version>1.2.7</c_version>

with Gtk.Object;
with Gtk.Box;
with Gtk.Button;
with Gtk.Widget;
with Gtk.Window;

package Gtk.File_Selection is

   type Gtk_File_Selection_Record is new Gtk.Window.Gtk_Window_Record
     with private;
   type Gtk_File_Selection is access all Gtk_File_Selection_Record'Class;

   ------------------------------
   -- Operations on the dialog --
   ------------------------------

   procedure Gtk_New (File_Selection : out Gtk_File_Selection;
                      Title          : in String);
   --  Create a new file selection dialog.
   --  Title is the name of the dialog, as displayed in its title bar.

   procedure Initialize
     (File_Selection : access Gtk_File_Selection_Record'Class;
      Title          : in String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_File_Selection.

   procedure Set_Filename (File_Selection  : access Gtk_File_Selection_Record;
                           Filename        : in String);
   --  Highlight the given file in the dialog.
   --  Note that this does not close the dialog.

   function Get_Filename (File_Selection : access Gtk_File_Selection_Record)
                         return String;
   --  Get the selected file name.

   procedure Complete (File_Selection : access Gtk_File_Selection_Record;
                       Pattern        : in String);
   --  Set the filter used to display the files.
   --  The pattern is displayed in the entry at the bottom of the dialog, and
   --  the list of files displayed in the list.

   procedure Show_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record);
   --  When this function is called, the dialog includes a series of buttons
   --  for file operations (create directory, rename a file, delete a file).

   procedure Hide_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record);
   --  Hide the buttons for file operations.

   procedure Set_Show_File_Op_Buttons
     (File_Selection : access Gtk_File_Selection_Record;
      Flag           : Boolean);
   --  Choose whether to display or not the file operation buttons.
   --  This button is kept for backward compatibility only.
   --
   --  If Flag is true, calls Show_Fileop_Buttons, otherwise calls
   --  Hide_Fileop_Buttons.

   ------------------------
   -- Getting the fields --
   ------------------------
   --  The following functions are provided to access the fields of the
   --  file selection dialog.
   --  This dialog is divided into two main areas, the Action_Area which is
   --  the top part that contains the list of files, and the button area which
   --  is the bottom part that contains the OK and Cancel buttons.

   function Get_Action_Area (File_Selection : access Gtk_File_Selection_Record)
                            return Gtk.Box.Gtk_Box;
   --  Return the action area.
   --  This is the area that contains the list of files, the filter entry,etc.

   function Get_Button_Area (File_Selection : access Gtk_File_Selection_Record)
                            return Gtk.Box.Gtk_Box;
   --  Return the button area.
   --  This is the area that contains the OK and Cancel buttons.

   function Get_Dir_List (File_Selection : access Gtk_File_Selection_Record)
                         return Gtk.Widget.Gtk_Widget;
   --  Return the list that display the list of directories.

   function Get_File_List (File_Selection : access Gtk_File_Selection_Record)
                          return Gtk.Widget.Gtk_Widget;
   --  Return the list that display the list of files in the selected directory

   function Get_Cancel_Button
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button;
   --  Return the Cancel button.
   --  To remove this button from the dialog, call Hide on the return value.
   --  The callbacks on this button should simply close the dialog, but should
   --  ignore the file selected by the user.

   function Get_Help_Button (File_Selection : access Gtk_File_Selection_Record)
                            return Gtk.Button.Gtk_Button;
   --  Return the Help button.
   --  To remove this button from the dialog, call Hide on the return value.
   --  The callbacks on this button should display a new dialog that explain
   --  what file the user should select.

   function Get_Ok_Button (File_Selection : access Gtk_File_Selection_Record)
                          return Gtk.Button.Gtk_Button;
   --  Return the OK button.
   --  The callbacks on this button should close the dialog and do something
   --  with the file selected by the user.

   function Get_History_Pulldown
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget;
   --  Return the menu that display the history of directories
   --  for easy access by the user.

   function Get_Selection_Entry
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget;
   --  Return the entry used to set the filter on the list of directories.
   --  The simplest way to insert text in this entry is to use the
   --  Complete procedure above.

   function Get_Selection_Text
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget;
   --  Return the text displayed just above the Selection_Entry.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (File_Selection : in out Gtk.Object.Gtk_Object;
                       N : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_File_Selection_Record is new Gtk.Window.Gtk_Window_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_file_selection_get_type");
end Gtk.File_Selection;
