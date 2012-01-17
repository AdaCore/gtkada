------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

--  <description>
--  Gtk_File_Chooser_Dialog is a dialog box suitable for use with "File/Open"
--  or "File/Save as" commands. This widget works by putting a
--  Gtk_File_Chooser_Widget inside a Gtk_Dialog. It exposes the
--  Gtk_File_Chooser interface, so you can use all of the Gtk_File_Chooser
--  functions on the file chooser dialog as well as those for GtkDialog.
--
--  Note that Gtk_File_Chooser_Dialog does not have any methods of its own.
--  Instead, you should use the functions that work on a Gtk_File_Chooser.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>
--  <screenshot>filechooser.png</screenshot>

with Glib.Types;
with Gtk.Dialog;
with Gtk.Enums;
with Gtk.File_Chooser;
with Gtk.Window;

package Gtk.File_Chooser_Dialog is

   type Gtk_File_Chooser_Dialog_Record is
     new Gtk.Dialog.Gtk_Dialog_Record with null record;
   type Gtk_File_Chooser_Dialog is
     access all Gtk_File_Chooser_Dialog_Record'Class;

   function Get_Type return GType;
   --  Return the internal value asociated with a Gtk_File_Chooser_Button

   procedure Gtk_New
     (Dialog            : out Gtk_File_Chooser_Dialog;
      Title             : String;
      Parent            : Gtk.Window.Gtk_Window;
      Action            : Gtk.Enums.Gtk_File_Chooser_Action);
   procedure Initialize
     (Dialog            : access Gtk_File_Chooser_Dialog_Record'Class;
      Title             : String;
      Parent            : Gtk.Window.Gtk_Window;
      Action            : Gtk.Enums.Gtk_File_Chooser_Action);
   --  Creates a new file chooser dialog

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_File_Chooser"

   package Implements_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser,
      Gtk_File_Chooser_Dialog_Record, Gtk_File_Chooser_Dialog);
   function "+"
     (Dialog : access Gtk_File_Chooser_Dialog_Record'Class)
      return Gtk.File_Chooser.Gtk_File_Chooser
      renames Implements_File_Chooser.To_Interface;
   function "-"
     (File : Gtk.File_Chooser.Gtk_File_Chooser)
      return Gtk_File_Chooser_Dialog
      renames Implements_File_Chooser.To_Object;
   --  Converts to and from the Gtk_File_Chooser interface

private
   pragma Import (C, Get_Type, "gtk_file_chooser_dialog_get_type");
end Gtk.File_Chooser_Dialog;

--  Binding is done through our own C functions:
--  No binding: gtk_file_chooser_dialog_new
--  No binding: gtk_file_chooser_dialog_new_with_backend
