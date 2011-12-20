------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk;
with Gtk.Box;
with Gtk.Widget;

package Gnome.File_Entry is

   type Gnome_File_Entry_Record is new Gtk.Box.Gtk_Hbox_Record with private;
   type Gnome_File_Entry is access all Gnome_File_Entry_Record'Class;

   procedure Gnome_New
     (Widget              : out Gnome_File_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String);

   procedure Initialize
     (Widget              : access Gnome_File_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Gtk_Entry
     (Fentry : access Gnome_File_Entry_Record) return Gtk.Widget.Gtk_Widget;

   function Gnome_Entry
     (Fentry : access Gnome_File_Entry_Record) return Gtk.Widget.Gtk_Widget;

   function Get_Full_Path
     (Fentry          : access Gnome_File_Entry_Record;
      File_Must_Exist : Boolean)
      return String;

   procedure Set_Default_Path
     (Fentry : access Gnome_File_Entry_Record;
      Path   : String);

   procedure Set_Directory
     (Fentry          : access Gnome_File_Entry_Record;
      Directory_Entry : Boolean);

   procedure Set_Modal
     (Fentry   : access Gnome_File_Entry_Record;
      Is_Modal : Boolean);

   procedure Set_Title
     (Fentry              : access Gnome_File_Entry_Record;
      Browse_Dialog_Title : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "browse_clicked"
   --    procedure Handler (Widget : access Gnome_File_Entry_Record'Class);
   --
   --  </signals>

private
   type Gnome_File_Entry_Record is new
     Gtk.Box.Gtk_Hbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_file_entry_get_type");
end Gnome.File_Entry;
