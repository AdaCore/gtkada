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

package Gnome.Icon_Selection is

   type Gnome_Icon_Selection_Record is new
     Gtk.Box.Gtk_Vbox_Record with private;
   type Gnome_Icon_Selection is access all Gnome_Icon_Selection_Record'Class;

   procedure Gnome_New (Widget : out Gnome_Icon_Selection);

   procedure Initialize (Widget : access Gnome_Icon_Selection_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Add_Defaults (Gis : access Gnome_Icon_Selection_Record);

   procedure Add_Directory
     (Gis : access Gnome_Icon_Selection_Record;
      Dir : String);

   procedure Clear
     (Gis       : access Gnome_Icon_Selection_Record;
      Not_Shown : Boolean);

   procedure Select_Icon
     (Gis      : access Gnome_Icon_Selection_Record;
      Filename : String);

   procedure Show_Icons (Gis : access Gnome_Icon_Selection_Record);

   procedure Stop_Loading (Gis : access Gnome_Icon_Selection_Record);

private
   type Gnome_Icon_Selection_Record is new
     Gtk.Box.Gtk_Vbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_icon_selection_get_type");
end Gnome.Icon_Selection;
