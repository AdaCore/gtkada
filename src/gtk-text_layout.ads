-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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
--  This package defines the Gtk_Text_Layout object.
--
--  This is a semi-private type, therefore only parts of the subprograms
--  associated to Gtk_Text_Layout are bound.
--  </description>

with Glib;
with Gtk.Text_Attributes;

package Gtk.Text_Layout is

   type Gtk_Text_Layout_Record is new Glib.Object.GObject_Record
     with private;
   type Gtk_Text_Layout is access all Gtk_Text_Layout_Record'Class;

   procedure Gtk_New (Layout : out Gtk_Text_Layout);
   --  Create a new Gtk_Text_Layout.

   procedure Initialize (Layout : access Gtk_Text_Layout_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal type associated to the Gtk_Text_Layout class.

   function Get_Default_Style (Layout : access Gtk_Text_Layout_Record)
     return Gtk.Text_Attributes.Gtk_Text_Attributes;
   --  Return the default style for the given Layout.

   procedure Set_Default_Style
     (Layout : access Gtk_Text_Layout_Record;
      Values : Gtk.Text_Attributes.Gtk_Text_Attributes);
   --  Set the default style for the given Layout.

   procedure Default_Style_Changed (Layout : access Gtk_Text_Layout_Record);
   --  Signal the layout that the default style has changed.

private

   type Gtk_Text_Layout_Record is new Glib.Object.GObject_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_text_layout_get_type");

end Gtk.Text_Layout;
