-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
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
--  This package defines a separator widget that can be inserted in a
--  toolbar, to create groups of widgets in the latter.
--  </description>
--  <c_version>2.8.17</c_version>

with Glib.Properties;
with Gtk.Tool_Item;

package Gtk.Separator_Tool_Item is

   type Gtk_Separator_Tool_Item_Record is
     new Gtk.Tool_Item.Gtk_Tool_Item_Record with null record;
   type Gtk_Separator_Tool_Item
     is access all Gtk_Separator_Tool_Item_Record'Class;

   procedure Gtk_New
     (Separator : out Gtk_Separator_Tool_Item);
   procedure Initialize
     (Separator : access Gtk_Separator_Tool_Item_Record'Class);
   --  Create or initialize a new separator

   function Get_Type return GType;
   --  Return the internal type used for separators

   procedure Set_Draw
     (Item : access Gtk_Separator_Tool_Item_Record;
      Draw : Boolean);
   function Get_Draw
     (Item : access Gtk_Separator_Tool_Item_Record)
      return Boolean;
   --  Sets whether the separator is drawn as a vertical line, or just a
   --  blank.
   --  Settings this to False along with using Gtk.Tool_Item.Set_Expand is
   --  useful to create an item that forces the following items to the end of
   --  the toolbar.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Draw_Property
   --  Type: Boolean
   --  See:  Set_Draw / Get_Draw
   --
   --  </properties>

   Draw_Property : constant Glib.Properties.Property_Boolean;

private
   Draw_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw");

   pragma Import (C, Get_Type, "gtk_separator_tool_item_get_type");
end Gtk.Separator_Tool_Item;
