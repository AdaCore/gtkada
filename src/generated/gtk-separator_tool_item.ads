-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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
--  This package defines a separator widget that can be inserted in a toolbar,
--  to create groups of widgets in the latter.
-- 
--  </description>
--  <group>Menus and Toolbars</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Gtk.Tool_Item;   use Gtk.Tool_Item;

package Gtk.Separator_Tool_Item is

   type Gtk_Separator_Tool_Item_Record is new Gtk_Tool_Item_Record with null record;
   type Gtk_Separator_Tool_Item is access all Gtk_Separator_Tool_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Separator_Tool_Item);
   procedure Initialize (Self : access Gtk_Separator_Tool_Item_Record'Class);
   --  Create a new Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item
   --  Since: gtk+ 2.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_separator_tool_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Draw
      (Self : access Gtk_Separator_Tool_Item_Record) return Boolean;
   procedure Set_Draw
      (Self : access Gtk_Separator_Tool_Item_Record;
       Draw : Boolean);
   --  Whether Item is drawn as a vertical line, or just blank. Setting this
   --  to %FALSE along with gtk_tool_item_set_expand is useful to create an
   --  item that forces following items to the end of the toolbar.
   --  Since: gtk+ 2.4
   --  "draw": whether Item is drawn as a vertical line

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   -- 
   --  Name: Draw_Property
   --  Type: Boolean
   --  Flags: read-write

   Draw_Property : constant Glib.Properties.Property_Boolean;

private
   Draw_Property : constant Glib.Properties.Property_Boolean:=
     Glib.Properties.Build ("draw");
end Gtk.Separator_Tool_Item;
