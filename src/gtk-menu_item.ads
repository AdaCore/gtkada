-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

--  <c_version>1.3.6</c_version>

with Gtk.Enums;
with Gtk.Item;
with Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with private;
   type Gtk_Menu_Item is access all Gtk_Menu_Item_Record'Class;

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item; Label : String := "");

   procedure Gtk_New_With_Mnemonic
     (Menu_Item : out Gtk_Menu_Item;
      Label     : String);
   --  Create a new Gtk_Menu_Item containing a label. The label is created
   --  using Gtk.Label.Gtk_New_With_Mnemonic, so underscores in Label indicate
   --  the mnemonic for the menu item.

   procedure Initialize
     (Menu_Item : access Gtk_Menu_Item_Record'Class; Label : String);
   --  Internal initialization procedure.

   procedure Initialize_With_Mnemonic
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Label     : String);
   --  Internal initialization procedure.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu_Item.

   procedure Set_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record;
      Submenu   : access Widget.Gtk_Widget_Record'Class);

   procedure Remove_Submenu (Menu_Item : access Gtk_Menu_Item_Record);

   function Get_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record) return Gtk.Widget.Gtk_Widget;

   procedure Set_Placement
     (Menu_Item : access Gtk_Menu_Item_Record;
      Placement : Enums.Gtk_Submenu_Placement);

   procedure Gtk_Select (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Deselect (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Activate (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Right_Justify (Menu_Item : access Gtk_Menu_Item_Record);
   --  Deprecated. Use Set_Right_Justified with Justify = True instead.

   procedure Set_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean);

   procedure Set_Right_Justify
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean);
   --  Call Right_Justify when Justify. Noop otherwise
   --  This procedure is needed by Gate to automate the code generation.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with null record;

   pragma Import (C, Get_Type, "gtk_menu_item_get_type");
end Gtk.Menu_Item;

--  missing:
--  Toggle_Size_Request
--  Toggle_Size_Allocate
