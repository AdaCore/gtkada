-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  This widget represents one of the lines in a menu, on which the user can
--  click to execute an action. The menu items can be bound to a submenu, so
--  that clicking on them will in fact display the submenu on the screen.
--
--  They can also be associated with key shortcuts (called accelerators). See
--  the subprogram Set_Accel_Path, and the subprograms in the package
--  Gtk.Accel_Map.
--
--  Activating the proper options in the theme files will allow the user to
--  interactively modify the shortcuts.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Gtk.Item;
with Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with private;
   type Gtk_Menu_Item is access all Gtk_Menu_Item_Record'Class;

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item; Label : String := "");

   procedure Gtk_New_With_Mnemonic
     (Menu_Item : out Gtk_Menu_Item;
      Label     : String);
   --  Create a new Gtk_Menu_Item containing a label.
   --  The label is created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the menu item.

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
   --  Set the submenu underneath Menu_Item.

   function Get_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record) return Gtk.Widget.Gtk_Widget;
   --  Get the submenu underneath this menu item, if any, null otherwise.

   procedure Remove_Submenu (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Gtk_Select (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Deselect (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Activate (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Right_Justify (Menu_Item : access Gtk_Menu_Item_Record);
   --  Deprecated. Use Set_Right_Justified with Justify = True instead.

   procedure Set_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean := True);

   function Get_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record) return Boolean;

   procedure Set_Right_Justify
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean) renames Set_Right_Justified;
   --  This procedure is needed by Gate to automate the code generation.

   procedure Set_Accel_Path
     (Menu_Item  : access Gtk_Menu_Item_Record;
      Accel_Path : String);
   --  Set the path that will be used to reference the widget in calls to the
   --  subprograms in Gtk.Accel_Map. This means, for instance, that the widget
   --  is fully setup for interactive modification of the shortcuts by the
   --  user, should he choose to activate this possibility in his themes (see
   --  gtk-accel_map.ads for more information).

   -------------
   -- Signals --
   -------------

   --  activate
   --  activate_item
   --  toggle_size_request
   --  toggle_size_allocate

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
--  procedure Toggle_Size_Request
--    (Menu_Item   : access Gtk_Menu_Item_Record;
--     Requisition : int*);
--  Emit the signal "toggle_size_request"

--  procedure Toggle_Size_Allocate
--    (Menu_Item  : access Gtk_Menu_Item_Record;
--     Allocation : Gint);
--  Emit the signal "toggle_size_allocate"

