------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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
--  This package contains a special type of menu item, which is displayed as
--  a hashed line, and which is used to tear off a menu (ie detach it from the
--  menu bar, and into its own toplevel window, so that the user can keep it
--  visible at all time).
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Gtk.Menu_Item;

package Gtk.Tearoff_Menu_Item is

   type Gtk_Tearoff_Menu_Item_Record is
     new Menu_Item.Gtk_Menu_Item_Record with private;
   type Gtk_Tearoff_Menu_Item is access all Gtk_Tearoff_Menu_Item_Record'Class;

   procedure Gtk_New (Menu_Item : out Gtk_Tearoff_Menu_Item);
   procedure Initialize
     (Menu_Item : access Gtk_Tearoff_Menu_Item_Record'Class);
   --  Creates or Initializes a menu item

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Tearoff_Menu_Item.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Tearoff_Menu_Item_Record is new Menu_Item.Gtk_Menu_Item_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_tearoff_menu_item_get_type");
end Gtk.Tearoff_Menu_Item;
