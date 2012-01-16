------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--  This package declares an abstract type, parent of several widgets in
--  GtkAda.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Abstract base classes</group>

with Gtk.Bin;

package Gtk.Item is

   type Gtk_Item_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Item is access all Gtk_Item_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Item.

   procedure Item_Select (Item : access Gtk_Item_Record);
   --  Emits the "select" signal on Item

   procedure Item_Deselect (Item : access Gtk_Item_Record);
   --  Emits the "deselect" signal on item

   procedure Toggle (Item : access Gtk_Item_Record);
   --  Emits the "toggle" signal on item

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "select"
   --    procedure Handler (Item : access Gtk_Item_Record'Class);
   --    Emitted when the item is selected
   --
   --  - "deselect"
   --    procedure Handler (Item : access Gtk_Item_Record'Class);
   --    Emitted when the item is deselected
   --
   --  - "toggle"
   --    procedure Handler (Item : access Gtk_Item_Record'Class);
   --    Emitted when the item is toggled
   --  </signals>

   Signal_Deselect : constant Glib.Signal_Name := "deselect";
   Signal_Select   : constant Glib.Signal_Name := "select";
   Signal_Toggle   : constant Glib.Signal_Name := "toggle";

private
   type Gtk_Item_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

   pragma Import (C, Get_Type, "gtk_item_get_type");
end Gtk.Item;
