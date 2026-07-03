------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Represents items in a cell in [classGtk.ColumnView].
--
--  The `GtkColumnViewCell`s are managed by the [classGtk.ColumnView] widget
--  (with its factory) and cannot be created by applications, but they need to
--  be populated by application code. This is done by calling
--  [methodGtk.ColumnViewCell.set_child].
--
--  `GtkColumnViewCell`s exist in 2 stages:
--
--  1. The unbound stage where the listitem is not currently connected to an
--  item in the list. In that case, the [propertyGtk.ColumnViewCell:item]
--  property is set to null.
--
--  2. The bound stage where the listitem references an item from the list.
--  The [propertyGtk.ColumnViewCell:item] property is not null.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Gtk.List_Item;   use Gtk.List_Item;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Column_View_Cell is

   type Gtk_Column_View_Cell_Record is new Gtk_List_Item_Record with null record;
   type Gtk_Column_View_Cell is access all Gtk_Column_View_Cell_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_column_view_cell_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Column_View_Cell_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child previously set via Gtk.Column_View_Cell.Set_Child or
   --  null if none was set.
   --  Since: gtk+ 4.12
   --  @return The child

   procedure Set_Child
      (Self  : not null access Gtk_Column_View_Cell_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child to be used for this listitem.
   --  This function is typically called by applications when setting up a
   --  listitem so that the widget can be reused when binding it multiple
   --  times.
   --  Since: gtk+ 4.12
   --  @param Child The list item's child or null to unset

   function Get_Focusable
      (Self : not null access Gtk_Column_View_Cell_Record) return Boolean;
   --  Checks if a list item has been set to be focusable via
   --  Gtk.Column_View_Cell.Set_Focusable.
   --  Since: gtk+ 4.12
   --  @return True if the item is focusable

   procedure Set_Focusable
      (Self      : not null access Gtk_Column_View_Cell_Record;
       Focusable : Boolean);
   --  Sets Self to be focusable.
   --  If an item is focusable, it can be focused using the keyboard. This
   --  works similar to [methodGtk.Widget.set_focusable].
   --  Note that if items are not focusable, the keyboard cannot be used to
   --  activate them and selecting only works if one of the listitem's children
   --  is focusable.
   --  By default, list items are focusable.
   --  Since: gtk+ 4.12
   --  @param Focusable if the item should be focusable

   function Get_Item
      (Self : not null access Gtk_Column_View_Cell_Record)
       return System.Address;
   --  Gets the model item that associated with Self.
   --  If Self is unbound, this function returns null.
   --  Since: gtk+ 4.12
   --  @return The item displayed

   function Get_Position
      (Self : not null access Gtk_Column_View_Cell_Record) return Guint;
   --  Gets the position in the model that Self currently displays.
   --  If Self is unbound, GTK_INVALID_LIST_POSITION is returned.
   --  Since: gtk+ 4.12
   --  @return The position of this item

   function Get_Selected
      (Self : not null access Gtk_Column_View_Cell_Record) return Boolean;
   --  Checks if the item is displayed as selected.
   --  The selected state is maintained by the list widget and its model and
   --  cannot be set otherwise.
   --  Since: gtk+ 4.12
   --  @return True if the item is selected.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  Widget used for display.

   Focusable_Property : constant Glib.Properties.Property_Boolean;
   --  If the item can be focused with the keyboard.

   Item_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Object.GObject
   --  Displayed item.

   Position_Property : constant Glib.Properties.Property_Uint;
   --  Position of the item.

   Selected_Property : constant Glib.Properties.Property_Boolean;
   --  If the item is currently selected.

private
   Selected_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selected");
   Position_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("position");
   Item_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("item");
   Focusable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focusable");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Column_View_Cell;
