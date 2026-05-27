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

--  Used by list widgets to represent items in a [ifaceGio.ListModel].
--
--  `GtkListItem` objects are managed by the list widget (with its factory)
--  and cannot be created by applications, but they need to be populated by
--  application code. This is done by calling [methodGtk.ListItem.set_child].
--
--  `GtkListItem` objects exist in 2 stages:
--
--  1. The unbound stage where the listitem is not currently connected to an
--  item in the list. In that case, the [propertyGtk.ListItem:item] property is
--  set to `NULL`.
--
--  2. The bound stage where the listitem references an item from the list.
--  The [propertyGtk.ListItem:item] property is not `NULL`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.List_Item is

   type Gtk_List_Item_Record is new GObject_Record with null record;
   type Gtk_List_Item is access all Gtk_List_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Accessible_Description
      (Self : not null access Gtk_List_Item_Record) return UTF8_String;
   --  Gets the accessible description of Self.
   --  Since: gtk+ 4.12
   --  @return the accessible description

   procedure Set_Accessible_Description
      (Self        : not null access Gtk_List_Item_Record;
       Description : UTF8_String);
   --  Sets the accessible description for the listitem.
   --  The accessible description may be used by e.g. screen readers.
   --  Since: gtk+ 4.12
   --  @param Description the description

   function Get_Accessible_Label
      (Self : not null access Gtk_List_Item_Record) return UTF8_String;
   --  Gets the accessible label of Self.
   --  Since: gtk+ 4.12
   --  @return the accessible label

   procedure Set_Accessible_Label
      (Self  : not null access Gtk_List_Item_Record;
       Label : UTF8_String);
   --  Sets the accessible label for the listitem.
   --  The accessible label may be used by e.g. screen readers.
   --  Since: gtk+ 4.12
   --  @param Label the label

   function Get_Activatable
      (Self : not null access Gtk_List_Item_Record) return Boolean;
   --  Checks if a listitem has been set to be activatable via
   --  [methodGtk.ListItem.set_activatable].
   --  @return true if the item is activatable

   procedure Set_Activatable
      (Self        : not null access Gtk_List_Item_Record;
       Activatable : Boolean);
   --  Sets Self to be activatable.
   --  If an item is activatable, double-clicking on the item, using the
   --  Return key or calling [methodGtk.Widget.activate] will activate the
   --  item. Activating instructs the containing view to handle activation.
   --  `GtkListView` for example will be emitting the
   --  [signalGtk.ListView::activate] signal.
   --  By default, listitems are activatable.
   --  @param Activatable if the item should be activatable

   function Get_Child
      (Self : not null access Gtk_List_Item_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child previously set via [methodGtk.ListItem.set_child] or
   --  `NULL` if none was set.
   --  @return The child

   procedure Set_Child
      (Self  : not null access Gtk_List_Item_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child to be used for this listitem.
   --  This function is typically called by applications when setting up a
   --  listitem so that the widget can be reused when binding it multiple
   --  times.
   --  @param Child The listitem's child or `NULL` to unset

   function Get_Focusable
      (Self : not null access Gtk_List_Item_Record) return Boolean;
   --  Checks if a listitem has been set to be focusable via
   --  [methodGtk.ListItem.set_focusable].
   --  Since: gtk+ 4.12
   --  @return true if the item is focusable

   procedure Set_Focusable
      (Self      : not null access Gtk_List_Item_Record;
       Focusable : Boolean);
   --  Sets Self to be focusable.
   --  If an item is focusable, it can be focused using the keyboard. This
   --  works similar to [methodGtk.Widget.set_focusable].
   --  Note that if items are not focusable, the keyboard cannot be used to
   --  activate them and selecting only works if one of the listitem's children
   --  is focusable.
   --  By default, listitems are focusable.
   --  Since: gtk+ 4.12
   --  @param Focusable if the item should be focusable

   function Get_Item
      (Self : not null access Gtk_List_Item_Record) return System.Address;
   --  Gets the model item that associated with Self.
   --  If Self is unbound, this function returns `NULL`.
   --  @return The item displayed

   function Get_Position
      (Self : not null access Gtk_List_Item_Record) return Guint;
   --  Gets the position in the model that Self currently displays.
   --  If Self is unbound, `GTK_INVALID_LIST_POSITION` is returned.
   --  @return The position of this item

   function Get_Selectable
      (Self : not null access Gtk_List_Item_Record) return Boolean;
   --  Checks if a listitem has been set to be selectable via
   --  [methodGtk.ListItem.set_selectable].
   --  Do not confuse this function with [methodGtk.ListItem.get_selected].
   --  @return true if the item is selectable

   procedure Set_Selectable
      (Self       : not null access Gtk_List_Item_Record;
       Selectable : Boolean);
   --  Sets Self to be selectable.
   --  If an item is selectable, clicking on the item or using the keyboard
   --  will try to select or unselect the item. If this succeeds is up to the
   --  model to determine, as it is managing the selected state.
   --  Note that this means that making an item non-selectable has no
   --  influence on the selected state at all. A non-selectable item may still
   --  be selected.
   --  By default, listitems are selectable. When rebinding them to a new
   --  item, they will also be reset to be selectable by GTK.
   --  @param Selectable if the item should be selectable

   function Get_Selected
      (Self : not null access Gtk_List_Item_Record) return Boolean;
   --  Checks if the item is displayed as selected.
   --  The selected state is maintained by the list widget and its model and
   --  cannot be set otherwise.
   --  @return true if the item is selected.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accessible_Description_Property : constant Glib.Properties.Property_String;
   --  The accessible description to set on the listitem.

   Accessible_Label_Property : constant Glib.Properties.Property_String;
   --  The accessible label to set on the listitem.

   Activatable_Property : constant Glib.Properties.Property_Boolean;
   --  If the item can be activated by the user.

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

   Selectable_Property : constant Glib.Properties.Property_Boolean;
   --  If the item can be selected by the user.

   Selected_Property : constant Glib.Properties.Property_Boolean;
   --  If the item is currently selected.

private
   Selected_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selected");
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Position_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("position");
   Item_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("item");
   Focusable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focusable");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activatable");
   Accessible_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accessible-label");
   Accessible_Description_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accessible-description");
end Gtk.List_Item;
