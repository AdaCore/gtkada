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

--  The type of item used by `GtkTreeListModel`.
--
--  It allows navigating the model as a tree and modify the state of rows.
--
--  `GtkTreeListRow` instances are created by a `GtkTreeListModel` only when
--  the [propertyGtk.TreeListModel:passthrough] property is not set.
--
--  There are various support objects that can make use of `GtkTreeListRow`
--  objects, such as the [classGtk.TreeExpander] widget that allows displaying
--  an icon to expand or collapse a row or [classGtk.TreeListRowSorter] that
--  makes it possible to sort trees properly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Tree_List_Row is

   type Gtk_Tree_List_Row_Record is new GObject_Record with null record;
   type Gtk_Tree_List_Row is access all Gtk_Tree_List_Row_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_list_row_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child_Row
      (Self     : not null access Gtk_Tree_List_Row_Record;
       Position : Guint) return Gtk_Tree_List_Row;
   --  If Self is not expanded or Position is greater than the number of
   --  children, null is returned.
   --  @param Position position of the child to get
   --  @return the child in Position

   function Get_Children
      (Self : not null access Gtk_Tree_List_Row_Record)
       return Glib.List_Model.Glist_Model;
   --  If the row is expanded, gets the model holding the children of Self.
   --  This model is the model created by the
   --  [callbackGtk.TreeListModelCreateModelFunc] and contains the original
   --  items, no matter what value [propertyGtk.TreeListModel:passthrough] is
   --  set to.
   --  @return The model containing the children

   function Get_Depth
      (Self : not null access Gtk_Tree_List_Row_Record) return Guint;
   --  Gets the depth of this row.
   --  Rows that correspond to items in the root model have a depth of zero,
   --  rows corresponding to items of models of direct children of the root
   --  model have a depth of 1 and so on.
   --  The depth of a row never changes until the row is removed from its
   --  model at which point it will forever return 0.
   --  @return The depth of this row

   function Get_Expanded
      (Self : not null access Gtk_Tree_List_Row_Record) return Boolean;
   --  Gets if a row is currently expanded.
   --  @return True if the row is expanded

   procedure Set_Expanded
      (Self     : not null access Gtk_Tree_List_Row_Record;
       Expanded : Boolean);
   --  Expands or collapses a row.
   --  If a row is expanded, the model of calling the
   --  [callbackGtk.TreeListModelCreateModelFunc] for the row's item will be
   --  inserted after this row. If a row is collapsed, those items will be
   --  removed from the model.
   --  If the row is not expandable, this function does nothing.
   --  @param Expanded True if the row should be expanded

   function Get_Item
      (Self : not null access Gtk_Tree_List_Row_Record)
       return Glib.Object.GObject;
   --  Gets the item corresponding to this row,
   --  Return has transfer-ownership='none'

   function Get_Parent
      (Self : not null access Gtk_Tree_List_Row_Record)
       return Gtk_Tree_List_Row;
   --  Gets the row representing the parent for Self.
   --  That is the row that would need to be collapsed to make this row
   --  disappear.
   --  If Self is a row corresponding to the root model, null is returned.
   --  The value returned by this function never changes until the row is
   --  removed from its model at which point it will forever return null.
   --  @return The parent of Self

   function Get_Position
      (Self : not null access Gtk_Tree_List_Row_Record) return Guint;
   --  Returns the position in the `GtkTreeListModel` that Self occupies at
   --  the moment.
   --  @return The position in the model

   function Is_Expandable
      (Self : not null access Gtk_Tree_List_Row_Record) return Boolean;
   --  Checks if a row can be expanded.
   --  This does not mean that the row is actually expanded, this can be
   --  checked with [methodGtk.TreeListRow.get_expanded].
   --  If a row is expandable never changes until the row is removed from its
   --  model at which point it will forever return False.
   --  @return True if the row is expandable

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Children_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The model holding the row's children.

   Depth_Property : constant Glib.Properties.Property_Uint;
   --  The depth in the tree of this row.

   Expandable_Property : constant Glib.Properties.Property_Boolean;
   --  If this row can ever be expanded.

   Expanded_Property : constant Glib.Properties.Property_Boolean;
   --  If this row is currently expanded.

   Item_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Object.GObject
   --  The item held in this row.

private
   Item_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("item");
   Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expanded");
   Expandable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expandable");
   Depth_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("depth");
   Children_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("children");
end Gtk.Tree_List_Row;
