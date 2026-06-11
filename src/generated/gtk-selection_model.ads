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

--  An interface that adds support for selection to list models.
--
--  This support is then used by widgets using list models to add the ability
--  to select and unselect various items.
--
--  GTK provides default implementations of the most common selection modes
--  such as [classGtk.SingleSelection], so you will only need to implement this
--  interface if you want detailed control about how selections should be
--  handled.
--
--  A `GtkSelectionModel` supports a single boolean per item indicating if an
--  item is selected or not. This can be queried via
--  [methodGtk.SelectionModel.is_selected]. When the selected state of one or
--  more items changes, the model will emit the
--  [signalGtk.SelectionModel::selection-changed] signal by calling the
--  [methodGtk.SelectionModel.selection_changed] function. The positions given
--  in that signal may have their selection state changed, though that is not a
--  requirement. If new items added to the model via the
--  [signalGio.ListModel::items-changed] signal are selected or not is up to
--  the implementation.
--
--  Note that items added via [signalGio.ListModel::items-changed] may already
--  be selected and no [signalGtk.SelectionModel::selection-changed] will be
--  emitted for them. So to track which items are selected, it is necessary to
--  listen to both signals.
--
--  Additionally, the interface can expose functionality to select and
--  unselect items. If these functions are implemented, GTK's list widgets will
--  allow users to select and unselect items. However, `GtkSelectionModel`s are
--  free to only implement them partially or not at all. In that case the
--  widgets will not support the unimplemented operations.
--
--  When selecting or unselecting is supported by a model, the return values
--  of the selection functions do *not* indicate if selection or unselection
--  happened. They are only meant to indicate complete failure, like when this
--  mode of selecting is not supported by the model.
--
--  Selections may happen asynchronously, so the only reliable way to find out
--  when an item was selected is to listen to the signals that indicate
--  selection.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;
with Gtk.Bitset;  use Gtk.Bitset;

package Gtk.Selection_Model is

   type Gtk_Selection_Model is new Glib.Types.GType_Interface;
   Null_Gtk_Selection_Model : constant Gtk_Selection_Model;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_selection_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Selection
      (Self : Gtk_Selection_Model) return Gtk.Bitset.Gtk_Bitset;
   --  Gets the set containing all currently selected items in the model.
   --  This function may be slow, so if you are only interested in single
   --  item, consider using [methodGtk.SelectionModel.is_selected] or if you
   --  are only interested in a few, consider
   --  [methodGtk.SelectionModel.get_selection_in_range].
   --  @return a `GtkBitset` containing all the values currently selected in
   --  Model. If no items are selected, the bitset is empty. The bitset must
   --  not be modified.

   function Set_Selection
      (Self     : Gtk_Selection_Model;
       Selected : Gtk.Bitset.Gtk_Bitset;
       Mask     : Gtk.Bitset.Gtk_Bitset) return Boolean;
   --  Make selection changes.
   --  This is the most advanced selection updating method that allows the
   --  most fine-grained control over selection changes. If you can, you should
   --  try the simpler versions, as implementations are more likely to
   --  implement support for those.
   --  Requests that the selection state of all positions set in Mask be
   --  updated to the respective value in the Selected bitmask.
   --  In pseudocode, it would look something like this:
   --  ```c for (i = 0; i < n_items; i++) { // don't change values not in the
   --  mask if (!gtk_bitset_contains (mask, i)) continue;
   --  if (gtk_bitset_contains (selected, i)) select_item (i); else
   --  unselect_item (i); }
   --  gtk_selection_model_selection_changed (model, first_changed_item,
   --  n_changed_items); ```
   --  Mask and Selected must not be modified. They may refer to the same
   --  bitset, which would mean that every item in the set should be selected.
   --  @param Selected bitmask specifying if items should be selected or
   --  unselected
   --  @param Mask bitmask specifying which items should be updated
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean that all items were updated according to the
   --  inputs.

   function Get_Selection_In_Range
      (Self     : Gtk_Selection_Model;
       Position : Guint;
       N_Items  : Guint) return Gtk.Bitset.Gtk_Bitset;
   --  Gets the set of selected items in a range.
   --  This function is an optimization for
   --  [methodGtk.SelectionModel.get_selection] when you are only interested in
   --  part of the model's selected state. A common use case is in response to
   --  the [signalGtk.SelectionModel::selection-changed] signal.
   --  @param Position start of the queried range
   --  @param N_Items number of items in the queried range
   --  @return A `GtkBitset` that matches the selection state for the given
   --  range with all other values being undefined. The bitset must not be
   --  modified.

   function Is_Selected
      (Self     : Gtk_Selection_Model;
       Position : Guint) return Boolean;
   --  Checks if the given item is selected.
   --  @param Position the position of the item to query
   --  @return True if the item is selected

   function Select_All (Self : Gtk_Selection_Model) return Boolean;
   --  Requests to select all items in the model.
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean that all items are now selected.

   function Select_Item
      (Self          : Gtk_Selection_Model;
       Position      : Guint;
       Unselect_Rest : Boolean) return Boolean;
   --  Requests to select an item in the model.
   --  @param Position the position of the item to select
   --  @param Unselect_Rest whether previously selected items should be
   --  unselected
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the item was selected.

   function Select_Range
      (Self          : Gtk_Selection_Model;
       Position      : Guint;
       N_Items       : Guint;
       Unselect_Rest : Boolean) return Boolean;
   --  Requests to select a range of items in the model.
   --  @param Position the first item to select
   --  @param N_Items the number of items to select
   --  @param Unselect_Rest whether previously selected items should be
   --  unselected
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the range was selected.

   procedure Selection_Changed
      (Self     : Gtk_Selection_Model;
       Position : Guint;
       N_Items  : Guint);
   pragma Import (C, Selection_Changed, "gtk_selection_model_selection_changed");
   --  Helper function for implementations of `GtkSelectionModel`.
   --  Call this when the selection changes to emit the
   --  [signalGtk.SelectionModel::selection-changed] signal.
   --  @param Position the first changed item
   --  @param N_Items the number of changed items

   function Unselect_All (Self : Gtk_Selection_Model) return Boolean;
   --  Requests to unselect all items in the model.
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean that all items are now unselected.

   function Unselect_Item
      (Self     : Gtk_Selection_Model;
       Position : Guint) return Boolean;
   --  Requests to unselect an item in the model.
   --  @param Position the position of the item to unselect
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the item was unselected.

   function Unselect_Range
      (Self     : Gtk_Selection_Model;
       Position : Guint;
       N_Items  : Guint) return Boolean;
   --  Requests to unselect a range of items in the model.
   --  @param Position the first item to unselect
   --  @param N_Items the number of items to unselect
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the range was unselected.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Selection_Model_Guint_Guint_Void is not null access procedure
     (Self     : Gtk_Selection_Model;
      Position : Guint;
      N_Items  : Guint);

   type Cb_GObject_Guint_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Guint;
      N_Items  : Guint);

   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";
   procedure On_Selection_Changed
      (Self  : Gtk_Selection_Model;
       Call  : Cb_Gtk_Selection_Model_Guint_Guint_Void;
       After : Boolean := False);
   procedure On_Selection_Changed
      (Self  : Gtk_Selection_Model;
       Call  : Cb_GObject_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the selection state of some of the items in Model changes.
   --
   --  Note that this signal does not specify the new selection state of the
   --  items, they need to be queried manually. It is also not necessary for a
   --  model to change the selection state of any of the items in the selection
   --  model, though it would be rather useless to emit such a signal.
   -- 
   --  Callback parameters:
   --    --  @param Position The first item that may have changed
   --    --  @param N_Items number of items with changes

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Selection_Model"

   function "+" (W : Gtk_Selection_Model) return Gtk_Selection_Model;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Selection_In_Range is access function
     (Self     : Gtk_Selection_Model;
      Position : Guint;
      N_Items  : Guint) return System.Address;
   pragma Convention (C, Virtual_Get_Selection_In_Range);
   --  Gets the set of selected items in a range.
   --  This function is an optimization for
   --  [methodGtk.SelectionModel.get_selection] when you are only interested in
   --  part of the model's selected state. A common use case is in response to
   --  the [signalGtk.SelectionModel::selection-changed] signal.
   --  @param Position start of the queried range
   --  @param N_Items number of items in the queried range
   --  @return A `GtkBitset` that matches the selection state for the given
   --  range with all other values being undefined. The bitset must not be
   --  modified.

   type Virtual_Is_Selected is access function
     (Self     : Gtk_Selection_Model;
      Position : Guint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Is_Selected);
   --  Checks if the given item is selected.
   --  @param Position the position of the item to query
   --  @return True if the item is selected

   type Virtual_Select_All is access function (Self : Gtk_Selection_Model) return Glib.Gboolean;
   pragma Convention (C, Virtual_Select_All);
   --  Requests to select all items in the model.
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean that all items are now selected.

   type Virtual_Select_Item is access function
     (Self          : Gtk_Selection_Model;
      Position      : Guint;
      Unselect_Rest : Glib.Gboolean) return Glib.Gboolean;
   pragma Convention (C, Virtual_Select_Item);
   --  Requests to select an item in the model.
   --  @param Position the position of the item to select
   --  @param Unselect_Rest whether previously selected items should be
   --  unselected
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the item was selected.

   type Virtual_Select_Range is access function
     (Self          : Gtk_Selection_Model;
      Position      : Guint;
      N_Items       : Guint;
      Unselect_Rest : Glib.Gboolean) return Glib.Gboolean;
   pragma Convention (C, Virtual_Select_Range);
   --  Requests to select a range of items in the model.
   --  @param Position the first item to select
   --  @param N_Items the number of items to select
   --  @param Unselect_Rest whether previously selected items should be
   --  unselected
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the range was selected.

   type Virtual_Set_Selection is access function
     (Self     : Gtk_Selection_Model;
      Selected : System.Address;
      Mask     : System.Address) return Glib.Gboolean;
   pragma Convention (C, Virtual_Set_Selection);
   --  Make selection changes.
   --  This is the most advanced selection updating method that allows the
   --  most fine-grained control over selection changes. If you can, you should
   --  try the simpler versions, as implementations are more likely to
   --  implement support for those.
   --  Requests that the selection state of all positions set in Mask be
   --  updated to the respective value in the Selected bitmask.
   --  In pseudocode, it would look something like this:
   --  ```c for (i = 0; i < n_items; i++) { // don't change values not in the
   --  mask if (!gtk_bitset_contains (mask, i)) continue;
   --  if (gtk_bitset_contains (selected, i)) select_item (i); else
   --  unselect_item (i); }
   --  gtk_selection_model_selection_changed (model, first_changed_item,
   --  n_changed_items); ```
   --  Mask and Selected must not be modified. They may refer to the same
   --  bitset, which would mean that every item in the set should be selected.
   --  @param Selected bitmask specifying if items should be selected or
   --  unselected
   --  @param Mask bitmask specifying which items should be updated
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean that all items were updated according to the
   --  inputs.

   type Virtual_Unselect_All is access function (Self : Gtk_Selection_Model) return Glib.Gboolean;
   pragma Convention (C, Virtual_Unselect_All);
   --  Requests to unselect all items in the model.
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean that all items are now unselected.

   type Virtual_Unselect_Item is access function
     (Self     : Gtk_Selection_Model;
      Position : Guint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Unselect_Item);
   --  Requests to unselect an item in the model.
   --  @param Position the position of the item to unselect
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the item was unselected.

   type Virtual_Unselect_Range is access function
     (Self     : Gtk_Selection_Model;
      Position : Guint;
      N_Items  : Guint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Unselect_Range);
   --  Requests to unselect a range of items in the model.
   --  @param Position the first item to unselect
   --  @param N_Items the number of items to unselect
   --  @return True if this action was supported and no fallback should be
   --  tried. This does not mean the range was unselected.

   subtype Selection_Model_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Selection_In_Range
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Get_Selection_In_Range);
   pragma Import (C, Set_Get_Selection_In_Range, "gtkada_Selection_Model_set_get_selection_in_range");

   procedure Set_Is_Selected
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Is_Selected);
   pragma Import (C, Set_Is_Selected, "gtkada_Selection_Model_set_is_selected");

   procedure Set_Select_All
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Select_All);
   pragma Import (C, Set_Select_All, "gtkada_Selection_Model_set_select_all");

   procedure Set_Select_Item
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Select_Item);
   pragma Import (C, Set_Select_Item, "gtkada_Selection_Model_set_select_item");

   procedure Set_Select_Range
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Select_Range);
   pragma Import (C, Set_Select_Range, "gtkada_Selection_Model_set_select_range");

   procedure Set_Set_Selection
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Set_Selection);
   pragma Import (C, Set_Set_Selection, "gtkada_Selection_Model_set_set_selection");

   procedure Set_Unselect_All
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Unselect_All);
   pragma Import (C, Set_Unselect_All, "gtkada_Selection_Model_set_unselect_all");

   procedure Set_Unselect_Item
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Unselect_Item);
   pragma Import (C, Set_Unselect_Item, "gtkada_Selection_Model_set_unselect_item");

   procedure Set_Unselect_Range
     (Self    : Selection_Model_Interface_Descr;
      Handler : Virtual_Unselect_Range);
   pragma Import (C, Set_Unselect_Range, "gtkada_Selection_Model_set_unselect_range");
   --  See Glib.Object.Add_Interface

private

Null_Gtk_Selection_Model : constant Gtk_Selection_Model :=
   Gtk_Selection_Model (Glib.Types.Null_Interface);
end Gtk.Selection_Model;
