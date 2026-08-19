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

--  A list model that sorts the elements of another model.
--
--  The elements are sorted according to a `GtkSorter`.
--
--  The model is a stable sort. If two items compare equal according to the
--  sorter, the one that appears first in the original model will also appear
--  first after sorting.
--
--  Note that if you change the sorter, the previous order will have no
--  influence on the new order. If you want that, consider using a
--  `GtkMultiSorter` and appending the previous sorter to it.
--
--  The model can be set up to do incremental sorting, so that sorting long
--  lists doesn't block the UI. See [methodGtk.SortListModel.set_incremental]
--  for details.
--
--  `GtkSortListModel` is a generic model and because of that it cannot take
--  advantage of any external knowledge when sorting. If you run into
--  performance issues with `GtkSortListModel`, it is strongly recommended that
--  you write your own sorting list model.
--
--  `GtkSortListModel` allows sorting the items into sections. It implements
--  `GtkSectionModel` and when [propertyGtk.SortListModel:section-sorter] is
--  set, it will sort all items with that sorter and items comparing equal with
--  it will be put into the same section. The
--  [propertyGtk.SortListModel:sorter] will then be used to sort items inside
--  their sections.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Section_Model; use Gtk.Section_Model;
with Gtk.Sorter;        use Gtk.Sorter;

package Gtk.Sort_List_Model is

   type Gtk_Sort_List_Model_Record is new GObject_Record with null record;
   type Gtk_Sort_List_Model is access all Gtk_Sort_List_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Sort_List_Model;
       Model  : Glib.List_Model.Glist_Model;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Sort_List_Model_Record'Class;
       Model  : Glib.List_Model.Glist_Model;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Creates a new sort list model that uses the Sorter to sort Model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model to sort
   --  @param Sorter the `GtkSorter` to sort Model with,

   function Gtk_Sort_List_Model_New
      (Model  : Glib.List_Model.Glist_Model;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
       return Gtk_Sort_List_Model;
   --  Creates a new sort list model that uses the Sorter to sort Model.
   --  @param Model the model to sort
   --  @param Sorter the `GtkSorter` to sort Model with,

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_sort_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Incremental
      (Self : not null access Gtk_Sort_List_Model_Record) return Boolean;
   --  Returns whether incremental sorting is enabled.
   --  See [methodGtk.SortListModel.set_incremental].
   --  @return True if incremental sorting is enabled

   procedure Set_Incremental
      (Self        : not null access Gtk_Sort_List_Model_Record;
       Incremental : Boolean);
   --  Sets the sort model to do an incremental sort.
   --  When incremental sorting is enabled, the `GtkSortListModel` will not do
   --  a complete sort immediately, but will instead queue an idle handler that
   --  incrementally sorts the items towards their correct position. This of
   --  course means that items do not instantly appear in the right place. It
   --  also means that the total sorting time is a lot slower.
   --  When your filter blocks the UI while sorting, you might consider
   --  turning this on. Depending on your model and sorters, this may become
   --  interesting around 10,000 to 100,000 items.
   --  By default, incremental sorting is disabled.
   --  See [methodGtk.SortListModel.get_pending] for progress information
   --  about an ongoing incremental sorting operation.
   --  @param Incremental True to sort incrementally

   function Get_Model
      (Self : not null access Gtk_Sort_List_Model_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model currently sorted or null if none.
   --  @return The model that gets sorted

   procedure Set_Model
      (Self  : not null access Gtk_Sort_List_Model_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets the model to be sorted.
   --  The Model's item type must conform to the item type of Self.
   --  @param Model The model to be sorted

   function Get_Pending
      (Self : not null access Gtk_Sort_List_Model_Record) return Guint;
   --  Estimates progress of an ongoing sorting operation.
   --  The estimate is the number of items that would still need to be sorted
   --  to finish the sorting operation if this was a linear algorithm. So this
   --  number is not related to how many items are already correctly sorted.
   --  If you want to estimate the progress, you can use code like this: ```c
   --  pending = gtk_sort_list_model_get_pending (self); model =
   --  gtk_sort_list_model_get_model (self); progress = 1.0 - pending /
   --  (double) MAX (1, g_list_model_get_n_items (model)); ```
   --  If no sort operation is ongoing - in particular when
   --  [propertyGtk.SortListModel:incremental] is False - this function returns
   --  0.
   --  @return a progress estimate of remaining items to sort

   function Get_Section_Sorter
      (Self : not null access Gtk_Sort_List_Model_Record)
       return Gtk.Sorter.Gtk_Sorter;
   --  Gets the section sorter that is used to sort items of Self into
   --  sections.
   --  Since: gtk+ 4.12
   --  @return the sorter of self
   --  Return has transfer-ownership='none'

   procedure Set_Section_Sorter
      (Self   : not null access Gtk_Sort_List_Model_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Sets a new section sorter on Self.
   --  Since: gtk+ 4.12
   --  @param Sorter the `GtkSorter` to sort Model with

   function Get_Sorter
      (Self : not null access Gtk_Sort_List_Model_Record)
       return Gtk.Sorter.Gtk_Sorter;
   --  Gets the sorter that is used to sort Self.
   --  @return the sorter of self
   --  Return has transfer-ownership='none'

   procedure Set_Sorter
      (Self   : not null access Gtk_Sort_List_Model_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Sets a new sorter on Self.
   --  @param Sorter the `GtkSorter` to sort Model with

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Sort_List_Model_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Sort_List_Model_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Sort_List_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Sort_List_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   procedure Get_Section
      (Self      : not null access Gtk_Sort_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);

   procedure Sections_Changed
      (Self     : not null access Gtk_Sort_List_Model_Record;
       Position : Guint;
       N_Items  : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Incremental_Property : constant Glib.Properties.Property_Boolean;
   --  If the model should sort items incrementally.

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The model being sorted.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   Pending_Property : constant Glib.Properties.Property_Uint;
   --  Estimate of unsorted items remaining.

   Section_Sorter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Sorter.Gtk_Sorter
   --  The section sorter for this model, if one is set.

   Sorter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Sorter.Gtk_Sorter
   --  The sorter for this model.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.SectionModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Sort_List_Model_Record, Gtk_Sort_List_Model);
   function "+"
     (Widget : access Gtk_Sort_List_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Sort_List_Model
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Section_Model is new Glib.Types.Implements
     (Gtk.Section_Model.Gtk_Section_Model, Gtk_Sort_List_Model_Record, Gtk_Sort_List_Model);
   function "+"
     (Widget : access Gtk_Sort_List_Model_Record'Class)
   return Gtk.Section_Model.Gtk_Section_Model
   renames Implements_Gtk_Section_Model.To_Interface;
   function "-"
     (Interf : Gtk.Section_Model.Gtk_Section_Model)
   return Gtk_Sort_List_Model
   renames Implements_Gtk_Section_Model.To_Object;

private
   Sorter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("sorter");
   Section_Sorter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("section-sorter");
   Pending_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("pending");
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
   Incremental_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("incremental");
end Gtk.Sort_List_Model;
