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

--  A list model that filters the elements of another model.
--
--  It hides some elements from the underlying model according to criteria
--  given by a `GtkFilter`.
--
--  The model can be set up to do incremental filtering, so that filtering
--  long lists doesn't block the UI. See
--  [methodGtk.FilterListModel.set_incremental] for details.
--
--  `GtkFilterListModel` passes through sections from the underlying model.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Filter;        use Gtk.Filter;
with Gtk.Section_Model; use Gtk.Section_Model;

package Gtk.Filter_List_Model is

   type Gtk_Filter_List_Model_Record is new GObject_Record with null record;
   type Gtk_Filter_List_Model is access all Gtk_Filter_List_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Filter_List_Model;
       Model  : Glib.List_Model.Glist_Model;
       Filter : access Gtk.Filter.Gtk_Filter_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Filter_List_Model_Record'Class;
       Model  : Glib.List_Model.Glist_Model;
       Filter : access Gtk.Filter.Gtk_Filter_Record'Class);
   --  Creates a new `GtkFilterListModel` that will filter Model using the
   --  given Filter.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model to sort
   --  @param Filter filter

   function Gtk_Filter_List_Model_New
      (Model  : Glib.List_Model.Glist_Model;
       Filter : access Gtk.Filter.Gtk_Filter_Record'Class)
       return Gtk_Filter_List_Model;
   --  Creates a new `GtkFilterListModel` that will filter Model using the
   --  given Filter.
   --  @param Model the model to sort
   --  @param Filter filter

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_filter_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Filter
      (Self : not null access Gtk_Filter_List_Model_Record)
       return Gtk.Filter.Gtk_Filter;
   --  Gets the `GtkFilter` currently set on Self.
   --  @return The filter currently in use
   --  Return has transfer-ownership='none'

   procedure Set_Filter
      (Self   : not null access Gtk_Filter_List_Model_Record;
       Filter : access Gtk.Filter.Gtk_Filter_Record'Class);
   --  Sets the filter used to filter items.
   --  @param Filter filter to use

   function Get_Incremental
      (Self : not null access Gtk_Filter_List_Model_Record) return Boolean;
   --  Returns whether incremental filtering is enabled.
   --  See [methodGtk.FilterListModel.set_incremental].
   --  @return True if incremental filtering is enabled

   procedure Set_Incremental
      (Self        : not null access Gtk_Filter_List_Model_Record;
       Incremental : Boolean);
   --  Sets the filter model to do an incremental sort.
   --  When incremental filtering is enabled, the `GtkFilterListModel` will
   --  not run filters immediately, but will instead queue an idle handler that
   --  incrementally filters the items and adds them to the list. This of
   --  course means that items are not instantly added to the list, but only
   --  appear incrementally.
   --  When your filter blocks the UI while filtering, you might consider
   --  turning this on. Depending on your model and filters, this may become
   --  interesting around 10,000 to 100,000 items.
   --  By default, incremental filtering is disabled.
   --  See [methodGtk.FilterListModel.get_pending] for progress information
   --  about an ongoing incremental filtering operation.
   --  @param Incremental True to enable incremental filtering

   function Get_Model
      (Self : not null access Gtk_Filter_List_Model_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model currently filtered or null if none.
   --  @return The model that gets filtered

   procedure Set_Model
      (Self  : not null access Gtk_Filter_List_Model_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets the model to be filtered.
   --  Note that GTK makes no effort to ensure that Model conforms to the item
   --  type of Self. It assumes that the caller knows what they are doing and
   --  have set up an appropriate filter to ensure that item types match.
   --  @param Model The model to be filtered

   function Get_Pending
      (Self : not null access Gtk_Filter_List_Model_Record) return Guint;
   --  Returns the number of items that have not been filtered yet.
   --  You can use this value to check if Self is busy filtering by comparing
   --  the return value to 0 or you can compute the percentage of the filter
   --  remaining by dividing the return value by the total number of items in
   --  the underlying model:
   --  ```c pending = gtk_filter_list_model_get_pending (self); model =
   --  gtk_filter_list_model_get_model (self); percentage = pending / (double)
   --  g_list_model_get_n_items (model); ```
   --  If no filter operation is ongoing - in particular when
   --  [propertyGtk.FilterListModel:incremental] is False - this function
   --  returns 0.
   --  @return The number of items not yet filtered

   function Get_Watch_Items
      (Self : not null access Gtk_Filter_List_Model_Record) return Boolean;
   --  Returns whether watching items is enabled.
   --  See [methodGtk.FilterListModel.set_watch_items].
   --  Since: gtk+ 4.20
   --  @return True if watching items is enabled

   procedure Set_Watch_Items
      (Self        : not null access Gtk_Filter_List_Model_Record;
       Watch_Items : Boolean);
   --  Sets the filter model to monitor properties of its items.
   --  This allows implementations of [classGtk.Filter] that support
   --  expression watching to react to property changes. This property has no
   --  effect if the current filter doesn't support watching items.
   --  By default, watching items is disabled.
   --  Since: gtk+ 4.20
   --  @param Watch_Items True to watch items for property changes

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Filter_List_Model_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Filter_List_Model_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Filter_List_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Filter_List_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   procedure Get_Section
      (Self      : not null access Gtk_Filter_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);

   procedure Sections_Changed
      (Self     : not null access Gtk_Filter_List_Model_Record;
       Position : Guint;
       N_Items  : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Filter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Filter.Gtk_Filter
   --  The filter for this model.

   Incremental_Property : constant Glib.Properties.Property_Boolean;
   --  If the model should filter items incrementally.

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The model being filtered.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   Pending_Property : constant Glib.Properties.Property_Uint;
   --  Number of items not yet filtered.

   Watch_Items_Property : constant Glib.Properties.Property_Boolean;
   --  Monitor the list items for changes. It may impact performance.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.SectionModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Filter_List_Model_Record, Gtk_Filter_List_Model);
   function "+"
     (Widget : access Gtk_Filter_List_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Filter_List_Model
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Section_Model is new Glib.Types.Implements
     (Gtk.Section_Model.Gtk_Section_Model, Gtk_Filter_List_Model_Record, Gtk_Filter_List_Model);
   function "+"
     (Widget : access Gtk_Filter_List_Model_Record'Class)
   return Gtk.Section_Model.Gtk_Section_Model
   renames Implements_Gtk_Section_Model.To_Interface;
   function "-"
     (Interf : Gtk.Section_Model.Gtk_Section_Model)
   return Gtk_Filter_List_Model
   renames Implements_Gtk_Section_Model.To_Object;

private
   Watch_Items_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("watch-items");
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
   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");
end Gtk.Filter_List_Model;
