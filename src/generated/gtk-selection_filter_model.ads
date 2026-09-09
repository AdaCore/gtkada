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

--  A list model that presents the selection from a `GtkSelectionModel`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Types;          use Glib.Types;
with Gtk.Selection_Model; use Gtk.Selection_Model;

package Gtk.Selection_Filter_Model is

   type Gtk_Selection_Filter_Model_Record is new GObject_Record with null record;
   type Gtk_Selection_Filter_Model is access all Gtk_Selection_Filter_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Selection_Filter_Model;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   procedure Initialize
      (Self  : not null access Gtk_Selection_Filter_Model_Record'Class;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   --  Creates a new `GtkSelectionFilterModel` that will include the selected
   --  items from the underlying selection model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the selection model to filter

   function Gtk_Selection_Filter_Model_New
      (Model : Gtk.Selection_Model.Gtk_Selection_Model)
       return Gtk_Selection_Filter_Model;
   --  Creates a new `GtkSelectionFilterModel` that will include the selected
   --  items from the underlying selection model.
   --  @param Model the selection model to filter

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_selection_filter_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Model
      (Self : not null access Gtk_Selection_Filter_Model_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model;
   --  Gets the model currently filtered or null if none.
   --  @return The model that gets filtered

   procedure Set_Model
      (Self  : not null access Gtk_Selection_Filter_Model_Record;
       Model : Gtk.Selection_Model.Gtk_Selection_Model);
   --  Sets the model to be filtered.
   --  Note that GTK makes no effort to ensure that Model conforms to the item
   --  type of Self. It assumes that the caller knows what they are doing and
   --  have set up an appropriate filter to ensure that item types match.
   --  @param Model The model to be filtered

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Selection_Filter_Model_Record)
       return GType;

   function Get_N_Items
      (Self : not null access Gtk_Selection_Filter_Model_Record)
       return Guint;

   function Get_Item
      (Self     : not null access Gtk_Selection_Filter_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Selection_Filter_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Selection_Model.Gtk_Selection_Model
   --  The model being filtered.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Selection_Filter_Model_Record, Gtk_Selection_Filter_Model);
   function "+"
     (Widget : access Gtk_Selection_Filter_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Selection_Filter_Model
   renames Implements_Glist_Model.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Gtk.Selection_Filter_Model;
