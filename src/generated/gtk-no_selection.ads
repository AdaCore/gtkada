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

--  A selection model that does not allow selecting anything.
--
--  This model is meant to be used as a simple wrapper around a `GListModel`
--  when a `GtkSelectionModel` is required.
--
--  `GtkNoSelection` passes through sections from the underlying model.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Types;          use Glib.Types;
with Gtk.Bitset;          use Gtk.Bitset;
with Gtk.Section_Model;   use Gtk.Section_Model;
with Gtk.Selection_Model; use Gtk.Selection_Model;

package Gtk.No_Selection is

   type Gtk_No_Selection_Record is new GObject_Record with null record;
   type Gtk_No_Selection is access all Gtk_No_Selection_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_No_Selection;
       Model : Glib.List_Model.Glist_Model);
   procedure Initialize
      (Self  : not null access Gtk_No_Selection_Record'Class;
       Model : Glib.List_Model.Glist_Model);
   --  Creates a new selection to handle Model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the `GListModel` to manage

   function Gtk_No_Selection_New
      (Model : Glib.List_Model.Glist_Model) return Gtk_No_Selection;
   --  Creates a new selection to handle Model.
   --  @param Model the `GListModel` to manage

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_no_selection_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Model
      (Self : not null access Gtk_No_Selection_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model that Self is wrapping.
   --  @return The model being wrapped

   procedure Set_Model
      (Self  : not null access Gtk_No_Selection_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets the model that Self should wrap.
   --  If Model is null, this model will be empty.
   --  @param Model A `GListModel` to wrap

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_No_Selection_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_No_Selection_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   procedure Get_Section
      (Self      : not null access Gtk_No_Selection_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);

   procedure Sections_Changed
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint;
       N_Items  : Guint);

   function Get_Selection
      (Self : not null access Gtk_No_Selection_Record)
       return Gtk.Bitset.Gtk_Bitset;

   function Set_Selection
      (Self     : not null access Gtk_No_Selection_Record;
       Selected : Gtk.Bitset.Gtk_Bitset;
       Mask     : Gtk.Bitset.Gtk_Bitset) return Boolean;

   function Get_Selection_In_Range
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint;
       N_Items  : Guint) return Gtk.Bitset.Gtk_Bitset;

   function Is_Selected
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint) return Boolean;

   function Select_All
      (Self : not null access Gtk_No_Selection_Record) return Boolean;

   function Select_Item
      (Self          : not null access Gtk_No_Selection_Record;
       Position      : Guint;
       Unselect_Rest : Boolean) return Boolean;

   function Select_Range
      (Self          : not null access Gtk_No_Selection_Record;
       Position      : Guint;
       N_Items       : Guint;
       Unselect_Rest : Boolean) return Boolean;

   procedure Selection_Changed
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint;
       N_Items  : Guint);

   function Unselect_All
      (Self : not null access Gtk_No_Selection_Record) return Boolean;

   function Unselect_Item
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint) return Boolean;

   function Unselect_Range
      (Self     : not null access Gtk_No_Selection_Record;
       Position : Guint;
       N_Items  : Guint) return Boolean;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The model being managed.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.SectionModel"
   --
   --  - "Gtk.SelectionModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_No_Selection_Record, Gtk_No_Selection);
   function "+"
     (Widget : access Gtk_No_Selection_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_No_Selection
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Section_Model is new Glib.Types.Implements
     (Gtk.Section_Model.Gtk_Section_Model, Gtk_No_Selection_Record, Gtk_No_Selection);
   function "+"
     (Widget : access Gtk_No_Selection_Record'Class)
   return Gtk.Section_Model.Gtk_Section_Model
   renames Implements_Gtk_Section_Model.To_Interface;
   function "-"
     (Interf : Gtk.Section_Model.Gtk_Section_Model)
   return Gtk_No_Selection
   renames Implements_Gtk_Section_Model.To_Object;

   package Implements_Gtk_Selection_Model is new Glib.Types.Implements
     (Gtk.Selection_Model.Gtk_Selection_Model, Gtk_No_Selection_Record, Gtk_No_Selection);
   function "+"
     (Widget : access Gtk_No_Selection_Record'Class)
   return Gtk.Selection_Model.Gtk_Selection_Model
   renames Implements_Gtk_Selection_Model.To_Interface;
   function "-"
     (Interf : Gtk.Selection_Model.Gtk_Selection_Model)
   return Gtk_No_Selection
   renames Implements_Gtk_Selection_Model.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Gtk.No_Selection;
