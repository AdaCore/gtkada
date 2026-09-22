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

--  A list model that presents a slice of another model.
--
--  This is useful when implementing paging by setting the size to the number
--  of elements per page and updating the offset whenever a different page is
--  opened.
--
--  `GtkSliceListModel` passes through sections from the underlying model.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Section_Model; use Gtk.Section_Model;

package Gtk.Slice_List_Model is

   type Gtk_Slice_List_Model_Record is new GObject_Record with null record;
   type Gtk_Slice_List_Model is access all Gtk_Slice_List_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Slice_List_Model;
       Model  : Glib.List_Model.Glist_Model;
       Offset : Guint;
       Size   : Guint);
   procedure Initialize
      (Self   : not null access Gtk_Slice_List_Model_Record'Class;
       Model  : Glib.List_Model.Glist_Model;
       Offset : Guint;
       Size   : Guint);
   --  Creates a new slice model.
   --  It presents the slice from Offset to offset + Size of the given Model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model The model to use
   --  @param Offset the offset of the slice
   --  @param Size maximum size of the slice

   function Gtk_Slice_List_Model_New
      (Model  : Glib.List_Model.Glist_Model;
       Offset : Guint;
       Size   : Guint) return Gtk_Slice_List_Model;
   --  Creates a new slice model.
   --  It presents the slice from Offset to offset + Size of the given Model.
   --  @param Model The model to use
   --  @param Offset the offset of the slice
   --  @param Size maximum size of the slice

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_slice_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Model
      (Self : not null access Gtk_Slice_List_Model_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model that is currently being used or null if none.
   --  @return The model in use

   procedure Set_Model
      (Self  : not null access Gtk_Slice_List_Model_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets the model to show a slice of.
   --  The model's item type must conform to Self's item type.
   --  @param Model The model to be sliced

   function Get_Offset
      (Self : not null access Gtk_Slice_List_Model_Record) return Guint;
   --  Gets the offset set via Gtk.Slice_List_Model.Set_Offset.
   --  @return The offset

   procedure Set_Offset
      (Self   : not null access Gtk_Slice_List_Model_Record;
       Offset : Guint);
   --  Sets the offset into the original model for this slice.
   --  If the offset is too large for the sliced model, Self will end up
   --  empty.
   --  @param Offset the new offset to use

   function Get_Size
      (Self : not null access Gtk_Slice_List_Model_Record) return Guint;
   --  Gets the size set via Gtk.Slice_List_Model.Set_Size.
   --  @return The size

   procedure Set_Size
      (Self : not null access Gtk_Slice_List_Model_Record;
       Size : Guint);
   --  Sets the maximum size. Self will never have more items than Size.
   --  It can however have fewer items if the offset is too large or the model
   --  sliced from doesn't have enough items.
   --  @param Size the maximum size

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Slice_List_Model_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Slice_List_Model_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Slice_List_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Slice_List_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   procedure Get_Section
      (Self      : not null access Gtk_Slice_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);

   procedure Sections_Changed
      (Self     : not null access Gtk_Slice_List_Model_Record;
       Position : Guint;
       N_Items  : Guint);

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
   --  Child model to take slice from.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   Offset_Property : constant Glib.Properties.Property_Uint;
   --  Offset of slice.

   Size_Property : constant Glib.Properties.Property_Uint;
   --  Maximum size of slice.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.SectionModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Slice_List_Model_Record, Gtk_Slice_List_Model);
   function "+"
     (Widget : access Gtk_Slice_List_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Slice_List_Model
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Section_Model is new Glib.Types.Implements
     (Gtk.Section_Model.Gtk_Section_Model, Gtk_Slice_List_Model_Record, Gtk_Slice_List_Model);
   function "+"
     (Widget : access Gtk_Slice_List_Model_Record'Class)
   return Gtk.Section_Model.Gtk_Section_Model
   renames Implements_Gtk_Section_Model.To_Interface;
   function "-"
     (Interf : Gtk.Section_Model.Gtk_Section_Model)
   return Gtk_Slice_List_Model
   renames Implements_Gtk_Section_Model.To_Object;

private
   Size_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("size");
   Offset_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("offset");
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Gtk.Slice_List_Model;
