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

--  A list model that concatenates other list models.
--
--  `GtkFlattenListModel` takes a list model containing list models, and
--  flattens it into a single model. Each list model becomes a section in the
--  single model.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Section_Model; use Gtk.Section_Model;

package Gtk.Flatten_List_Model is

   type Gtk_Flatten_List_Model_Record is new GObject_Record with null record;
   type Gtk_Flatten_List_Model is access all Gtk_Flatten_List_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Flatten_List_Model;
       Model : Glib.List_Model.Glist_Model);
   procedure Initialize
      (Self  : not null access Gtk_Flatten_List_Model_Record'Class;
       Model : Glib.List_Model.Glist_Model);
   --  Creates a new `GtkFlattenListModel` that flattens List.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model to be flattened

   function Gtk_Flatten_List_Model_New
      (Model : Glib.List_Model.Glist_Model) return Gtk_Flatten_List_Model;
   --  Creates a new `GtkFlattenListModel` that flattens List.
   --  @param Model the model to be flattened

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_flatten_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Model
      (Self : not null access Gtk_Flatten_List_Model_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model set via Gtk.Flatten_List_Model.Set_Model.
   --  @return The model flattened by Self

   procedure Set_Model
      (Self  : not null access Gtk_Flatten_List_Model_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets a new model to be flattened.
   --  @param Model the new model

   function Get_Model_For_Item
      (Self     : not null access Gtk_Flatten_List_Model_Record;
       Position : Guint) return Glib.List_Model.Glist_Model;
   --  Returns the model containing the item at the given position.
   --  @param Position a position
   --  @return the model containing the item at Position

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Flatten_List_Model_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Flatten_List_Model_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Flatten_List_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Flatten_List_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   procedure Get_Section
      (Self      : not null access Gtk_Flatten_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);

   procedure Sections_Changed
      (Self     : not null access Gtk_Flatten_List_Model_Record;
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
   --  The model being flattened.

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

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Flatten_List_Model_Record, Gtk_Flatten_List_Model);
   function "+"
     (Widget : access Gtk_Flatten_List_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Flatten_List_Model
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Section_Model is new Glib.Types.Implements
     (Gtk.Section_Model.Gtk_Section_Model, Gtk_Flatten_List_Model_Record, Gtk_Flatten_List_Model);
   function "+"
     (Widget : access Gtk_Flatten_List_Model_Record'Class)
   return Gtk.Section_Model.Gtk_Section_Model
   renames Implements_Gtk_Section_Model.To_Interface;
   function "-"
     (Interf : Gtk.Section_Model.Gtk_Section_Model)
   return Gtk_Flatten_List_Model
   renames Implements_Gtk_Section_Model.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Gtk.Flatten_List_Model;
