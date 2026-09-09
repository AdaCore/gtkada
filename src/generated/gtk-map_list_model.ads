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

--  A list model that maps the items in another model to different items.
--
--  `GtkMapListModel` uses a [callbackGtk.MapListModelMapFunc].
--
--  Example: Create a list of `GtkEventControllers` ```c static gpointer
--  map_to_controllers (gpointer widget, gpointer data) { gpointer result =
--  gtk_widget_observe_controllers (widget); g_object_unref (widget); return
--  result; }
--
--  widgets = gtk_widget_observe_children (widget);
--
--  controllers = gtk_map_list_model_new (widgets, map_to_controllers, NULL,
--  NULL);
--
--  model = gtk_flatten_list_model_new (GTK_TYPE_EVENT_CONTROLLER,
--  controllers); ```
--
--  `GtkMapListModel` will attempt to discard the mapped objects as soon as
--  they are no longer needed and recreate them if necessary.
--
--  `GtkMapListModel` passes through sections from the underlying model.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Section_Model; use Gtk.Section_Model;

package Gtk.Map_List_Model is

   type Gtk_Map_List_Model_Record is new GObject_Record with null record;
   type Gtk_Map_List_Model is access all Gtk_Map_List_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_map_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Model
      (Self : not null access Gtk_Map_List_Model_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the model that is currently being mapped or null if none.
   --  @return The model that gets mapped

   procedure Set_Model
      (Self  : not null access Gtk_Map_List_Model_Record;
       Model : Glib.List_Model.Glist_Model);
   --  Sets the model to be mapped.
   --  GTK makes no effort to ensure that Model conforms to the item type
   --  expected by the map function. It assumes that the caller knows what they
   --  are doing and have set up an appropriate map function.
   --  @param Model The model to be mapped

   function Has_Map
      (Self : not null access Gtk_Map_List_Model_Record) return Boolean;
   --  Checks if a map function is currently set on Self.
   --  @return True if a map function is set

   ----------------------
   -- GtkAda additions --
   ----------------------

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Map_List_Model_Map_Func is access function (Item : Glib.Object.GObject) return Glib.Object.GObject;
   --  User function that is called to map an Item of the original model to an
   --  item expected by the map model.
   --  The returned items must conform to the item type of the model they are
   --  used with.
   --  Parameter Item has transfer-ownership='full'
   --  @param Item The item to map

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
     (Self     : out Gtk_Map_List_Model;
      Model    : Glib.List_Model.Glist_Model;
      Map_Func : Gtk_Map_List_Model_Map_Func);
   procedure Initialize
     (Self     : not null access Gtk_Map_List_Model_Record'Class;
      Model    : Glib.List_Model.Glist_Model;
      Map_Func : Gtk_Map_List_Model_Map_Func);
   --  Creates a new `GtkMapListModel` for the given arguments.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model The model to map
   --  @param Map_Func map function

   function Gtk_Map_List_Model_New
     (Model    : Glib.List_Model.Glist_Model;
      Map_Func : Gtk_Map_List_Model_Map_Func)
   return Gtk_Map_List_Model;
   --  Creates a new `GtkMapListModel` for the given arguments.
   --  @param Model The model to map
   --  @param Map_Func map function

   procedure Set_Map_Func
     (Self     : not null access Gtk_Map_List_Model_Record;
      Map_Func : Gtk_Map_List_Model_Map_Func);
   --  Sets the function used to map items.
   --  The function will be called whenever an item needs to be mapped and
   --  must return the item to use for the given input item.
   --  Note that `GtkMapListModel` may call this function multiple times on
   --  the same item, because it may delete items it doesn't need anymore.
   --  GTK makes no effort to ensure that Map_Func conforms to the item type
   --  of Self. It assumes that the caller knows what they are doing and the
   --  map function returns items of the appropriate type.
   --  @param Map_Func map function

   generic
   type User_Data_Type (<>) is private;
   with procedure Destroy (Data : in out User_Data_Type) is null;
   package Map_List_Model_User_Data is

      type Gtk_Map_List_Model_Map_User_Func is access function
        (Item      : Glib.Object.GObject;
         User_Data : User_Data_Type) return Glib.Object.GObject;
      --  User function that is called to map an Item of the original model to an
      --  item expected by the map model.
      --  The returned items must conform to the item type of the model they are
      --  used with.
      --  Parameter Item has transfer-ownership='full'
      --  Parameter Item has transfer-ownership='full'
      --  @param Item The item to map
      --  @param User_Data user data

      procedure Gtk_New_User
        (Self      : out Gtk_Map_List_Model;
         Model     : Glib.List_Model.Glist_Model;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type);

      procedure Initialize_User
        (Self      : not null access Gtk_Map_List_Model_Record'Class;
         Model     : Glib.List_Model.Glist_Model;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type);
      --  Creates a new filter using the given function to filter items.
      --  If Match_Func is `NULL`, the filter matches all items.
      --  If the filter func changes its filtering behavior,
      --  [methodGtk.Filter.changed] needs to be called.
      --  Initialize does nothing if the object was already created with another
      --  call to Initialize* or G_New.
      --  @param Match_Func function to filter items
      --  @param User_Data user data to pass to Match_Func

      function Gtk_Map_List_Model_New_User
        (Model     : Glib.List_Model.Glist_Model;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type)
      return Gtk_Map_List_Model;
      --  Creates a new filter using the given function to filter items.
      --  If Match_Func is `NULL`, the filter matches all items.
      --  If the filter func changes its filtering behavior,
      --  [methodGtk.Filter.changed] needs to be called.
      --  @param Match_Func function to filter items

      procedure Set_Map_Func
        (Self      : not null access Gtk.Map_List_Model.Gtk_Map_List_Model_Record'Class;
         Map_Func  : Gtk_Map_List_Model_Map_User_Func;
         User_Data : User_Data_Type);
      --  Sets the function used to map items.
      --  The function will be called whenever an item needs to be mapped and
      --  must return the item to use for the given input item.
      --  Note that `GtkMapListModel` may call this function multiple times on
      --  the same item, because it may delete items it doesn't need anymore.
      --  GTK makes no effort to ensure that Map_Func conforms to the item
      --  type of Self. It assumes that the caller knows what they are doing
      --  and the map function returns items of the appropriate type.
      --  @param Map_Func map function
      --  @param User_Data user data passed to Map_Func

   end Map_List_Model_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Map_List_Model_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Map_List_Model_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Map_List_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Map_List_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   procedure Get_Section
      (Self      : not null access Gtk_Map_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);

   procedure Sections_Changed
      (Self     : not null access Gtk_Map_List_Model_Record;
       Position : Guint;
       N_Items  : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Has_Map_Property : constant Glib.Properties.Property_Boolean;
   --  If a map is set for this model

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The model being mapped.

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
     (Glib.List_Model.Glist_Model, Gtk_Map_List_Model_Record, Gtk_Map_List_Model);
   function "+"
     (Widget : access Gtk_Map_List_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Map_List_Model
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Section_Model is new Glib.Types.Implements
     (Gtk.Section_Model.Gtk_Section_Model, Gtk_Map_List_Model_Record, Gtk_Map_List_Model);
   function "+"
     (Widget : access Gtk_Map_List_Model_Record'Class)
   return Gtk.Section_Model.Gtk_Section_Model
   renames Implements_Gtk_Section_Model.To_Interface;
   function "-"
     (Interf : Gtk.Section_Model.Gtk_Section_Model)
   return Gtk_Map_List_Model
   renames Implements_Gtk_Section_Model.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
   Has_Map_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-map");
end Gtk.Map_List_Model;
