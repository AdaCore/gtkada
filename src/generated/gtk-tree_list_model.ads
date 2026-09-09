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

--  A list model that can create child models on demand.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Tree_List_Row; use Gtk.Tree_List_Row;

package Gtk.Tree_List_Model is

   type Gtk_Tree_List_Model_Record is new GObject_Record with null record;
   type Gtk_Tree_List_Model is access all Gtk_Tree_List_Model_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Autoexpand
      (Self : not null access Gtk_Tree_List_Model_Record) return Boolean;
   --  Gets whether the model is set to automatically expand new rows that get
   --  added.
   --  This can be either rows added by changes to the underlying models or
   --  via [methodGtk.TreeListRow.set_expanded].
   --  @return True if the model is set to autoexpand

   procedure Set_Autoexpand
      (Self       : not null access Gtk_Tree_List_Model_Record;
       Autoexpand : Boolean);
   --  Sets whether the model should autoexpand.
   --  If set to True, the model will recursively expand all rows that get
   --  added to the model. This can be either rows added by changes to the
   --  underlying models or via [methodGtk.TreeListRow.set_expanded].
   --  @param Autoexpand True to make the model autoexpand its rows

   function Get_Child_Row
      (Self     : not null access Gtk_Tree_List_Model_Record;
       Position : Guint) return Gtk.Tree_List_Row.Gtk_Tree_List_Row;
   --  Gets the row item corresponding to the child at index Position for
   --  Self's root model.
   --  If Position is greater than the number of children in the root model,
   --  null is returned.
   --  Do not confuse this function with [methodGtk.TreeListModel.get_row].
   --  @param Position position of the child to get
   --  @return the child in Position

   function Get_Model
      (Self : not null access Gtk_Tree_List_Model_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the root model that Self was created with.
   --  @return the root model

   function Get_Passthrough
      (Self : not null access Gtk_Tree_List_Model_Record) return Boolean;
   --  Gets whether the model is passing through original row items.
   --  If this function returns False, the `GListModel` functions for Self
   --  return custom `GtkTreeListRow` objects. You need to call
   --  [methodGtk.TreeListRow.get_item] on these objects to get the original
   --  item.
   --  If True, the values of the child models are passed through in their
   --  original state. You then need to call [methodGtk.TreeListModel.get_row]
   --  to get the custom `GtkTreeListRow`s.
   --  @return True if the model is passing through original row items

   function Get_Row
      (Self     : not null access Gtk_Tree_List_Model_Record;
       Position : Guint) return Gtk.Tree_List_Row.Gtk_Tree_List_Row;
   --  Gets the row object for the given row.
   --  If Position is greater than the number of items in Self, null is
   --  returned.
   --  The row object can be used to expand and collapse rows as well as to
   --  inspect its position in the tree. See its documentation for details.
   --  This row object is persistent and will refer to the current item as
   --  long as the row is present in Self, independent of other rows being
   --  added or removed.
   --  If Self is set to not be passthrough, this function is equivalent to
   --  calling g_list_model_get_item.
   --  Do not confuse this function with
   --  [methodGtk.TreeListModel.get_child_row].
   --  @param Position the position of the row to fetch
   --  @return The row item

   ----------------------
   -- GtkAda additions --
   ----------------------

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_List_Model_Create_Model_Func is access function
     (Item : Glib.Object.GObject) return Glib.List_Model.Glist_Model;
   --  Prototype of the function called to create new child models when
   --  gtk_tree_list_row_set_expanded() is called.

   --  This function can return %NULL to indicate that @item is guaranteed to be
   --  a leaf node and will never have children. If it does not have children but
   --  may get children later, it should return an empty model that is filled once
   --  children arrive.
   --  @param Item The item that is being expanded
   --  Return has transfer-ownership='full'
   --  @retrun The model tracking the children of
   --   item or NULL if item can never have children.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
     (Self        : out Gtk_Tree_List_Model;
      Root        : Glib.List_Model.Glist_Model;
      Passthrough : Boolean;
      Autoexpand  : Boolean;
      Create_Func : Gtk_Tree_List_Model_Create_Model_Func);
   procedure Initialize
     (Self        : not null access Gtk_Tree_List_Model_Record'Class;
      Root        : Glib.List_Model.Glist_Model;
      Passthrough : Boolean;
      Autoexpand  : Boolean;
      Create_Func : Gtk_Tree_List_Model_Create_Model_Func);
   --  Creates a new empty `GtkTreeListModel` displaying Root with all rows
   --  collapsed.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Root The `GListModel` to use as root
   --  @param Passthrough True to pass through items from the models
   --  @param Autoexpand True to set the autoexpand property and expand the
   --  Root model
   --  @param Create_Func function to call to create the `GListModel` for the
   --  children of an item

   function Gtk_Tree_List_Model_New
     (Root        : Glib.List_Model.Glist_Model;
      Passthrough : Boolean;
      Autoexpand  : Boolean;
      Create_Func : Gtk_Tree_List_Model_Create_Model_Func)
   return Gtk_Tree_List_Model;
   --  Creates a new empty `GtkTreeListModel` displaying Root with all rows
   --  collapsed.
   --  @param Root The `GListModel` to use as root
   --  @param Passthrough True to pass through items from the models
   --  @param Autoexpand True to set the autoexpand property and expand the
   --  Root model
   --  @param Create_Func function to call to create the `GListModel` for the
   --  children of an item

   generic
   type User_Data_Type (<>) is private;
   with procedure Destroy (Data : in out User_Data_Type) is null;
   package Gtk_Tree_List_Model_User_Data is

      type Gtk_Tree_List_Model_Create_Model_User_Func is access function
        (Item : Glib.Object.GObject; User_Data : User_Data_Type)
      return Glib.List_Model.Glist_Model;

      procedure Gtk_New_User
        (Self         : out Gtk_Tree_List_Model;
         Root         : Glib.List_Model.Glist_Model;
         Passthrough  : Boolean;
         Autoexpand   : Boolean;
         Create_Func  : Gtk_Tree_List_Model_Create_Model_User_Func;
         User_Data    : User_Data_Type);
      procedure Initialize_User
        (Self         : not null access Gtk_Tree_List_Model_Record'Class;
         Root         : Glib.List_Model.Glist_Model;
         Passthrough  : Boolean;
         Autoexpand   : Boolean;
         Create_Func  : Gtk_Tree_List_Model_Create_Model_User_Func;
         User_Data    : User_Data_Type);
      --  Creates a new empty `GtkTreeListModel` displaying Root with all rows
      --  collapsed.
      --  Initialize does nothing if the object was already created with another
      --  call to Initialize* or G_New.
      --  @param Root The `GListModel` to use as root
      --  @param Passthrough True to pass through items from the models
      --  @param Autoexpand True to set the autoexpand property and expand the
      --  Root model
      --  @param Create_Func function to call to create the `GListModel` for the
      --  children of an item
      --  @param User_Data Data to pass to Create_Func

      function Gtk_Tree_List_Model_New_User
        (Root         : Glib.List_Model.Glist_Model;
         Passthrough  : Boolean;
         Autoexpand   : Boolean;
         Create_Func  : Gtk_Tree_List_Model_Create_Model_User_Func;
         User_Data    : User_Data_Type)
      return Gtk_Tree_List_Model;
      --  Creates a new empty `GtkTreeListModel` displaying Root with all rows
      --  collapsed.
      --  @param Root The `GListModel` to use as root
      --  @param Passthrough True to pass through items from the models
      --  @param Autoexpand True to set the autoexpand property and expand the
      --  Root model
      --  @param Create_Func function to call to create the `GListModel` for the
      --  children of an item
      --  @param User_Data Data to pass to Create_Func
   end Gtk_Tree_List_Model_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Gtk_Tree_List_Model_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Tree_List_Model_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Tree_List_Model_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Tree_List_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Autoexpand_Property : constant Glib.Properties.Property_Boolean;
   --  If all rows should be expanded by default.

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  The root model displayed.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   Passthrough_Property : constant Glib.Properties.Property_Boolean;
   --  Gets whether the model is in passthrough mode.
   --
   --  If False, the `GListModel` functions for this object return custom
   --  [classGtk.TreeListRow] objects. If True, the values of the child models
   --  are pass through unmodified.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Tree_List_Model_Record, Gtk_Tree_List_Model);
   function "+"
     (Widget : access Gtk_Tree_List_Model_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Tree_List_Model
   renames Implements_Glist_Model.To_Object;

private
   Passthrough_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("passthrough");
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
   Autoexpand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("autoexpand");
end Gtk.Tree_List_Model;
