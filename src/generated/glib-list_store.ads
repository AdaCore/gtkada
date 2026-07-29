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

--  Glib.List_Store.Glist_Store is a simple implementation of
--  Glib.List_Model.Glist_Model that stores all items in memory.
--
--  It provides insertions, deletions, and lookups in logarithmic time with a
--  fast path for the common case of iterating the list linearly.

pragma Warnings (Off, "*is already use-visible*");
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;

package Glib.List_Store is

   type Glist_Store_Record is new GObject_Record with null record;
   type Glist_Store is access all Glist_Store_Record'Class;

   type Array_GObject is array (Natural range <>) of GObject;

   ---------------
   -- Callbacks --
   ---------------

   type Equal_Func is access function
     (A, B : not null access GObject_Record'Class) return Glib.Gboolean;
   --  Specifies the type of a function used to test two values for equality.
   --  The function should return True if both values are equal and False
   --  otherwise.
   --  @param A a value
   --  @param B a value to compare with
   --  @return True if A = B; False otherwise

   type Compare_Data_Func is access function
     (A, B : not null access GObject_Record'Class) return Glib.Gint;
   --  Specifies the type of a comparison function used to compare two values.
   --  The function should return a negative integer if the first value comes
   --  before the second, 0 if they are equal, or a positive integer if the
   --  first value comes after the second.
   --  @param A a value
   --  @param B a value to compare with
   --  @return negative value if A < B
   --          zero if A = B
   --          positive value if A > B
   pragma Convention (C, Compare_Data_Func);

   ------------------
   -- Constructors --
   ------------------

   procedure G_New
      (Self      : out Glist_Store;
       Item_Type : GType := Glib.GType_Object);
   --  Creates a new Glib.List_Store.Glist_Store with items of type Item_Type.
   --  Item_Type must be a subclass of Glib.Object.GObject.
   --  Default Item_Type is GObject for polymorphism
   --  Since: gtk+ 2.44
   --  @param Item_Type the GType of items in the list

   procedure Initialize
      (Self      : not null access Glist_Store_Record'Class;
       Item_Type : GType := Glib.GType_Object);
   --  Creates a new Glib.List_Store.Glist_Store with items of type Item_Type.
   --  Item_Type must be a subclass of Glib.Object.GObject.
   --  Default Item_Type is GObject for polymorphism
   --  Since: gtk+ 2.44
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Item_Type the GType of items in the list

   function Glist_Store_New
      (Item_Type : GType := Glib.GType_Object) return Glist_Store;
   --  Creates a new Glib.List_Store.Glist_Store with items of type Item_Type.
   --  Item_Type must be a subclass of Glib.Object.GObject.
   --  Default Item_Type is GObject for polymorphism
   --  Since: gtk+ 2.44
   --  @param Item_Type the GType of items in the list

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_list_store_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self : not null access Glist_Store_Record;
       Item : not null access GObject_Record'Class);
   --  Appends Item to Store. Item must be of type
   --  Glib.List_Store.Glist_Store:item-type.
   --  This function takes a ref on Item.
   --  Use Glib.List_Store.Splice to append multiple items at the same time
   --  efficiently.
   --  Since: gtk+ 2.44
   --  @param Item the new item

   function Find
      (Self     : not null access Glist_Store_Record;
       Item     : not null access GObject_Record'Class;
       Position : access Guint := null) return Boolean;
   --  Looks up the given Item in the list store by looping over the items
   --  until the first occurrence of Item. If Item was not found, then Position
   --  will not be set, and this method will return False.
   --  If you need to compare the two items with a custom comparison function,
   --  use Glib.List_Store.Find_With_Equal_Func with a custom Gequal_Func
   --  instead.
   --  Since: gtk+ 2.64
   --  @param Item an item
   --  @param Position the first position of Item, if it was found.
   --  @return Whether Store contains Item. If it was found, Position will be
   --  set to the position where Item occurred for the first time.

   function Find_With_Equal_Func
      (Self     : not null access Glist_Store_Record;
       Item     : not null access GObject_Record'Class;
       Func     : Equal_Func;
       Position : access Guint := null) return Boolean;
   --  Looks up the given Item in the list store by looping over the items and
   --  comparing them with Compare_Func until the first occurrence of Item
   --  which matches. If Item was not found, then Position will not be set, and
   --  this method will return False.
   --  Since: gtk+ 2.64
   --  @param Item an item
   --  @param Func A custom equality check function
   --  @param Position the first position of Item, if it was found.
   --  @return Whether Store contains Item. If it was found, Position will be
   --  set to the position where Item occurred for the first time.

   procedure Insert
      (Self     : not null access Glist_Store_Record;
       Position : Guint;
       Item     : not null access GObject_Record'Class);
   --  Inserts Item into Store at Position. Item must be of type
   --  Glib.List_Store.Glist_Store:item-type or derived from it. Position must
   --  be smaller than the length of the list, or equal to it to append.
   --  This function takes a ref on Item.
   --  Use Glib.List_Store.Splice to insert multiple items at the same time
   --  efficiently.
   --  Since: gtk+ 2.44
   --  @param Position the position at which to insert the new item
   --  @param Item the new item

   function Insert_Sorted
      (Self : not null access Glist_Store_Record;
       Item : not null access GObject_Record'Class;
       Func : Compare_Data_Func) return Guint;
   --  Inserts Item into Store at a position to be determined by the
   --  Compare_Func.
   --  The list must already be sorted before calling this function or the
   --  result is undefined. Usually you would approach this by only ever
   --  inserting items by way of this function.
   --  This function takes a ref on Item.
   --  Since: gtk+ 2.44
   --  @param Item the new item
   --  @param Func pairwise comparison function for sorting
   --  @return the position at which Item was inserted

   procedure Remove
      (Self     : not null access Glist_Store_Record;
       Position : Guint);
   --  Removes the item from Store that is at Position. Position must be
   --  smaller than the current length of the list.
   --  Use Glib.List_Store.Splice to remove multiple items at the same time
   --  efficiently.
   --  Since: gtk+ 2.44
   --  @param Position the position of the item that is to be removed

   procedure Remove_All (Self : not null access Glist_Store_Record);
   --  Removes all items from Store.
   --  Since: gtk+ 2.44

   procedure Sort
      (Self : not null access Glist_Store_Record;
       Func : Compare_Data_Func);
   --  Sort the items in Store according to Compare_Func.
   --  Since: gtk+ 2.46
   --  @param Func pairwise comparison function for sorting

   procedure Splice
      (Self       : not null access Glist_Store_Record;
       Position   : Guint;
       N_Removals : Guint;
       Additions  : Array_GObject);
   --  Changes Store by removing N_Removals items and adding N_Additions items
   --  to it. Additions must contain N_Additions items of type
   --  Glib.List_Store.Glist_Store:item-type. null is not permitted.
   --  This function is more efficient than Glib.List_Store.Insert and
   --  Glib.List_Store.Remove, because it only emits
   --  Glib.List_Model.Glist_Model::items-changed once for the change.
   --  This function takes a ref on each item in Additions.
   --  The parameters Position and N_Removals must be correct (ie: Position +
   --  N_Removals must be less than or equal to the length of the list at the
   --  time this function is called).
   --  Since: gtk+ 2.44
   --  @param Position the position at which to make the change
   --  @param N_Removals the number of items to remove
   --  @param Additions the items to add

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Item_Type
      (Self : not null access Glist_Store_Record) return GType;

   function Get_N_Items
      (Self : not null access Glist_Store_Record) return Guint;

   function Get_Item
      (Self     : not null access Glist_Store_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Glist_Store_Record;
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
   --  The type of items contained in this list store. Items must be
   --  subclasses of Glib.Object.GObject.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Glist_Store_Record, Glist_Store);
   function "+"
     (Widget : access Glist_Store_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Glist_Store
   renames Implements_Glist_Model.To_Object;

private
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Glib.List_Store;
