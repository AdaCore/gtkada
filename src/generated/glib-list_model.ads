------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Glib.List_Model.Glist_Model is an interface that represents a mutable list
--  of GObjects. Its main intention is as a model for various widgets in user
--  interfaces, such as list views, but it can also be used as a convenient
--  method of returning lists of data, with support for updates.
--
--  Each object in the list may also report changes in itself via some
--  mechanism (normally the Glib.Object.GObject::notify signal). Taken together
--  with the Glib.List_Model.Glist_Model::items-changed signal, this provides
--  for a list that can change its membership, and in which the members can
--  change their individual properties.
--
--  A good example would be the list of visible wireless network access
--  points, where each access point can report dynamic properties such as
--  signal strength.
--
--  It is important to note that the Glib.List_Model.Glist_Model itself does
--  not report changes to the individual items. It only reports changes to the
--  list membership. If you want to observe changes to the objects themselves
--  then you need to connect signals to the objects that you are interested in.
--
--  All items in a Glib.List_Model.Glist_Model are of (or derived from) the
--  same type. Glib.List_Model.Get_Item_Type returns that type. The type may be
--  an interface, in which case all objects in the list must implement it.
--
--  The semantics are close to that of an array: Glib.List_Model.Get_N_Items
--  returns the number of items in the list and Glib.List_Model.Get_Item
--  returns an item at a (0-based) position. In order to allow implementations
--  to calculate the list length lazily, you can also iterate over items:
--  starting from 0, repeatedly call Glib.List_Model.Get_Item until it returns
--  null.
--
--  An implementation may create objects lazily, but must take care to return
--  the same object for a given position until all references to it are gone.
--
--  On the other side, a consumer is expected only to hold references on
--  objects that are currently "user visible", in order to facilitate the
--  maximum level of laziness in the implementation of the list and to reduce
--  the required number of signal connections at a given time.
--
--  This interface is intended only to be used from a single thread. The
--  thread in which it is appropriate to use it depends on the particular
--  implementation, but typically it will be from the thread that owns the
--  [thread-default main context][g-main-context-push-thread-default] in effect
--  at the time that the model was created.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;

package Glib.List_Model is

   type Glist_Model is new Glib.Types.GType_Interface;
   Null_Glist_Model : constant Glist_Model;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_list_model_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Item
      (Self     : Glist_Model;
       Position : Guint) return System.Address;
   pragma Import (C, Get_Item, "g_list_model_get_item");
   --  Get the item at Position. If Position is greater than the number of
   --  items in List, null is returned.
   --  null is never returned for an index that is smaller than the length of
   --  the list. See Glib.List_Model.Get_N_Items.
   --  Since: gtk+ 2.44
   --  "position": the position of the item to fetch

   function Get_Item_Type (Self : Glist_Model) return GType;
   pragma Import (C, Get_Item_Type, "g_list_model_get_item_type");
   --  Gets the type of the items in List. All items returned from
   --  g_list_model_get_type are of that type or a subtype, or are an
   --  implementation of that interface.
   --  The item type of a Glib.List_Model.Glist_Model can not change during
   --  the life of the model.
   --  Since: gtk+ 2.44

   function Get_N_Items (Self : Glist_Model) return Guint;
   pragma Import (C, Get_N_Items, "g_list_model_get_n_items");
   --  Gets the number of items in List.
   --  Depending on the model implementation, calling this function may be
   --  less efficient than iterating the list with increasing values for
   --  Position until Glib.List_Model.Get_Item returns null.
   --  Since: gtk+ 2.44

   function Get_Object
      (Self     : Glist_Model;
       Position : Guint) return Glib.Object.GObject;
   --  Get the item at Position. If Position is greater than the number of
   --  items in List, null is returned.
   --  null is never returned for an index that is smaller than the length of
   --  the list. See Glib.List_Model.Get_N_Items.
   --  Since: gtk+ 2.44
   --  "position": the position of the item to fetch

   procedure Items_Changed
      (Self     : Glist_Model;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);
   pragma Import (C, Items_Changed, "g_list_model_items_changed");
   --  Emits the Glib.List_Model.Glist_Model::items-changed signal on List.
   --  This function should only be called by classes implementing
   --  Glib.List_Model.Glist_Model. It has to be called after the internal
   --  representation of List has been updated, because handlers connected to
   --  this signal might query the new state of the list.
   --  Implementations must only make changes to the model (as visible to its
   --  consumer) in places that will not cause problems for that consumer. For
   --  models that are driven directly by a write API (such as
   --  Glist.Store.Glist_Store), changes can be reported in response to uses of
   --  that API. For models that represent remote data, changes should only be
   --  made from a fresh mainloop dispatch. It is particularly not permitted to
   --  make changes in response to a call to the Glib.List_Model.Glist_Model
   --  consumer API.
   --  Stated another way: in general, it is assumed that code making a series
   --  of accesses to the model via the API, without returning to the mainloop,
   --  and without calling other code, will continue to view the same contents
   --  of the model.
   --  Since: gtk+ 2.44
   --  "position": the position at which List changed
   --  "removed": the number of items removed
   --  "added": the number of items added

   -------------
   -- Signals --
   -------------

   type Cb_Glist_Model_Guint_Guint_Guint_Void is not null access procedure
     (Self     : Glist_Model;
      Position : Guint;
      Removed  : Guint;
      Added    : Guint);

   type Cb_GObject_Guint_Guint_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Guint;
      Removed  : Guint;
      Added    : Guint);

   Signal_Items_Changed : constant Glib.Signal_Name := "items-changed";
   procedure On_Items_Changed
      (Self  : Glist_Model;
       Call  : Cb_Glist_Model_Guint_Guint_Guint_Void;
       After : Boolean := False);
   procedure On_Items_Changed
      (Self  : Glist_Model;
       Call  : Cb_GObject_Guint_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever items were added to or removed from
   --  List. At Position, Removed items were removed and Added items were added
   --  in their place.
   --
   --  Note: If Removed != Added, the positions of all later items in the
   --  model change.
   -- 
   --  Callback parameters:
   --    --  "position": the position at which List changed
   --    --  "removed": the number of items removed
   --    --  "added": the number of items added

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Glist_Model"

   function "+" (W : Glist_Model) return Glist_Model;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Item is access function (Self : Glist_Model; Position : Guint) return System.Address;
   pragma Convention (C, Virtual_Get_Item);
   --  Get the item at Position. If Position is greater than the number of
   --  items in List, null is returned.
   --  null is never returned for an index that is smaller than the length of
   --  the list. See Glib.List_Model.Get_N_Items.
   --  Since: gtk+ 2.44
   --  "position": the position of the item to fetch

   type Virtual_Get_Item_Type is access function (Self : Glist_Model) return GType;
   pragma Convention (C, Virtual_Get_Item_Type);
   --  Gets the type of the items in List. All items returned from
   --  g_list_model_get_type are of that type or a subtype, or are an
   --  implementation of that interface.
   --  The item type of a Glib.List_Model.Glist_Model can not change during
   --  the life of the model.
   --  Since: gtk+ 2.44

   type Virtual_Get_N_Items is access function (Self : Glist_Model) return Guint;
   pragma Convention (C, Virtual_Get_N_Items);
   --  Gets the number of items in List.
   --  Depending on the model implementation, calling this function may be
   --  less efficient than iterating the list with increasing values for
   --  Position until Glib.List_Model.Get_Item returns null.
   --  Since: gtk+ 2.44

   subtype List_Model_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Item
     (Self    : List_Model_Interface_Descr;
      Handler : Virtual_Get_Item);
   pragma Import (C, Set_Get_Item, "gtkada_List_Model_set_get_item");

   procedure Set_Get_Item_Type
     (Self    : List_Model_Interface_Descr;
      Handler : Virtual_Get_Item_Type);
   pragma Import (C, Set_Get_Item_Type, "gtkada_List_Model_set_get_item_type");

   procedure Set_Get_N_Items
     (Self    : List_Model_Interface_Descr;
      Handler : Virtual_Get_N_Items);
   pragma Import (C, Set_Get_N_Items, "gtkada_List_Model_set_get_n_items");
   --  See Glib.Object.Add_Interface

private

Null_Glist_Model : constant Glist_Model :=
   Glist_Model (Glib.Types.Null_Interface);
end Glib.List_Model;
