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
--  Glib.Menu_Model.Gmenu_Model represents the contents of a menu -- an
--  ordered list of menu items. The items are associated with actions, which
--  can be activated through them. Items can be grouped in sections, and may
--  have submenus associated with them. Both items and sections usually have
--  some representation data, such as labels or icons. The type of the
--  associated action (ie whether it is stateful, and what kind of state it
--  has) can influence the representation of the item.
--
--  The conceptual model of menus in Glib.Menu_Model.Gmenu_Model is
--  hierarchical: sections and submenus are again represented by GMenu_Models.
--  Menus themselves do not define their own roles. Rather, the role of a
--  particular Glib.Menu_Model.Gmenu_Model is defined by the item that
--  references it (or, in the case of the 'root' menu, is defined by the
--  context in which it is used).
--
--  As an example, consider the visible portions of this menu:
--
--  ## An example menu # {menu-example}
--
--  ![](menu-example.png)
--
--  There are 8 "menus" visible in the screenshot: one menubar, two submenus
--  and 5 sections:
--
--  - the toplevel menubar (containing 4 items) - the View submenu (containing
--  3 sections) - the first section of the View submenu (containing 2 items) -
--  the second section of the View submenu (containing 1 item) - the final
--  section of the View submenu (containing 1 item) - the Highlight Mode
--  submenu (containing 2 sections) - the Sources section (containing 2 items)
--  - the Markup section (containing 2 items)
--
--  The [example][menu-model] illustrates the conceptual connection between
--  these 8 menus. Each large block in the figure represents a menu and the
--  smaller blocks within the large block represent items in that menu. Some
--  items contain references to other menus.
--
--  ## A menu example # {menu-model}
--
--  ![](menu-model.png)
--
--  Notice that the separators visible in the [example][menu-example] appear
--  nowhere in the [menu model][menu-model]. This is because separators are not
--  explicitly represented in the menu model. Instead, a separator is inserted
--  between any two non-empty sections of a menu. Section items can have labels
--  just like any other item. In that case, a display system may show a section
--  header instead of a separator.
--
--  The motivation for this abstract model of application controls is that
--  modern user interfaces tend to make these controls available outside the
--  application. Examples include global menus, jumplists, dash boards, etc. To
--  support such uses, it is necessary to 'export' information about actions
--  and their representation in menus, which is exactly what the [GActionGroup
--  exporter][gio-GActionGroup-exporter] and the [GMenuModel
--  exporter][gio-GMenuModel-exporter] do for Glib.Action_Group.Gaction_Group
--  and Glib.Menu_Model.Gmenu_Model. The client-side counterparts to make use
--  of the exported information are Gdbus.Action_Group.Gdbus_Action_Group and
--  Gdbus.Menu_Model.Gdbus_Menu_Model.
--
--  The API of Glib.Menu_Model.Gmenu_Model is very generic, with iterators for
--  the attributes and links of an item, see
--  Glib.Menu_Model.Iterate_Item_Attributes and
--  Glib.Menu_Model.Iterate_Item_Links. The 'standard' attributes and link
--  types have predefined names: G_MENU_ATTRIBUTE_LABEL,
--  G_MENU_ATTRIBUTE_ACTION, G_MENU_ATTRIBUTE_TARGET, G_MENU_LINK_SECTION and
--  G_MENU_LINK_SUBMENU.
--
--  Items in a Glib.Menu_Model.Gmenu_Model represent active controls if they
--  refer to an action that can get activated when the user interacts with the
--  menu item. The reference to the action is encoded by the string id in the
--  G_MENU_ATTRIBUTE_ACTION attribute. An action id uniquely identifies an
--  action in an action group. Which action group(s) provide actions depends on
--  the context in which the menu model is used. E.g. when the model is
--  exported as the application menu of a Gtk.Application.Gtk_Application,
--  actions can be application-wide or window-specific (and thus come from two
--  different action groups). By convention, the application-wide actions have
--  names that start with "app.", while the names of window-specific actions
--  start with "win.".
--
--  While a wide variety of stateful actions is possible, the following is the
--  minimum that is expected to be supported by all users of exported menu
--  information: - an action with no parameter type and no state - an action
--  with no parameter type and boolean state - an action with string parameter
--  type and string state
--
--  ## Stateless
--
--  A stateless action typically corresponds to an ordinary menu item.
--
--  Selecting such a menu item will activate the action (with no parameter).
--
--  ## Boolean State
--
--  An action with a boolean state will most typically be used with a "toggle"
--  or "switch" menu item. The state can be set directly, but activating the
--  action (with no parameter) results in the state being toggled.
--
--  Selecting a toggle menu item will activate the action. The menu item
--  should be rendered as "checked" when the state is true.
--
--  ## String Parameter and State
--
--  Actions with string parameters and state will most typically be used to
--  represent an enumerated choice over the items available for a group of
--  radio menu items. Activating the action with a string parameter is
--  equivalent to setting that parameter as the state.
--
--  Radio menu items, in addition to being associated with the action, will
--  have a target value. Selecting that menu item will result in activation of
--  the action with the target value as the parameter. The menu item should be
--  rendered as "selected" when the state of the action is equal to the target
--  value of the menu item.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Object;  use Glib.Object;
with Glib.Variant; use Glib.Variant;

package Glib.Menu_Model is

   type Gmenu_Model_Record is new GObject_Record with null record;
   type Gmenu_Model is access all Gmenu_Model_Record'Class;

   type Gmenu_Attribute_Iter_Record is new GObject_Record with null record;
   type Gmenu_Attribute_Iter is access all Gmenu_Attribute_Iter_Record'Class;

   type Gmenu_Link_Iter_Record is new GObject_Record with null record;
   type Gmenu_Link_Iter is access all Gmenu_Link_Iter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type_Menu_Model return Glib.GType;
   pragma Import (C, Get_Type_Menu_Model, "g_menu_model_get_type");

   function Get_Type_Menu_Attribute_Iter return Glib.GType;
   pragma Import (C, Get_Type_Menu_Attribute_Iter, "g_menu_attribute_iter_get_type");

   function Get_Type_Menu_Link_Iter return Glib.GType;
   pragma Import (C, Get_Type_Menu_Link_Iter, "g_menu_link_iter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Item_Attribute_Value
      (Self          : not null access Gmenu_Model_Record;
       Item_Index    : Glib.Gint;
       Attribute     : UTF8_String;
       Expected_Type : Glib.Variant.Gvariant_Type)
       return Glib.Variant.Gvariant;
   --  Queries the item at position Item_Index in Model for the attribute
   --  specified by Attribute.
   --  If Expected_Type is non-null then it specifies the expected type of the
   --  attribute. If it is null then any type will be accepted.
   --  If the attribute exists and matches Expected_Type (or if the expected
   --  type is unspecified) then the value is returned.
   --  If the attribute does not exist, or does not match the expected type
   --  then null is returned.
   --  Since: gtk+ 2.32
   --  "item_index": the index of the item
   --  "attribute": the attribute to query
   --  "expected_type": the expected type of the attribute, or null

   function Get_Item_Link
      (Self       : not null access Gmenu_Model_Record;
       Item_Index : Glib.Gint;
       Link       : UTF8_String) return Gmenu_Model;
   --  Queries the item at position Item_Index in Model for the link specified
   --  by Link.
   --  If the link exists, the linked Glib.Menu_Model.Gmenu_Model is returned.
   --  If the link does not exist, null is returned.
   --  Since: gtk+ 2.32
   --  "item_index": the index of the item
   --  "link": the link to query

   function Get_N_Items
      (Self : not null access Gmenu_Model_Record) return Glib.Gint;
   --  Query the number of items in Model.
   --  Since: gtk+ 2.32

   function Is_Mutable
      (Self : not null access Gmenu_Model_Record) return Boolean;
   --  Queries if Model is mutable.
   --  An immutable Glib.Menu_Model.Gmenu_Model will never emit the
   --  Glib.Menu_Model.Gmenu_Model::items-changed signal. Consumers of the
   --  model may make optimisations accordingly.
   --  Since: gtk+ 2.32

   procedure Items_Changed
      (Self     : not null access Gmenu_Model_Record;
       Position : Glib.Gint;
       Removed  : Glib.Gint;
       Added    : Glib.Gint);
   --  Requests emission of the Glib.Menu_Model.Gmenu_Model::items-changed
   --  signal on Model.
   --  This function should never be called except by
   --  Glib.Menu_Model.Gmenu_Model subclasses. Any other calls to this function
   --  will very likely lead to a violation of the interface of the model.
   --  The implementation should update its internal representation of the
   --  menu before emitting the signal. The implementation should further
   --  expect to receive queries about the new state of the menu (and
   --  particularly added menu items) while signal handlers are running.
   --  The implementation must dispatch this call directly from a mainloop
   --  entry and not in response to calls -- particularly those from the
   --  Glib.Menu_Model.Gmenu_Model API. Said another way: the menu must not
   --  change while user code is running without returning to the mainloop.
   --  Since: gtk+ 2.32
   --  "position": the position of the change
   --  "removed": the number of items removed
   --  "added": the number of items added

   function Iterate_Item_Attributes
      (Self       : not null access Gmenu_Model_Record;
       Item_Index : Glib.Gint) return Gmenu_Attribute_Iter;
   --  Creates a Glib.Menu_Model.Gmenu_Attribute_Iter to iterate over the
   --  attributes of the item at position Item_Index in Model.
   --  You must free the iterator with g_object_unref when you are done.
   --  Since: gtk+ 2.32
   --  "item_index": the index of the item

   function Iterate_Item_Links
      (Self       : not null access Gmenu_Model_Record;
       Item_Index : Glib.Gint) return Gmenu_Link_Iter;
   --  Creates a Glib.Menu_Model.Gmenu_Link_Iter to iterate over the links of
   --  the item at position Item_Index in Model.
   --  You must free the iterator with g_object_unref when you are done.
   --  Since: gtk+ 2.32
   --  "item_index": the index of the item

   function Get_Name
      (Self : not null access Gmenu_Attribute_Iter_Record)
       return UTF8_String;
   --  Gets the name of the attribute at the current iterator position, as a
   --  string.
   --  The iterator is not advanced.
   --  Since: gtk+ 2.32

   function Get_Name
      (Self : not null access Gmenu_Link_Iter_Record) return UTF8_String;
   --  Gets the name of the link at the current iterator position.
   --  The iterator is not advanced.
   --  Since: gtk+ 2.32

   function Get_Value
      (Self : not null access Gmenu_Attribute_Iter_Record)
       return Glib.Variant.Gvariant;
   --  Gets the value of the attribute at the current iterator position.
   --  The iterator is not advanced.
   --  Since: gtk+ 2.32

   function Get_Value
      (Self : not null access Gmenu_Link_Iter_Record) return Gmenu_Model;
   --  Gets the linked Glib.Menu_Model.Gmenu_Model at the current iterator
   --  position.
   --  The iterator is not advanced.
   --  Since: gtk+ 2.32

   function Next
      (Self : not null access Gmenu_Attribute_Iter_Record) return Boolean;
   --  Attempts to advance the iterator to the next (possibly first)
   --  attribute.
   --  True is returned on success, or False if there are no more attributes.
   --  You must call this function when you first acquire the iterator to
   --  advance it to the first attribute (and determine if the first attribute
   --  exists at all).
   --  Since: gtk+ 2.32

   function Next
      (Self : not null access Gmenu_Link_Iter_Record) return Boolean;
   --  Attempts to advance the iterator to the next (possibly first) link.
   --  True is returned on success, or False if there are no more links.
   --  You must call this function when you first acquire the iterator to
   --  advance it to the first link (and determine if the first link exists at
   --  all).
   --  Since: gtk+ 2.32

   -------------
   -- Signals --
   -------------

   type Cb_Gmenu_Model_Gint_Gint_Gint_Void is not null access procedure
     (Self     : access Gmenu_Model_Record'Class;
      Position : Glib.Gint;
      Removed  : Glib.Gint;
      Added    : Glib.Gint);

   type Cb_GObject_Gint_Gint_Gint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Glib.Gint;
      Removed  : Glib.Gint;
      Added    : Glib.Gint);

   Signal_Items_Changed : constant Glib.Signal_Name := "items-changed";
   procedure On_Items_Changed
      (Self  : not null access Gmenu_Model_Record;
       Call  : Cb_Gmenu_Model_Gint_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Items_Changed
      (Self  : not null access Gmenu_Model_Record;
       Call  : Cb_GObject_Gint_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a change has occurred to the menu.
   --
   --  The only changes that can occur to a menu is that items are removed or
   --  added. Items may not change (except by being removed and added back in
   --  the same location). This signal is capable of describing both of those
   --  changes (at the same time).
   --
   --  The signal means that starting at the index Position, Removed items
   --  were removed and Added items were added in their place. If Removed is
   --  zero then only items were added. If Added is zero then only items were
   --  removed.
   --
   --  As an example, if the menu contains items a, b, c, d (in that order)
   --  and the signal (2, 1, 3) occurs then the new composition of the menu
   --  will be a, b, _, _, _, d (with each _ representing some new item).
   --
   --  Signal handlers may query the model (particularly the added items) and
   --  expect to see the results of the modification that is being reported.
   --  The signal is emitted after the modification.
   -- 
   --  Callback parameters:
   --    --  "position": the position of the change
   --    --  "removed": the number of items removed
   --    --  "added": the number of items added

end Glib.Menu_Model;
