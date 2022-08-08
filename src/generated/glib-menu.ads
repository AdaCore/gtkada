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
--  Glib.Menu.Gmenu is a simple implementation of Glib.Menu_Model.Gmenu_Model.
--  You populate a Glib.Menu.Gmenu by adding Glib.Menu.Gmenu_Item instances to
--  it.
--
--  There are some convenience functions to allow you to directly add items
--  (avoiding Glib.Menu.Gmenu_Item) for the common cases. To add a regular
--  item, use Glib.Menu.Insert. To add a section, use Glib.Menu.Insert_Section.
--  To add a submenu, use Glib.Menu.Insert_Submenu.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.G_Icon;     use Glib.G_Icon;
with Glib.Menu_Model; use Glib.Menu_Model;
with Glib.Object;     use Glib.Object;
with Glib.Variant;    use Glib.Variant;

package Glib.Menu is

   type Gmenu_Record is new Gmenu_Model_Record with null record;
   type Gmenu is access all Gmenu_Record'Class;

   type Gmenu_Item_Record is new GObject_Record with null record;
   type Gmenu_Item is access all Gmenu_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gmenu);
   --  Creates a new Glib.Menu.Gmenu.
   --  The new menu has no items.
   --  Since: gtk+ 2.32

   procedure Initialize (Self : not null access Gmenu_Record'Class);
   --  Creates a new Glib.Menu.Gmenu.
   --  The new menu has no items.
   --  Since: gtk+ 2.32
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gmenu_New return Gmenu;
   --  Creates a new Glib.Menu.Gmenu.
   --  The new menu has no items.
   --  Since: gtk+ 2.32

   function Get_Type_Menu return Glib.GType;
   pragma Import (C, Get_Type_Menu, "g_menu_get_type");

   procedure G_New
      (Self            : out Gmenu_Item;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "");
   --  Creates a new Glib.Menu.Gmenu_Item.
   --  If Label is non-null it is used to set the "label" attribute of the new
   --  item.
   --  If Detailed_Action is non-null it is used to set the "action" and
   --  possibly the "target" attribute of the new item. See
   --  Glib.Menu.Set_Detailed_Action for more information.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "detailed_action": the detailed action string, or null

   procedure Initialize
      (Self            : not null access Gmenu_Item_Record'Class;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "");
   --  Creates a new Glib.Menu.Gmenu_Item.
   --  If Label is non-null it is used to set the "label" attribute of the new
   --  item.
   --  If Detailed_Action is non-null it is used to set the "action" and
   --  possibly the "target" attribute of the new item. See
   --  Glib.Menu.Set_Detailed_Action for more information.
   --  Since: gtk+ 2.32
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": the section label, or null
   --  "detailed_action": the detailed action string, or null

   function Gmenu_Item_New
      (Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "") return Gmenu_Item;
   --  Creates a new Glib.Menu.Gmenu_Item.
   --  If Label is non-null it is used to set the "label" attribute of the new
   --  item.
   --  If Detailed_Action is non-null it is used to set the "action" and
   --  possibly the "target" attribute of the new item. See
   --  Glib.Menu.Set_Detailed_Action for more information.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "detailed_action": the detailed action string, or null

   procedure G_New_From_Model
      (Self       : out Gmenu_Item;
       Model      : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Item_Index : Glib.Gint);
   --  Creates a Glib.Menu.Gmenu_Item as an exact copy of an existing menu
   --  item in a Glib.Menu_Model.Gmenu_Model.
   --  Item_Index must be valid (ie: be sure to call
   --  Glib.Menu_Model.Get_N_Items first).
   --  Since: gtk+ 2.34
   --  "model": a Glib.Menu_Model.Gmenu_Model
   --  "item_index": the index of an item in Model

   procedure Initialize_From_Model
      (Self       : not null access Gmenu_Item_Record'Class;
       Model      : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Item_Index : Glib.Gint);
   --  Creates a Glib.Menu.Gmenu_Item as an exact copy of an existing menu
   --  item in a Glib.Menu_Model.Gmenu_Model.
   --  Item_Index must be valid (ie: be sure to call
   --  Glib.Menu_Model.Get_N_Items first).
   --  Since: gtk+ 2.34
   --  Initialize_From_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "model": a Glib.Menu_Model.Gmenu_Model
   --  "item_index": the index of an item in Model

   function Gmenu_Item_New_From_Model
      (Model      : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Item_Index : Glib.Gint) return Gmenu_Item;
   --  Creates a Glib.Menu.Gmenu_Item as an exact copy of an existing menu
   --  item in a Glib.Menu_Model.Gmenu_Model.
   --  Item_Index must be valid (ie: be sure to call
   --  Glib.Menu_Model.Get_N_Items first).
   --  Since: gtk+ 2.34
   --  "model": a Glib.Menu_Model.Gmenu_Model
   --  "item_index": the index of an item in Model

   procedure G_New_Section
      (Self    : out Gmenu_Item;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a new Glib.Menu.Gmenu_Item representing a section.
   --  This is a convenience API around Glib.Menu.G_New and
   --  Glib.Menu.Set_Section.
   --  The effect of having one menu appear as a section of another is exactly
   --  as it sounds: the items from Section become a direct part of the menu
   --  that Menu_Item is added to.
   --  Visual separation is typically displayed between two non-empty
   --  sections. If Label is non-null then it will be encorporated into this
   --  visual indication. This allows for labeled subsections of a menu.
   --  As a simple example, consider a typical "Edit" menu from a simple
   --  program. It probably contains an "Undo" and "Redo" item, followed by a
   --  separator, followed by "Cut", "Copy" and "Paste".
   --  This would be accomplished by creating three Glib.Menu.Gmenu instances.
   --  The first would be populated with the "Undo" and "Redo" items, and the
   --  second with the "Cut", "Copy" and "Paste" items. The first and second
   --  menus would then be added as submenus of the third. In XML format, this
   --  would look something like the following: |[ <menu id='edit-menu'>
   --  <section> <item label='Undo'/> <item label='Redo'/> </section> <section>
   --  <item label='Cut'/> <item label='Copy'/> <item label='Paste'/>
   --  </section> </menu> ]|
   --  The following example is exactly equivalent. It is more illustrative of
   --  the exact relationship between the menus and items (keeping in mind that
   --  the 'link' element defines a new menu that is linked to the containing
   --  one). The style of the second example is more verbose and difficult to
   --  read (and therefore not recommended except for the purpose of
   --  understanding what is really going on). |[ <menu id='edit-menu'> <item>
   --  <link name='section'> <item label='Undo'/> <item label='Redo'/> </link>
   --  </item> <item> <link name='section'> <item label='Cut'/> <item
   --  label='Copy'/> <item label='Paste'/> </link> </item> </menu> ]|
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "section": a Glib.Menu_Model.Gmenu_Model with the items of the section

   procedure Initialize_Section
      (Self    : not null access Gmenu_Item_Record'Class;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a new Glib.Menu.Gmenu_Item representing a section.
   --  This is a convenience API around Glib.Menu.G_New and
   --  Glib.Menu.Set_Section.
   --  The effect of having one menu appear as a section of another is exactly
   --  as it sounds: the items from Section become a direct part of the menu
   --  that Menu_Item is added to.
   --  Visual separation is typically displayed between two non-empty
   --  sections. If Label is non-null then it will be encorporated into this
   --  visual indication. This allows for labeled subsections of a menu.
   --  As a simple example, consider a typical "Edit" menu from a simple
   --  program. It probably contains an "Undo" and "Redo" item, followed by a
   --  separator, followed by "Cut", "Copy" and "Paste".
   --  This would be accomplished by creating three Glib.Menu.Gmenu instances.
   --  The first would be populated with the "Undo" and "Redo" items, and the
   --  second with the "Cut", "Copy" and "Paste" items. The first and second
   --  menus would then be added as submenus of the third. In XML format, this
   --  would look something like the following: |[ <menu id='edit-menu'>
   --  <section> <item label='Undo'/> <item label='Redo'/> </section> <section>
   --  <item label='Cut'/> <item label='Copy'/> <item label='Paste'/>
   --  </section> </menu> ]|
   --  The following example is exactly equivalent. It is more illustrative of
   --  the exact relationship between the menus and items (keeping in mind that
   --  the 'link' element defines a new menu that is linked to the containing
   --  one). The style of the second example is more verbose and difficult to
   --  read (and therefore not recommended except for the purpose of
   --  understanding what is really going on). |[ <menu id='edit-menu'> <item>
   --  <link name='section'> <item label='Undo'/> <item label='Redo'/> </link>
   --  </item> <item> <link name='section'> <item label='Cut'/> <item
   --  label='Copy'/> <item label='Paste'/> </link> </item> </menu> ]|
   --  Since: gtk+ 2.32
   --  Initialize_Section does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "label": the section label, or null
   --  "section": a Glib.Menu_Model.Gmenu_Model with the items of the section

   function Gmenu_Item_New_Section
      (Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gmenu_Item;
   --  Creates a new Glib.Menu.Gmenu_Item representing a section.
   --  This is a convenience API around Glib.Menu.G_New and
   --  Glib.Menu.Set_Section.
   --  The effect of having one menu appear as a section of another is exactly
   --  as it sounds: the items from Section become a direct part of the menu
   --  that Menu_Item is added to.
   --  Visual separation is typically displayed between two non-empty
   --  sections. If Label is non-null then it will be encorporated into this
   --  visual indication. This allows for labeled subsections of a menu.
   --  As a simple example, consider a typical "Edit" menu from a simple
   --  program. It probably contains an "Undo" and "Redo" item, followed by a
   --  separator, followed by "Cut", "Copy" and "Paste".
   --  This would be accomplished by creating three Glib.Menu.Gmenu instances.
   --  The first would be populated with the "Undo" and "Redo" items, and the
   --  second with the "Cut", "Copy" and "Paste" items. The first and second
   --  menus would then be added as submenus of the third. In XML format, this
   --  would look something like the following: |[ <menu id='edit-menu'>
   --  <section> <item label='Undo'/> <item label='Redo'/> </section> <section>
   --  <item label='Cut'/> <item label='Copy'/> <item label='Paste'/>
   --  </section> </menu> ]|
   --  The following example is exactly equivalent. It is more illustrative of
   --  the exact relationship between the menus and items (keeping in mind that
   --  the 'link' element defines a new menu that is linked to the containing
   --  one). The style of the second example is more verbose and difficult to
   --  read (and therefore not recommended except for the purpose of
   --  understanding what is really going on). |[ <menu id='edit-menu'> <item>
   --  <link name='section'> <item label='Undo'/> <item label='Redo'/> </link>
   --  </item> <item> <link name='section'> <item label='Cut'/> <item
   --  label='Copy'/> <item label='Paste'/> </link> </item> </menu> ]|
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "section": a Glib.Menu_Model.Gmenu_Model with the items of the section

   procedure G_New_Submenu
      (Self    : out Gmenu_Item;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a new Glib.Menu.Gmenu_Item representing a submenu.
   --  This is a convenience API around Glib.Menu.G_New and
   --  Glib.Menu.Set_Submenu.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "submenu": a Glib.Menu_Model.Gmenu_Model with the items of the submenu

   procedure Initialize_Submenu
      (Self    : not null access Gmenu_Item_Record'Class;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a new Glib.Menu.Gmenu_Item representing a submenu.
   --  This is a convenience API around Glib.Menu.G_New and
   --  Glib.Menu.Set_Submenu.
   --  Since: gtk+ 2.32
   --  Initialize_Submenu does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "label": the section label, or null
   --  "submenu": a Glib.Menu_Model.Gmenu_Model with the items of the submenu

   function Gmenu_Item_New_Submenu
      (Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gmenu_Item;
   --  Creates a new Glib.Menu.Gmenu_Item representing a submenu.
   --  This is a convenience API around Glib.Menu.G_New and
   --  Glib.Menu.Set_Submenu.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "submenu": a Glib.Menu_Model.Gmenu_Model with the items of the submenu

   function Get_Type_Menu_Item return Glib.GType;
   pragma Import (C, Get_Type_Menu_Item, "g_menu_item_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self            : not null access Gmenu_Record;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "");
   --  Convenience function for appending a normal menu item to the end of
   --  Menu. Combine Glib.Menu.G_New and Glib.Menu.Insert_Item for a more
   --  flexible alternative.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "detailed_action": the detailed action string, or null

   procedure Append_Item
      (Self : not null access Gmenu_Record;
       Item : not null access Gmenu_Item_Record'Class);
   --  Appends Item to the end of Menu.
   --  See Glib.Menu.Insert_Item for more information.
   --  Since: gtk+ 2.32
   --  "item": a Glib.Menu.Gmenu_Item to append

   procedure Append_Section
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Convenience function for appending a section menu item to the end of
   --  Menu. Combine Glib.Menu.G_New_Section and Glib.Menu.Insert_Item for a
   --  more flexible alternative.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "section": a Glib.Menu_Model.Gmenu_Model with the items of the section

   procedure Append_Submenu
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Convenience function for appending a submenu menu item to the end of
   --  Menu. Combine Glib.Menu.G_New_Submenu and Glib.Menu.Insert_Item for a
   --  more flexible alternative.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "submenu": a Glib.Menu_Model.Gmenu_Model with the items of the submenu

   procedure Freeze (Self : not null access Gmenu_Record);
   --  Marks Menu as frozen.
   --  After the menu is frozen, it is an error to attempt to make any changes
   --  to it. In effect this means that the Glib.Menu.Gmenu API must no longer
   --  be used.
   --  This function causes Glib.Menu_Model.Is_Mutable to begin returning
   --  False, which has some positive performance implications.
   --  Since: gtk+ 2.32

   procedure Insert
      (Self            : not null access Gmenu_Record;
       Position        : Glib.Gint;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "");
   --  Convenience function for inserting a normal menu item into Menu.
   --  Combine Glib.Menu.G_New and Glib.Menu.Insert_Item for a more flexible
   --  alternative.
   --  Since: gtk+ 2.32
   --  "position": the position at which to insert the item
   --  "label": the section label, or null
   --  "detailed_action": the detailed action string, or null

   procedure Insert_Item
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint;
       Item     : not null access Gmenu_Item_Record'Class);
   --  Inserts Item into Menu.
   --  The "insertion" is actually done by copying all of the attribute and
   --  link values of Item and using them to form a new item within Menu. As
   --  such, Item itself is not really inserted, but rather, a menu item that
   --  is exactly the same as the one presently described by Item.
   --  This means that Item is essentially useless after the insertion occurs.
   --  Any changes you make to it are ignored unless it is inserted again (at
   --  which point its updated values will be copied).
   --  You should probably just free Item once you're done.
   --  There are many convenience functions to take care of common cases. See
   --  Glib.Menu.Insert, Glib.Menu.Insert_Section and Glib.Menu.Insert_Submenu
   --  as well as "prepend" and "append" variants of each of these functions.
   --  Since: gtk+ 2.32
   --  "position": the position at which to insert the item
   --  "item": the Glib.Menu.Gmenu_Item to insert

   procedure Insert_Section
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint;
       Label    : UTF8_String := "";
       Section  : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Convenience function for inserting a section menu item into Menu.
   --  Combine Glib.Menu.G_New_Section and Glib.Menu.Insert_Item for a more
   --  flexible alternative.
   --  Since: gtk+ 2.32
   --  "position": the position at which to insert the item
   --  "label": the section label, or null
   --  "section": a Glib.Menu_Model.Gmenu_Model with the items of the section

   procedure Insert_Submenu
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint;
       Label    : UTF8_String := "";
       Submenu  : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Convenience function for inserting a submenu menu item into Menu.
   --  Combine Glib.Menu.G_New_Submenu and Glib.Menu.Insert_Item for a more
   --  flexible alternative.
   --  Since: gtk+ 2.32
   --  "position": the position at which to insert the item
   --  "label": the section label, or null
   --  "submenu": a Glib.Menu_Model.Gmenu_Model with the items of the submenu

   procedure Prepend
      (Self            : not null access Gmenu_Record;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "");
   --  Convenience function for prepending a normal menu item to the start of
   --  Menu. Combine Glib.Menu.G_New and Glib.Menu.Insert_Item for a more
   --  flexible alternative.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "detailed_action": the detailed action string, or null

   procedure Prepend_Item
      (Self : not null access Gmenu_Record;
       Item : not null access Gmenu_Item_Record'Class);
   --  Prepends Item to the start of Menu.
   --  See Glib.Menu.Insert_Item for more information.
   --  Since: gtk+ 2.32
   --  "item": a Glib.Menu.Gmenu_Item to prepend

   procedure Prepend_Section
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Convenience function for prepending a section menu item to the start of
   --  Menu. Combine Glib.Menu.G_New_Section and Glib.Menu.Insert_Item for a
   --  more flexible alternative.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "section": a Glib.Menu_Model.Gmenu_Model with the items of the section

   procedure Prepend_Submenu
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Convenience function for prepending a submenu menu item to the start of
   --  Menu. Combine Glib.Menu.G_New_Submenu and Glib.Menu.Insert_Item for a
   --  more flexible alternative.
   --  Since: gtk+ 2.32
   --  "label": the section label, or null
   --  "submenu": a Glib.Menu_Model.Gmenu_Model with the items of the submenu

   procedure Remove
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint);
   --  Removes an item from the menu.
   --  Position gives the index of the item to remove.
   --  It is an error if position is not in range the range from 0 to one less
   --  than the number of items in the menu.
   --  It is not possible to remove items by identity since items are added to
   --  the menu simply by copying their links and attributes (ie: identity of
   --  the item itself is not preserved).
   --  Since: gtk+ 2.32
   --  "position": the position of the item to remove

   procedure Remove_All (Self : not null access Gmenu_Record);
   --  Removes all items in the menu.
   --  Since: gtk+ 2.38

   function Get_Attribute_Value
      (Self          : not null access Gmenu_Item_Record;
       Attribute     : UTF8_String;
       Expected_Type : Glib.Variant.Gvariant_Type)
       return Glib.Variant.Gvariant;
   --  Queries the named Attribute on Menu_Item.
   --  If Expected_Type is specified and the attribute does not have this
   --  type, null is returned. null is also returned if the attribute simply
   --  does not exist.
   --  Since: gtk+ 2.34
   --  "attribute": the attribute name to query
   --  "expected_type": the expected type of the attribute

   procedure Set_Attribute_Value
      (Self      : not null access Gmenu_Item_Record;
       Attribute : UTF8_String;
       Value     : Glib.Variant.Gvariant);
   --  Sets or unsets an attribute on Menu_Item.
   --  The attribute to set or unset is specified by Attribute. This can be
   --  one of the standard attribute names G_MENU_ATTRIBUTE_LABEL,
   --  G_MENU_ATTRIBUTE_ACTION, G_MENU_ATTRIBUTE_TARGET, or a custom attribute
   --  name. Attribute names are restricted to lowercase characters, numbers
   --  and '-'. Furthermore, the names must begin with a lowercase character,
   --  must not end with a '-', and must not contain consecutive dashes.
   --  must consist only of lowercase ASCII characters, digits and '-'.
   --  If Value is non-null then it is used as the new value for the
   --  attribute. If Value is null then the attribute is unset. If the Value
   --  Glib.Variant.Gvariant is floating, it is consumed.
   --  See also g_menu_item_set_attribute for a more convenient way to do the
   --  same.
   --  Since: gtk+ 2.32
   --  "attribute": the attribute to set
   --  "value": a Glib.Variant.Gvariant to use as the value, or null

   function Get_Link
      (Self : not null access Gmenu_Item_Record;
       Link : UTF8_String) return Glib.Menu_Model.Gmenu_Model;
   --  Queries the named Link on Menu_Item.
   --  Since: gtk+ 2.34
   --  "link": the link name to query

   procedure Set_Link
      (Self  : not null access Gmenu_Item_Record;
       Link  : UTF8_String;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a link from Menu_Item to Model if non-null, or unsets it.
   --  Links are used to establish a relationship between a particular menu
   --  item and another menu. For example, G_MENU_LINK_SUBMENU is used to
   --  associate a submenu with a particular menu item, and G_MENU_LINK_SECTION
   --  is used to create a section. Other types of link can be used, but there
   --  is no guarantee that clients will be able to make sense of them. Link
   --  types are restricted to lowercase characters, numbers and '-'.
   --  Furthermore, the names must begin with a lowercase character, must not
   --  end with a '-', and must not contain consecutive dashes.
   --  Since: gtk+ 2.32
   --  "link": type of link to establish or unset
   --  "model": the Glib.Menu_Model.Gmenu_Model to link to (or null to unset)

   procedure Set_Action_And_Target_Value
      (Self         : not null access Gmenu_Item_Record;
       Action       : UTF8_String := "";
       Target_Value : Glib.Variant.Gvariant);
   --  Sets or unsets the "action" and "target" attributes of Menu_Item.
   --  If Action is null then both the "action" and "target" attributes are
   --  unset (and Target_Value is ignored).
   --  If Action is non-null then the "action" attribute is set. The "target"
   --  attribute is then set to the value of Target_Value if it is non-null or
   --  unset otherwise.
   --  Normal menu items (ie: not submenu, section or other custom item types)
   --  are expected to have the "action" attribute set to identify the action
   --  that they are associated with. The state type of the action help to
   --  determine the disposition of the menu item. See Glib.Action.Gaction and
   --  Glib.Action_Group.Gaction_Group for an overview of actions.
   --  In general, clicking on the menu item will result in activation of the
   --  named action with the "target" attribute given as the parameter to the
   --  action invocation. If the "target" attribute is not set then the action
   --  is invoked with no parameter.
   --  If the action has no state then the menu item is usually drawn as a
   --  plain menu item (ie: with no additional decoration).
   --  If the action has a boolean state then the menu item is usually drawn
   --  as a toggle menu item (ie: with a checkmark or equivalent indication).
   --  The item should be marked as 'toggled' or 'checked' when the boolean
   --  state is True.
   --  If the action has a string state then the menu item is usually drawn as
   --  a radio menu item (ie: with a radio bullet or equivalent indication).
   --  The item should be marked as 'selected' when the string state is equal
   --  to the value of the Target property.
   --  See g_menu_item_set_action_and_target or Glib.Menu.Set_Detailed_Action
   --  for two equivalent calls that are probably more convenient for most
   --  uses.
   --  Since: gtk+ 2.32
   --  "action": the name of the action for this item
   --  "target_value": a Glib.Variant.Gvariant to use as the action target

   procedure Set_Detailed_Action
      (Self            : not null access Gmenu_Item_Record;
       Detailed_Action : UTF8_String);
   --  Sets the "action" and possibly the "target" attribute of Menu_Item.
   --  The format of Detailed_Action is the same format parsed by
   --  g_action_parse_detailed_name.
   --  See g_menu_item_set_action_and_target or
   --  Glib.Menu.Set_Action_And_Target_Value for more flexible (but slightly
   --  less convenient) alternatives.
   --  See also Glib.Menu.Set_Action_And_Target_Value for a description of the
   --  semantics of the action and target attributes.
   --  Since: gtk+ 2.32
   --  "detailed_action": the "detailed" action string

   procedure Set_Icon
      (Self : not null access Gmenu_Item_Record;
       Icon : Glib.G_Icon.G_Icon);
   --  Sets (or unsets) the icon on Menu_Item.
   --  This call is the same as calling Glib.G_Icon.Serialize and using the
   --  result as the value to Glib.Menu.Set_Attribute_Value for
   --  G_MENU_ATTRIBUTE_ICON.
   --  This API is only intended for use with "noun" menu items; things like
   --  bookmarks or applications in an "Open With" menu. Don't use it on menu
   --  items corresponding to verbs (eg: stock icons for 'Save' or 'Quit').
   --  If Icon is null then the icon is unset.
   --  Since: gtk+ 2.38
   --  "icon": a Glib.G_Icon.G_Icon, or null

   procedure Set_Label
      (Self  : not null access Gmenu_Item_Record;
       Label : UTF8_String := "");
   --  Sets or unsets the "label" attribute of Menu_Item.
   --  If Label is non-null it is used as the label for the menu item. If it
   --  is null then the label attribute is unset.
   --  Since: gtk+ 2.32
   --  "label": the label to set, or null to unset

   procedure Set_Section
      (Self    : not null access Gmenu_Item_Record;
       Section : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets or unsets the "section" link of Menu_Item to Section.
   --  The effect of having one menu appear as a section of another is exactly
   --  as it sounds: the items from Section become a direct part of the menu
   --  that Menu_Item is added to. See Glib.Menu.G_New_Section for more
   --  information about what it means for a menu item to be a section.
   --  Since: gtk+ 2.32
   --  "section": a Glib.Menu_Model.Gmenu_Model, or null

   procedure Set_Submenu
      (Self    : not null access Gmenu_Item_Record;
       Submenu : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets or unsets the "submenu" link of Menu_Item to Submenu.
   --  If Submenu is non-null, it is linked to. If it is null then the link is
   --  unset.
   --  The effect of having one menu appear as a submenu of another is exactly
   --  as it sounds.
   --  Since: gtk+ 2.32
   --  "submenu": a Glib.Menu_Model.Gmenu_Model, or null

end Glib.Menu;
