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
--  A radio menu item is a check menu item that belongs to a group. At each
--  instant exactly one of the radio menu items from a group is selected.
--
--  The group list does not need to be freed, as each
--  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item will remove itself and its list
--  item when it is destroyed.
--
--  The correct way to create a group of radio menu items is approximatively
--  this:
--
--  ## How to create a group of radio menu items.
--
--  |[<!-- language="C" --> GSList *group = NULL; GtkWidget *item; gint i;
--
--  for (i = 0; i < 5; i++) { item = gtk_radio_menu_item_new_with_label
--  (group, "This is an example"); group = gtk_radio_menu_item_get_group
--  (GTK_RADIO_MENU_ITEM (item)); if (i == 1) gtk_check_menu_item_set_active
--  (GTK_CHECK_MENU_ITEM (item), TRUE); } ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> menuitem ├── radio.left ╰── <child> ]|
--
--  GtkRadioMenuItem has a main CSS node with name menuitem, and a subnode
--  with name radio, which gets the .left or .right style class.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Types;          use Glib.Types;
with Glib.Variant;        use Glib.Variant;
with Gtk.Action;          use Gtk.Action;
with Gtk.Actionable;      use Gtk.Actionable;
with Gtk.Activatable;     use Gtk.Activatable;
with Gtk.Buildable;       use Gtk.Buildable;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Widget;          use Gtk.Widget;

package Gtk.Radio_Menu_Item is

   type Gtk_Radio_Menu_Item_Record is new Gtk_Check_Menu_Item_Record with null record;
   type Gtk_Radio_Menu_Item is access all Gtk_Radio_Menu_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String := "");
   procedure Initialize
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record'Class;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String := "");
   --  Creates a new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item whose child is a
   --  simple Gtk.Label.Gtk_Label.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "group": group the radio menu item is inside, or null
   --  "label": the text for the label

   function Gtk_Radio_Menu_Item_New_With_Label
      (Group : Gtk.Widget.Widget_SList.GSlist;
       Label : UTF8_String := "") return Gtk_Radio_Menu_Item;
   --  Creates a new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item whose child is a
   --  simple Gtk.Label.Gtk_Label.
   --  "group": group the radio menu item is inside, or null
   --  "label": the text for the label

   procedure Gtk_New_With_Mnemonic
      (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record'Class;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String);
   --  Creates a new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item containing a
   --  label. The label will be created using Gtk.Label.Gtk_New_With_Mnemonic,
   --  so underscores in Label indicate the mnemonic for the menu item.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "group": group the radio menu item is inside, or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Gtk_Radio_Menu_Item_New_With_Mnemonic
      (Group : Gtk.Widget.Widget_SList.GSlist;
       Label : UTF8_String) return Gtk_Radio_Menu_Item;
   --  Creates a new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item containing a
   --  label. The label will be created using Gtk.Label.Gtk_New_With_Mnemonic,
   --  so underscores in Label indicate the mnemonic for the menu item.
   --  "group": group the radio menu item is inside, or null
   --  "label": the text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_radio_menu_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Group
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the group to which the radio menu item belongs, as a GList of
   --  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item. The list belongs to GTK+ and
   --  should not be freed.

   procedure Set_Group
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record;
       Group           : Gtk.Widget.Widget_SList.GSlist);
   --  Sets the group of a radio menu item, or changes it.
   --  "group": the new group, or null.

   procedure Join_Group
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record;
       Group_Source    : access Gtk_Radio_Menu_Item_Record'Class);
   --  Joins a Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item object to the group of
   --  another Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item object.
   --  This function should be used by language bindings to avoid the memory
   --  manangement of the opaque GSList of Gtk.Radio_Menu_Item.Get_Group and
   --  Gtk.Radio_Menu_Item.Set_Group.
   --  A common way to set up a group of
   --  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item instances is:
   --  |[ GtkRadioMenuItem *last_item = NULL;
   --  while ( ...more items to add... ) { GtkRadioMenuItem *radio_item;
   --  radio_item = gtk_radio_menu_item_new (...);
   --  gtk_radio_menu_item_join_group (radio_item, last_item); last_item =
   --  radio_item; } ]|
   --  Since: gtk+ 3.18
   --  "group_source": a Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item whose group
   --  we are joining, or null to remove the Radio_Menu_Item from its current
   --  group

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Selected_Button (In_Group : Widget_SList.GSlist) return Natural;
   --  Return the button number of the selected button in the group.
   --  Note: This function is not part of Gtk+ itself, but is provided as a
   --  convenient function

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Radio_Menu_Item_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Radio_Menu_Item_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Radio_Menu_Item_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Radio_Menu_Item_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Radio_Menu_Item_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Radio_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Radio_Menu_Item_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Radio_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Radio_Menu_Item_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Radio_Menu_Item_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Radio_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Radio_Menu_Item
   --  Flags: write
   --  The radio menu item whose group this widget belongs to.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Radio_Menu_Item_Void is not null access procedure
     (Self : access Gtk_Radio_Menu_Item_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Group_Changed : constant Glib.Signal_Name := "group-changed";
   procedure On_Group_Changed
      (Self  : not null access Gtk_Radio_Menu_Item_Record;
       Call  : Cb_Gtk_Radio_Menu_Item_Void;
       After : Boolean := False);
   procedure On_Group_Changed
      (Self  : not null access Gtk_Radio_Menu_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_Radio_Menu_Item_Record, Gtk_Radio_Menu_Item);
   function "+"
     (Widget : access Gtk_Radio_Menu_Item_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Radio_Menu_Item
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Radio_Menu_Item_Record, Gtk_Radio_Menu_Item);
   function "+"
     (Widget : access Gtk_Radio_Menu_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Radio_Menu_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Radio_Menu_Item_Record, Gtk_Radio_Menu_Item);
   function "+"
     (Widget : access Gtk_Radio_Menu_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Radio_Menu_Item
   renames Implements_Gtk_Buildable.To_Object;

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
end Gtk.Radio_Menu_Item;
