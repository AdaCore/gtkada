------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  The Gtk.Menu_Item.Gtk_Menu_Item widget and the derived widgets are the
--  only valid childs for menus. Their function is to correctly handle
--  highlighting, alignment, events and submenus.
--
--  As it derives from Gtk.Bin.Gtk_Bin it can hold any valid child widget,
--  altough only a few are really useful.
--
--  == GtkMenuItem as GtkBuildable ==
--
--  The GtkMenuItem implementation of the GtkBuildable interface supports
--  adding a submenu by specifying "submenu" as the "type" attribute of a
--  <child> element.
--
--  == A UI definition fragment with submenus ==
--
--    <object class="GtkMenuItem">
--    <child type="submenu">
--    <object class="GtkMenu"/>
--    </child>
--    </object>
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item_Record is new Gtk_Bin_Record with null record;
   type Gtk_Menu_Item is access all Gtk_Menu_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item);
   procedure Initialize
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class);
   --  Creates a new Gtk.Menu_Item.Gtk_Menu_Item.

   procedure Gtk_New_With_Label
      (Menu_Item : out Gtk_Menu_Item;
       Label     : UTF8_String);
   procedure Initialize_With_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class;
       Label     : UTF8_String);
   --  Creates a new Gtk.Menu_Item.Gtk_Menu_Item whose child is a
   --  Gtk.Label.Gtk_Label.
   --  "label": the text for the label

   procedure Gtk_New_With_Mnemonic
      (Menu_Item : out Gtk_Menu_Item;
       Label     : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class;
       Label     : UTF8_String);
   --  Creates a new Gtk.Menu_Item.Gtk_Menu_Item containing a label.
   --  The label will be created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the menu item.
   --  "label": The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_menu_item_get_type");

   -------------
   -- Methods --
   -------------

   procedure Activate (Menu_Item : not null access Gtk_Menu_Item_Record);
   --  Emits the Gtk.Menu_Item.Gtk_Menu_Item::activate signal on the given
   --  item

   procedure Deselect (Menu_Item : not null access Gtk_Menu_Item_Record);
   --  Emits the Gtk.Menu_Item.Gtk_Menu_Item::deselect signal on the given
   --  item. Behaves exactly like gtk_item_deselect.

   function Get_Accel_Path
      (Menu_Item : not null access Gtk_Menu_Item_Record) return UTF8_String;
   procedure Set_Accel_Path
      (Menu_Item  : not null access Gtk_Menu_Item_Record;
       Accel_Path : UTF8_String := "");
   --  Set the accelerator path on Menu_Item, through which runtime changes of
   --  the menu item's accelerator caused by the user can be identified and
   --  saved to persistent storage (see gtk_accel_map_save on this). To set up
   --  a default accelerator for this menu item, call Gtk.Accel_Map.Add_Entry
   --  with the same Accel_Path. See also Gtk.Accel_Map.Add_Entry on the
   --  specifics of accelerator paths, and Gtk.Menu.Set_Accel_Path for a more
   --  convenient variant of this function.
   --  This function is basically a convenience wrapper that handles calling
   --  Gtk.Widget.Set_Accel_Path with the appropriate accelerator group for the
   --  menu item.
   --  Note that you do need to set an accelerator on the parent menu with
   --  Gtk.Menu.Set_Accel_Group for this to work.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  "accel_path": accelerator path, corresponding to this menu item's
   --  functionality, or null to unset the current path.

   function Get_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record) return UTF8_String;
   procedure Set_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Label     : UTF8_String);
   --  Sets Text on the Menu_Item label
   --  Since: gtk+ 2.16
   --  "label": the text you want to set

   function Get_Reserve_Indicator
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean;
   procedure Set_Reserve_Indicator
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Reserve   : Boolean);
   --  Sets whether the Menu_Item should reserve space for the submenu
   --  indicator, regardless if it actually has a submenu or not.
   --  There should be little need for applications to call this functions.
   --  Since: gtk+ 3.0
   --  "reserve": the new value

   function Get_Right_Justified
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean;
   pragma Obsolescent (Get_Right_Justified);
   procedure Set_Right_Justified
      (Menu_Item       : not null access Gtk_Menu_Item_Record;
       Right_Justified : Boolean := True);
   pragma Obsolescent (Set_Right_Justified);
   --  Sets whether the menu item appears justified at the right side of a
   --  menu bar. This was traditionally done for "Help" menu items, but is now
   --  considered a bad idea. (If the widget layout is reversed for a
   --  right-to-left language like Hebrew or Arabic, right-justified-menu-items
   --  appear at the left.)
   --  Gtk.Widget.Set_Hexpand and Gtk.Widget.Set_Halign.
   --  Deprecated since 3.2, If you insist on using it, use
   --  "right_justified": if True the menu item will appear at the far right
   --  if added to a menu bar

   function Get_Submenu
      (Menu_Item : not null access Gtk_Menu_Item_Record)
       return Gtk.Widget.Gtk_Widget;
   procedure Set_Submenu
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Submenu   : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets or replaces the menu item's submenu, or removes it when a null
   --  submenu is passed.
   --  "submenu": the submenu, or null

   function Get_Use_Underline
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean;
   procedure Set_Use_Underline
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Setting   : Boolean);
   --  If true, an underline in the text indicates the next character should
   --  be used for the mnemonic accelerator key.
   --  Since: gtk+ 2.16
   --  "setting": True if underlines in the text indicate mnemonics

   procedure Gtk_Select (Menu_Item : not null access Gtk_Menu_Item_Record);
   --  Emits the Gtk.Menu_Item.Gtk_Menu_Item::select signal on the given item.
   --  Behaves exactly like gtk_item_select.

   procedure Toggle_Size_Allocate
      (Menu_Item  : not null access Gtk_Menu_Item_Record;
       Allocation : Gint);
   --  Emits the Gtk.Menu_Item.Gtk_Menu_Item::toggle-size-allocate signal on
   --  the given item.
   --  "allocation": the allocation to use as signal data.

   procedure Toggle_Size_Request
      (Menu_Item   : not null access Gtk_Menu_Item_Record;
       Requisition : in out Gint);
   --  Emits the Gtk.Menu_Item.Gtk_Menu_Item::toggle-size-request signal on
   --  the given item.
   --  "requisition": the requisition to use as signal data.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Menu_Item : out Gtk_Menu_Item;
      Label     : UTF8_String) renames Gtk_New_With_Label;
   procedure Initialize
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Label     : UTF8_String) renames Initialize_With_Label;
   --  For backwards compatibility.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : not null access Gtk_Menu_Item_Record)
       return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : not null access Gtk_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Menu_Item_Record) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Menu_Item_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Menu_Item_Record, Gtk_Menu_Item);
   function "+"
     (Widget : access Gtk_Menu_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Menu_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Menu_Item_Record, Gtk_Menu_Item);
   function "+"
     (Widget : access Gtk_Menu_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Menu_Item
   renames Implements_Gtk_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Accel_Path_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Sets the accelerator path of the menu item, through which runtime
   --  changes of the menu item's accelerator caused by the user can be
   --  identified and saved to persistant storage.
   --
   --  Name: Label_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The text for the child label.
   --
   --  Name: Right_Justified_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Sets whether the menu item appears justified at the right side of a
   --  menu bar.
   --
   --  Name: Submenu_Property
   --  Type: Gtk.Menu.Gtk_Menu
   --  Flags: read-write
   --  The submenu attached to the menu item, or null if it has none.
   --
   --  Name: Use_Underline_Property
   --  Type: Boolean
   --  Flags: read-write
   --  True if underlines in the text indicate mnemonics.

   Accel_Path_Property : constant Glib.Properties.Property_String;
   Label_Property : constant Glib.Properties.Property_String;
   Right_Justified_Property : constant Glib.Properties.Property_Boolean;
   Submenu_Property : constant Glib.Properties.Property_Object;
   Use_Underline_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate"
   --     procedure Handler (Self : access Gtk_Menu_Item_Record'Class);
   --  Emitted when the item is activated.
   --
   --  "activate-item"
   --     procedure Handler (Self : access Gtk_Menu_Item_Record'Class);
   --  Emitted when the item is activated, but also if the menu item has a
   --  submenu. For normal applications, the relevant signal is
   --  Gtk.Menu_Item.Gtk_Menu_Item::activate.
   --
   --  "deselect"
   --     procedure Handler (Self : access Gtk_Menu_Item_Record'Class);
   --
   --  "select"
   --     procedure Handler (Self : access Gtk_Menu_Item_Record'Class);
   --
   --  "toggle-size-allocate"
   --     procedure Handler
   --       (Self   : access Gtk_Menu_Item_Record'Class;
   --        Object : Gint);
   --
   --  "toggle-size-request"
   --     procedure Handler
   --       (Self   : access Gtk_Menu_Item_Record'Class;
   --        Object : System.Address);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   Signal_Activate_Item : constant Glib.Signal_Name := "activate-item";
   Signal_Deselect : constant Glib.Signal_Name := "deselect";
   Signal_Gtk_Select : constant Glib.Signal_Name := "select";
   Signal_Toggle_Size_Allocate : constant Glib.Signal_Name := "toggle-size-allocate";
   Signal_Toggle_Size_Request : constant Glib.Signal_Name := "toggle-size-request";

private
   Accel_Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accel-path");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Right_Justified_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right-justified");
   Submenu_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("submenu");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
end Gtk.Menu_Item;
