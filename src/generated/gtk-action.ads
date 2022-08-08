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
--  > In GTK+ 3.10, GtkAction has been deprecated. Use Glib.Action.Gaction >
--  instead, and associate actions with Gtk.Actionable.Gtk_Actionable widgets.
--  Use > Glib.Menu_Model.Gmenu_Model for creating menus with
--  Gtk.Menu.Gtk_New_From_Model.
--
--  Actions represent operations that the user can be perform, along with some
--  information how it should be presented in the interface. Each action
--  provides methods to create icons, menu items and toolbar items representing
--  itself.
--
--  As well as the callback that is called when the action gets activated, the
--  following also gets associated with the action:
--
--  - a name (not translated, for path lookup)
--
--  - a label (translated, for display)
--
--  - an accelerator
--
--  - whether label indicates a stock id
--
--  - a tooltip (optional, translated)
--
--  - a toolbar label (optional, shorter than label)
--
--  The action will also have some state information:
--
--  - visible (shown/hidden)
--
--  - sensitive (enabled/disabled)
--
--  Apart from regular actions, there are [toggle actions][GtkToggleAction],
--  which can be toggled between two states and [radio
--  actions][GtkRadioAction], of which only one in a group can be in the
--  "active" state. Other actions can be implemented as Gtk.Action.Gtk_Action
--  subclasses.
--
--  Each action can have one or more proxy widgets. To act as an action proxy,
--  widget needs to implement Gtk.Activatable.Gtk_Activatable interface.
--  Proxies mirror the state of the action and should change when the action's
--  state changes. Properties that are always mirrored by proxies are
--  Gtk.Action.Gtk_Action:sensitive and Gtk.Action.Gtk_Action:visible.
--  Gtk.Action.Gtk_Action:gicon, Gtk.Action.Gtk_Action:icon-name,
--  Gtk.Action.Gtk_Action:label, Gtk.Action.Gtk_Action:short-label and
--  Gtk.Action.Gtk_Action:stock-id properties are only mirorred if proxy widget
--  has Gtk.Activatable.Gtk_Activatable:use-action-appearance property set to
--  True.
--
--  When the proxy is activated, it should activate its action.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.G_Icon;     use Glib.G_Icon;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Action is

   type Gtk_Action_Record is new GObject_Record with null record;
   type Gtk_Action is access all Gtk_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Action   : out Gtk_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "");
   procedure Initialize
      (Action   : not null access Gtk_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "");
   --  Creates a new Gtk.Action.Gtk_Action object. To add the action to a
   --  Gtk.Action_Group.Gtk_Action_Group and set the accelerator for the
   --  action, call Gtk.Action_Group.Add_Action_With_Accel. See the [UI
   --  Definition section][XML-UI] for information on allowed action names.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": A unique name for the action
   --  "label": the label displayed in menu items and on buttons, or null
   --  "tooltip": a tooltip for the action, or null
   --  "stock_id": the stock icon to display in widgets representing the
   --  action, or null

   function Gtk_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "") return Gtk_Action;
   --  Creates a new Gtk.Action.Gtk_Action object. To add the action to a
   --  Gtk.Action_Group.Gtk_Action_Group and set the accelerator for the
   --  action, call Gtk.Action_Group.Add_Action_With_Accel. See the [UI
   --  Definition section][XML-UI] for information on allowed action names.
   --  Since: gtk+ 2.4
   --  "name": A unique name for the action
   --  "label": the label displayed in menu items and on buttons, or null
   --  "tooltip": a tooltip for the action, or null
   --  "stock_id": the stock icon to display in widgets representing the
   --  action, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_action_get_type");

   -------------
   -- Methods --
   -------------

   procedure Activate (Action : not null access Gtk_Action_Record);
   pragma Obsolescent (Activate);
   --  Emits the "activate" signal on the specified action, if it isn't
   --  insensitive. This gets called by the proxy widgets when they get
   --  activated.
   --  It can also be used to manually activate an action.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Block_Activate (Action : not null access Gtk_Action_Record);
   pragma Obsolescent (Block_Activate);
   --  Disable activation signals from the action
   --  This is needed when updating the state of your proxy
   --  Gtk.Activatable.Gtk_Activatable widget could result in calling
   --  Gtk.Action.Activate, this is a convenience function to avoid recursing
   --  in those cases (updating toggle state for instance).
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Connect_Accelerator
      (Action : not null access Gtk_Action_Record);
   pragma Obsolescent (Connect_Accelerator);
   --  Installs the accelerator for Action if Action has an accel path and
   --  group. See Gtk.Action.Set_Accel_Path and Gtk.Action.Set_Accel_Group
   --  Since multiple proxies may independently trigger the installation of
   --  the accelerator, the Action counts the number of times this function has
   --  been called and doesn't remove the accelerator until
   --  Gtk.Action.Disconnect_Accelerator has been called as many times.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Create_Icon
      (Action    : not null access Gtk_Action_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size) return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Create_Icon);
   --  This function is intended for use by action implementations to create
   --  icons displayed in the proxy widgets.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "icon_size": the size of the icon (Gtk.Enums.Gtk_Icon_Size) that should
   --  be created.

   function Create_Menu
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Create_Menu);
   --  If Action provides a Gtk.Menu.Gtk_Menu widget as a submenu for the menu
   --  item or the toolbar item it creates, this function returns an instance
   --  of that menu.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.10, 1

   function Create_Menu_Item
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Create_Menu_Item);
   --  Creates a menu item widget that proxies for the given action.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Create_Tool_Item
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Create_Tool_Item);
   --  Creates a toolbar item widget that proxies for the given action.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Disconnect_Accelerator
      (Action : not null access Gtk_Action_Record);
   pragma Obsolescent (Disconnect_Accelerator);
   --  Undoes the effect of one call to Gtk.Action.Connect_Accelerator.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Accel_Path
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Accel_Path);
   --  Returns the accel path for this action.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.10, 1

   procedure Set_Accel_Path
      (Action     : not null access Gtk_Action_Record;
       Accel_Path : UTF8_String);
   pragma Obsolescent (Set_Accel_Path);
   --  Sets the accel path for this action. All proxy widgets associated with
   --  the action will have this accel path, so that their accelerators are
   --  consistent.
   --  Note that Accel_Path string will be stored in a Glib.GQuark. Therefore,
   --  if you pass a static string, you can save some memory by interning it
   --  first with g_intern_static_string.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "accel_path": the accelerator path

   function Get_Always_Show_Image
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Get_Always_Show_Image);
   --  Returns whether Action's menu item proxies will always show their
   --  image, if available.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.10, 1

   procedure Set_Always_Show_Image
      (Action      : not null access Gtk_Action_Record;
       Always_Show : Boolean);
   pragma Obsolescent (Set_Always_Show_Image);
   --  Sets whether Action's menu item proxies will ignore the
   --  Gtk.Settings.Gtk_Settings:gtk-menu-images setting and always show their
   --  image, if available.
   --  Use this if the menu item would be useless or hard to use without their
   --  image.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.10, 1
   --  "always_show": True if menuitem proxies should always show their image

   function Get_Gicon
      (Action : not null access Gtk_Action_Record) return Glib.G_Icon.G_Icon;
   pragma Obsolescent (Get_Gicon);
   --  Gets the gicon of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Gicon
      (Action : not null access Gtk_Action_Record;
       Icon   : Glib.G_Icon.G_Icon);
   pragma Obsolescent (Set_Gicon);
   --  Sets the icon of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "icon": the Glib.G_Icon.G_Icon to set

   function Get_Icon_Name
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Icon_Name);
   --  Gets the icon name of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Icon_Name
      (Action    : not null access Gtk_Action_Record;
       Icon_Name : UTF8_String);
   pragma Obsolescent (Set_Icon_Name);
   --  Sets the icon name on Action
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "icon_name": the icon name to set

   function Get_Is_Important
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Get_Is_Important);
   --  Checks whether Action is important or not
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Is_Important
      (Action       : not null access Gtk_Action_Record;
       Is_Important : Boolean);
   pragma Obsolescent (Set_Is_Important);
   --  Sets whether the action is important, this attribute is used primarily
   --  by toolbar items to decide whether to show a label or not.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "is_important": True to make the action important

   function Get_Label
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Label);
   --  Gets the label text of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Label
      (Action : not null access Gtk_Action_Record;
       Label  : UTF8_String);
   pragma Obsolescent (Set_Label);
   --  Sets the label of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "label": the label text to set

   function Get_Name
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Name);
   --  Returns the name of the action.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Proxies
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   pragma Obsolescent (Get_Proxies);
   --  Returns the proxy widgets for an action. See also
   --  Gtk.Activatable.Get_Related_Action.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Get_Sensitive
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Get_Sensitive);
   --  Returns whether the action itself is sensitive. Note that this doesn't
   --  necessarily mean effective sensitivity. See Gtk.Action.Is_Sensitive for
   --  that.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Sensitive
      (Action    : not null access Gtk_Action_Record;
       Sensitive : Boolean);
   pragma Obsolescent (Set_Sensitive);
   --  Sets the :sensitive property of the action to Sensitive. Note that this
   --  doesn't necessarily mean effective sensitivity. See
   --  Gtk.Action.Is_Sensitive for that.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.10, 1
   --  "sensitive": True to make the action sensitive

   function Get_Short_Label
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Short_Label);
   --  Gets the short label text of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Short_Label
      (Action      : not null access Gtk_Action_Record;
       Short_Label : UTF8_String);
   pragma Obsolescent (Set_Short_Label);
   --  Sets a shorter label text on Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "short_label": the label text to set

   function Get_Stock_Id
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Stock_Id);
   --  Gets the stock id of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Stock_Id
      (Action   : not null access Gtk_Action_Record;
       Stock_Id : UTF8_String);
   pragma Obsolescent (Set_Stock_Id);
   --  Sets the stock id on Action
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "stock_id": the stock id

   function Get_Tooltip
      (Action : not null access Gtk_Action_Record) return UTF8_String;
   pragma Obsolescent (Get_Tooltip);
   --  Gets the tooltip text of Action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Tooltip
      (Action  : not null access Gtk_Action_Record;
       Tooltip : UTF8_String);
   pragma Obsolescent (Set_Tooltip);
   --  Sets the tooltip text on Action
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "tooltip": the tooltip text

   function Get_Visible
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Get_Visible);
   --  Returns whether the action itself is visible. Note that this doesn't
   --  necessarily mean effective visibility. See Gtk.Action.Is_Sensitive for
   --  that.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Visible
      (Action  : not null access Gtk_Action_Record;
       Visible : Boolean);
   pragma Obsolescent (Set_Visible);
   --  Sets the :visible property of the action to Visible. Note that this
   --  doesn't necessarily mean effective visibility. See Gtk.Action.Is_Visible
   --  for that.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.10, 1
   --  "visible": True to make the action visible

   function Get_Visible_Horizontal
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Get_Visible_Horizontal);
   --  Checks whether Action is visible when horizontal
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Visible_Horizontal
      (Action             : not null access Gtk_Action_Record;
       Visible_Horizontal : Boolean);
   pragma Obsolescent (Set_Visible_Horizontal);
   --  Sets whether Action is visible when horizontal
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "visible_horizontal": whether the action is visible horizontally

   function Get_Visible_Vertical
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Get_Visible_Vertical);
   --  Checks whether Action is visible when horizontal
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Visible_Vertical
      (Action           : not null access Gtk_Action_Record;
       Visible_Vertical : Boolean);
   pragma Obsolescent (Set_Visible_Vertical);
   --  Sets whether Action is visible when vertical
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "visible_vertical": whether the action is visible vertically

   function Is_Sensitive
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Is_Sensitive);
   --  Returns whether the action is effectively sensitive.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   function Is_Visible
      (Action : not null access Gtk_Action_Record) return Boolean;
   pragma Obsolescent (Is_Visible);
   --  Returns whether the action is effectively visible.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1

   procedure Set_Accel_Group
      (Action      : not null access Gtk_Action_Record;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   pragma Obsolescent (Set_Accel_Group);
   --  Sets the Gtk.Accel_Group.Gtk_Accel_Group in which the accelerator for
   --  this action will be installed.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.10, 1
   --  "accel_group": a Gtk.Accel_Group.Gtk_Accel_Group or null

   procedure Unblock_Activate (Action : not null access Gtk_Action_Record);
   pragma Obsolescent (Unblock_Activate);
   --  Reenable activation signals from the action
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Action_Group_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Action_Group.Gtk_Action_Group
   --  The GtkActionGroup this GtkAction is associated with, or NULL (for
   --  internal use).

   Always_Show_Image_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the action's menu item proxies will ignore the
   --  Gtk.Settings.Gtk_Settings:gtk-menu-images setting and always show their
   --  image, if available.
   --
   --  Use this property if the menu item would be useless or hard to use
   --  without their image.

   G_Icon_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Glib.G_Icon.G_Icon
   --  The Glib.G_Icon.G_Icon displayed in the Gtk.Action.Gtk_Action.
   --
   --  Note that the stock icon is preferred, if the
   --  Gtk.Action.Gtk_Action:stock-id property holds the id of an existing
   --  stock icon.
   --
   --  This is an appearance property and thus only applies if
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance is True.

   Hide_If_Empty_Property : constant Glib.Properties.Property_Boolean;
   --  When TRUE, empty menu proxies for this action are hidden.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the icon from the icon theme.
   --
   --  Note that the stock icon is preferred, if the
   --  Gtk.Action.Gtk_Action:stock-id property holds the id of an existing
   --  stock icon, and the Glib.G_Icon.G_Icon is preferred if the
   --  Gtk.Action.Gtk_Action:gicon property is set.
   --
   --  This is an appearance property and thus only applies if
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance is True.

   Is_Important_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the action is considered important. When TRUE, toolitem proxies
   --  for this action show text in GTK_TOOLBAR_BOTH_HORIZ mode.

   Label_Property : constant Glib.Properties.Property_String;
   --  The label used for menu items and buttons that activate this action. If
   --  the label is null, GTK+ uses the stock label specified via the stock-id
   --  property.
   --
   --  This is an appearance property and thus only applies if
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance is True.

   Name_Property : constant Glib.Properties.Property_String;
   --  A unique name for the action.

   Sensitive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the action is enabled.

   Short_Label_Property : constant Glib.Properties.Property_String;
   --  A shorter label that may be used on toolbar buttons.
   --
   --  This is an appearance property and thus only applies if
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance is True.

   Stock_Id_Property : constant Glib.Properties.Property_String;
   --  The stock icon displayed in widgets representing this action.
   --
   --  This is an appearance property and thus only applies if
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance is True.

   Tooltip_Property : constant Glib.Properties.Property_String;
   --  A tooltip for this action.

   Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the action is visible.

   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the toolbar item is visible when the toolbar is in a horizontal
   --  orientation.

   Visible_Overflown_Property : constant Glib.Properties.Property_Boolean;
   --  When True, toolitem proxies for this action are represented in the
   --  toolbar overflow menu.

   Visible_Vertical_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the toolbar item is visible when the toolbar is in a vertical
   --  orientation.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Action_Void is not null access procedure (Self : access Gtk_Action_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Action_Record;
       Call  : Cb_Gtk_Action_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Action_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The "activate" signal is emitted when the action is activated.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Action_Record, Gtk_Action);
   function "+"
     (Widget : access Gtk_Action_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Action
   renames Implements_Gtk_Buildable.To_Object;

private
   Visible_Vertical_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-vertical");
   Visible_Overflown_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-overflown");
   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-horizontal");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Tooltip_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip");
   Stock_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-id");
   Short_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("short-label");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Is_Important_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-important");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Hide_If_Empty_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hide-if-empty");
   G_Icon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gicon");
   Always_Show_Image_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("always-show-image");
   Action_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("action-group");
end Gtk.Action;
