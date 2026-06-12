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

--  Displays a popup when clicked.
--
--  <picture> <source srcset="menu-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkMenuButton"
--  src="menu-button.png"> </picture>
--  This popup can be provided either as a `GtkPopover` or as an abstract
--  `GMenuModel`.
--
--  The `GtkMenuButton` widget can show either an icon (set with the
--  [propertyGtk.MenuButton:icon-name] property) or a label (set with the
--  [propertyGtk.MenuButton:label] property). If neither is explicitly set, a
--  [classGtk.Image] is automatically created, using an arrow image oriented
--  according to [propertyGtk.MenuButton:direction] or the generic
--  "open-menu-symbolic" icon if the direction is not set.
--
--  The positioning of the popup is determined by the
--  [propertyGtk.MenuButton:direction] property of the menu button.
--
--  For menus, the [propertyGtk.Widget:halign] and [propertyGtk.Widget:valign]
--  properties of the menu are also taken into account. For example, when the
--  direction is Gtk.Enums.Arrow_Down and the horizontal alignment is
--  Gtk.Widget.Align_Start, the menu will be positioned below the button, with
--  the starting edge (depending on the text direction) of the menu aligned
--  with the starting edge of the button. If there is not enough space below
--  the button, the menu is popped up above the button instead. If the
--  alignment would move part of the menu offscreen, it is "pushed in".
--
--  | | start | center | end | | - | --- | --- | --- | | **down** |
--  ![](down-start.png) | ![](down-center.png) | ![](down-end.png) | | **up** |
--  ![](up-start.png) | ![](up-center.png) | ![](up-end.png) | | **left** |
--  ![](left-start.png) | ![](left-center.png) | ![](left-end.png) | |
--  **right** | ![](right-start.png) | ![](right-center.png) |
--  ![](right-end.png) |
--
--  # CSS nodes
--
--  ``` menubutton ╰── button.toggle ╰── <content> ╰── [arrow] ```
--
--  `GtkMenuButton` has a single CSS node with name `menubutton` which
--  contains a `button` node with a `.toggle` style class.
--
--  If the button contains an icon, it will have the `.image-button` style
--  class, if it contains text, it will have `.text-button` style class. If an
--  arrow is visible in addition to an icon, text or a custom child, it will
--  also have `.arrow-button` style class.
--
--  Inside the toggle button content, there is an `arrow` node for the
--  indicator, which will carry one of the `.none`, `.up`, `.down`, `.left` or
--  `.right` style classes to indicate the direction that the menu will appear
--  in. The CSS is expected to provide a suitable image for each of these cases
--  using the `-gtk-icon-source` property.
--
--  Optionally, the `menubutton` node can carry the `.circular` style class to
--  request a round appearance.
--
--  # Accessibility
--
--  `GtkMenuButton` uses the [enumGtk.AccessibleRole.button] role.
--
--  <group>Menus and Toolbars</group>
--  <gtkada_demo>create_menu.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Menu_Model;       use Glib.Menu_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Popover;           use Gtk.Popover;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Menu_Button is

   type Gtk_Menu_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Menu_Button is access all Gtk_Menu_Button_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Menu_Button_Create_Popup_Func is access procedure
     (Menu_Button : not null access Gtk_Menu_Button_Record'Class);
   --  User-provided callback function to create a popup for a `GtkMenuButton`
   --  on demand.
   --  This function is called when the popup of Menu_Button is shown, but
   --  none has been provided via [methodGtk.MenuButton.set_popover] or
   --  [methodGtk.MenuButton.set_menu_model].
   --  @param Menu_Button the `GtkMenuButton`

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Menu_Button);
   procedure Initialize
      (Self : not null access Gtk_Menu_Button_Record'Class);
   --  Creates a new `GtkMenuButton` widget with downwards-pointing arrow as
   --  the only child.
   --  You can replace the child widget with another `GtkWidget` should you
   --  wish to.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Menu_Button_New return Gtk_Menu_Button;
   --  Creates a new `GtkMenuButton` widget with downwards-pointing arrow as
   --  the only child.
   --  You can replace the child widget with another `GtkWidget` should you
   --  wish to.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_menu_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Returns whether the menu button is active.
   --  Since: gtk+ 4.10
   --  @return TRUE if the button is active

   procedure Set_Active
      (Self   : not null access Gtk_Menu_Button_Record;
       Active : Boolean);
   --  Sets whether the menu button is active.
   --  Since: gtk+ 4.10
   --  @param Active whether the menu button is active

   function Get_Always_Show_Arrow
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Gets whether to show a dropdown arrow even when using an icon or a
   --  custom child.
   --  Since: gtk+ 4.4
   --  @return whether to show a dropdown arrow even when using an icon or a
   --  custom child.

   procedure Set_Always_Show_Arrow
      (Self              : not null access Gtk_Menu_Button_Record;
       Always_Show_Arrow : Boolean);
   --  Sets whether to show a dropdown arrow even when using an icon or a
   --  custom child.
   --  Since: gtk+ 4.4
   --  @param Always_Show_Arrow whether to show a dropdown arrow even when
   --  using an icon or a custom child

   function Get_Can_Shrink
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Retrieves whether the button can be smaller than the natural size of
   --  its contents.
   --  Since: gtk+ 4.12
   --  @return true if the button can shrink, and false otherwise

   procedure Set_Can_Shrink
      (Self       : not null access Gtk_Menu_Button_Record;
       Can_Shrink : Boolean);
   --  Sets whether the button size can be smaller than the natural size of
   --  its contents.
   --  For text buttons, setting Can_Shrink to true will ellipsize the label.
   --  For icon buttons, this function has no effect.
   --  Since: gtk+ 4.12
   --  @param Can_Shrink whether the button can shrink

   function Get_Child
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Menu_Button.
   --  Since: gtk+ 4.6
   --  @return the child widget of Menu_Button

   procedure Set_Child
      (Self  : not null access Gtk_Menu_Button_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Menu_Button.
   --  Setting a child resets [propertyGtk.MenuButton:label] and
   --  [propertyGtk.MenuButton:icon-name].
   --  If [propertyGtk.MenuButton:always-show-arrow] is set to `TRUE` and
   --  [propertyGtk.MenuButton:direction] is not `GTK_ARROW_NONE`, a dropdown
   --  arrow will be shown next to the child.
   --  Since: gtk+ 4.6
   --  @param Child the child widget

   function Get_Direction
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Enums.Gtk_Arrow_Type;
   --  Returns the direction the popup will be pointing at when popped up.
   --  @return a `GtkArrowType` value

   procedure Set_Direction
      (Self      : not null access Gtk_Menu_Button_Record;
       Direction : Gtk.Enums.Gtk_Arrow_Type);
   --  Sets the direction in which the popup will be popped up.
   --  If the button is automatically populated with an arrow icon, its
   --  direction will be changed to match.
   --  If the does not fit in the available space in the given direction, GTK
   --  will its best to keep it inside the screen and fully visible.
   --  If you pass Gtk.Enums.Arrow_None for a Direction, the popup will behave
   --  as if you passed Gtk.Enums.Arrow_Down (although you won't see any
   --  arrows).
   --  @param Direction a `GtkArrowType`

   function Get_Has_Frame
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Returns whether the button has a frame.
   --  @return True if the button has a frame

   procedure Set_Has_Frame
      (Self      : not null access Gtk_Menu_Button_Record;
       Has_Frame : Boolean);
   --  Sets the style of the button.
   --  @param Has_Frame whether the button should have a visible frame

   function Get_Icon_Name
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String;
   --  Gets the name of the icon shown in the button.
   --  @return the name of the icon shown in the button

   procedure Set_Icon_Name
      (Self      : not null access Gtk_Menu_Button_Record;
       Icon_Name : UTF8_String);
   --  Sets the name of an icon to show inside the menu button.
   --  Setting icon name resets [propertyGtk.MenuButton:label] and
   --  [propertyGtk.MenuButton:child].
   --  If [propertyGtk.MenuButton:always-show-arrow] is set to `TRUE` and
   --  [propertyGtk.MenuButton:direction] is not `GTK_ARROW_NONE`, a dropdown
   --  arrow will be shown next to the icon.
   --  @param Icon_Name the icon name

   function Get_Label
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String;
   --  Gets the label shown in the button
   --  @return the label shown in the button

   procedure Set_Label
      (Self  : not null access Gtk_Menu_Button_Record;
       Label : UTF8_String);
   --  Sets the label to show inside the menu button.
   --  Setting a label resets [propertyGtk.MenuButton:icon-name] and
   --  [propertyGtk.MenuButton:child].
   --  If [propertyGtk.MenuButton:direction] is not `GTK_ARROW_NONE`, a
   --  dropdown arrow will be shown next to the label.
   --  @param Label the label

   function Get_Menu_Model
      (Self : not null access Gtk_Menu_Button_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the `GMenuModel` used to generate the popup.
   --  @return a `GMenuModel`

   procedure Set_Menu_Model
      (Self       : not null access Gtk_Menu_Button_Record;
       Menu_Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets the `GMenuModel` from which the popup will be constructed.
   --  If Menu_Model is null, the button is disabled.
   --  A [classGtk.Popover] will be created from the menu model with
   --  [ctorGtk.PopoverMenu.new_from_model]. Actions will be connected as
   --  documented for this function.
   --  If [propertyGtk.MenuButton:popover] is already set, it will be
   --  dissociated from the Menu_Button, and the property is set to null.
   --  @param Menu_Model a `GMenuModel`, or null to unset and disable the
   --  button

   function Get_Popover
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Popover.Gtk_Popover;
   --  Returns the `GtkPopover` that pops out of the button.
   --  If the button is not using a `GtkPopover`, this function returns null.
   --  @return a `GtkPopover` or null

   procedure Set_Popover
      (Self    : not null access Gtk_Menu_Button_Record;
       Popover : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the `GtkPopover` that will be popped up when the Menu_Button is
   --  clicked.
   --  If Popover is null, the button is disabled.
   --  If [propertyGtk.MenuButton:menu-model] is set, the menu model is
   --  dissociated from the Menu_Button, and the property is set to null.
   --  @param Popover a `GtkPopover`, or null to unset and disable the button

   function Get_Primary
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Returns whether the menu button acts as a primary menu.
   --  Since: gtk+ 4.4
   --  @return True if the button is a primary menu

   procedure Set_Primary
      (Self    : not null access Gtk_Menu_Button_Record;
       Primary : Boolean);
   --  Sets whether menu button acts as a primary menu.
   --  Primary menus can be opened with the <kbd>F10</kbd> key.
   --  Since: gtk+ 4.4
   --  @param Primary whether the menubutton should act as a primary menu

   function Get_Use_Underline
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Returns whether an embedded underline in the text indicates a mnemonic.
   --  @return True whether an embedded underline in the text indicates the
   --  mnemonic accelerator keys.

   procedure Set_Use_Underline
      (Self          : not null access Gtk_Menu_Button_Record;
       Use_Underline : Boolean);
   --  If true, an underline in the text indicates a mnemonic.
   --  @param Use_Underline True if underlines in the text indicate mnemonics

   procedure Popdown (Self : not null access Gtk_Menu_Button_Record);
   --  Dismiss the menu.

   procedure Popup (Self : not null access Gtk_Menu_Button_Record);
   --  Pop up the menu.

   procedure Set_Create_Popup_Func
      (Self           : not null access Gtk_Menu_Button_Record;
       Func           : Gtk_Menu_Button_Create_Popup_Func;
       Destroy_Notify : Glib.G_Destroy_Notify_Address);
   --  Sets Func to be called when a popup is about to be shown.
   --  Func should use one of
   --  - [methodGtk.MenuButton.set_popover] -
   --  [methodGtk.MenuButton.set_menu_model]
   --  to set a popup for Menu_Button. If Func is non-null, Menu_Button will
   --  always be sensitive.
   --  Using this function will not reset the menu widget attached to
   --  Menu_Button. Instead, this can be done manually in Func.
   --  @param Func function to call when a popup is about to be shown, but
   --  none has been provided via other means, or null to reset to default
   --  behavior
   --  @param Destroy_Notify destroy notify for User_Data

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Create_Popup_Func_User_Data is

      type Gtk_Menu_Button_Create_Popup_Func is access procedure
        (Menu_Button : not null access Gtk.Menu_Button.Gtk_Menu_Button_Record'Class;
         User_Data   : User_Data_Type);
      --  User-provided callback function to create a popup for a `GtkMenuButton`
      --  on demand.
      --  This function is called when the popup of Menu_Button is shown, but
      --  none has been provided via [methodGtk.MenuButton.set_popover] or
      --  [methodGtk.MenuButton.set_menu_model].
      --  @param Menu_Button the `GtkMenuButton`
      --  @param User_Data User data passed to
      --  Gtk.Menu_Button.Set_Create_Popup_Func

      procedure Set_Create_Popup_Func
         (Self           : not null access Gtk.Menu_Button.Gtk_Menu_Button_Record'Class;
          Func           : Gtk_Menu_Button_Create_Popup_Func;
          User_Data      : User_Data_Type;
          Destroy_Notify : Glib.G_Destroy_Notify_Address);
      --  Sets Func to be called when a popup is about to be shown.
      --  Func should use one of
      --  - [methodGtk.MenuButton.set_popover] -
      --  [methodGtk.MenuButton.set_menu_model]
      --  to set a popup for Menu_Button. If Func is non-null, Menu_Button
      --  will always be sensitive.
      --  Using this function will not reset the menu widget attached to
      --  Menu_Button. Instead, this can be done manually in Func.
      --  @param Func function to call when a popup is about to be shown, but
      --  none has been provided via other means, or null to reset to default
      --  behavior
      --  @param User_Data user data to pass to Func
      --  @param Destroy_Notify destroy notify for User_Data

   end Set_Create_Popup_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Menu_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Menu_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Menu_Button_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Menu_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Menu_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Menu_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Menu_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Menu_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Menu_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the menu button is active.

   Always_Show_Arrow_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show a dropdown arrow even when using an icon or a custom
   --  child.

   Can_Shrink_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the size of the button can be made smaller than the natural
   --  size of its contents.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Direction_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type;
   --  The `GtkArrowType` representing the direction in which the menu or
   --  popover will be popped out.

   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the button has a frame.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the icon used to automatically populate the button.

   Label_Property : constant Glib.Properties.Property_String;
   --  The label for the button.

   Menu_Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  The `GMenuModel` from which the popup will be created.
   --
   --  See [methodGtk.MenuButton.set_menu_model] for the interaction with the
   --  [propertyGtk.MenuButton:popover] property.

   Popover_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Popover.Gtk_Popover
   --  The `GtkPopover` that will be popped up when the button is clicked.

   Primary_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the menu button acts as a primary menu.
   --
   --  Primary menus can be opened using the <kbd>F10</kbd> key

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   --  If set an underscore in the text indicates a mnemonic.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Menu_Button_Void is not null access procedure
     (Self : access Gtk_Menu_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Menu_Button_Record;
       Call  : Cb_Gtk_Menu_Button_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Menu_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to when the menu button is activated.
   --
   --  The `::activate` signal on `GtkMenuButton` is an action signal and
   --  emitting it causes the button to pop up its menu.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Menu_Button_Record, Gtk_Menu_Button);
   function "+"
     (Widget : access Gtk_Menu_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Menu_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Menu_Button_Record, Gtk_Menu_Button);
   function "+"
     (Widget : access Gtk_Menu_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Menu_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Menu_Button_Record, Gtk_Menu_Button);
   function "+"
     (Widget : access Gtk_Menu_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Menu_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Primary_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary");
   Popover_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("popover");
   Menu_Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("menu-model");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type :=
     Gtk.Enums.Build ("direction");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Can_Shrink_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-shrink");
   Always_Show_Arrow_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("always-show-arrow");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Menu_Button;
