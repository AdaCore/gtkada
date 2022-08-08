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
--  The Gtk.Menu_Button.Gtk_Menu_Button widget is used to display a popup when
--  clicked on. This popup can be provided either as a Gtk.Menu.Gtk_Menu, a
--  Gtk.Popover.Gtk_Popover or an abstract Glib.Menu_Model.Gmenu_Model.
--
--  The Gtk.Menu_Button.Gtk_Menu_Button widget can hold any valid child
--  widget. That is, it can hold almost any other standard
--  Gtk.Widget.Gtk_Widget. The most commonly used child is Gtk.Image.Gtk_Image.
--  If no widget is explicitely added to the Gtk.Menu_Button.Gtk_Menu_Button, a
--  Gtk.Image.Gtk_Image is automatically created, using an arrow image oriented
--  according to Gtk.Menu_Button.Gtk_Menu_Button:direction or the generic
--  "open-menu-symbolic" icon if the direction is not set.
--
--  The positioning of the popup is determined by the
--  Gtk.Menu_Button.Gtk_Menu_Button:direction property of the menu button.
--
--  For menus, the Gtk.Widget.Gtk_Widget:halign and
--  Gtk.Widget.Gtk_Widget:valign properties of the menu are also taken into
--  account. For example, when the direction is Gtk.Enums.Arrow_Down and the
--  horizontal alignment is Gtk.Widget.Align_Start, the menu will be positioned
--  below the button, with the starting edge (depending on the text direction)
--  of the menu aligned with the starting edge of the button. If there is not
--  enough space below the button, the menu is popped up above the button
--  instead. If the alignment would move part of the menu offscreen, it is
--  "pushed in".
--
--  ## Direction = Down
--
--  - halign = start
--
--  ![](down-start.png)
--
--  - halign = center
--
--  ![](down-center.png)
--
--  - halign = end
--
--  ![](down-end.png)
--
--  ## Direction = Up
--
--  - halign = start
--
--  ![](up-start.png)
--
--  - halign = center
--
--  ![](up-center.png)
--
--  - halign = end
--
--  ![](up-end.png)
--
--  ## Direction = Left
--
--  - valign = start
--
--  ![](left-start.png)
--
--  - valign = center
--
--  ![](left-center.png)
--
--  - valign = end
--
--  ![](left-end.png)
--
--  ## Direction = Right
--
--  - valign = start
--
--  ![](right-start.png)
--
--  - valign = center
--
--  ![](right-center.png)
--
--  - valign = end
--
--  ![](right-end.png)
--
--  # CSS nodes
--
--  GtkMenuButton has a single CSS node with name button. To differentiate it
--  from a plain Gtk.Button.Gtk_Button, it gets the .popup style class.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Menu_Model;   use Glib.Menu_Model;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Glib.Variant;      use Glib.Variant;
with Gtk.Action;        use Gtk.Action;
with Gtk.Actionable;    use Gtk.Actionable;
with Gtk.Activatable;   use Gtk.Activatable;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Popover;       use Gtk.Popover;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;        use Gtk.Widget;

package Gtk.Menu_Button is

   type Gtk_Menu_Button_Record is new Gtk_Toggle_Button_Record with null record;
   type Gtk_Menu_Button is access all Gtk_Menu_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Menu_Button);
   procedure Initialize
      (Self : not null access Gtk_Menu_Button_Record'Class);
   --  Creates a new Gtk.Menu_Button.Gtk_Menu_Button widget with
   --  downwards-pointing arrow as the only child. You can replace the child
   --  widget with another Gtk.Widget.Gtk_Widget should you wish to.
   --  Since: gtk+ 3.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Menu_Button_New return Gtk_Menu_Button;
   --  Creates a new Gtk.Menu_Button.Gtk_Menu_Button widget with
   --  downwards-pointing arrow as the only child. You can replace the child
   --  widget with another Gtk.Widget.Gtk_Widget should you wish to.
   --  Since: gtk+ 3.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_menu_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Align_Widget
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the parent Gtk.Widget.Gtk_Widget to use to line up with menu.
   --  Since: gtk+ 3.6

   procedure Set_Align_Widget
      (Self         : not null access Gtk_Menu_Button_Record;
       Align_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the Gtk.Widget.Gtk_Widget to use to line the menu with when popped
   --  up. Note that the Align_Widget must contain the
   --  Gtk.Menu_Button.Gtk_Menu_Button itself.
   --  Setting it to null means that the menu will be aligned with the button
   --  itself.
   --  Note that this property is only used with menus currently, and not for
   --  popovers.
   --  Since: gtk+ 3.6
   --  "align_widget": a Gtk.Widget.Gtk_Widget

   function Get_Direction
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Enums.Gtk_Arrow_Type;
   --  Returns the direction the popup will be pointing at when popped up.
   --  Since: gtk+ 3.6

   procedure Set_Direction
      (Self      : not null access Gtk_Menu_Button_Record;
       Direction : Gtk.Enums.Gtk_Arrow_Type);
   --  Sets the direction in which the popup will be popped up, as well as
   --  changing the arrow's direction. The child will not be changed to an
   --  arrow if it was customized.
   --  If the does not fit in the available space in the given direction, GTK+
   --  will its best to keep it inside the screen and fully visible.
   --  If you pass Gtk.Enums.Arrow_None for a Direction, the popup will behave
   --  as if you passed Gtk.Enums.Arrow_Down (although you won't see any
   --  arrows).
   --  Since: gtk+ 3.6
   --  "direction": a Gtk.Enums.Gtk_Arrow_Type

   function Get_Menu_Model
      (Self : not null access Gtk_Menu_Button_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the Glib.Menu_Model.Gmenu_Model used to generate the popup.
   --  Since: gtk+ 3.6

   procedure Set_Menu_Model
      (Self       : not null access Gtk_Menu_Button_Record;
       Menu_Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets the Glib.Menu_Model.Gmenu_Model from which the popup will be
   --  constructed, or null to dissociate any existing menu model and disable
   --  the button.
   --  Depending on the value of Gtk.Menu_Button.Gtk_Menu_Button:use-popover,
   --  either a Gtk.Menu.Gtk_Menu will be created with
   --  Gtk.Menu.Gtk_New_From_Model, or a Gtk.Popover.Gtk_Popover with
   --  Gtk.Popover.Gtk_New_From_Model. In either case, actions will be
   --  connected as documented for these functions.
   --  If Gtk.Menu_Button.Gtk_Menu_Button:popup or
   --  Gtk.Menu_Button.Gtk_Menu_Button:popover are already set, those widgets
   --  are dissociated from the Menu_Button, and those properties are set to
   --  null.
   --  Since: gtk+ 3.6
   --  "menu_model": a Glib.Menu_Model.Gmenu_Model, or null to unset and
   --  disable the button

   function Get_Popover
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Popover.Gtk_Popover;
   --  Returns the Gtk.Popover.Gtk_Popover that pops out of the button. If the
   --  button is not using a Gtk.Popover.Gtk_Popover, this function returns
   --  null.
   --  Since: gtk+ 3.12

   procedure Set_Popover
      (Self    : not null access Gtk_Menu_Button_Record;
       Popover : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the Gtk.Popover.Gtk_Popover that will be popped up when the
   --  Menu_Button is clicked, or null to dissociate any existing popover and
   --  disable the button.
   --  If Gtk.Menu_Button.Gtk_Menu_Button:menu-model or
   --  Gtk.Menu_Button.Gtk_Menu_Button:popup are set, those objects are
   --  dissociated from the Menu_Button, and those properties are set to null.
   --  Since: gtk+ 3.12
   --  "popover": a Gtk.Popover.Gtk_Popover, or null to unset and disable the
   --  button

   function Get_Popup
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Menu.Gtk_Menu;
   --  Returns the Gtk.Menu.Gtk_Menu that pops out of the button. If the
   --  button does not use a Gtk.Menu.Gtk_Menu, this function returns null.
   --  Since: gtk+ 3.6

   procedure Set_Popup
      (Self : not null access Gtk_Menu_Button_Record;
       Menu : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the Gtk.Menu.Gtk_Menu that will be popped up when the Menu_Button
   --  is clicked, or null to dissociate any existing menu and disable the
   --  button.
   --  If Gtk.Menu_Button.Gtk_Menu_Button:menu-model or
   --  Gtk.Menu_Button.Gtk_Menu_Button:popover are set, those objects are
   --  dissociated from the Menu_Button, and those properties are set to null.
   --  Since: gtk+ 3.6
   --  "menu": a Gtk.Menu.Gtk_Menu, or null to unset and disable the button

   function Get_Use_Popover
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   --  Returns whether a Gtk.Popover.Gtk_Popover or a Gtk.Menu.Gtk_Menu will
   --  be constructed from the menu model.
   --  Since: gtk+ 3.12

   procedure Set_Use_Popover
      (Self        : not null access Gtk_Menu_Button_Record;
       Use_Popover : Boolean);
   --  Sets whether to construct a Gtk.Popover.Gtk_Popover instead of
   --  Gtk.Menu.Gtk_Menu when Gtk.Menu_Button.Set_Menu_Model is called. Note
   --  that this property is only consulted when a new menu model is set.
   --  Since: gtk+ 3.12
   --  "use_popover": True to construct a popover from the menu model

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Menu_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Menu_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Menu_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Menu_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Menu_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Menu_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Menu_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Menu_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Align_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Container.Gtk_Container
   --  The Gtk.Widget.Gtk_Widget to use to align the menu with.

   Direction_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type;
   --  The Gtk.Enums.Gtk_Arrow_Type representing the direction in which the
   --  menu or popover will be popped out.

   Menu_Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  The Glib.Menu_Model.Gmenu_Model from which the popup will be created.
   --  Depending on the Gtk.Menu_Button.Gtk_Menu_Button:use-popover property,
   --  that may be a menu or a popover.
   --
   --  See Gtk.Menu_Button.Set_Menu_Model for the interaction with the
   --  Gtk.Menu_Button.Gtk_Menu_Button:popup property.

   Popover_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Popover.Gtk_Popover
   --  The Gtk.Popover.Gtk_Popover that will be popped up when the button is
   --  clicked.

   Popup_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Menu.Gtk_Menu
   --  The Gtk.Menu.Gtk_Menu that will be popped up when the button is
   --  clicked.

   Use_Popover_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to construct a Gtk.Popover.Gtk_Popover from the menu model, or
   --  a Gtk.Menu.Gtk_Menu.

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
     (Gtk.Actionable.Gtk_Actionable, Gtk_Menu_Button_Record, Gtk_Menu_Button);
   function "+"
     (Widget : access Gtk_Menu_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Menu_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Menu_Button_Record, Gtk_Menu_Button);
   function "+"
     (Widget : access Gtk_Menu_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Menu_Button
   renames Implements_Gtk_Activatable.To_Object;

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

private
   Use_Popover_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-popover");
   Popup_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("popup");
   Popover_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("popover");
   Menu_Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("menu-model");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type :=
     Gtk.Enums.Build ("direction");
   Align_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("align-widget");
end Gtk.Menu_Button;
