------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
--  The Gtk.Menu_Button.Gtk_Menu_Button widget is used to display a menu when
--  clicked on. This menu can be provided either as a Gtk.Menu.Gtk_Menu, or an
--  abstract Glib.Menu_Model.Gmenu_Model.
--
--  The Gtk.Menu_Button.Gtk_Menu_Button widget can hold any valid child
--  widget. That is, it can hold almost any other standard
--  Gtk.Widget.Gtk_Widget. The most commonly used child is the provided
--  Gtk.Arrow.Gtk_Arrow.
--
--  The positioning of the menu is determined by the
--  Gtk.Menu_Button.Gtk_Menu_Button:direction property of the menu button and
--  the Gtk.Widget.Gtk_Widget:halign or Gtk.Widget.Gtk_Widget:valign properties
--  of the menu. For example, when the direction is Gtk.Enums.Arrow_Down and
--  the horizontal alignment is Gtk.Widget.Align_Start, the menu will be
--  positioned below the button, with the starting edge (depending on the text
--  direction) of the menu aligned with the starting edge of the button. If
--  there is not enough space below the button, the menu is popped up above the
--  button instead. If the alignment would move part of the menu offscreen, it
--  is 'pushed in'.
--
-- 
--
-- 
--
--  halign = start
--
--  halign = center
--
--  halign = end
--
--  direction = down
--
--  <inlinemediaobject> <imageobject><imagedata fileref="down-start.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="down-center.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="down-end.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  direction = up
--
--  <inlinemediaobject> <imageobject><imagedata fileref="up-start.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="up-center.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="up-end.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
-- 
--
-- 
--
--  direction = left
--
--  direction = right
--
--  valign = start
--
--  <inlinemediaobject> <imageobject><imagedata fileref="left-start.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="right-start.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  valign = center
--
--  <inlinemediaobject> <imageobject><imagedata fileref="left-center.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="right-center.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  valign = end
--
--  <inlinemediaobject> <imageobject><imagedata fileref="left-end.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  <inlinemediaobject> <imageobject><imagedata fileref="right-end.png"
--  format="PNG"/></imageobject> </inlinemediaobject>
--  </description>
pragma Ada_2005;

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
   --  Setting it to null means that the popup menu will be aligned with the
   --  button itself.
   --  Since: gtk+ 3.6
   --  "align_widget": a Gtk.Widget.Gtk_Widget

   function Get_Direction
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Enums.Gtk_Arrow_Type;
   --  Returns the direction the menu will be pointing at when popped up.
   --  Since: gtk+ 3.6

   procedure Set_Direction
      (Self      : not null access Gtk_Menu_Button_Record;
       Direction : Gtk.Enums.Gtk_Arrow_Type);
   --  Sets the direction in which the menu will be popped up, as well as
   --  changing the arrow's direction. The child will not be changed to an
   --  arrow if it was customized.
   --  If the menu when popped out would have collided with screen edges, we
   --  will do our best to keep it inside the screen and fully visible.
   --  If you pass Gtk.Enums.Arrow_None for a Direction, the menu will behave
   --  as if you passed Gtk.Enums.Arrow_Down (although you won't see any
   --  arrows).
   --  Since: gtk+ 3.6
   --  "direction": a Gtk.Enums.Gtk_Arrow_Type

   function Get_Menu_Model
      (Self : not null access Gtk_Menu_Button_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Returns the Glib.Menu_Model.Gmenu_Model used to generate the menu.
   --  Since: gtk+ 3.6

   procedure Set_Menu_Model
      (Self       : not null access Gtk_Menu_Button_Record;
       Menu_Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets the Glib.Menu_Model.Gmenu_Model from which the
   --  Gtk.Menu_Button.Gtk_Menu_Button:popup property will be filled in, or
   --  null to disable the button.
   --  The Gtk.Menu.Gtk_Menu will be created with Gtk.Menu.Gtk_New_From_Model,
   --  so actions will be connected as documented there.
   --  If Gtk.Menu_Button.Gtk_Menu_Button:popup is already set, then its
   --  content will be lost and replaced by our newly created
   --  Gtk.Menu.Gtk_Menu.
   --  Since: gtk+ 3.6
   --  "menu_model": a Glib.Menu_Model.Gmenu_Model

   function Get_Popup
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Menu.Gtk_Menu;
   --  Returns the Gtk.Menu.Gtk_Menu that pops out of the button.
   --  Since: gtk+ 3.6

   procedure Set_Popup
      (Self  : not null access Gtk_Menu_Button_Record;
       Popup : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the Gtk.Menu.Gtk_Menu that will be popped up when the button is
   --  clicked, or null to disable the button. If
   --  Gtk.Menu_Button.Gtk_Menu_Button:menu-model is set, it will be set to
   --  null.
   --  Since: gtk+ 3.6
   --  "popup": a Gtk.Menu.Gtk_Menu

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
       Action_Name : UTF8_String);

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

   function Get_Related_Action
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Action.Gtk_Action;

   procedure Set_Related_Action
      (Self   : not null access Gtk_Menu_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Menu_Button_Record) return Boolean;

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Menu_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Menu_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Align_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Container.Gtk_Container
   --  The Gtk.Widget.Gtk_Widget to use to align the popup menu with.

   Direction_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type;
   --  The Gtk.Enums.Gtk_Arrow_Type representing the direction in which the
   --  menu will be popped out.

   Menu_Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  The Glib.Menu_Model.Gmenu_Model from which the menu to pop up will be
   --  created. See Gtk.Menu_Button.Set_Menu_Model for the interaction with the
   --  Gtk.Menu_Button.Gtk_Menu_Button:popup property.

   Popup_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Menu.Gtk_Menu
   --  The Gtk.Menu.Gtk_Menu that will be popped up when the button is
   --  clicked.

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
   Popup_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("popup");
   Menu_Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("menu-model");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type :=
     Gtk.Enums.Build ("direction");
   Align_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("align-widget");
end Gtk.Menu_Button;
