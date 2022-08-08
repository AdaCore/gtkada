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
--  Gtk_Tool_Items are widgets that can appear on a toolbar. To create a
--  toolbar item that contain something else than a button, use
--  Gtk.Tool_Item.Gtk_New. Use Gtk.Container.Add to add a child widget to the
--  tool item.
--
--  For toolbar items that contain buttons, see the
--  Gtk.Tool_Button.Gtk_Tool_Button,
--  Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button and
--  Gtk.Radio_Tool_Button.Gtk_Radio_Tool_Button classes.
--
--  See the Gtk.Toolbar.Gtk_Toolbar class for a description of the toolbar
--  widget, and Gtk.Tool_Shell.Gtk_Tool_Shell for a description of the tool
--  shell interface.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Size_Group;  use Gtk.Size_Group;
with Pango.Layout;    use Pango.Layout;

package Gtk.Tool_Item is

   type Gtk_Tool_Item_Record is new Gtk_Bin_Record with null record;
   type Gtk_Tool_Item is access all Gtk_Tool_Item_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Tool_Item : out Gtk_Tool_Item);
   procedure Initialize
      (Tool_Item : not null access Gtk_Tool_Item_Record'Class);
   --  Creates a new Gtk.Tool_Item.Gtk_Tool_Item
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tool_Item_New return Gtk_Tool_Item;
   --  Creates a new Gtk.Tool_Item.Gtk_Tool_Item
   --  Since: gtk+ 2.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tool_item_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Ellipsize_Mode
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   --  Returns the ellipsize mode used for Tool_Item. Custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item should call this function to find out how
   --  text should be ellipsized.
   --  Since: gtk+ 2.20

   function Get_Expand
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Boolean;
   --  Returns whether Tool_Item is allocated extra space. See
   --  Gtk.Tool_Item.Set_Expand.
   --  Since: gtk+ 2.4

   procedure Set_Expand
      (Tool_Item : not null access Gtk_Tool_Item_Record;
       Expand    : Boolean);
   --  Sets whether Tool_Item is allocated extra space when there is more room
   --  on the toolbar then needed for the items. The effect is that the item
   --  gets bigger when the toolbar gets bigger and smaller when the toolbar
   --  gets smaller.
   --  Since: gtk+ 2.4
   --  "expand": Whether Tool_Item is allocated extra space

   function Get_Homogeneous
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Boolean;
   --  Returns whether Tool_Item is the same size as other homogeneous items.
   --  See Gtk.Tool_Item.Set_Homogeneous.
   --  Since: gtk+ 2.4

   procedure Set_Homogeneous
      (Tool_Item   : not null access Gtk_Tool_Item_Record;
       Homogeneous : Boolean);
   --  Sets whether Tool_Item is to be allocated the same size as other
   --  homogeneous items. The effect is that all homogeneous items will have
   --  the same width as the widest of the items.
   --  Since: gtk+ 2.4
   --  "homogeneous": whether Tool_Item is the same size as other homogeneous
   --  items

   function Get_Icon_Size
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Enums.Gtk_Icon_Size;
   --  Returns the icon size used for Tool_Item. Custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item should call this function to find out what
   --  size icons they should use.
   --  Since: gtk+ 2.4

   function Get_Is_Important
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Boolean;
   --  Returns whether Tool_Item is considered important. See
   --  Gtk.Tool_Item.Set_Is_Important
   --  Since: gtk+ 2.4

   procedure Set_Is_Important
      (Tool_Item    : not null access Gtk_Tool_Item_Record;
       Is_Important : Boolean);
   --  Sets whether Tool_Item should be considered important. The
   --  Gtk.Tool_Button.Gtk_Tool_Button class uses this property to determine
   --  whether to show or hide its label when the toolbar style is
   --  Gtk.Enums.Toolbar_Both_Horiz. The result is that only tool buttons with
   --  the "is_important" property set have labels, an effect known as
   --  "priority text"
   --  Since: gtk+ 2.4
   --  "is_important": whether the tool item should be considered important

   function Get_Orientation
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Enums.Gtk_Orientation;
   --  Returns the orientation used for Tool_Item. Custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item should call this function to find out what
   --  size icons they should use.
   --  Since: gtk+ 2.4

   function Get_Proxy_Menu_Item
      (Tool_Item    : not null access Gtk_Tool_Item_Record;
       Menu_Item_Id : UTF8_String) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  If Menu_Item_Id matches the string passed to
   --  Gtk.Tool_Item.Set_Proxy_Menu_Item return the corresponding
   --  Gtk.Menu_Item.Gtk_Menu_Item.
   --  Custom subclasses of Gtk.Tool_Item.Gtk_Tool_Item should use this
   --  function to update their menu item when the Gtk.Tool_Item.Gtk_Tool_Item
   --  changes. That the Menu_Item_Ids must match ensures that a
   --  Gtk.Tool_Item.Gtk_Tool_Item will not inadvertently change a menu item
   --  that they did not create.
   --  Since: gtk+ 2.4
   --  "menu_item_id": a string used to identify the menu item

   procedure Set_Proxy_Menu_Item
      (Tool_Item    : not null access Gtk_Tool_Item_Record;
       Menu_Item_Id : UTF8_String;
       Menu_Item    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   --  Sets the Gtk.Menu_Item.Gtk_Menu_Item used in the toolbar overflow menu.
   --  The Menu_Item_Id is used to identify the caller of this function and
   --  should also be used with Gtk.Tool_Item.Get_Proxy_Menu_Item.
   --  See also Gtk.Tool_Item.Gtk_Tool_Item::create-menu-proxy.
   --  Since: gtk+ 2.4
   --  "menu_item_id": a string used to identify Menu_Item
   --  "menu_item": a Gtk.Menu_Item.Gtk_Menu_Item to use in the overflow menu,
   --  or null

   function Get_Relief_Style
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Enums.Gtk_Relief_Style;
   --  Returns the relief style of Tool_Item. See Gtk.Button.Set_Relief.
   --  Custom subclasses of Gtk.Tool_Item.Gtk_Tool_Item should call this
   --  function in the handler of the
   --  Gtk.Tool_Item.Gtk_Tool_Item::toolbar_reconfigured signal to find out the
   --  relief style of buttons.
   --  Since: gtk+ 2.4

   function Get_Text_Alignment
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Gfloat;
   --  Returns the text alignment used for Tool_Item. Custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item should call this function to find out how
   --  text should be aligned.
   --  Since: gtk+ 2.20

   function Get_Text_Orientation
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Enums.Gtk_Orientation;
   --  Returns the text orientation used for Tool_Item. Custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item should call this function to find out how
   --  text should be orientated.
   --  Since: gtk+ 2.20

   function Get_Text_Size_Group
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Size_Group.Gtk_Size_Group;
   --  Returns the size group used for labels in Tool_Item. Custom subclasses
   --  of Gtk.Tool_Item.Gtk_Tool_Item should call this function and use the
   --  size group for labels.
   --  Since: gtk+ 2.20

   function Get_Toolbar_Style
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Enums.Gtk_Toolbar_Style;
   --  Returns the toolbar style used for Tool_Item. Custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item should call this function in the handler of
   --  the GtkToolItem::toolbar_reconfigured signal to find out in what style
   --  the toolbar is displayed and change themselves accordingly
   --  Possibilities are: - Gtk.Enums.Toolbar_Both, meaning the tool item
   --  should show both an icon and a label, stacked vertically -
   --  Gtk.Enums.Toolbar_Icons, meaning the toolbar shows only icons -
   --  Gtk.Enums.Toolbar_Text, meaning the tool item should only show text -
   --  Gtk.Enums.Toolbar_Both_Horiz, meaning the tool item should show both an
   --  icon and a label, arranged horizontally
   --  Since: gtk+ 2.4

   function Get_Use_Drag_Window
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Boolean;
   --  Returns whether Tool_Item has a drag window. See
   --  Gtk.Tool_Item.Set_Use_Drag_Window.
   --  Since: gtk+ 2.4

   procedure Set_Use_Drag_Window
      (Tool_Item       : not null access Gtk_Tool_Item_Record;
       Use_Drag_Window : Boolean);
   --  Sets whether Tool_Item has a drag window. When True the toolitem can be
   --  used as a drag source through gtk_drag_source_set. When Tool_Item has a
   --  drag window it will intercept all events, even those that would
   --  otherwise be sent to a child of Tool_Item.
   --  Since: gtk+ 2.4
   --  "use_drag_window": Whether Tool_Item has a drag window.

   function Get_Visible_Horizontal
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Boolean;
   --  Returns whether the Tool_Item is visible on toolbars that are docked
   --  horizontally.
   --  Since: gtk+ 2.4

   procedure Set_Visible_Horizontal
      (Tool_Item          : not null access Gtk_Tool_Item_Record;
       Visible_Horizontal : Boolean);
   --  Sets whether Tool_Item is visible when the toolbar is docked
   --  horizontally.
   --  Since: gtk+ 2.4
   --  "visible_horizontal": Whether Tool_Item is visible when in horizontal
   --  mode

   function Get_Visible_Vertical
      (Tool_Item : not null access Gtk_Tool_Item_Record) return Boolean;
   --  Returns whether Tool_Item is visible when the toolbar is docked
   --  vertically. See Gtk.Tool_Item.Set_Visible_Vertical.
   --  Since: gtk+ 2.4

   procedure Set_Visible_Vertical
      (Tool_Item        : not null access Gtk_Tool_Item_Record;
       Visible_Vertical : Boolean);
   --  Sets whether Tool_Item is visible when the toolbar is docked
   --  vertically. Some tool items, such as text entries, are too wide to be
   --  useful on a vertically docked toolbar. If Visible_Vertical is False
   --  Tool_Item will not appear on toolbars that are docked vertically.
   --  Since: gtk+ 2.4
   --  "visible_vertical": whether Tool_Item is visible when the toolbar is in
   --  vertical mode

   procedure Rebuild_Menu (Tool_Item : not null access Gtk_Tool_Item_Record);
   --  Calling this function signals to the toolbar that the overflow menu
   --  item for Tool_Item has changed. If the overflow menu is visible when
   --  this function it called, the menu will be rebuilt.
   --  The function must be called when the tool item changes what it will do
   --  in response to the Gtk.Tool_Item.Gtk_Tool_Item::create-menu-proxy
   --  signal.
   --  Since: gtk+ 2.6

   function Retrieve_Proxy_Menu_Item
      (Tool_Item : not null access Gtk_Tool_Item_Record)
       return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Returns the Gtk.Menu_Item.Gtk_Menu_Item that was last set by
   --  Gtk.Tool_Item.Set_Proxy_Menu_Item, ie. the Gtk.Menu_Item.Gtk_Menu_Item
   --  that is going to appear in the overflow menu.
   --  Since: gtk+ 2.4

   procedure Set_Tooltip_Markup
      (Tool_Item : not null access Gtk_Tool_Item_Record;
       Markup    : UTF8_String);
   --  Sets the markup text to be displayed as tooltip on the item. See
   --  Gtk.Widget.Set_Tooltip_Markup.
   --  Since: gtk+ 2.12
   --  "markup": markup text to be used as tooltip for Tool_Item

   procedure Set_Tooltip_Text
      (Tool_Item : not null access Gtk_Tool_Item_Record;
       Text      : UTF8_String);
   --  Sets the text to be displayed as tooltip on the item. See
   --  Gtk.Widget.Set_Tooltip_Text.
   --  Since: gtk+ 2.12
   --  "text": text to be used as tooltip for Tool_Item

   procedure Toolbar_Reconfigured
      (Tool_Item : not null access Gtk_Tool_Item_Record);
   --  Emits the signal Gtk.Tool_Item.Gtk_Tool_Item::toolbar_reconfigured on
   --  Tool_Item. Gtk.Toolbar.Gtk_Toolbar and other
   --  Gtk.Tool_Shell.Gtk_Tool_Shell implementations use this function to
   --  notify children, when some aspect of their configuration changes.
   --  Since: gtk+ 2.14

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Tool_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Tool_Item_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Tool_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Tool_Item_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Tool_Item_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Tool_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Is_Important_Property : constant Glib.Properties.Property_Boolean;

   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean;

   Visible_Vertical_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tool_Item_Boolean is not null access function
     (Self : access Gtk_Tool_Item_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Create_Menu_Proxy : constant Glib.Signal_Name := "create-menu-proxy";
   procedure On_Create_Menu_Proxy
      (Self  : not null access Gtk_Tool_Item_Record;
       Call  : Cb_Gtk_Tool_Item_Boolean;
       After : Boolean := False);
   procedure On_Create_Menu_Proxy
      (Self  : not null access Gtk_Tool_Item_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the toolbar needs information from
   --  Tool_Item about whether the item should appear in the toolbar overflow
   --  menu. In response the tool item should either
   --
   --  - call Gtk.Tool_Item.Set_Proxy_Menu_Item with a null pointer and return
   --  True to indicate that the item should not appear in the overflow menu
   --
   --  - call Gtk.Tool_Item.Set_Proxy_Menu_Item with a new menu item and
   --  return True, or
   --
   --  - return False to indicate that the signal was not handled by the item.
   --  This means that the item will not appear in the overflow menu unless a
   --  later handler installs a menu item.
   --
   --  The toolbar may cache the result of this signal. When the tool item
   --  changes how it will respond to this signal it must call
   --  Gtk.Tool_Item.Rebuild_Menu to invalidate the cache and ensure that the
   --  toolbar rebuilds its overflow menu.
   -- 
   --  Callback parameters:
   --    --  Returns True if the signal was handled, False if not

   type Cb_Gtk_Tool_Item_Void is not null access procedure (Self : access Gtk_Tool_Item_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Toolbar_Reconfigured : constant Glib.Signal_Name := "toolbar-reconfigured";
   procedure On_Toolbar_Reconfigured
      (Self  : not null access Gtk_Tool_Item_Record;
       Call  : Cb_Gtk_Tool_Item_Void;
       After : Boolean := False);
   procedure On_Toolbar_Reconfigured
      (Self  : not null access Gtk_Tool_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when some property of the toolbar that the item
   --  is a child of changes. For custom subclasses of
   --  Gtk.Tool_Item.Gtk_Tool_Item, the default handler of this signal use the
   --  functions - Gtk.Tool_Shell.Get_Orientation - Gtk.Tool_Shell.Get_Style -
   --  Gtk.Tool_Shell.Get_Icon_Size - Gtk.Tool_Shell.Get_Relief_Style to find
   --  out what the toolbar should look like and change themselves accordingly.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Tool_Item_Record, Gtk_Tool_Item);
   function "+"
     (Widget : access Gtk_Tool_Item_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Tool_Item
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tool_Item_Record, Gtk_Tool_Item);
   function "+"
     (Widget : access Gtk_Tool_Item_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tool_Item
   renames Implements_Gtk_Buildable.To_Object;

private
   Visible_Vertical_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-vertical");
   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-horizontal");
   Is_Important_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-important");
end Gtk.Tool_Item;
