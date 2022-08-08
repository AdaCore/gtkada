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
--  A toolbar is created with a call to Gtk.Toolbar.Gtk_New.
--
--  A toolbar can contain instances of a subclass of
--  Gtk.Tool_Item.Gtk_Tool_Item. To add a Gtk.Tool_Item.Gtk_Tool_Item to the a
--  toolbar, use Gtk.Toolbar.Insert. To remove an item from the toolbar use
--  Gtk.Container.Remove. To add a button to the toolbar, add an instance of
--  Gtk.Tool_Button.Gtk_Tool_Button.
--
--  Toolbar items can be visually grouped by adding instances of
--  Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item to the toolbar. If the
--  GtkToolbar child property "expand" is TRUE and the property
--  Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item:draw is set to FALSE, the
--  effect is to force all following items to the end of the toolbar.
--
--  By default, a toolbar can be shrunk, upon which it will add an arrow
--  button to show an overflow menu offering access to any
--  Gtk.Tool_Item.Gtk_Tool_Item child that has a proxy menu item. To disable
--  this and request enough size for all children, call
--  Gtk.Toolbar.Set_Show_Arrow to set Gtk.Toolbar.Gtk_Toolbar:show-arrow to
--  False.
--
--  Creating a context menu for the toolbar can be done by connecting to the
--  Gtk.Toolbar.Gtk_Toolbar::popup-context-menu signal.
--
--  # CSS nodes
--
--  GtkToolbar has a single CSS node with name toolbar.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Size_Group;  use Gtk.Size_Group;
with Gtk.Tool_Item;   use Gtk.Tool_Item;
with Gtk.Tool_Shell;  use Gtk.Tool_Shell;
with Pango.Layout;    use Pango.Layout;

package Gtk.Toolbar is

   type Gtk_Toolbar_Record is new Gtk_Container_Record with null record;
   type Gtk_Toolbar is access all Gtk_Toolbar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Toolbar : out Gtk_Toolbar);
   procedure Initialize (Toolbar : not null access Gtk_Toolbar_Record'Class);
   --  Creates a new toolbar.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Toolbar_New return Gtk_Toolbar;
   --  Creates a new toolbar.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_toolbar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Drop_Index
      (Toolbar : not null access Gtk_Toolbar_Record;
       X       : Glib.Gint;
       Y       : Glib.Gint) return Glib.Gint;
   --  Returns the position corresponding to the indicated point on Toolbar.
   --  This is useful when dragging items to the toolbar: this function returns
   --  the position a new item should be inserted.
   --  X and Y are in Toolbar coordinates.
   --  Since: gtk+ 2.4
   --  "x": x coordinate of a point on the toolbar
   --  "y": y coordinate of a point on the toolbar

   function Get_Icon_Size
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Icon_Size;
   --  Retrieves the icon size for the toolbar. See Gtk.Toolbar.Set_Icon_Size.

   procedure Set_Icon_Size
      (Toolbar   : not null access Gtk_Toolbar_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size);
   --  This function sets the size of stock icons in the toolbar. You can call
   --  it both before you add the icons and after they've been added. The size
   --  you set will override user preferences for the default icon size.
   --  This should only be used for special-purpose toolbars, normal
   --  application toolbars should respect the user preferences for the size of
   --  icons.
   --  "icon_size": The Gtk.Enums.Gtk_Icon_Size that stock icons in the
   --  toolbar shall have.

   function Get_Item_Index
      (Toolbar : not null access Gtk_Toolbar_Record;
       Item    : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class)
       return Glib.Gint;
   --  Returns the position of Item on the toolbar, starting from 0. It is an
   --  error if Item is not a child of the toolbar.
   --  Since: gtk+ 2.4
   --  "item": a Gtk.Tool_Item.Gtk_Tool_Item that is a child of Toolbar

   function Get_N_Items
      (Toolbar : not null access Gtk_Toolbar_Record) return Glib.Gint;
   --  Returns the number of items on the toolbar.
   --  Since: gtk+ 2.4

   function Get_Nth_Item
      (Toolbar : not null access Gtk_Toolbar_Record;
       N       : Glib.Gint) return Gtk.Tool_Item.Gtk_Tool_Item;
   --  Returns the N'th item on Toolbar, or null if the toolbar does not
   --  contain an N'th item.
   --  Since: gtk+ 2.4
   --  "n": A position on the toolbar

   function Get_Relief_Style
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Relief_Style;
   --  Returns the relief style of buttons on Toolbar. See
   --  Gtk.Button.Set_Relief.
   --  Since: gtk+ 2.4

   function Get_Show_Arrow
      (Toolbar : not null access Gtk_Toolbar_Record) return Boolean;
   --  Returns whether the toolbar has an overflow menu. See
   --  Gtk.Toolbar.Set_Show_Arrow.
   --  Since: gtk+ 2.4

   procedure Set_Show_Arrow
      (Toolbar    : not null access Gtk_Toolbar_Record;
       Show_Arrow : Boolean := True);
   --  Sets whether to show an overflow menu when Toolbar isn't allocated
   --  enough size to show all of its items. If True, items which can't fit in
   --  Toolbar, and which have a proxy menu item set by
   --  Gtk.Tool_Item.Set_Proxy_Menu_Item or
   --  Gtk.Tool_Item.Gtk_Tool_Item::create-menu-proxy, will be available in an
   --  overflow menu, which can be opened by an added arrow button. If False,
   --  Toolbar will request enough size to fit all of its child items without
   --  any overflow.
   --  Since: gtk+ 2.4
   --  "show_arrow": Whether to show an overflow menu

   function Get_Style
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Toolbar_Style;
   --  Retrieves whether the toolbar has text, icons, or both . See
   --  Gtk.Toolbar.Set_Style.

   procedure Set_Style
      (Toolbar : not null access Gtk_Toolbar_Record;
       Style   : Gtk.Enums.Gtk_Toolbar_Style);
   --  Alters the view of Toolbar to display either icons only, text only, or
   --  both.
   --  "style": the new style for Toolbar.

   procedure Insert
      (Toolbar : not null access Gtk_Toolbar_Record;
       Item    : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Pos     : Glib.Gint := -1);
   --  Insert a Gtk.Tool_Item.Gtk_Tool_Item into the toolbar at position Pos.
   --  If Pos is 0 the item is prepended to the start of the toolbar. If Pos is
   --  negative, the item is appended to the end of the toolbar.
   --  Since: gtk+ 2.4
   --  "item": a Gtk.Tool_Item.Gtk_Tool_Item
   --  "pos": the position of the new item

   procedure Set_Drop_Highlight_Item
      (Toolbar   : not null access Gtk_Toolbar_Record;
       Tool_Item : access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Index     : Glib.Gint);
   --  Highlights Toolbar to give an idea of what it would look like if Item
   --  was added to Toolbar at the position indicated by Index_. If Item is
   --  null, highlighting is turned off. In that case Index_ is ignored.
   --  The Tool_Item passed to this function must not be part of any widget
   --  hierarchy. When an item is set as drop highlight item it can not added
   --  to any widget hierarchy or used as highlight item for another toolbar.
   --  Since: gtk+ 2.4
   --  "tool_item": a Gtk.Tool_Item.Gtk_Tool_Item, or null to turn of
   --  highlighting
   --  "index_": a position on Toolbar

   procedure Unset_Icon_Size (Toolbar : not null access Gtk_Toolbar_Record);
   --  Unsets toolbar icon size set with Gtk.Toolbar.Set_Icon_Size, so that
   --  user preferences will be used to determine the icon size.

   procedure Unset_Style (Toolbar : not null access Gtk_Toolbar_Record);
   --  Unsets a toolbar style set with Gtk.Toolbar.Set_Style, so that user
   --  preferences will be used to determine the toolbar style.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Toolbar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   function Get_Ellipsize_Mode
      (Self : not null access Gtk_Toolbar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;

   function Get_Text_Alignment
      (Self : not null access Gtk_Toolbar_Record) return Gfloat;

   function Get_Text_Orientation
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Orientation;

   function Get_Text_Size_Group
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Size_Group.Gtk_Size_Group;

   procedure Rebuild_Menu (Self : not null access Gtk_Toolbar_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size;
   --  The size of the icons in a toolbar is normally determined by the
   --  toolbar-icon-size setting. When this property is set, it overrides the
   --  setting.
   --
   --  This should only be used for special-purpose toolbars, normal
   --  application toolbars should respect the user preferences for the size of
   --  icons.

   Icon_Size_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Is True if the icon-size property has been set.

   Show_Arrow_Property : constant Glib.Properties.Property_Boolean;

   Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Toolbar_Boolean_Boolean is not null access function
     (Self       : access Gtk_Toolbar_Record'Class;
      Focus_Home : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self       : access Glib.Object.GObject_Record'Class;
      Focus_Home : Boolean) return Boolean;

   Signal_Focus_Home_Or_End : constant Glib.Signal_Name := "focus-home-or-end";
   procedure On_Focus_Home_Or_End
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Focus_Home_Or_End
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A keybinding signal used internally by GTK+. This signal can't be used
   --  in application code
   -- 
   --  Callback parameters:
   --    --  "focus_home": True if the first item should be focused
   --    --  Returns True if the signal was handled, False if not

   type Cb_Gtk_Toolbar_Gtk_Orientation_Void is not null access procedure
     (Self        : access Gtk_Toolbar_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation);

   type Cb_GObject_Gtk_Orientation_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation);

   Signal_Orientation_Changed : constant Glib.Signal_Name := "orientation-changed";
   procedure On_Orientation_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Gtk_Orientation_Void;
       After : Boolean := False);
   procedure On_Orientation_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Gtk_Orientation_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the orientation of the toolbar changes.

   type Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean is not null access function
     (Self   : access Gtk_Toolbar_Record'Class;
      X      : Glib.Gint;
      Y      : Glib.Gint;
      Button : Glib.Gint) return Boolean;

   type Cb_GObject_Gint_Gint_Gint_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      X      : Glib.Gint;
      Y      : Glib.Gint;
      Button : Glib.Gint) return Boolean;

   Signal_Popup_Context_Menu : constant Glib.Signal_Name := "popup-context-menu";
   procedure On_Popup_Context_Menu
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean;
       After : Boolean := False);
   procedure On_Popup_Context_Menu
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Gint_Gint_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user right-clicks the toolbar or uses the keybinding
   --  to display a popup menu.
   --
   --  Application developers should handle this signal if they want to
   --  display a context menu on the toolbar. The context-menu should appear at
   --  the coordinates given by X and Y. The mouse button number is given by
   --  the Button parameter. If the menu was popped up using the keybaord,
   --  Button is -1.
   -- 
   --  Callback parameters:
   --    --  "x": the x coordinate of the point where the menu should appear
   --    --  "y": the y coordinate of the point where the menu should appear
   --    --  "button": the mouse button the user pressed, or -1
   --    --  Returns return True if the signal was handled, False if not

   type Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void is not null access procedure
     (Self  : access Gtk_Toolbar_Record'Class;
      Style : Gtk.Enums.Gtk_Toolbar_Style);

   type Cb_GObject_Gtk_Toolbar_Style_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Style : Gtk.Enums.Gtk_Toolbar_Style);

   Signal_Style_Changed : constant Glib.Signal_Name := "style-changed";
   procedure On_Style_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void;
       After : Boolean := False);
   procedure On_Style_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Gtk_Toolbar_Style_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the style of the toolbar changes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"
   --
   --  - "ToolShell"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Toolbar_Record, Gtk_Toolbar);
   function "+"
     (Widget : access Gtk_Toolbar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Toolbar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Toolbar_Record, Gtk_Toolbar);
   function "+"
     (Widget : access Gtk_Toolbar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Toolbar
   renames Implements_Gtk_Orientable.To_Object;

   package Implements_Gtk_Tool_Shell is new Glib.Types.Implements
     (Gtk.Tool_Shell.Gtk_Tool_Shell, Gtk_Toolbar_Record, Gtk_Toolbar);
   function "+"
     (Widget : access Gtk_Toolbar_Record'Class)
   return Gtk.Tool_Shell.Gtk_Tool_Shell
   renames Implements_Gtk_Tool_Shell.To_Interface;
   function "-"
     (Interf : Gtk.Tool_Shell.Gtk_Tool_Shell)
   return Gtk_Toolbar
   renames Implements_Gtk_Tool_Shell.To_Object;

private
   Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style :=
     Gtk.Enums.Build ("toolbar-style");
   Show_Arrow_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-arrow");
   Icon_Size_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("icon-size-set");
   Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size :=
     Gtk.Enums.Build ("icon-size");
end Gtk.Toolbar;
