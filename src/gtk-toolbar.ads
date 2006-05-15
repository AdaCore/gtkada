-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <documentation>
--  A toolbar groups a number of items (buttons, combo boxes,...), generally
--  at the top of the application window, just below the menu bar. It provides
--  quick access to the most commonly used features of your application.
--  It is common for an application to have multiple toolbars.
--  </documentation>
--  <c_version>2.8.17</c_version>

with Glib;
with Glib.Properties;
with Gtk.Button;
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tool_Item;
with Gtk.Widget;

package Gtk.Toolbar is

   type Gtk_Toolbar_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Toolbar is access all Gtk_Toolbar_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Toolbar);
   procedure Initialize (Widget : access Gtk_Toolbar_Record'Class);
   --  Create or initialize a new toolbar

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Toolbar.

   -----------
   -- Items --
   -----------

   procedure Insert
     (Toolbar : access Gtk_Toolbar_Record;
      Item    : access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
      Pos     : Gint);
   --  Insert a new item anywhere in the toolbar

   function Get_Item_Index
     (Toolbar : access Gtk_Toolbar_Record;
      Item    : access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class)
      return Gint;
   --  Get the position of Item within the toolbar

   function Get_N_Items
     (Toolbar : access Gtk_Toolbar_Record) return Gint;
   --  Return the number of items in the toolbar

   function Get_Nth_Item
     (Toolbar : access Gtk_Toolbar_Record;
      N       : Gint)
      return Gtk.Tool_Item.Gtk_Tool_Item;
   --  Return the n-th item in the toolbar

   procedure Set_Drop_Highlight_Item
     (Toolbar   : access Gtk_Toolbar_Record;
      Tool_Item : access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
      Index     : Gint);
   --  Highlights Toolbar to give an idea of what it would look like
   --  if Item was added at the position indicated by Index.
   --  If Item is %NULL, highlighting is turned off. In that case Index is
   --  ignored.
   --
   --  The item passed to this function must not be part of any widget
   --  hierarchy. When an item is set as drop highlight item it can not
   --  be added to any widget hierarchy or used as highlight item for another
   --  toolbar.

   ---------------------
   -- Style functions --
   ---------------------

   procedure Set_Orientation
     (Toolbar     : access Gtk_Toolbar_Record;
      Orientation : Gtk_Orientation);
   function Get_Orientation
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Orientation;
   --  Set or get the orientation (horizontal, vertical) for the toolbar

   procedure Set_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : Gtk_Toolbar_Style);
   function Get_Style
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Toolbar_Style;
   --  Set the style of the toolbar: text only, images only, or both

   procedure Unset_Style (Toolbar : access Gtk_Toolbar_Record);
   --  Unsets a toolbar style set with Set_Style, so that user preferences
   --  will be used to determine the toolbar style. These user preferences are
   --  defined through the current gtk+ theme

   procedure Set_Tooltips
     (Toolbar : access Gtk_Toolbar_Record;
      Enable  : Boolean);
   function Get_Tooltips
     (Toolbar : access Gtk_Toolbar_Record) return Boolean;
   --  Sets whether tooltips should be enabled for items in the toolbar

   function Get_Relief_Style
     (Toolbar : access Gtk_Toolbar_Record)
      return Gtk_Relief_Style;
   --  Returns the relief style of buttons on Toolbar. See
   --  Gtk.Button.Set_Relief for more information on reliefs.

   procedure Set_Show_Arrow
     (Toolbar    : access Gtk_Toolbar_Record;
      Show_Arrow : Boolean := True);
   function Get_Show_Arrow
     (Toolbar : access Gtk_Toolbar_Record)
      return Boolean;
   --  Sets or Gets whether to show an overflow arrow when the toolbar doesn't
   --  have room for all items on it. If True, the items that have no room are
   --  still available to the user.

   ----------
   -- Misc --
   ----------

   function Get_Drop_Index
     (Toolbar : access Gtk_Toolbar_Record;
      X       : Gint;
      Y       : Gint)
      return Gint;
   --  Returns the position corresponding to the indicated point on
   --  Toolbar. This is useful when dragging items to the toolbar:
   --  this function returns the position a new item should be
   --  inserted.
   --  (X, Y) are the coordinates, in pixels, within the toolbar

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch

   procedure Gtk_New
     (Widget      : out Gtk_Toolbar;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style);
   --  pragma Obsolescent;

   procedure Initialize
     (Widget      : access Gtk_Toolbar_Record'Class;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style);
   --  pragma Obsolescent;

   function Append_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : Gtk.Widget.Gtk_Widget := null;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead"); --  Append_Element

   function Prepend_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Prepend_Element

   function Insert_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null;
      Position             : Gint)
      return Gtk.Widget.Gtk_Widget;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Insert_Element

   function Append_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Append_Item

   function Prepend_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Prepend_Item

   function Insert_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null;
      Position             : Gint)
      return Gtk.Button.Gtk_Button;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Insert_Item

   function Insert_Stock
     (Toolbar              : access Gtk_Toolbar_Record;
      Stock_Id             : UTF8_String;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Position             : Gint := -1) return Gtk.Button.Gtk_Button;
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Insert_Stock

   procedure Append_Space (Toolbar : access Gtk_Toolbar_Record);
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Append_Space

   procedure Prepend_Space (Toolbar : access Gtk_Toolbar_Record);
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Prepend_Space

   procedure Insert_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint);
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Insert_Space

   procedure Remove_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint);
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Remove_Space

   procedure Append_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "");
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Append_Widget

   procedure Prepend_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "");
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Prepend_Widget

   procedure Insert_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Position             : Gint);
   --  pragma Obsolescent ("Use Gtk.Toolbar.Insert instead");  --  Insert_Widget

   procedure Set_Icon_Size
     (Toolbar   : access Gtk_Toolbar_Record;
      Icon_Size : Gtk_Icon_Size);
   --  pragma Obsolescent  --  Set_Icon_Size
   --  ("Applications should respect user preferences (gtk+ themes)");

   function Get_Icon_Size
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Icon_Size;
   --  pragma Obsolescent  --  Get_Icon_Size
   --  ("Applications should respect user preferences (gtk+ themes)");

   procedure Unset_Icon_Size (Toolbar : access Gtk_Toolbar_Record);
   --  pragma Obsolescent;  --  Unset_Icon_Size
   --  Unsets icon sizes set through Set_Icon_Size, so that user preferences
   --  set through the gtk+ theme are used

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  "orientation-changed"
   --  procedure Handler (Toolbar     : access Gtk_Toolbar_Record'Class;
   --                     Orientation : Gtk_Orientation);
   --  Emitted when the orientation of the toolbar changes
   --
   --  "style-changed"
   --  procedure Handler (Toolbar     : access Gtk_Toolbar_Record'Class;
   --                     Style       : Gtk_Toolbar_Style);
   --  Emitted when the style of the toolbar changes
   --
   --  "popup_context_menu"
   --  function Handler (Toolbar      : access Gtk_Toolbar_Record'Class;
   --                    X, Y, Button : Gint) return Boolean;
   --  Emitted when the user right-clicks the toolbar or uses the keybindg to
   --  display a popup menu.
   --  Application developers should handle this signal if they want to display
   --  a context menu on the toolbar. The context-menu should appear at the
   --  coordinates given by (x, y). The mouse button number is given by the
   --  Button parameter (set to -1 if popped up with the keyboard).
   --  Return value is True if the signal was handled.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Orientation_Property
   --  Type: Gtk_Orientation
   --  See:  Set_Orientation
   --
   --  Name: Toolbar_Style_Property
   --  Type: Gtk_Toolbar_Style
   --  See:  Set_Style
   --
   --  Name: Show_Arrow_Property
   --  Type: Boolean
   --  See:  Set_Show_Arrow
   --
   --  </properties>

   Orientation_Property   : constant Gtk.Enums.Property_Gtk_Orientation;
   Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style;
   Show_Arrow_Property    : constant Glib.Properties.Property_Boolean;

private
   type Gtk_Toolbar_Record is
     new Gtk.Container.Gtk_Container_Record with null record;

   Orientation_Property   : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Build ("orientation");
   Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style :=
     Gtk.Enums.Build ("toolbar-style");
   Show_Arrow_Property    : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-arrow");

   pragma Import (C, Get_Type, "gtk_toolbar_get_type");
end Gtk.Toolbar;
