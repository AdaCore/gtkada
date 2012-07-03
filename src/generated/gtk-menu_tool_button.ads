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

pragma Ada_05;
--  <description>
--  A Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button is a
--  Gtk.Tool_Item.Gtk_Tool_Item that contains a button and a small additional
--  button with an arrow. When clicked, the arrow button pops up a dropdown
--  menu.
--
--  Use Gtk.Menu_Tool_Button.Gtk_New to create a new
--  Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button. Use
--  Gtk.Menu_Tool_Button.Gtk_New_From_Stock to create a new
--  Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button containing a stock item.
--
--  == GtkMenuToolButton as GtkBuildable ==
--
--  The GtkMenuToolButton implementation of the GtkBuildable interface
--  supports adding a menu by specifying "menu" as the "type" attribute of a
--  <child> element.
--
--  == A UI definition fragment with menus ==
--
--    <object class="GtkMenuToolButton">
--    <child type="menu">
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
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Menu;        use Gtk.Menu;
with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Menu_Tool_Button is

   type Gtk_Menu_Tool_Button_Record is new Gtk_Tool_Button_Record with null record;
   type Gtk_Menu_Tool_Button is access all Gtk_Menu_Tool_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Menu        : out Gtk_Menu_Tool_Button;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "");
   procedure Initialize
      (Menu        : not null access Gtk_Menu_Tool_Button_Record'Class;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "");
   --  Creates a new Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button using
   --  Icon_Widget as icon and Label as label.
   --  Since: gtk+ 2.6
   --  "icon_widget": a widget that will be used as icon widget, or null
   --  "label": a string that will be used as label, or null

   procedure Gtk_New_From_Stock
      (Menu     : out Gtk_Menu_Tool_Button;
       Stock_Id : UTF8_String);
   procedure Initialize_From_Stock
      (Menu     : not null access Gtk_Menu_Tool_Button_Record'Class;
       Stock_Id : UTF8_String);
   --  Creates a new Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button. The new
   --  Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button will contain an icon and label
   --  from the stock item indicated by Stock_Id.
   --  Since: gtk+ 2.6
   --  "stock_id": the name of a stock item

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_menu_tool_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Menu
      (Button : not null access Gtk_Menu_Tool_Button_Record)
       return Gtk.Menu.Gtk_Menu;
   procedure Set_Menu
      (Button : not null access Gtk_Menu_Tool_Button_Record;
       Menu   : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the Gtk.Menu.Gtk_Menu that is popped up when the user clicks on
   --  the arrow. If Menu is NULL, the arrow button becomes insensitive.
   --  Since: gtk+ 2.6
   --  "menu": the Gtk.Menu.Gtk_Menu associated with
   --  Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button

   procedure Set_Arrow_Tooltip_Markup
      (Button : not null access Gtk_Menu_Tool_Button_Record;
       Markup : UTF8_String);
   --  Sets the tooltip markup text to be used as tooltip for the arrow button
   --  which pops up the menu. See Gtk.Tool_Item.Set_Tooltip_Text for setting a
   --  tooltip on the whole Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button.
   --  Since: gtk+ 2.12
   --  "markup": markup text to be used as tooltip text for button's arrow
   --  button

   procedure Set_Arrow_Tooltip_Text
      (Button : not null access Gtk_Menu_Tool_Button_Record;
       Text   : UTF8_String);
   --  Sets the tooltip text to be used as tooltip for the arrow button which
   --  pops up the menu. See Gtk.Tool_Item.Set_Tooltip_Text for setting a
   --  tooltip on the whole Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button.
   --  Since: gtk+ 2.12
   --  "text": text to be used as tooltip text for button's arrow button

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : not null access Gtk_Menu_Tool_Button_Record)
       return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Menu_Tool_Button_Record) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Menu_Tool_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
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
     (Gtk.Activatable.Gtk_Activatable, Gtk_Menu_Tool_Button_Record, Gtk_Menu_Tool_Button);
   function "+"
     (Widget : access Gtk_Menu_Tool_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Menu_Tool_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Menu_Tool_Button_Record, Gtk_Menu_Tool_Button);
   function "+"
     (Widget : access Gtk_Menu_Tool_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Menu_Tool_Button
   renames Implements_Gtk_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Menu_Property
   --  Type: Gtk.Menu.Gtk_Menu
   --  Flags: read-write

   Menu_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "show-menu"
   --     procedure Handler
   --       (Self : access Gtk_Menu_Tool_Button_Record'Class);
   --  The ::show-menu signal is emitted before the menu is shown.
   --
   --  It can be used to populate the menu on demand, using
   --  Gtk.Menu_Tool_Button.Get_Menu. Note that even if you populate the menu
   --  dynamically in this way, you must set an empty menu on the
   --  Gtk.Menu_Tool_Button.Gtk_Menu_Tool_Button beforehand, since the arrow is
   --  made insensitive if the menu is not set.

   Signal_Show_Menu : constant Glib.Signal_Name := "show-menu";

private
   Menu_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("menu");
end Gtk.Menu_Tool_Button;
