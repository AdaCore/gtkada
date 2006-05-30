-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
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

--  <description>
--  This package contains various subprograms to easily share settings between
--  applications, or even between various parts of your application.
--  </description>
--  <c_version>2.8.17</c_version>

with Gdk;
with Glib.Object;
with Glib.Values;
with Gtk.Style;
with Interfaces.C.Strings;

package Gtk.Settings is

   type Gtk_Settings_Record is new Glib.Object.GObject_Record with null record;
   type Gtk_Settings is access all Gtk_Settings_Record'Class;

   function Get_Default return Gtk_Settings;
   --  Gets the settings object for the default GDK screen, creating
   --  it if necessary.

   function Get_For_Screen (Screen : Gdk.Gdk_Screen) return Gtk_Settings;
   --  Gets the settings object for Screen, creating it if necessary.

   function Get_Type return Glib.GType;
   --  Return the internal type used to identify a Gtk_Settings

   procedure Install_Property (Pspec : Glib.Param_Spec);
   --  Declares a property that can be shared among various parts of the
   --  application

   procedure Install_Property_Parser
     (Pspec  : Glib.Param_Spec;
      Parser : Gtk.Style.Gtk_Rc_Property_Parser);
   --  Install a new parser for the given property. This parser is responsible
   --  for reading the property's value in a gtk configuration file, and
   --  convert it to a suitable value.

   --------------------------------
   -- Precoded parsing functions --
   --------------------------------

   function Parse_Color
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Enum
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Flags
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Requisition
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Border
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   --  These functions parse some of the predefined property types

   -----------------------------------
   -- Setting predefined properties --
   -----------------------------------

   procedure Set_Property_Value
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Glib.Values.GValue;
      Origin   : String);
   procedure Set_String_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : String;
      Origin   : String);
   procedure Set_Long_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Glong;
      Origin   : String);
   procedure Set_Double_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Gdouble;
      Origin   : String);
   --  Set the value of a property. This automatically propagates the new
   --  value to all listeners, so that they can refresh themselves.
   --  Origin should be something like "filename:line" for rc files, or the
   --  name of the function that sets it otherwise

   ----------------
   -- Properties --
   ----------------
   --  The following settings are predefined
   --  - "gtk-alternative-button-order" : Boolean
   --    Whether buttons in dialogs should use the alternative button order.
   --    This is used on systems like windows where OK and Cancel buttons are
   --    generally in an order different from that of GNOME and GTK
   --
   --  - "gtk-button-images": Boolean
   --    Whether stock icons should be shown in buttons.
   --
   --  - "gtk-can-change-accels": Boolean
   --    Whether menu accelerators can be changed by pressing a key over the
   --    menu item.
   --
   --  - "gtk-color-palette": String
   --    Palette to use in the color selector. See Gtk.Color_Selection.
   --    Default is "black:white:gray50:red:purple"
   --
   --  - "gtk-cursor-blink": Boolean
   --    Whether the cursor should blink.
   --
   --  - "gtk-cursor-blink-time": Gint
   --    Length of the cursor blink cycle, in milliseconds. (>= 100)
   --
   --  - "gtk-cursor-theme-name": String
   --    Name of the cursor theme to use.
   --
   --  - "gtk-cursor-theme-size": Gint
   --    Size to use for cursors (0 to 128)
   --
   --  - "gtk-dnd-drag-threshold": Gint
   --    Number of pixels the cursor can move before dragging (>= 1)
   --
   --  - "gtk-double-click-distance": Gint
   --    Maximum distance allowed between two clicks for them to be considered
   --    a double click (in pixels).
   --
   --  - "gtk-double-click-time": Gint
   --    Maximum time allowed between two clicks for them to be considered a
   --    double click (in milliseconds).
   --
   --  - "gtk-entry-select-on-focus": Boolean
   --    Whether to select the contents of an entry when it is focused.
   --
   --  - "gtk-fallback-icon-theme": String
   --    Name of a icon theme to fall back to.
   --
   --  - "gtk-font-name": String
   --    Name of default font to use.
   --
   --  - "gtk-icon-sizes": String
   --    List of icon sizes (gtk-menu=16,16:gtk-button=20,20...)
   --
   --  - "gtk-icon-theme-name": String
   --    Name of icon theme to use (default="hicolor")
   --
   --  - "gtk-key-theme-name": String
   --    Name of key theme RC file to load (for instance "Emacs")
   --
   --  - "gtk-menu-bar-accel": String
   --    Keybinding to activate the menu bar (default="F10")
   --
   --  - "gtk-menu-bar-popup-delay": Gint
   --    Delay before the submenus of a menu bar appear (Default=0)
   --
   --  - "gtk-menu-images": Boolean
   --    Whether images should be shown in menus (Default=True)
   --
   --  - "gtk-menu-popdown-delay": Gint
   --    The time before hiding a submenu when the pointer is moving towards
   --    the submenu (Default=1000)
   --
   --  - "gtk-menu-popup-delay": Gint
   --    Minimum time the pointer must stay over a menu item before the submenu
   --    appear (Default=225)
   --
   --  - "gtk-modules": String
   --    List of currently active GTK modules.
   --
   --  - "gtk-split-cursor": Boolean
   --    Whether two cursors should be displayed for mixed left-to-right and
   --    right-to-left text (Default True)
   --
   --  - "gtk-theme-name": String
   --    Name of theme RC file to load (Default: "Raleigh")
   --
   --  - "gtk-toolbar-icon-size": Gtk_Icon_Size
   --    Size of icons in default toolbars (default=Large)
   --
   --  - "gtk-toolbar-style": Gtk_Toolbar_Style
   --    Whether default toolbars have text only, text and icons, icons only,
   --    etc (default: both)
   --
   --  - "gtk-xft-antialias": Gint
   --    Whether to antialias Xft fonts; 0=no, 1=yes, -1=default
   --
   --  - "gtk-xft-dpi": Gint
   --    Resolution for Xft, in 1024 * dots/inch. -1 to use default value.
   --
   --  - "gtk-xft-hinting": Gint
   --    Whether to hint Xft fonts; 0=no, 1=yes, -1=default.
   --
   --  - "gtk-xft-hintstyle": String
   --    What degree of hinting to use; hintnone, hintslight, hintmedium, or
   --    hintfull.
   --
   --  - "gtk-xft-rgba": String
   --    Type of subpixel antialiasing; none, rgb, bgr, vrgb, vbgr.

   Gtk_Alternative_Button_Order : constant String :=
     "gtk-alternative-button-order";
   Gtk_Button_Images         : constant String := "gtk-button-images";
   Gtk_Can_Change_Accels     : constant String := "gtk-can-change-accels";
   Gtk_Color_Palette         : constant String := "gtk-color-palette";
   Gtk_Cursor_Blink          : constant String := "gtk-cursor-blink";
   Gtk_Cursor_Blink_Time     : constant String := "gtk-cursor-blink-time";
   Gtk_Cursor_Theme_Name     : constant String := "gtk-cursor-theme-name";
   Gtk_Cursor_Theme_Size     : constant String := "gtk-cursor-theme-size";
   Gtk_Dnd_Drag_Threshold    : constant String := "gtk-dnd-drag-threshold";
   Gtk_Double_Click_Distance : constant String := "gtk-double-click-distance";
   Gtk_Double_Click_Time     : constant String := "gtk-double-click-time";
   Gtk_Entry_Select_On_Focus : constant String := "gtk-entry-select-on-focus";
   Gtk_Fallback_Icon_Theme   : constant String := "gtk-fallback-icon-theme";
   Gtk_Font_Name             : constant String := "gtk-font-name";
   Gtk_Icon_Sizes            : constant String := "gtk-icon-sizes";
   Gtk_Icon_Theme_Name       : constant String := "gtk-icon-theme-name";
   Gtk_Key_Theme_Name        : constant String := "gtk-key-theme-name";
   Gtk_Menu_Bar_Accel        : constant String := "gtk-menu-bar-accel";
   Gtk_Menu_Bar_Popup_Delay  : constant String := "gtk-menu-bar-popup-delay";
   Gtk_Menu_Images           : constant String := "gtk-menu-images";
   Gtk_Menu_Popdown_Delay    : constant String := "gtk-menu-popdown-delay";
   Gtk_Menu_Popup_Delay      : constant String := "gtk-menu-popup-delay";
   Gtk_Modules               : constant String := "gtk-modules";
   Gtk_Split_Cursor          : constant String := "gtk-split-cursor";
   Gtk_Theme_Name            : constant String := "gtk-theme-name";
   Gtk_Toolbar_Icon_Size     : constant String := "gtk-toolbar-icon-size";
   Gtk_Toolbar_Style         : constant String := "gtk-toolbar-style";
   Gtk_Xft_Antialias         : constant String := "gtk-xft-antialias";
   Gtk_Xft_Dpi               : constant String := "gtk-xft-dpi";
   Gtk_Xft_Hinting           : constant String := "gtk-xft-hinting";
   Gtk_Xft_Hintstyle         : constant String := "gtk-xft-hintstyle";
   Gtk_Xft_Rgba              : constant String := "gtk-xft-rgba";

private
   pragma Import (C, Get_Type,          "gtk_settings_get_type");
   pragma Import (C, Install_Property_Parser,
                  "gtk_settings_install_property_parser");
   pragma Import (C, Install_Property,  "gtk_settings_install_property");
   pragma Import (C, Parse_Color,       "gtk_rc_property_parse_color");
   pragma Import (C, Parse_Enum,        "gtk_rc_property_parse_enum");
   pragma Import (C, Parse_Flags,       "gtk_rc_property_parse_flags");
   pragma Import (C, Parse_Requisition, "gtk_rc_property_parse_requisition");
   pragma Import (C, Parse_Border,      "gtk_rc_property_parse_border");
end Gtk.Settings;
