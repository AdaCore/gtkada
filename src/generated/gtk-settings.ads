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
--  GtkSettings provide a mechanism to share global settings between
--  applications.
--
--  On the X window system, this sharing is realized by an
--  [XSettings](http://www.freedesktop.org/wiki/Specifications/xsettings-spec)
--  manager that is usually part of the desktop environment, along with
--  utilities that let the user change these settings. In the absence of an
--  Xsettings manager, GTK+ reads default values for settings from
--  `settings.ini` files in `/etc/gtk-3.0`, `$XDG_CONFIG_DIRS/gtk-3.0` and
--  `$XDG_CONFIG_HOME/gtk-3.0`. These files must be valid key files (see
--  Gkey.File.Gkey_File), and have a section called Settings. Themes can also
--  provide default values for settings by installing a `settings.ini` file
--  next to their `gtk.css` file.
--
--  Applications can override system-wide settings by setting the property of
--  the GtkSettings object with g_object_set. This should be restricted to
--  special cases though; GtkSettings are not meant as an application
--  configuration facility. When doing so, you need to be aware that settings
--  that are specific to individual widgets may not be available before the
--  widget type has been realized at least once. The following example
--  demonstrates a way to do this: |[<!-- language="C" --> gtk_init (&argc,
--  &argv);
--
--  // make sure the type is realized g_type_class_unref (g_type_class_ref
--  (GTK_TYPE_IMAGE_MENU_ITEM));
--
--  g_object_set (gtk_settings_get_default (), "gtk-enable-animations", FALSE,
--  NULL); ]|
--
--  There is one GtkSettings instance per screen. It can be obtained with
--  Gtk.Settings.Get_For_Screen, but in many cases, it is more convenient to
--  use gtk_widget_get_settings. Gtk.Settings.Get_Default returns the
--  GtkSettings instance for the default screen.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Screen;         use Gdk.Screen;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Types;         use Glib.Types;
with Glib.Values;        use Glib.Values;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Settings is

   type Gtk_Settings_Record is new GObject_Record with null record;
   type Gtk_Settings is access all Gtk_Settings_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_settings_get_type");

   -------------
   -- Methods --
   -------------

   procedure Reset_Property
      (Self : not null access Gtk_Settings_Record;
       Name : UTF8_String);
   --  Undoes the effect of calling g_object_set to install an
   --  application-specific value for a setting. After this call, the setting
   --  will again follow the session-wide value for this setting.
   --  Since: gtk+ 3.20
   --  "name": the name of the setting to reset

   procedure Set_Double_Property
      (Self     : not null access Gtk_Settings_Record;
       Name     : UTF8_String;
       V_Double : Gdouble;
       Origin   : UTF8_String);
   pragma Obsolescent (Set_Double_Property);
   --  Deprecated since 3.16, 1

   procedure Set_Long_Property
      (Self   : not null access Gtk_Settings_Record;
       Name   : UTF8_String;
       V_Long : Glong;
       Origin : UTF8_String);
   pragma Obsolescent (Set_Long_Property);
   --  Deprecated since 3.16, 1

   procedure Set_String_Property
      (Self     : not null access Gtk_Settings_Record;
       Name     : UTF8_String;
       V_String : UTF8_String;
       Origin   : UTF8_String);
   pragma Obsolescent (Set_String_Property);
   --  Deprecated since 3.16, 1

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set_Property_Value
     (Settings : not null access Gtk_Settings_Record;
      Name     : String;
      Value    : GValue;
      Origin   : String);

   function Get_Settings
     (Widget   : not null access Gtk_Widget_Record'Class)
   return Gtk_Settings;
   --  Get the settings object holding the settings used for this widget.
   --
   --  Note that this function can only be called when the widget is
   --  attached to a toplevel, since the settings object is specific to a
   --  particular Gdk.Screen.Gdk_Screen.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Get_Style_Property
      (Self  : not null access Gtk_Settings_Record;
       Path  : Gtk.Widget.Gtk_Widget_Path;
       State : Gtk.Enums.Gtk_State_Flags;
       Pspec : in out Glib.Param_Spec;
       Value : out Glib.Values.GValue;
       Found : out Boolean);

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gtk_Settings;
   --  Gets the Gtk.Settings.Gtk_Settings object for the default GDK screen,
   --  creating it if necessary. See Gtk.Settings.Get_For_Screen.

   function Get_For_Screen
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
       return Gtk_Settings;
   --  Gets the Gtk.Settings.Gtk_Settings object for Screen, creating it if
   --  necessary.
   --  Since: gtk+ 2.2
   --  "screen": a Gdk.Screen.Gdk_Screen.

   procedure Install_Property (Pspec : in out Glib.Param_Spec);
   pragma Obsolescent (Install_Property);
   --  Deprecated since 3.16, 1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Color_Hash_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Hash_Table
   --  Holds a hash table representation of the
   --  Gtk.Settings.Gtk_Settings:gtk-color-scheme setting, mapping color names
   --  to Gdk_Colors.

   Gtk_Alternative_Button_Order_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Alternative_Sort_Arrows_Property : constant Glib.Properties.Property_Boolean;
   --  Controls the direction of the sort indicators in sorted list and tree
   --  views. By default an arrow pointing down means the column is sorted in
   --  ascending order. When set to True, this order will be inverted.

   Gtk_Application_Prefer_Dark_Theme_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the application prefers to use a dark theme. If a GTK+ theme
   --  includes a dark variant, it will be used instead of the configured
   --  theme.
   --
   --  Some applications benefit from minimizing the amount of light pollution
   --  that interferes with the content. Good candidates for dark themes are
   --  photo and video editors that make the actual content get all the
   --  attention and minimize the distraction of the chrome.
   --
   --  Dark themes should not be used for documents, where large spaces are
   --  white/light and the dark chrome creates too much contrast (web browser,
   --  text editor...).

   Gtk_Auto_Mnemonics_Property : constant Glib.Properties.Property_Boolean;
   --  Whether mnemonics should be automatically shown and hidden when the
   --  user presses the mnemonic activator.

   Gtk_Button_Images_Property : constant Glib.Properties.Property_Boolean;
   --  Whether images should be shown on buttons

   Gtk_Can_Change_Accels_Property : constant Glib.Properties.Property_Boolean;
   --  Whether menu accelerators can be changed by pressing a key over the
   --  menu item.

   Gtk_Color_Palette_Property : constant Glib.Properties.Property_String;
   --  Palette to use in the deprecated color selector.

   Gtk_Color_Scheme_Property : constant Glib.Properties.Property_String;
   --  A palette of named colors for use in themes. The format of the string
   --  is |[ name1: color1 name2: color2 ... ]| Color names must be acceptable
   --  as identifiers in the [gtkrc][gtk3-Resource-Files] syntax, and color
   --  specifications must be in the format accepted by gdk_color_parse.
   --
   --  Note that due to the way the color tables from different sources are
   --  merged, color specifications will be converted to hexadecimal form when
   --  getting this property.
   --
   --  Starting with GTK+ 2.12, the entries can alternatively be separated by
   --  ';' instead of newlines: |[ name1: color1; name2: color2; ... ]|

   Gtk_Cursor_Aspect_Ratio_Property : constant Glib.Properties.Property_Float;

   Gtk_Cursor_Blink_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the cursor should blink.
   --
   --  Also see the Gtk.Settings.Gtk_Settings:gtk-cursor-blink-timeout
   --  setting, which allows more flexible control over cursor blinking.

   Gtk_Cursor_Blink_Time_Property : constant Glib.Properties.Property_Int;

   Gtk_Cursor_Blink_Timeout_Property : constant Glib.Properties.Property_Int;
   --  Time after which the cursor stops blinking, in seconds. The timer is
   --  reset after each user interaction.
   --
   --  Setting this to zero has the same effect as setting
   --  Gtk.Settings.Gtk_Settings:gtk-cursor-blink to False.

   Gtk_Cursor_Theme_Name_Property : constant Glib.Properties.Property_String;

   Gtk_Cursor_Theme_Size_Property : constant Glib.Properties.Property_Int;

   Gtk_Decoration_Layout_Property : constant Glib.Properties.Property_String;
   --  This setting determines which buttons should be put in the titlebar of
   --  client-side decorated windows, and whether they should be placed at the
   --  left of right.
   --
   --  The format of the string is button names, separated by commas. A colon
   --  separates the buttons that should appear on the left from those on the
   --  right. Recognized button names are minimize, maximize, close, icon (the
   --  window icon) and menu (a menu button for the fallback app menu).
   --
   --  For example, "menu:minimize,maximize,close" specifies a menu on the
   --  left, and minimize, maximize and close buttons on the right.
   --
   --  Note that buttons will only be shown when they are meaningful. E.g. a
   --  menu button only appears when the desktop shell does not show the app
   --  menu, and a close button only appears on a window that can be closed.
   --
   --  Also note that the setting can be overridden with the
   --  Gtk.Header_Bar.Gtk_Header_Bar:decoration-layout property.

   Gtk_Dialogs_Use_Header_Property : constant Glib.Properties.Property_Boolean;
   --  Whether builtin GTK+ dialogs such as the file chooser, the color
   --  chooser or the font chooser will use a header bar at the top to show
   --  action widgets, or an action area at the bottom.
   --
   --  This setting does not affect custom dialogs using GtkDialog directly,
   --  or message dialogs.

   Gtk_Dnd_Drag_Threshold_Property : constant Glib.Properties.Property_Int;

   Gtk_Double_Click_Distance_Property : constant Glib.Properties.Property_Int;

   Gtk_Double_Click_Time_Property : constant Glib.Properties.Property_Int;

   Gtk_Enable_Accels_Property : constant Glib.Properties.Property_Boolean;
   --  Whether menu items should have visible accelerators which can be
   --  activated.

   Gtk_Enable_Animations_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Enable_Event_Sounds_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to play any event sounds at all.
   --
   --  See the [Sound Theme
   --  Specifications](http://www.freedesktop.org/wiki/Specifications/sound-theme-spec)
   --  for more information on event sounds and sound themes.
   --
   --  GTK+ itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.

   Gtk_Enable_Input_Feedback_Sounds_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to play event sounds as feedback to user input.
   --
   --  See the [Sound Theme
   --  Specifications](http://www.freedesktop.org/wiki/Specifications/sound-theme-spec)
   --  for more information on event sounds and sound themes.
   --
   --  GTK+ itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.

   Gtk_Enable_Mnemonics_Property : constant Glib.Properties.Property_Boolean;
   --  Whether labels and menu items should have visible mnemonics which can
   --  be activated.

   Gtk_Enable_Primary_Paste_Property : constant Glib.Properties.Property_Boolean;
   --  Whether a middle click on a mouse should paste the 'PRIMARY' clipboard
   --  content at the cursor location.

   Gtk_Enable_Tooltips_Property : constant Glib.Properties.Property_Boolean;
   --  Whether tooltips should be shown on widgets.

   Gtk_Entry_Password_Hint_Timeout_Property : constant Glib.Properties.Property_Uint;
   --  How long to show the last input character in hidden entries. This value
   --  is in milliseconds. 0 disables showing the last char. 600 is a good
   --  value for enabling it.

   Gtk_Entry_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Error_Bell_Property : constant Glib.Properties.Property_Boolean;
   --  When True, keyboard navigation and other input-related errors will
   --  cause a beep. Since the error bell is implemented using Gdk.Window.Beep,
   --  the windowing system may offer ways to configure the error bell in many
   --  ways, such as flashing the window or similar visual effects.

   Gtk_Fallback_Icon_Theme_Property : constant Glib.Properties.Property_String;
   --  Name of a icon theme to fall back to.

   Gtk_File_Chooser_Backend_Property : constant Glib.Properties.Property_String;
   --  Name of the GtkFileChooser backend to use by default.

   Gtk_Font_Name_Property : constant Glib.Properties.Property_String;
   --  The default font to use. GTK+ uses the family name and size from this
   --  string.

   Gtk_Fontconfig_Timestamp_Property : constant Glib.Properties.Property_Uint;

   Gtk_Icon_Sizes_Property : constant Glib.Properties.Property_String;
   --  A list of icon sizes. The list is separated by colons, and item has the
   --  form:
   --
   --  `size-name` = `width` , `height`
   --
   --  E.g. "gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48". GTK+ itself
   --  use the following named icon sizes: gtk-menu, gtk-button,
   --  gtk-small-toolbar, gtk-large-toolbar, gtk-dnd, gtk-dialog. Applications
   --  can register their own named icon sizes with
   --  Gtk.Icon_Factory.Icon_Size_Register.

   Gtk_Icon_Theme_Name_Property : constant Glib.Properties.Property_String;

   Gtk_Im_Module_Property : constant Glib.Properties.Property_String;
   --  Which IM (input method) module should be used by default. This is the
   --  input method that will be used if the user has not explicitly chosen
   --  another input method from the IM context menu. This also can be a
   --  colon-separated list of input methods, which GTK+ will try in turn until
   --  it finds one available on the system.
   --
   --  See Gtk.IM_Context.Gtk_IM_Context.

   Gtk_Im_Preedit_Style_Property : constant Glib.Properties.Property_Boxed;
   --  Type: IMPreedit_Style
   --  How to draw the input method preedit string.

   Gtk_Im_Status_Style_Property : constant Glib.Properties.Property_Boxed;
   --  Type: IMStatus_Style
   --  How to draw the input method statusbar.

   Gtk_Key_Theme_Name_Property : constant Glib.Properties.Property_String;

   Gtk_Keynav_Cursor_Only_Property : constant Glib.Properties.Property_Boolean;
   --  When True, keyboard navigation should be able to reach all widgets by
   --  using the cursor keys only. Tab, Shift etc. keys can't be expected to be
   --  present on the used input device.

   Gtk_Keynav_Use_Caret_Property : constant Glib.Properties.Property_Boolean;
   --  Whether GTK+ should make sure that text can be navigated with a caret,
   --  even if it is not editable. This is useful when using a screen reader.

   Gtk_Keynav_Wrap_Around_Property : constant Glib.Properties.Property_Boolean;
   --  When True, some widgets will wrap around when doing keyboard
   --  navigation, such as menus, menubars and notebooks.

   Gtk_Label_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Long_Press_Time_Property : constant Glib.Properties.Property_Uint;
   --  The time for a button or touch press to be considered a "long press".

   Gtk_Menu_Bar_Accel_Property : constant Glib.Properties.Property_String;
   --  Keybinding to activate the menu bar.

   Gtk_Menu_Bar_Popup_Delay_Property : constant Glib.Properties.Property_Int;
   --  Delay before the submenus of a menu bar appear.

   Gtk_Menu_Images_Property : constant Glib.Properties.Property_Boolean;
   --  Whether images should be shown in menu items

   Gtk_Menu_Popdown_Delay_Property : constant Glib.Properties.Property_Int;
   --  The time before hiding a submenu when the pointer is moving towards the
   --  submenu.

   Gtk_Menu_Popup_Delay_Property : constant Glib.Properties.Property_Int;
   --  Minimum time the pointer must stay over a menu item before the submenu
   --  appear.

   Gtk_Modules_Property : constant Glib.Properties.Property_String;

   Gtk_Overlay_Scrolling_Property : constant Glib.Properties.Property_Boolean;
   --  Whether scrolled windows may use overlayed scrolling indicators. If
   --  this is set to False, scrolled windows will have permanent scrollbars.

   Gtk_Primary_Button_Warps_Slider_Property : constant Glib.Properties.Property_Boolean;
   --  If the value of this setting is True, clicking the primary button in a
   --  Gtk.GRange.Gtk_Range trough will move the slider, and hence set the
   --  range's value, to the point that you clicked. If it is False, a primary
   --  click will cause the slider/value to move by the range's page-size
   --  towards the point clicked.
   --
   --  Whichever action you choose for the primary button, the other action
   --  will be available by holding Shift and primary-clicking, or (since GTK+
   --  3.22.25) clicking the middle mouse button.

   Gtk_Print_Backends_Property : constant Glib.Properties.Property_String;
   --  A comma-separated list of print backends to use in the print dialog.
   --  Available print backends depend on the GTK+ installation, and may
   --  include "file", "cups", "lpr" or "papi".

   Gtk_Print_Preview_Command_Property : constant Glib.Properties.Property_String;
   --  A command to run for displaying the print preview. The command should
   --  contain a `%f` placeholder, which will get replaced by the path to the
   --  pdf file. The command may also contain a `%s` placeholder, which will
   --  get replaced by the path to a file containing the print settings in the
   --  format produced by Gtk.Print_Settings.To_File.
   --
   --  The preview application is responsible for removing the pdf file and
   --  the print settings file when it is done.

   Gtk_Recent_Files_Enabled_Property : constant Glib.Properties.Property_Boolean;
   --  Whether GTK+ should keep track of items inside the recently used
   --  resources list. If set to False, the list will always be empty.

   Gtk_Recent_Files_Limit_Property : constant Glib.Properties.Property_Int;
   --  The number of recently used files that should be displayed by default
   --  by Gtk.Recent_Chooser.Gtk_Recent_Chooser implementations and by the
   --  Gtk.File_Chooser.Gtk_File_Chooser. A value of -1 means every recently
   --  used file stored.

   Gtk_Recent_Files_Max_Age_Property : constant Glib.Properties.Property_Int;
   --  The maximum age, in days, of the items inside the recently used
   --  resources list. Items older than this setting will be excised from the
   --  list. If set to 0, the list will always be empty; if set to -1, no item
   --  will be removed.

   Gtk_Scrolled_Window_Placement_Property : constant Gtk.Enums.Property_Gtk_Corner_Type;
   --  Where the contents of scrolled windows are located with respect to the
   --  scrollbars, if not overridden by the scrolled window's own placement.

   Gtk_Shell_Shows_App_Menu_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Shell_Shows_Desktop_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Shell_Shows_Menubar_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Show_Input_Method_Menu_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Show_Unicode_Menu_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Sound_Theme_Name_Property : constant Glib.Properties.Property_String;
   --  The XDG sound theme to use for event sounds.
   --
   --  See the [Sound Theme
   --  Specifications](http://www.freedesktop.org/wiki/Specifications/sound-theme-spec)
   --  for more information on event sounds and sound themes.
   --
   --  GTK+ itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.

   Gtk_Split_Cursor_Property : constant Glib.Properties.Property_Boolean;

   Gtk_Theme_Name_Property : constant Glib.Properties.Property_String;

   Gtk_Timeout_Expand_Property : constant Glib.Properties.Property_Int;

   Gtk_Timeout_Initial_Property : constant Glib.Properties.Property_Int;

   Gtk_Timeout_Repeat_Property : constant Glib.Properties.Property_Int;

   Gtk_Titlebar_Double_Click_Property : constant Glib.Properties.Property_String;
   --  This setting determines the action to take when a double-click occurs
   --  on the titlebar of client-side decorated windows.
   --
   --  Recognized actions are minimize, toggle-maximize, menu, lower or none.

   Gtk_Titlebar_Middle_Click_Property : constant Glib.Properties.Property_String;
   --  This setting determines the action to take when a middle-click occurs
   --  on the titlebar of client-side decorated windows.
   --
   --  Recognized actions are minimize, toggle-maximize, menu, lower or none.

   Gtk_Titlebar_Right_Click_Property : constant Glib.Properties.Property_String;
   --  This setting determines the action to take when a right-click occurs on
   --  the titlebar of client-side decorated windows.
   --
   --  Recognized actions are minimize, toggle-maximize, menu, lower or none.

   Gtk_Toolbar_Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size;
   --  The size of icons in default toolbars.

   Gtk_Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style;
   --  The size of icons in default toolbars.

   Gtk_Tooltip_Browse_Mode_Timeout_Property : constant Glib.Properties.Property_Int;
   --  Amount of time, in milliseconds, after which the browse mode will be
   --  disabled.
   --
   --  See Gtk.Settings.Gtk_Settings:gtk-tooltip-browse-timeout for more
   --  information about browse mode.

   Gtk_Tooltip_Browse_Timeout_Property : constant Glib.Properties.Property_Int;
   --  Controls the time after which tooltips will appear when browse mode is
   --  enabled, in milliseconds.
   --
   --  Browse mode is enabled when the mouse pointer moves off an object where
   --  a tooltip was currently being displayed. If the mouse pointer hits
   --  another object before the browse mode timeout expires (see
   --  Gtk.Settings.Gtk_Settings:gtk-tooltip-browse-mode-timeout), it will take
   --  the amount of milliseconds specified by this setting to popup the
   --  tooltip for the new object.

   Gtk_Tooltip_Timeout_Property : constant Glib.Properties.Property_Int;
   --  Time, in milliseconds, after which a tooltip could appear if the cursor
   --  is hovering on top of a widget.

   Gtk_Touchscreen_Mode_Property : constant Glib.Properties.Property_Boolean;
   --  When True, there are no motion notify events delivered on this screen,
   --  and widgets can't use the pointer hovering them for any essential
   --  functionality.

   Gtk_Visible_Focus_Property : constant Gtk.Enums.Property_Gtk_Policy_Type;
   --  Whether 'focus rectangles' should be always visible, never visible, or
   --  hidden until the user starts to use the keyboard.

   Gtk_Xft_Antialias_Property : constant Glib.Properties.Property_Int;

   Gtk_Xft_Dpi_Property : constant Glib.Properties.Property_Int;

   Gtk_Xft_Hinting_Property : constant Glib.Properties.Property_Int;

   Gtk_Xft_Hintstyle_Property : constant Glib.Properties.Property_String;

   Gtk_Xft_Rgba_Property : constant Glib.Properties.Property_String;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "StyleProvider"

   package Implements_Gtk_Style_Provider is new Glib.Types.Implements
     (Gtk.Style_Provider.Gtk_Style_Provider, Gtk_Settings_Record, Gtk_Settings);
   function "+"
     (Widget : access Gtk_Settings_Record'Class)
   return Gtk.Style_Provider.Gtk_Style_Provider
   renames Implements_Gtk_Style_Provider.To_Interface;
   function "-"
     (Interf : Gtk.Style_Provider.Gtk_Style_Provider)
   return Gtk_Settings
   renames Implements_Gtk_Style_Provider.To_Object;

private
   Gtk_Xft_Rgba_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-xft-rgba");
   Gtk_Xft_Hintstyle_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-xft-hintstyle");
   Gtk_Xft_Hinting_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-xft-hinting");
   Gtk_Xft_Dpi_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-xft-dpi");
   Gtk_Xft_Antialias_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-xft-antialias");
   Gtk_Visible_Focus_Property : constant Gtk.Enums.Property_Gtk_Policy_Type :=
     Gtk.Enums.Build ("gtk-visible-focus");
   Gtk_Touchscreen_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-touchscreen-mode");
   Gtk_Tooltip_Timeout_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-tooltip-timeout");
   Gtk_Tooltip_Browse_Timeout_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-tooltip-browse-timeout");
   Gtk_Tooltip_Browse_Mode_Timeout_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-tooltip-browse-mode-timeout");
   Gtk_Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style :=
     Gtk.Enums.Build ("gtk-toolbar-style");
   Gtk_Toolbar_Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size :=
     Gtk.Enums.Build ("gtk-toolbar-icon-size");
   Gtk_Titlebar_Right_Click_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-titlebar-right-click");
   Gtk_Titlebar_Middle_Click_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-titlebar-middle-click");
   Gtk_Titlebar_Double_Click_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-titlebar-double-click");
   Gtk_Timeout_Repeat_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-timeout-repeat");
   Gtk_Timeout_Initial_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-timeout-initial");
   Gtk_Timeout_Expand_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-timeout-expand");
   Gtk_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-theme-name");
   Gtk_Split_Cursor_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-split-cursor");
   Gtk_Sound_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-sound-theme-name");
   Gtk_Show_Unicode_Menu_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-show-unicode-menu");
   Gtk_Show_Input_Method_Menu_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-show-input-method-menu");
   Gtk_Shell_Shows_Menubar_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-shell-shows-menubar");
   Gtk_Shell_Shows_Desktop_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-shell-shows-desktop");
   Gtk_Shell_Shows_App_Menu_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-shell-shows-app-menu");
   Gtk_Scrolled_Window_Placement_Property : constant Gtk.Enums.Property_Gtk_Corner_Type :=
     Gtk.Enums.Build ("gtk-scrolled-window-placement");
   Gtk_Recent_Files_Max_Age_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-recent-files-max-age");
   Gtk_Recent_Files_Limit_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-recent-files-limit");
   Gtk_Recent_Files_Enabled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-recent-files-enabled");
   Gtk_Print_Preview_Command_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-print-preview-command");
   Gtk_Print_Backends_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-print-backends");
   Gtk_Primary_Button_Warps_Slider_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-primary-button-warps-slider");
   Gtk_Overlay_Scrolling_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-overlay-scrolling");
   Gtk_Modules_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-modules");
   Gtk_Menu_Popup_Delay_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-menu-popup-delay");
   Gtk_Menu_Popdown_Delay_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-menu-popdown-delay");
   Gtk_Menu_Images_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-menu-images");
   Gtk_Menu_Bar_Popup_Delay_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-menu-bar-popup-delay");
   Gtk_Menu_Bar_Accel_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-menu-bar-accel");
   Gtk_Long_Press_Time_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("gtk-long-press-time");
   Gtk_Label_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-label-select-on-focus");
   Gtk_Keynav_Wrap_Around_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-keynav-wrap-around");
   Gtk_Keynav_Use_Caret_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-keynav-use-caret");
   Gtk_Keynav_Cursor_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-keynav-cursor-only");
   Gtk_Key_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-key-theme-name");
   Gtk_Im_Status_Style_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gtk-im-status-style");
   Gtk_Im_Preedit_Style_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gtk-im-preedit-style");
   Gtk_Im_Module_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-im-module");
   Gtk_Icon_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-icon-theme-name");
   Gtk_Icon_Sizes_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-icon-sizes");
   Gtk_Fontconfig_Timestamp_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("gtk-fontconfig-timestamp");
   Gtk_Font_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-font-name");
   Gtk_File_Chooser_Backend_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-file-chooser-backend");
   Gtk_Fallback_Icon_Theme_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-fallback-icon-theme");
   Gtk_Error_Bell_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-error-bell");
   Gtk_Entry_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-entry-select-on-focus");
   Gtk_Entry_Password_Hint_Timeout_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("gtk-entry-password-hint-timeout");
   Gtk_Enable_Tooltips_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-tooltips");
   Gtk_Enable_Primary_Paste_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-primary-paste");
   Gtk_Enable_Mnemonics_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-mnemonics");
   Gtk_Enable_Input_Feedback_Sounds_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-input-feedback-sounds");
   Gtk_Enable_Event_Sounds_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-event-sounds");
   Gtk_Enable_Animations_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-animations");
   Gtk_Enable_Accels_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-accels");
   Gtk_Double_Click_Time_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-double-click-time");
   Gtk_Double_Click_Distance_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-double-click-distance");
   Gtk_Dnd_Drag_Threshold_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-dnd-drag-threshold");
   Gtk_Dialogs_Use_Header_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-dialogs-use-header");
   Gtk_Decoration_Layout_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-decoration-layout");
   Gtk_Cursor_Theme_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-cursor-theme-size");
   Gtk_Cursor_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-cursor-theme-name");
   Gtk_Cursor_Blink_Timeout_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-cursor-blink-timeout");
   Gtk_Cursor_Blink_Time_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-cursor-blink-time");
   Gtk_Cursor_Blink_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-cursor-blink");
   Gtk_Cursor_Aspect_Ratio_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("gtk-cursor-aspect-ratio");
   Gtk_Color_Scheme_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-color-scheme");
   Gtk_Color_Palette_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-color-palette");
   Gtk_Can_Change_Accels_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-can-change-accels");
   Gtk_Button_Images_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-button-images");
   Gtk_Auto_Mnemonics_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-auto-mnemonics");
   Gtk_Application_Prefer_Dark_Theme_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-application-prefer-dark-theme");
   Gtk_Alternative_Sort_Arrows_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-alternative-sort-arrows");
   Gtk_Alternative_Button_Order_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-alternative-button-order");
   Color_Hash_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("color-hash");
end Gtk.Settings;
