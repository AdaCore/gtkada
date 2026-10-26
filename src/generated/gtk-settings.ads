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

--  Provides a mechanism to share global settings between applications.
--
--  GTK relies on the platform-specific API for getting desktop-wide settings.
--
--  On Wayland, the settings are obtained via a settings portal that is part
--  of the Linux desktop APIs for application.
--
--  On the X window system, this sharing is realized by an
--  [XSettings](http://www.freedesktop.org/wiki/Specifications/xsettings-spec)
--  manager.
--
--  On macOS, the settings are obtained from `NSUserDefaults`.
--
--  In the absence of these sharing mechanisms, GTK reads default values for
--  settings from `settings.ini` files in `/etc/gtk-4.0`,
--  `$XDG_CONFIG_DIRS/gtk-4.0` and `$XDG_CONFIG_HOME/gtk-4.0`. These files must
--  be valid key files (see `GKeyFile`), and have a section called Settings.
--  Themes can also provide default values for settings by installing a
--  `settings.ini` file next to their `gtk.css` file.
--
--  Applications can override system-wide settings by setting the property of
--  the `GtkSettings` object with g_object_set. This should be restricted to
--  special cases though; `GtkSettings` are not meant as an application
--  configuration facility.
--
--  There is one `GtkSettings` instance per display. It can be obtained with
--  [funcGtk.Settings.get_for_display], but in many cases, it is more
--  convenient to use [methodGtk.Widget.get_settings].

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                use Gdk;
with Gdk.Display;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Types;         use Glib.Types;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Style_Provider; use Gtk.Style_Provider;

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
   --  application-specific value for a setting.
   --  After this call, the setting will again follow the session-wide value
   --  for this setting.
   --  @param Name the name of the setting to reset

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gtk_Settings;
   --  Gets the `GtkSettings` object for the default display, creating it if
   --  necessary.
   --  See [funcGtk.Settings.get_for_display].
   --  @return a `GtkSettings` object. If there is no default display, then
   --  returns null. Has transfer-ownership='none'.

   function Get_For_Display
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gtk_Settings;
   --  Gets the `GtkSettings` object for Display, creating it if necessary.
   --  @param Display a `GdkDisplay`
   --  @return a `GtkSettings` object. Has transfer-ownership='none'.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Gtk_Alternative_Button_Order_Property : constant Glib.Properties.Property_Boolean;
   --  Whether buttons in dialogs should use the alternative button order.

   Gtk_Alternative_Sort_Arrows_Property : constant Glib.Properties.Property_Boolean;
   --  Controls the direction of the sort indicators in sorted list and tree
   --  views.
   --
   --  By default an arrow pointing down means the column is sorted in
   --  ascending order. When set to True, this order will be inverted.

   Gtk_Application_Prefer_Dark_Theme_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the application prefers to use a dark theme.
   --
   --  If a GTK theme includes a dark variant, it will be used instead of the
   --  configured theme.
   --
   --  Some applications benefit from minimizing the amount of light pollution
   --  that interferes with the content. Good candidates for dark themes are
   --  photo and video editors that make the actual content get all the
   --  attention and minimize the distraction of the chrome.
   --
   --  Dark themes should not be used for documents, where large spaces are
   --  white/light and the dark chrome creates too much contrast (web browser,
   --  text editor...).

   Gtk_Cursor_Aspect_Ratio_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The aspect ratio of the text caret.

   Gtk_Cursor_Blink_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the cursor should blink.
   --
   --  Also see the [propertyGtk.Settings:gtk-cursor-blink-timeout] setting,
   --  which allows more flexible control over cursor blinking.

   Gtk_Cursor_Blink_Time_Property : constant Glib.Properties.Property_Int;
   --  Length of the cursor blink cycle, in milliseconds.

   Gtk_Cursor_Blink_Timeout_Property : constant Glib.Properties.Property_Int;
   --  Time after which the cursor stops blinking, in seconds.
   --
   --  The timer is reset after each user interaction.
   --
   --  Setting this to zero has the same effect as setting
   --  [propertyGtk.Settings:gtk-cursor-blink] to False.

   Gtk_Cursor_Theme_Name_Property : constant Glib.Properties.Property_String;
   --  Name of the cursor theme to use.
   --
   --  Use null to use the default theme.

   Gtk_Cursor_Theme_Size_Property : constant Glib.Properties.Property_Int;
   --  The size to use for cursors.
   --
   --  0 means to use the default size.

   Gtk_Decoration_Layout_Property : constant Glib.Properties.Property_String;
   --  Determines which buttons should be put in the titlebar of client-side
   --  decorated windows, and whether they should be placed on the left or
   --  right.
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
   --  [propertyGtk.HeaderBar:decoration-layout] property.

   Gtk_Dialogs_Use_Header_Property : constant Glib.Properties.Property_Boolean;
   --  Whether builtin GTK dialogs such as the file chooser, the color chooser
   --  or the font chooser will use a header bar at the top to show action
   --  widgets, or an action area at the bottom.
   --
   --  This setting does not affect custom dialogs using `GtkDialog` directly,
   --  or message dialogs.

   Gtk_Dnd_Drag_Threshold_Property : constant Glib.Properties.Property_Int;
   --  The number of pixels the cursor can move before dragging.

   Gtk_Double_Click_Distance_Property : constant Glib.Properties.Property_Int;
   --  The maximum distance allowed between two clicks for them to be
   --  considered a double click, in pixels.

   Gtk_Double_Click_Time_Property : constant Glib.Properties.Property_Int;
   --  The maximum time to allow between two clicks for them to be considered
   --  a double click, in milliseconds.

   Gtk_Enable_Accels_Property : constant Glib.Properties.Property_Boolean;
   --  Whether menu items should have visible accelerators which can be
   --  activated.

   Gtk_Enable_Animations_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to enable toolkit-wide animations.

   Gtk_Enable_Event_Sounds_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to play any event sounds at all.
   --
   --  See the [Sound Theme
   --  Specifications](http://www.freedesktop.org/wiki/Specifications/sound-theme-spec)
   --  for more information on event sounds and sound themes.
   --
   --  GTK itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.

   Gtk_Enable_Input_Feedback_Sounds_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to play event sounds as feedback to user input.
   --
   --  See the [Sound Theme
   --  Specifications](http://www.freedesktop.org/wiki/Specifications/sound-theme-spec)
   --  for more information on event sounds and sound themes.
   --
   --  GTK itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.

   Gtk_Enable_Primary_Paste_Property : constant Glib.Properties.Property_Boolean;
   --  Whether a middle click on a mouse should paste the 'PRIMARY' clipboard
   --  content at the cursor location.

   Gtk_Entry_Password_Hint_Timeout_Property : constant Glib.Properties.Property_Uint;
   --  How long to show the last input character in hidden entries.
   --
   --  This value is in milliseconds. 0 disables showing the last char. 600 is
   --  a good value for enabling it.

   Gtk_Entry_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to select the contents of an entry when it is focused.

   Gtk_Error_Bell_Property : constant Glib.Properties.Property_Boolean;
   --  When True, keyboard navigation and other input-related errors will
   --  cause a beep.
   --
   --  Since the error bell is implemented using Gdk.Surface.Beep, the
   --  windowing system may offer ways to configure the error bell in many
   --  ways, such as flashing the window or similar visual effects.

   Gtk_Font_Name_Property : constant Glib.Properties.Property_String;
   --  The default font to use.
   --
   --  GTK uses the family name and size from this string.

   Gtk_Font_Rendering_Property : constant Gtk.Enums.Property_Gtk_Font_Rendering;
   --  How GTK font rendering is set up.
   --
   --  When set to [enumGtk.FontRendering.MANUAL], GTK respects the low-level
   --  font-related settings ([propertyGtk.Settings:gtk-hint-font-metrics],
   --  [propertyGtk.Settings:gtk-xft-antialias],
   --  [propertyGtk.Settings:gtk-xft-hinting],
   --  [propertyGtk.Settings:gtk-xft-hintstyle] and
   --  [propertyGtk.Settings:gtk-xft-rgba]) as much as practical.
   --
   --  When set to [enumGtk.FontRendering.AUTOMATIC], GTK will consider
   --  factors such as screen resolution and scale in deciding how to render
   --  fonts.

   Gtk_Fontconfig_Timestamp_Property : constant Glib.Properties.Property_Uint;
   --  Timestamp of the current fontconfig configuration.

   Gtk_Hint_Font_Metrics_Property : constant Glib.Properties.Property_Boolean;
   --  Whether hinting should be applied to font metrics.
   --
   --  Note that this also turns off subpixel positioning of glyphs, since it
   --  conflicts with metrics hinting.

   Gtk_Icon_Theme_Name_Property : constant Glib.Properties.Property_String;
   --  Name of the icon theme to use.
   --
   --  See [classGtk.IconTheme] for details about how GTK handles icon themes.

   Gtk_Im_Module_Property : constant Glib.Properties.Property_String;
   --  Which IM (input method) module should be used by default.
   --
   --  This is the input method that will be used if the user has not
   --  explicitly chosen another input method from the IM context menu. This
   --  also can be a colon-separated list of input methods, which GTK will try
   --  in turn until it finds one available on the system.
   --
   --  See [classGtk.IMContext].

   Gtk_Interface_Color_Scheme_Property : constant Gtk.Enums.Property_Gtk_Interface_Color_Scheme;
   --  The color scheme used for rendering the user interface.
   --
   --  This setting communicates the system-wide preference. The color scheme
   --  that is actually used when applying CSS styles can be set with the
   --  [propertyGtk.CssProvider:prefers-color-scheme] property.

   Gtk_Interface_Contrast_Property : constant Gtk.Enums.Property_Gtk_Interface_Contrast;
   --  The level of contrast to use for the user interface.
   --
   --  This setting communicates the system-wide preference. The contrast
   --  level that is actually used when applying CSS styles can be set with the
   --  [propertyGtk.CssProvider:prefers-contrast] property.

   Gtk_Interface_Reduced_Motion_Property : constant Gtk.Enums.Property_Gtk_Reduced_Motion;
   --  Whether animations should be reduced to essential motions.
   --
   --  This setting communicates the system-wide preference. The motion level
   --  that is actually used when applying CSS styles can be set with the
   --  [propertyGtk.CssProvider:prefers-reduced-motion] property.

   Gtk_Keynav_Use_Caret_Property : constant Glib.Properties.Property_Boolean;
   --  Whether GTK should make sure that text can be navigated with a caret,
   --  even if it is not editable.
   --
   --  This is useful when using a screen reader.

   Gtk_Label_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to select the contents of a selectable label when it is
   --  focused.

   Gtk_Long_Press_Time_Property : constant Glib.Properties.Property_Uint;
   --  The time for a button or touch press to be considered a "long press".
   --
   --  See [classGtk.GestureLongPress].

   Gtk_Overlay_Scrolling_Property : constant Glib.Properties.Property_Boolean;
   --  Whether scrolled windows may use overlaid scrolling indicators.
   --
   --  If this is set to False, scrolled windows will have permanent
   --  scrollbars.

   Gtk_Primary_Button_Warps_Slider_Property : constant Glib.Properties.Property_Boolean;
   --  If the value of this setting is True, clicking the primary button in a
   --  `GtkRange` trough will move the slider, and hence set the range's value,
   --  to the point that you clicked.
   --
   --  If it is False, a primary click will cause the slider/value to move by
   --  the range's page-size towards the point clicked.
   --
   --  Whichever action you choose for the primary button, the other action
   --  will be available by holding Shift and primary-clicking, or clicking the
   --  middle mouse button.

   Gtk_Print_Backends_Property : constant Glib.Properties.Property_String;
   --  A comma-separated list of print backends to use in the print dialog.
   --
   --  Available print backends depend on the GTK installation, and may
   --  include "file", "cups", "lpr" or "papi".

   Gtk_Print_Preview_Command_Property : constant Glib.Properties.Property_String;
   --  A command to run for displaying the print preview.
   --
   --  The command should contain a `%f` placeholder, which will get replaced
   --  by the path to the pdf file. The command may also contain a `%s`
   --  placeholder, which will get replaced by the path to a file containing
   --  the print settings in the format produced by
   --  [methodGtk.PrintSettings.to_file].
   --
   --  The preview application is responsible for removing the pdf file and
   --  the print settings file when it is done.

   Gtk_Recent_Files_Enabled_Property : constant Glib.Properties.Property_Boolean;
   --  Whether GTK should keep track of items inside the recently used
   --  resources list.
   --
   --  If set to False, the list will always be empty.

   Gtk_Recent_Files_Max_Age_Property : constant Glib.Properties.Property_Int;
   --  The maximum age, in days, of the items inside the recently used
   --  resources list.
   --
   --  Items older than this setting will be excised from the list. If set to
   --  0, the list will always be empty; if set to -1, no item will be removed.

   Gtk_Shell_Shows_App_Menu_Property : constant Glib.Properties.Property_Boolean;
   --  Set to True if the desktop environment is displaying the app menu,
   --  False if the app should display it itself.

   Gtk_Shell_Shows_Desktop_Property : constant Glib.Properties.Property_Boolean;
   --  Set to True if the desktop environment is displaying the desktop
   --  folder, False if not.

   Gtk_Shell_Shows_Menubar_Property : constant Glib.Properties.Property_Boolean;
   --  Set to True if the desktop environment is displaying the menubar, False
   --  if the app should display it itself.

   Gtk_Show_Status_Shapes_Property : constant Glib.Properties.Property_Boolean;
   --  When True, widgets like switches include shapes to indicate their
   --  on/off state.

   Gtk_Sound_Theme_Name_Property : constant Glib.Properties.Property_String;
   --  The XDG sound theme to use for event sounds.
   --
   --  See the [Sound Theme
   --  Specifications](http://www.freedesktop.org/wiki/Specifications/sound-theme-spec)
   --  for more information on event sounds and sound themes.
   --
   --  GTK itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.

   Gtk_Split_Cursor_Property : constant Glib.Properties.Property_Boolean;
   --  Whether two cursors should be displayed for mixed left-to-right and
   --  right-to-left text.

   Gtk_Theme_Name_Property : constant Glib.Properties.Property_String;
   --  Name of the theme to load.
   --
   --  See [classGtk.CssProvider] for details about how GTK finds the CSS
   --  stylesheet for a theme.

   Gtk_Titlebar_Double_Click_Property : constant Glib.Properties.Property_String;
   --  Determines the action to take when a double-click occurs on the
   --  titlebar of client-side decorated windows.
   --
   --  Recognized actions are minimize, toggle-maximize, menu, lower or none.

   Gtk_Titlebar_Middle_Click_Property : constant Glib.Properties.Property_String;
   --  Determines the action to take when a middle-click occurs on the
   --  titlebar of client-side decorated windows.
   --
   --  Recognized actions are minimize, toggle-maximize, menu, lower or none.

   Gtk_Titlebar_Right_Click_Property : constant Glib.Properties.Property_String;
   --  Determines the action to take when a right-click occurs on the titlebar
   --  of client-side decorated windows.
   --
   --  Recognized actions are minimize, toggle-maximize, menu, lower or none.

   Gtk_Xft_Antialias_Property : constant Glib.Properties.Property_Int;
   --  Whether to antialias fonts.
   --
   --  The values are 0 for no, 1 for yes, or -1 for the system default.

   Gtk_Xft_Dpi_Property : constant Glib.Properties.Property_Int;
   --  The font resolution, in 1024 * dots/inch.
   --
   --  -1 to use the default value.

   Gtk_Xft_Hinting_Property : constant Glib.Properties.Property_Int;
   --  Whether to enable font hinting.
   --
   --  The values are 0 for no, 1 for yes, or -1 for the system default.

   Gtk_Xft_Hintstyle_Property : constant Glib.Properties.Property_String;
   --  What degree of font hinting to use.
   --
   --  The possible vaues are hintnone, hintslight, hintmedium, hintfull.

   Gtk_Xft_Rgba_Property : constant Glib.Properties.Property_String;
   --  The type of subpixel antialiasing to use.
   --
   --  The possible values are none, rgb, bgr, vrgb, vbgr.
   --
   --  Note that GSK does not support subpixel antialiasing, and this setting
   --  has no effect on font rendering in GTK.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.StyleProvider"

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
   Gtk_Titlebar_Right_Click_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-titlebar-right-click");
   Gtk_Titlebar_Middle_Click_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-titlebar-middle-click");
   Gtk_Titlebar_Double_Click_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-titlebar-double-click");
   Gtk_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-theme-name");
   Gtk_Split_Cursor_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-split-cursor");
   Gtk_Sound_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-sound-theme-name");
   Gtk_Show_Status_Shapes_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-show-status-shapes");
   Gtk_Shell_Shows_Menubar_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-shell-shows-menubar");
   Gtk_Shell_Shows_Desktop_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-shell-shows-desktop");
   Gtk_Shell_Shows_App_Menu_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-shell-shows-app-menu");
   Gtk_Recent_Files_Max_Age_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("gtk-recent-files-max-age");
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
   Gtk_Long_Press_Time_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("gtk-long-press-time");
   Gtk_Label_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-label-select-on-focus");
   Gtk_Keynav_Use_Caret_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-keynav-use-caret");
   Gtk_Interface_Reduced_Motion_Property : constant Gtk.Enums.Property_Gtk_Reduced_Motion :=
     Gtk.Enums.Build ("gtk-interface-reduced-motion");
   Gtk_Interface_Contrast_Property : constant Gtk.Enums.Property_Gtk_Interface_Contrast :=
     Gtk.Enums.Build ("gtk-interface-contrast");
   Gtk_Interface_Color_Scheme_Property : constant Gtk.Enums.Property_Gtk_Interface_Color_Scheme :=
     Gtk.Enums.Build ("gtk-interface-color-scheme");
   Gtk_Im_Module_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-im-module");
   Gtk_Icon_Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-icon-theme-name");
   Gtk_Hint_Font_Metrics_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-hint-font-metrics");
   Gtk_Fontconfig_Timestamp_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("gtk-fontconfig-timestamp");
   Gtk_Font_Rendering_Property : constant Gtk.Enums.Property_Gtk_Font_Rendering :=
     Gtk.Enums.Build ("gtk-font-rendering");
   Gtk_Font_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("gtk-font-name");
   Gtk_Error_Bell_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-error-bell");
   Gtk_Entry_Select_On_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-entry-select-on-focus");
   Gtk_Entry_Password_Hint_Timeout_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("gtk-entry-password-hint-timeout");
   Gtk_Enable_Primary_Paste_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-enable-primary-paste");
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
   Gtk_Cursor_Aspect_Ratio_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("gtk-cursor-aspect-ratio");
   Gtk_Application_Prefer_Dark_Theme_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-application-prefer-dark-theme");
   Gtk_Alternative_Sort_Arrows_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-alternative-sort-arrows");
   Gtk_Alternative_Button_Order_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("gtk-alternative-button-order");
end Gtk.Settings;
