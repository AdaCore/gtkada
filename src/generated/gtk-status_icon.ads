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
--  The "system tray" or notification area is normally used for transient
--  icons that indicate some special state. For example, a system tray icon
--  might appear to tell the user that they have new mail, or have an incoming
--  instant message, or something along those lines. The basic idea is that
--  creating an icon in the notification area is less annoying than popping up
--  a dialog.
--
--  A Gtk.Status_Icon.Gtk_Status_Icon object can be used to display an icon in
--  a "system tray". The icon can have a tooltip, and the user can interact
--  with it by activating it or popping up a context menu. Critical information
--  should not solely be displayed in a Gtk.Status_Icon.Gtk_Status_Icon, since
--  it may not be visible (e.g. when the user doesn't have a notification area
--  on his panel). This can be checked with Gtk.Status_Icon.Is_Embedded.
--
--  On X11, the implementation follows the freedesktop.org "System Tray"
--  <ulink
--  url="http://www.freedesktop.org/wiki/Specifications/systemtray-spec">specification</ulink>.
--  Implementations of the "tray" side of this specification can be found e.g.
--  in the GNOME 2 and KDE panel applications.
--
--  Note that a GtkStatusIcon is *not* a widget, but just a
--  Glib.Object.GObject. Making it a widget would be impractical, since the
--  system tray on Win32 doesn't allow to embed arbitrary widgets.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Gdk.Screen;      use Gdk.Screen;
with Glib;            use Glib;
with Glib.G_Icon;     use Glib.G_Icon;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Image;       use Gtk.Image;

package Gtk.Status_Icon is

   type Gtk_Status_Icon_Record is new GObject_Record with null record;
   type Gtk_Status_Icon is access all Gtk_Status_Icon_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Status_Icon : out Gtk_Status_Icon);
   procedure Initialize (Status_Icon : access Gtk_Status_Icon_Record'Class);
   --  Creates an empty status icon object.
   --  Since: gtk+ 2.10

   procedure Gtk_New_From_File
      (Status_Icon : out Gtk_Status_Icon;
       Filename    : UTF8_String);
   procedure Initialize_From_File
      (Status_Icon : access Gtk_Status_Icon_Record'Class;
       Filename    : UTF8_String);
   --  Creates a status icon displaying the file Filename.
   --  The image will be scaled down to fit in the available space in the
   --  notification area, if necessary.
   --  Since: gtk+ 2.10
   --  "filename": a filename

   procedure Gtk_New_From_Gicon
      (Status_Icon : out Gtk_Status_Icon;
       Icon        : Glib.G_Icon.G_Icon);
   procedure Initialize_From_Gicon
      (Status_Icon : access Gtk_Status_Icon_Record'Class;
       Icon        : Glib.G_Icon.G_Icon);
   --  Creates a status icon displaying a GIcon. If the icon is a themed icon,
   --  it will be updated when the theme changes.
   --  Since: gtk+ 2.14
   --  "icon": a GIcon

   procedure Gtk_New_From_Icon_Name
      (Status_Icon : out Gtk_Status_Icon;
       Icon_Name   : UTF8_String);
   procedure Initialize_From_Icon_Name
      (Status_Icon : access Gtk_Status_Icon_Record'Class;
       Icon_Name   : UTF8_String);
   --  Creates a status icon displaying an icon from the current icon theme.
   --  If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  Since: gtk+ 2.10
   --  "icon_name": an icon name

   procedure Gtk_New_From_Pixbuf
      (Status_Icon : out Gtk_Status_Icon;
       Pixbuf      : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize_From_Pixbuf
      (Status_Icon : access Gtk_Status_Icon_Record'Class;
       Pixbuf      : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a status icon displaying Pixbuf.
   --  The image will be scaled down to fit in the available space in the
   --  notification area, if necessary.
   --  Since: gtk+ 2.10
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf

   procedure Gtk_New_From_Stock
      (Status_Icon : out Gtk_Status_Icon;
       Stock_Id    : UTF8_String);
   procedure Initialize_From_Stock
      (Status_Icon : access Gtk_Status_Icon_Record'Class;
       Stock_Id    : UTF8_String);
   --  Creates a status icon displaying a stock icon. Sample stock icon names
   --  are GTK_STOCK_OPEN, GTK_STOCK_QUIT. You can register your own stock icon
   --  names, see Gtk.Icon_Factory.Add_Default and Gtk.Icon_Factory.Add.
   --  Since: gtk+ 2.10
   --  "stock_id": a stock icon id

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_status_icon_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Gicon
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Glib.G_Icon.G_Icon;
   procedure Set_From_Gicon
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Icon        : Glib.G_Icon.G_Icon);
   --  Makes Status_Icon display the GIcon. See
   --  Gtk.Status_Icon.Gtk_New_From_Gicon for details.
   --  Since: gtk+ 2.14
   --  "icon": a GIcon

   function Get_Has_Tooltip
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Boolean;
   procedure Set_Has_Tooltip
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Has_Tooltip : Boolean);
   --  Sets the has-tooltip property on Status_Icon to Has_Tooltip. See
   --  Gtk.Status_Icon.Gtk_Status_Icon:has-tooltip for more information.
   --  Since: gtk+ 2.16
   --  "has_tooltip": whether or not Status_Icon has a tooltip

   function Get_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String;
   procedure Set_From_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Icon_Name   : UTF8_String);
   --  Makes Status_Icon display the icon named Icon_Name from the current
   --  icon theme. See Gtk.Status_Icon.Gtk_New_From_Icon_Name for details.
   --  Since: gtk+ 2.10
   --  "icon_name": an icon name

   function Get_Pixbuf
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   procedure Set_From_Pixbuf
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Pixbuf      : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Makes Status_Icon display Pixbuf. See
   --  Gtk.Status_Icon.Gtk_New_From_Pixbuf for details.
   --  Since: gtk+ 2.10
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf or null

   function Get_Screen
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Gdk.Screen.Gdk_Screen;
   procedure Set_Screen
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Screen      : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Sets the Gdk.Screen.Gdk_Screen where Status_Icon is displayed; if the
   --  icon is already mapped, it will be unmapped, and then remapped on the
   --  new screen.
   --  Since: gtk+ 2.12
   --  "screen": a Gdk.Screen.Gdk_Screen

   function Get_Size
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Gint;
   --  Gets the size in pixels that is available for the image. Stock icons
   --  and named icons adapt their size automatically if the size of the
   --  notification area changes. For other storage types, the size-changed
   --  signal can be used to react to size changes.
   --  Note that the returned size is only meaningful while the status icon is
   --  embedded (see Gtk.Status_Icon.Is_Embedded).
   --  Since: gtk+ 2.10

   function Get_Stock
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String;
   procedure Set_From_Stock
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Stock_Id    : UTF8_String);
   --  Makes Status_Icon display the stock icon with the id Stock_Id. See
   --  Gtk.Status_Icon.Gtk_New_From_Stock for details.
   --  Since: gtk+ 2.10
   --  "stock_id": a stock icon id

   function Get_Storage_Type
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Gtk.Image.Gtk_Image_Type;
   --  Gets the type of representation being used by the
   --  Gtk.Status_Icon.Gtk_Status_Icon to store image data. If the
   --  Gtk.Status_Icon.Gtk_Status_Icon has no image data, the return value will
   --  be Gtk.Image.Image_Empty.
   --  Since: gtk+ 2.10

   function Get_Title
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String;
   procedure Set_Title
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Title       : UTF8_String);
   --  Sets the title of this tray icon. This should be a short,
   --  human-readable, localized string describing the tray icon. It may be
   --  used by tools like screen readers to render the tray icon.
   --  Since: gtk+ 2.18
   --  "title": the title

   function Get_Tooltip_Markup
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String;
   procedure Set_Tooltip_Markup
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Markup      : UTF8_String);
   --  Sets Markup as the contents of the tooltip, which is marked up with the
   --  <link linkend="PangoMarkupFormat">Pango text markup language</link>.
   --  This function will take care of setting
   --  Gtk.Status_Icon.Gtk_Status_Icon:has-tooltip to True and of the default
   --  handler for the Gtk.Status_Icon.Gtk_Status_Icon::query-tooltip signal.
   --  See also the Gtk.Status_Icon.Gtk_Status_Icon:tooltip-markup property and
   --  gtk_tooltip_set_markup.
   --  Since: gtk+ 2.16
   --  "markup": the contents of the tooltip for Status_Icon, or null

   function Get_Tooltip_Text
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String;
   procedure Set_Tooltip_Text
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Text        : UTF8_String);
   --  Sets Text as the contents of the tooltip.
   --  This function will take care of setting
   --  Gtk.Status_Icon.Gtk_Status_Icon:has-tooltip to True and of the default
   --  handler for the Gtk.Status_Icon.Gtk_Status_Icon::query-tooltip signal.
   --  See also the Gtk.Status_Icon.Gtk_Status_Icon:tooltip-text property and
   --  gtk_tooltip_set_text.
   --  Since: gtk+ 2.16
   --  "text": the contents of the tooltip for Status_Icon

   function Get_Visible
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Boolean;
   procedure Set_Visible
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Visible     : Boolean);
   --  Shows or hides a status icon.
   --  Since: gtk+ 2.10
   --  "visible": True to show the status icon, False to hide it

   function Get_X11_Window_Id
      (Status_Icon : not null access Gtk_Status_Icon_Record) return guint32;
   --  This function is only useful on the X11/freedesktop.org platform. It
   --  returns a window ID for the widget in the underlying status icon
   --  implementation. This is useful for the Galago notification service,
   --  which can send a window ID in the protocol in order for the server to
   --  position notification windows pointing to a status icon reliably.
   --  This function is not intended for other use cases which are more likely
   --  to be met by one of the non-X11 specific methods, such as
   --  Gtk.Status_Icon.Position_Menu.
   --  underlying X11 Window
   --  Since: gtk+ 2.14

   function Is_Embedded
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Boolean;
   --  Returns whether the status icon is embedded in a notification area.
   --  a notification area.
   --  Since: gtk+ 2.10

   procedure Set_From_File
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Filename    : UTF8_String);
   --  Makes Status_Icon display the file Filename. See
   --  Gtk.Status_Icon.Gtk_New_From_File for details.
   --  Since: gtk+ 2.10
   --  "filename": a filename

   procedure Set_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Name        : UTF8_String);
   --  Sets the name of this tray icon. This should be a string identifying
   --  this icon. It is may be used for sorting the icons in the tray and will
   --  not be shown to the user.
   --  Since: gtk+ 2.20
   --  "name": the name

   ---------------
   -- Functions --
   ---------------

   procedure Position_Menu
      (Menu      : System.Address;
       X         : out Gint;
       Y         : out Gint;
       Push_In   : out gboolean;
       User_Data : System.Address);
   --  Menu positioning function to use with gtk_menu_popup to position Menu
   --  aligned to the status icon User_Data.
   --  Since: gtk+ 2.10
   --  "menu": the Gtk.Menu.Gtk_Menu
   --  "x": return location for the x position
   --  "y": return location for the y position
   --  "push_in": whether the first menu item should be offset (pushed in) to
   --  be aligned with the menu popup position (only useful for GtkOptionMenu).
   --  "user_data": the status icon to position the menu on

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Embedded_Property
   --  Type: Boolean
   --  Flags: read-write
   --  True if the statusicon is embedded in a notification area.
   --
   --  Name: File_Property
   --  Type: UTF8_String
   --  Flags: write
   --
   --  Name: Gicon_Property
   --  Type: Glib.G_Icon.G_Icon
   --  Flags: read-write
   --  The GIcon displayed in the Gtk.Status_Icon.Gtk_Status_Icon. For themed
   --  icons, the image will be updated automatically if the theme changes.
   --
   --  Name: Has_Tooltip_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Enables or disables the emission of
   --  Gtk.Status_Icon.Gtk_Status_Icon::query-tooltip on Status_Icon. A value
   --  of True indicates that Status_Icon can have a tooltip, in this case the
   --  status icon will be queried using
   --  Gtk.Status_Icon.Gtk_Status_Icon::query-tooltip to determine whether it
   --  will provide a tooltip or not.
   --  Note that setting this property to True for the first time will change
   --  the event masks of the windows of this status icon to include
   --  leave-notify and motion-notify events. This will not be undone when the
   --  property is set to False again.
   --  Whether this property is respected is platform dependent. For plain text
   --  tooltips, use Gtk.Status_Icon.Gtk_Status_Icon:tooltip-text in
   --  preference.
   --
   --  Name: Icon_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Orientation_Property
   --  Type: Gtk.Enums.Gtk_Orientation
   --  Flags: read-write
   --  The orientation of the tray in which the statusicon is embedded.
   --
   --  Name: Pixbuf_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  Flags: read-write
   --
   --  Name: Screen_Property
   --  Type: Gdk.Screen
   --  Flags: read-write
   --
   --  Name: Size_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Stock_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Storage_Type_Property
   --  Type: Gtk.Image.Gtk_Image_Type
   --  Flags: read-write
   --
   --  Name: Title_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The title of this tray icon. This should be a short, human-readable,
   --  localized string describing the tray icon. It may be used by tools like
   --  screen readers to render the tray icon.
   --
   --  Name: Tooltip_Markup_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Sets the text of tooltip to be the given string, which is marked up
   --  with the <link linkend="PangoMarkupFormat">Pango text markup
   --  language</link>. Also see gtk_tooltip_set_markup.
   --  This is a convenience property which will take care of getting the
   --  tooltip shown if the given string is not null.
   --  Gtk.Status_Icon.Gtk_Status_Icon:has-tooltip will automatically be set to
   --  True and the default handler for the
   --  Gtk.Status_Icon.Gtk_Status_Icon::query-tooltip signal will take care of
   --  displaying the tooltip.
   --  On some platforms, embedded markup will be ignored.
   --
   --  Name: Tooltip_Text_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Sets the text of tooltip to be the given string.
   --  Also see gtk_tooltip_set_text.
   --  This is a convenience property which will take care of getting the
   --  tooltip shown if the given string is not null.
   --  Gtk.Status_Icon.Gtk_Status_Icon:has-tooltip will automatically be set to
   --  True and the default handler for the
   --  Gtk.Status_Icon.Gtk_Status_Icon::query-tooltip signal will take care of
   --  displaying the tooltip.
   --  Note that some platforms have limitations on the length of tooltips that
   --  they allow on status icons, e.g. Windows only shows the first 64
   --  characters.
   --
   --  Name: Visible_Property
   --  Type: Boolean
   --  Flags: read-write

   Embedded_Property : constant Glib.Properties.Property_Boolean;
   File_Property : constant Glib.Properties.Property_String;
   Gicon_Property : constant Glib.Properties.Property_Boxed;
   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean;
   Icon_Name_Property : constant Glib.Properties.Property_String;
   Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation;
   Pixbuf_Property : constant Glib.Properties.Property_Object;
   Screen_Property : constant Glib.Properties.Property_Boxed;
   Size_Property : constant Glib.Properties.Property_Int;
   Stock_Property : constant Glib.Properties.Property_String;
   Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type;
   Title_Property : constant Glib.Properties.Property_String;
   Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   Tooltip_Text_Property : constant Glib.Properties.Property_String;
   Visible_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate"
   --     procedure Handler (Self : access Gtk_Status_Icon_Record'Class);
   --  Gets emitted when the user activates the status icon. If and how status
   --  icons can activated is platform-dependent.
   --  Unlike most G_SIGNAL_ACTION signals, this signal is meant to be used by
   --  applications and should be wrapped by language bindings.
   --
   --  "button-press-event"
   --     function Handler
   --       (Self  : access Gtk_Status_Icon_Record'Class;
   --        Event : Gdk.Event_Button) return Boolean;
   --    --  "event": the Gdk_Event_Button which triggered this signal
   --  The ::button-press-event signal will be emitted when a button
   --  (typically from a mouse) is pressed.
   --  Whether this event is emitted is platform-dependent. Use the ::activate
   --  and ::popup-menu signals in preference.
   --  for the event. False to propagate the event further.
   --  Returns True to stop other handlers from being invoked
   --
   --  "button-release-event"
   --     function Handler
   --       (Self  : access Gtk_Status_Icon_Record'Class;
   --        Event : Gdk.Event_Button) return Boolean;
   --    --  "event": the Gdk_Event_Button which triggered this signal
   --  The ::button-release-event signal will be emitted when a button
   --  (typically from a mouse) is released.
   --  Whether this event is emitted is platform-dependent. Use the ::activate
   --  and ::popup-menu signals in preference.
   --  for the event. False to propagate the event further.
   --  Returns True to stop other handlers from being invoked
   --
   --  "popup-menu"
   --     procedure Handler
   --       (Self          : access Gtk_Status_Icon_Record'Class;
   --        Button        : Guint;
   --        Activate_Time : Guint);
   --    --  "button": the button that was pressed, or 0 if the signal is not
   --    --  emitted in response to a button press event
   --    --  "activate_time": the timestamp of the event that triggered the signal
   --    --  emission
   --  Gets emitted when the user brings up the context menu of the status
   --  icon. Whether status icons can have context menus and how these are
   --  activated is platform-dependent.
   --  The Button and Activate_Time parameters should be passed as the last to
   --  arguments to gtk_menu_popup.
   --  Unlike most G_SIGNAL_ACTION signals, this signal is meant to be used by
   --  applications and should be wrapped by language bindings.
   --
   --  "query-tooltip"
   --     function Handler
   --       (Self          : access Gtk_Status_Icon_Record'Class;
   --        X             : Gint;
   --        Y             : Gint;
   --        Keyboard_Mode : Boolean;
   --        Tooltip       : Gtk.Tooltip.Gtk_Tooltip) return Boolean;
   --    --  "x": the x coordinate of the cursor position where the request has been
   --    --  emitted, relative to Status_Icon
   --    --  "y": the y coordinate of the cursor position where the request has been
   --    --  emitted, relative to Status_Icon
   --    --  "keyboard_mode": True if the tooltip was trigged using the keyboard
   --    --  "tooltip": a Gtk.Tooltip.Gtk_Tooltip
   --  Emitted when the Gtk.Settings.Gtk_Settings:gtk-tooltip-timeout has
   --  expired with the cursor hovering above Status_Icon; or emitted when
   --  Status_Icon got focus in keyboard mode.
   --  Using the given coordinates, the signal handler should determine whether
   --  a tooltip should be shown for Status_Icon. If this is the case True
   --  should be returned, False otherwise. Note that if Keyboard_Mode is True,
   --  the values of X and Y are undefined and should not be used.
   --  The signal handler is free to manipulate Tooltip with the therefore
   --  destined function calls.
   --  Whether this signal is emitted is platform-dependent. For plain text
   --  tooltips, use Gtk.Status_Icon.Gtk_Status_Icon:tooltip-text in
   --  preference.
   --  Returns True if Tooltip should be shown right now, False otherwise.
   --
   --  "scroll-event"
   --     function Handler
   --       (Self  : access Gtk_Status_Icon_Record'Class;
   --        Event : Gdk.Event_Scroll) return Boolean;
   --    --  "event": the Gdk_Event_Scroll which triggered this signal
   --  The ::scroll-event signal is emitted when a button in the 4 to 7 range
   --  is pressed. Wheel mice are usually configured to generate button press
   --  events for buttons 4 and 5 when the wheel is turned.
   --  Whether this event is emitted is platform-dependent.
   --  False to propagate the event further.
   --  Returns True to stop other handlers from being invoked for the event.
   --
   --  "size-changed"
   --     function Handler
   --       (Self : access Gtk_Status_Icon_Record'Class;
   --        Size : Gint) return Boolean;
   --    --  "size": the new size
   --  Gets emitted when the size available for the image changes, e.g.
   --  because the notification area got resized.
   --  size. Otherwise, GTK+ will scale the icon as necessary.
   --  Returns True if the icon was updated for the new

   Signal_Activate : constant Glib.Signal_Name := "activate";
   Signal_Button_Press_Event : constant Glib.Signal_Name := "button-press-event";
   Signal_Button_Release_Event : constant Glib.Signal_Name := "button-release-event";
   Signal_Popup_Menu : constant Glib.Signal_Name := "popup-menu";
   Signal_Query_Tooltip : constant Glib.Signal_Name := "query-tooltip";
   Signal_Scroll_Event : constant Glib.Signal_Name := "scroll-event";
   Signal_Size_Changed : constant Glib.Signal_Name := "size-changed";

private
   Embedded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("embedded");
   File_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file");
   Gicon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gicon");
   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-tooltip");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Build ("orientation");
   Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Screen_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("screen");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock");
   Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type :=
     Gtk.Image.Build ("storage-type");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-markup");
   Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-text");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
end Gtk.Status_Icon;
