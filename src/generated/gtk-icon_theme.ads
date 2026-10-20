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

--  Loads themed icons.
--
--  The main reason for using a name rather than simply providing a filename
--  is to allow different icons to be used depending on what "icon theme" is
--  selected by the user. The operation of icon themes on Linux and Unix
--  follows the [Icon Theme
--  Specification](http://www.freedesktop.org/Standards/icon-theme-spec) There
--  is a fallback icon theme, named `hicolor`, where applications should
--  install their icons, but additional icon themes can be installed as
--  operating system vendors and users choose.
--
--  In many cases, named themes are used indirectly, via [classGtk.Image]
--  rather than directly, but looking up icons directly is also simple. The
--  `GtkIconTheme` object acts as a database of all the icons in the current
--  theme. You can create new `GtkIconTheme` objects, but it's much more
--  efficient to use the standard icon theme of the `GtkWidget` so that the
--  icon information is shared with other people looking up icons.
--
--  ```c GtkIconTheme *icon_theme; GtkIconPaintable *icon; GdkPaintable
--  *paintable;
--
--  icon_theme = gtk_icon_theme_get_for_display (gtk_widget_get_display
--  (my_widget)); icon = gtk_icon_theme_lookup_icon (icon_theme,
--  "my-icon-name", // icon name 48, // icon size 1, // scale 0, // flags);
--  paintable = GDK_PAINTABLE (icon); // Use the paintable g_object_unref
--  (icon); ```

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Gdk;                     use Gdk;
with Gdk.Display;
with Glib;                    use Glib;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Icon_Paintable;      use Gtk.Icon_Paintable;

package Gtk.Icon_Theme is

   type Gtk_Icon_Theme_Record is new GObject_Record with null record;
   type Gtk_Icon_Theme is access all Gtk_Icon_Theme_Record'Class;

   type Gtk_Icon_Lookup_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Icon_Lookup_Flags);
   --  Used to specify options for Gtk.Icon_Theme.Lookup_Icon.

   Icon_Lookup_None : constant Gtk_Icon_Lookup_Flags := 0;
   Icon_Lookup_Force_Regular : constant Gtk_Icon_Lookup_Flags := 1;
   Icon_Lookup_Force_Symbolic : constant Gtk_Icon_Lookup_Flags := 2;
   Icon_Lookup_Preload : constant Gtk_Icon_Lookup_Flags := 4;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Icon_Lookup_Flags_Properties is
      new Generic_Internal_Flags_Property (Gtk_Icon_Lookup_Flags);
   type Property_Gtk_Icon_Lookup_Flags is new Gtk_Icon_Lookup_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Icon_Theme);
   procedure Initialize (Self : not null access Gtk_Icon_Theme_Record'Class);
   --  Creates a new icon theme object.
   --  Icon theme objects are used to lookup up an icon by name in a
   --  particular icon theme. Usually, you'll want to use
   --  [funcGtk.IconTheme.get_for_display] rather than creating a new icon
   --  theme object for scratch.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Icon_Theme_New return Gtk_Icon_Theme;
   --  Creates a new icon theme object.
   --  Icon theme objects are used to lookup up an icon by name in a
   --  particular icon theme. Usually, you'll want to use
   --  [funcGtk.IconTheme.get_for_display] rather than creating a new icon
   --  theme object for scratch.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_theme_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Resource_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : UTF8_String);
   --  Adds a resource path that will be looked at when looking for icons,
   --  similar to search paths.
   --  See [methodGtk.IconTheme.set_resource_path].
   --  This function should be used to make application-specific icons
   --  available as part of the icon theme.
   --  @param Path a resource path

   procedure Add_Search_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : UTF8_String);
   --  Appends a directory to the search path.
   --  See [methodGtk.IconTheme.set_search_path].
   --  @param Path directory name to append to the icon path

   function Get_Display
      (Self : not null access Gtk_Icon_Theme_Record) return Gdk.Gdk_Display;
   --  Returns the display that the `GtkIconTheme` object was created for.
   --  @return the display of Icon_Theme. Has transfer-ownership='none'.

   function Get_Icon_Names
      (Self : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List;
   --  Lists the names of icons in the current icon theme.
   --  @return a string array holding the names of all the icons in the theme.
   --  You must free the array using g_strfreev.

   function Get_Icon_Sizes
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon_Name : UTF8_String) return Gint_Array;
   --  Returns an array of integers describing the sizes at which the icon is
   --  available without scaling.
   --  A size of -1 means that the icon is available in a scalable format. The
   --  array is zero-terminated.
   --  @param Icon_Name the name of an icon
   --  @return A newly allocated array describing the sizes at which the icon
   --  is available. The array should be freed with g_free when it is no longer
   --  needed.

   function Get_Resource_Path
      (Self : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List;
   --  Gets the current resource path.
   --  See [methodGtk.IconTheme.set_resource_path].
   --  @return A list of resource paths

   procedure Set_Resource_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : GNAT.Strings.String_List);
   --  Sets the resource paths that will be looked at when looking for icons,
   --  similar to search paths.
   --  The resources are considered as part of the hicolor icon theme and must
   --  be located in subdirectories that are defined in the hicolor icon theme,
   --  such as `Path/16x16/actions/run.png` or `Path/scalable/actions/run.svg`.
   --  Icons that are directly placed in the resource path instead of a
   --  subdirectory are also considered as ultimate fallback, but they are
   --  treated like unthemed icons.
   --  @param Path NULL-terminated array of resource paths that are searched
   --  for icons

   function Get_Search_Path
      (Self : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List;
   --  Gets the current search path.
   --  See [methodGtk.IconTheme.set_search_path].
   --  @return a list of icon theme path directories

   procedure Set_Search_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : GNAT.Strings.String_List);
   --  Sets the search path for the icon theme object.
   --  When looking for an icon theme, GTK will search for a subdirectory of
   --  one or more of the directories in Path with the same name as the icon
   --  theme containing an index.theme file. (Themes from multiple of the path
   --  elements are combined to allow themes to be extended by adding icons in
   --  the user's home directory.)
   --  In addition if an icon found isn't found either in the current icon
   --  theme or the default icon theme, and an image file with the right name
   --  is found directly in one of the elements of Path, then that image will
   --  be used for the icon name. (This is legacy feature, and new icons should
   --  be put into the fallback icon theme, which is called hicolor, rather
   --  than directly on the icon path.)
   --  @param Path NULL-terminated array of directories that are searched for
   --  icon themes

   function Get_Theme_Name
      (Self : not null access Gtk_Icon_Theme_Record) return UTF8_String;
   --  Gets the current icon theme name.
   --  @return the current icon theme name,

   procedure Set_Theme_Name
      (Self       : not null access Gtk_Icon_Theme_Record;
       Theme_Name : UTF8_String := "");
   --  Sets the name of the icon theme that the `GtkIconTheme` object uses
   --  overriding system configuration.
   --  This function cannot be called on the icon theme objects returned from
   --  [funcGtk.IconTheme.get_for_display].
   --  @param Theme_Name name of icon theme to use instead of configured
   --  theme, or null to unset a previously set custom theme

   function Has_Gicon
      (Self   : not null access Gtk_Icon_Theme_Record;
       G_Icon : Glib.G_Icon.G_Icon) return Boolean;
   --  Checks whether an icon theme includes an icon for a particular `GIcon`.
   --  Since: gtk+ 4.2
   --  @param G_Icon a `GIcon`
   --  @return True if Self includes an icon for Gicon

   function Has_Icon
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon_Name : UTF8_String) return Boolean;
   --  Checks whether an icon theme includes an icon for a particular name.
   --  @param Icon_Name the name of an icon
   --  @return True if Self includes an icon for Icon_Name.

   function Lookup_By_Gicon
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon      : Glib.G_Icon.G_Icon;
       Size      : Glib.Gint;
       Scale     : Glib.Gint;
       Direction : Gtk.Enums.Gtk_Text_Direction;
       Flags     : Gtk_Icon_Lookup_Flags)
       return Gtk.Icon_Paintable.Gtk_Icon_Paintable;
   --  Looks up a icon for a desired size and window scale.
   --  The icon can then be rendered by using it as a `GdkPaintable`, or you
   --  can get information such as the filename and size.
   --  @param Icon the `GIcon` to look up
   --  @param Size desired icon size, in application pixels
   --  @param Scale the desired scale
   --  @param Direction text direction the icon will be displayed in
   --  @param Flags flags modifying the behavior of the icon lookup
   --  @return a `GtkIconPaintable` containing information about the icon.
   --  Unref with g_object_unref. Has transfer-ownership='full'.

   function Lookup_Icon
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon_Name : UTF8_String;
       Fallbacks : GNAT.Strings.String_List;
       Size      : Glib.Gint;
       Scale     : Glib.Gint;
       Direction : Gtk.Enums.Gtk_Text_Direction;
       Flags     : Gtk_Icon_Lookup_Flags)
       return Gtk.Icon_Paintable.Gtk_Icon_Paintable;
   --  Looks up a named icon for a desired size and window scale, returning a
   --  `GtkIconPaintable`.
   --  The icon can then be rendered by using it as a `GdkPaintable`, or you
   --  can get information such as the filename and size.
   --  If the available Icon_Name is not available and Fallbacks are provided,
   --  they will be tried in order.
   --  If no matching icon is found, then a paintable that renders the
   --  "missing icon" icon is returned. If you need to do something else for
   --  missing icons you need to use [methodGtk.IconTheme.has_icon].
   --  Note that you probably want to listen for icon theme changes and update
   --  the icon. This is usually done by overriding the
   --  GtkWidgetClass.css-changed function.
   --  @param Icon_Name the name of the icon to lookup
   --  @param Fallbacks fallback names
   --  @param Size desired icon size, in application pixels
   --  @param Scale the window scale this will be displayed on
   --  @param Direction text direction the icon will be displayed in
   --  @param Flags flags modifying the behavior of the icon lookup
   --  @return a `GtkIconPaintable` object containing the icon. Has
   --  transfer-ownership='full'.

   ---------------
   -- Functions --
   ---------------

   function Get_For_Display
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gtk_Icon_Theme;
   --  Gets the icon theme object associated with Display.
   --  If this function has not previously been called for the given display,
   --  a new icon theme object will be created and associated with the display.
   --  Icon theme objects are fairly expensive to create, so using this
   --  function is usually a better choice than calling [ctorGtk.IconTheme.new]
   --  and setting the display yourself; by using this function a single icon
   --  theme object will be shared between users.
   --  @param Display a `GdkDisplay`
   --  @return A unique `GtkIconTheme` associated with the given display. This
   --  icon theme is associated with the display and can be used as long as the
   --  display is open. Do not ref or unref it. Has transfer-ownership='none'.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Display
   --  The display that this icon theme object is attached to.

   Theme_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the icon theme that is being used.
   --
   --  Unless set to a different value, this will be the value of the
   --  `GtkSettings:gtk-icon-theme-name` property of the `GtkSettings` object
   --  associated to the display of the icontheme object.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Icon_Theme_Void is not null access procedure (Self : access Gtk_Icon_Theme_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Icon_Theme_Record;
       Call  : Cb_Gtk_Icon_Theme_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Icon_Theme_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the icon theme changes.
   --
   --  This can happen because current icon theme is switched or because GTK
   --  detects that a change has occurred in the contents of the current icon
   --  theme.

private
   Theme_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("theme-name");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gtk.Icon_Theme;
