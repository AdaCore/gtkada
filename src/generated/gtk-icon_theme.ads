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
--  Gtk.Icon_Theme.Gtk_Icon_Theme provides a facility for looking up icons by
--  name and size. The main reason for using a name rather than simply
--  providing a filename is to allow different icons to be used depending on
--  what "icon theme" is selected by the user. The operation of icon themes on
--  Linux and Unix follows the [Icon Theme
--  Specification](http://www.freedesktop.org/Standards/icon-theme-spec) There
--  is a fallback icon theme, named `hicolor`, where applications should
--  install their icons, but additional icon themes can be installed as
--  operating system vendors and users choose.
--
--  Named icons are similar to the deprecated [Stock Items][gtkstock], and the
--  distinction between the two may be a bit confusing. A few things to keep in
--  mind:
--
--  - Stock images usually are used in conjunction with [Stock
--  Items][gtkstock], such as GTK_STOCK_OK or GTK_STOCK_OPEN. Named icons are
--  easier to set up and therefore are more useful for new icons that an
--  application wants to add, such as application icons or window icons.
--
--  - Stock images can only be loaded at the symbolic sizes defined by the
--  Gtk.Enums.Gtk_Icon_Size enumeration, or by custom sizes defined by
--  Gtk.Icon_Factory.Icon_Size_Register, while named icons are more flexible
--  and any pixel size can be specified.
--
--  - Because stock images are closely tied to stock items, and thus to
--  actions in the user interface, stock images may come in multiple variants
--  for different widget states or writing directions.
--
--  A good rule of thumb is that if there is a stock image for what you want
--  to use, use it, otherwise use a named icon. It turns out that internally
--  stock images are generally defined in terms of one or more named icons. (An
--  example of the more than one case is icons that depend on writing
--  direction; GTK_STOCK_GO_FORWARD uses the two themed icons
--  "gtk-stock-go-forward-ltr" and "gtk-stock-go-forward-rtl".)
--
--  In many cases, named themes are used indirectly, via Gtk.Image.Gtk_Image
--  or stock items, rather than directly, but looking up icons directly is also
--  simple. The Gtk.Icon_Theme.Gtk_Icon_Theme object acts as a database of all
--  the icons in the current theme. You can create new
--  Gtk.Icon_Theme.Gtk_Icon_Theme objects, but it's much more efficient to use
--  the standard icon theme for the Gdk.Screen.Gdk_Screen so that the icon
--  information is shared with other people looking up icons. |[<!--
--  language="C" --> GError *error = NULL; GtkIconTheme *icon_theme; GdkPixbuf
--  *pixbuf;
--
--  icon_theme = gtk_icon_theme_get_default (); pixbuf =
--  gtk_icon_theme_load_icon (icon_theme, "my-icon-name", // icon name 48, //
--  icon size 0, // flags &error); if (!pixbuf) { g_warning ("Couldn't load
--  icon: %s", error->message); g_error_free (error); } else { // Use the
--  pixbuf g_object_unref (pixbuf); } ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with GNAT.Strings;            use GNAT.Strings;
with Gdk;                     use Gdk;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Screen;              use Gdk.Screen;
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Error;              use Glib.Error;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Style;               use Gtk.Style;
with Gtk.Style_Context;       use Gtk.Style_Context;

package Gtk.Icon_Theme is

   type Gtk_Icon_Theme_Record is new GObject_Record with null record;
   type Gtk_Icon_Theme is access all Gtk_Icon_Theme_Record'Class;

   type Gtk_Icon_Lookup_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Icon_Lookup_Flags);
   --  Used to specify options for Gtk.Icon_Theme.Lookup_Icon

   Icon_Lookup_No_Svg : constant Gtk_Icon_Lookup_Flags := 1;
   Icon_Lookup_Force_Svg : constant Gtk_Icon_Lookup_Flags := 2;
   Icon_Lookup_Use_Builtin : constant Gtk_Icon_Lookup_Flags := 4;
   Icon_Lookup_Generic_Fallback : constant Gtk_Icon_Lookup_Flags := 8;
   Icon_Lookup_Force_Size : constant Gtk_Icon_Lookup_Flags := 16;
   Icon_Lookup_Force_Regular : constant Gtk_Icon_Lookup_Flags := 32;
   Icon_Lookup_Force_Symbolic : constant Gtk_Icon_Lookup_Flags := 64;
   Icon_Lookup_Dir_Ltr : constant Gtk_Icon_Lookup_Flags := 128;
   Icon_Lookup_Dir_Rtl : constant Gtk_Icon_Lookup_Flags := 256;

   type Gtk_Icon_Info_Record is new GObject_Record with null record;
   type Gtk_Icon_Info is access all Gtk_Icon_Info_Record'Class;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Icon_Lookup_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Icon_Lookup_Flags);
   type Property_Gtk_Icon_Lookup_Flags is new Gtk_Icon_Lookup_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Icon_Theme : out Gtk_Icon_Theme);
   procedure Initialize
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class);
   --  Creates a new icon theme object. Icon theme objects are used to lookup
   --  up an icon by name in a particular icon theme. Usually, you'll want to
   --  use Gtk.Icon_Theme.Get_Default or Gtk.Icon_Theme.Get_For_Screen rather
   --  than creating a new icon theme object for scratch.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Icon_Theme_New return Gtk_Icon_Theme;
   --  Creates a new icon theme object. Icon theme objects are used to lookup
   --  up an icon by name in a particular icon theme. Usually, you'll want to
   --  use Gtk.Icon_Theme.Get_Default or Gtk.Icon_Theme.Get_For_Screen rather
   --  than creating a new icon theme object for scratch.
   --  Since: gtk+ 2.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_theme_get_type");

   procedure Gtk_New_For_Pixbuf
      (Icon_Info  : out Gtk_Icon_Info;
       Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize_For_Pixbuf
      (Icon_Info  : not null access Gtk_Icon_Info_Record'Class;
       Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a Gtk.Icon_Theme.Gtk_Icon_Info for a Gdk.Pixbuf.Gdk_Pixbuf.
   --  Since: gtk+ 2.14
   --  Initialize_For_Pixbuf does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "icon_theme": a Gtk.Icon_Theme.Gtk_Icon_Theme
   --  "pixbuf": the pixbuf to wrap in a Gtk.Icon_Theme.Gtk_Icon_Info

   function Gtk_Icon_Info_New_For_Pixbuf
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Icon_Info;
   --  Creates a Gtk.Icon_Theme.Gtk_Icon_Info for a Gdk.Pixbuf.Gdk_Pixbuf.
   --  Since: gtk+ 2.14
   --  "icon_theme": a Gtk.Icon_Theme.Gtk_Icon_Theme
   --  "pixbuf": the pixbuf to wrap in a Gtk.Icon_Theme.Gtk_Icon_Info

   function Icon_Info_Get_Type return Glib.GType;
   pragma Import (C, Icon_Info_Get_Type, "gtk_icon_info_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Resource_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String);
   --  Adds a resource path that will be looked at when looking for icons,
   --  similar to search paths.
   --  This function should be used to make application-specific icons
   --  available as part of the icon theme.
   --  The resources are considered as part of the hicolor icon theme and must
   --  be located in subdirectories that are defined in the hicolor icon theme,
   --  such as `Path/16x16/actions/run.png`. Icons that are directly placed in
   --  the resource path instead of a subdirectory are also considered as
   --  ultimate fallback.
   --  Since: gtk+ 3.14
   --  "path": a resource path

   procedure Append_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String);
   --  Appends a directory to the search path. See
   --  Gtk.Icon_Theme.Set_Search_Path.
   --  Since: gtk+ 2.4
   --  "path": directory name to append to the icon path

   function Choose_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon_Names : GNAT.Strings.String_List;
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info;
   --  Looks up a named icon and returns a Gtk.Icon_Theme.Gtk_Icon_Info
   --  containing information such as the filename of the icon. The icon can
   --  then be rendered into a pixbuf using Gtk.Icon_Theme.Load_Icon.
   --  (gtk_icon_theme_load_icon combines these two steps if all you need is
   --  the pixbuf.)
   --  If Icon_Names contains more than one name, this function tries them all
   --  in the given order before falling back to inherited icon themes.
   --  Since: gtk+ 2.12
   --  "icon_names": null-terminated array of icon names to lookup
   --  "size": desired icon size
   --  "flags": flags modifying the behavior of the icon lookup

   function Choose_Icon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Names : GNAT.Strings.String_List;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info;
   --  Looks up a named icon for a particular window scale and returns a
   --  Gtk.Icon_Theme.Gtk_Icon_Info containing information such as the filename
   --  of the icon. The icon can then be rendered into a pixbuf using
   --  Gtk.Icon_Theme.Load_Icon. (gtk_icon_theme_load_icon combines these two
   --  steps if all you need is the pixbuf.)
   --  If Icon_Names contains more than one name, this function tries them all
   --  in the given order before falling back to inherited icon themes.
   --  Since: gtk+ 3.10
   --  "icon_names": null-terminated array of icon names to lookup
   --  "size": desired icon size
   --  "scale": desired scale
   --  "flags": flags modifying the behavior of the icon lookup

   function Get_Example_Icon_Name
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return UTF8_String;
   --  Gets the name of an icon that is representative of the current theme
   --  (for instance, to use when presenting a list of themes to the user.)
   --  Since: gtk+ 2.4

   function Get_Icon_Sizes
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String) return Gint_Array;
   --  Returns an array of integers describing the sizes at which the icon is
   --  available without scaling. A size of -1 means that the icon is available
   --  in a scalable format. The array is zero-terminated.
   --  Since: gtk+ 2.6
   --  "icon_name": the name of an icon

   function Get_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List;
   --  Gets the current search path. See Gtk.Icon_Theme.Set_Search_Path.
   --  Since: gtk+ 2.4

   procedure Set_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : GNAT.Strings.String_List);
   --  Sets the search path for the icon theme object. When looking for an
   --  icon theme, GTK+ will search for a subdirectory of one or more of the
   --  directories in Path with the same name as the icon theme containing an
   --  index.theme file. (Themes from multiple of the path elements are
   --  combined to allow themes to be extended by adding icons in the user's
   --  home directory.)
   --  In addition if an icon found isn't found either in the current icon
   --  theme or the default icon theme, and an image file with the right name
   --  is found directly in one of the elements of Path, then that image will
   --  be used for the icon name. (This is legacy feature, and new icons should
   --  be put into the fallback icon theme, which is called hicolor, rather
   --  than directly on the icon path.)
   --  Since: gtk+ 2.4
   --  "path": array of directories that are searched for icon themes

   function Has_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String) return Boolean;
   --  Checks whether an icon theme includes an icon for a particular name.
   --  Since: gtk+ 2.4
   --  "icon_name": the name of an icon

   function List_Contexts
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return Gtk.Enums.String_List.Glist;
   --  Gets the list of contexts available within the current hierarchy of
   --  icon themes. See Gtk.Icon_Theme.List_Icons for details about contexts.
   --  Since: gtk+ 2.12

   function List_Icons
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Context    : UTF8_String := "") return Gtk.Enums.String_List.Glist;
   --  Lists the icons in the current icon theme. Only a subset of the icons
   --  can be listed by providing a context string. The set of values for the
   --  context string is system dependent, but will typically include such
   --  values as "Applications" and "MimeTypes". Contexts are explained in the
   --  [Icon Theme
   --  Specification](http://www.freedesktop.org/wiki/Specifications/icon-theme-spec).
   --  The standard contexts are listed in the [Icon Naming
   --  Specification](http://www.freedesktop.org/wiki/Specifications/icon-naming-spec).
   --  Also see Gtk.Icon_Theme.List_Contexts.
   --  Since: gtk+ 2.4
   --  "context": a string identifying a particular type of icon, or null to
   --  list all icons.

   function Load_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags;
       Error      : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Looks up an icon in an icon theme, scales it to the given size and
   --  renders it into a pixbuf. This is a convenience function; if more
   --  details about the icon are needed, use Gtk.Icon_Theme.Lookup_Icon
   --  followed by Gtk.Icon_Theme.Load_Icon.
   --  Note that you probably want to listen for icon theme changes and update
   --  the icon. This is usually done by connecting to the GtkWidget::style-set
   --  signal. If for some reason you do not want to update the icon when the
   --  icon theme changes, you should consider using gdk_pixbuf_copy to make a
   --  private copy of the pixbuf returned by this function. Otherwise GTK+ may
   --  need to keep the old icon theme loaded, which would be a waste of
   --  memory.
   --  Since: gtk+ 2.4
   --  "icon_name": the name of the icon to lookup
   --  "size": the desired icon size. The resulting icon may not be exactly
   --  this size; see Gtk.Icon_Theme.Load_Icon.
   --  "flags": flags modifying the behavior of the icon lookup

   function Load_Icon
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Renders an icon previously looked up in an icon theme using
   --  Gtk.Icon_Theme.Lookup_Icon; the size will be based on the size passed to
   --  Gtk.Icon_Theme.Lookup_Icon. Note that the resulting pixbuf may not be
   --  exactly this size; an icon theme may have icons that differ slightly
   --  from their nominal sizes, and in addition GTK+ will avoid scaling icons
   --  that it considers sufficiently close to the requested size or for which
   --  the source image would have to be scaled up too far. (This maintains
   --  sharpness.). This behaviour can be changed by passing the
   --  Gtk.Icon_Theme.Icon_Lookup_Force_Size flag when obtaining the
   --  Gtk.Icon_Theme.Gtk_Icon_Info. If this flag has been specified, the
   --  pixbuf returned by this function will be scaled to the exact size.
   --  Since: gtk+ 2.4

   function Load_Icon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags;
       Error      : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Looks up an icon in an icon theme for a particular window scale, scales
   --  it to the given size and renders it into a pixbuf. This is a convenience
   --  function; if more details about the icon are needed, use
   --  Gtk.Icon_Theme.Lookup_Icon followed by Gtk.Icon_Theme.Load_Icon.
   --  Note that you probably want to listen for icon theme changes and update
   --  the icon. This is usually done by connecting to the GtkWidget::style-set
   --  signal. If for some reason you do not want to update the icon when the
   --  icon theme changes, you should consider using gdk_pixbuf_copy to make a
   --  private copy of the pixbuf returned by this function. Otherwise GTK+ may
   --  need to keep the old icon theme loaded, which would be a waste of
   --  memory.
   --  Since: gtk+ 3.10
   --  "icon_name": the name of the icon to lookup
   --  "size": the desired icon size. The resulting icon may not be exactly
   --  this size; see Gtk.Icon_Theme.Load_Icon.
   --  "scale": desired scale
   --  "flags": flags modifying the behavior of the icon lookup

   function Load_Surface
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       For_Window : Gdk.Gdk_Window;
       Flags      : Gtk_Icon_Lookup_Flags;
       Error      : access Glib.Error.GError) return Cairo.Cairo_Surface;
   --  Looks up an icon in an icon theme for a particular window scale, scales
   --  it to the given size and renders it into a cairo surface. This is a
   --  convenience function; if more details about the icon are needed, use
   --  Gtk.Icon_Theme.Lookup_Icon followed by Gtk.Icon_Theme.Load_Surface.
   --  Note that you probably want to listen for icon theme changes and update
   --  the icon. This is usually done by connecting to the GtkWidget::style-set
   --  signal.
   --  Since: gtk+ 3.10
   --  "icon_name": the name of the icon to lookup
   --  "size": the desired icon size. The resulting icon may not be exactly
   --  this size; see Gtk.Icon_Theme.Load_Icon.
   --  "scale": desired scale
   --  "for_window": Gdk.Gdk_Window to optimize drawing for, or null
   --  "flags": flags modifying the behavior of the icon lookup

   function Load_Surface
      (Icon_Info  : not null access Gtk_Icon_Info_Record;
       For_Window : Gdk.Gdk_Window) return Cairo.Cairo_Surface;
   --  Renders an icon previously looked up in an icon theme using
   --  Gtk.Icon_Theme.Lookup_Icon; the size will be based on the size passed to
   --  Gtk.Icon_Theme.Lookup_Icon. Note that the resulting surface may not be
   --  exactly this size; an icon theme may have icons that differ slightly
   --  from their nominal sizes, and in addition GTK+ will avoid scaling icons
   --  that it considers sufficiently close to the requested size or for which
   --  the source image would have to be scaled up too far. (This maintains
   --  sharpness.). This behaviour can be changed by passing the
   --  Gtk.Icon_Theme.Icon_Lookup_Force_Size flag when obtaining the
   --  Gtk.Icon_Theme.Gtk_Icon_Info. If this flag has been specified, the
   --  pixbuf returned by this function will be scaled to the exact size.
   --  Since: gtk+ 3.10
   --  "for_window": Gdk.Gdk_Window to optimize drawing for, or null

   function Lookup_By_Gicon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon       : Glib.G_Icon.G_Icon;
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info;
   --  Looks up an icon and returns a Gtk.Icon_Theme.Gtk_Icon_Info containing
   --  information such as the filename of the icon. The icon can then be
   --  rendered into a pixbuf using Gtk.Icon_Theme.Load_Icon.
   --  When rendering on displays with high pixel densities you should not use
   --  a Size multiplied by the scaling factor returned by functions like
   --  Gdk.Window.Get_Scale_Factor. Instead, you should use
   --  Gtk.Icon_Theme.Lookup_By_Gicon_For_Scale, as the assets loaded for a
   --  given scaling factor may be different.
   --  Since: gtk+ 2.14
   --  "icon": the Glib.G_Icon.G_Icon to look up
   --  "size": desired icon size
   --  "flags": flags modifying the behavior of the icon lookup

   function Lookup_By_Gicon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon       : Glib.G_Icon.G_Icon;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info;
   --  Looks up an icon and returns a Gtk.Icon_Theme.Gtk_Icon_Info containing
   --  information such as the filename of the icon. The icon can then be
   --  rendered into a pixbuf using Gtk.Icon_Theme.Load_Icon.
   --  Since: gtk+ 3.10
   --  "icon": the Glib.G_Icon.G_Icon to look up
   --  "size": desired icon size
   --  "scale": the desired scale
   --  "flags": flags modifying the behavior of the icon lookup

   function Lookup_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info;
   --  Looks up a named icon and returns a Gtk.Icon_Theme.Gtk_Icon_Info
   --  containing information such as the filename of the icon. The icon can
   --  then be rendered into a pixbuf using Gtk.Icon_Theme.Load_Icon.
   --  (gtk_icon_theme_load_icon combines these two steps if all you need is
   --  the pixbuf.)
   --  When rendering on displays with high pixel densities you should not use
   --  a Size multiplied by the scaling factor returned by functions like
   --  Gdk.Window.Get_Scale_Factor. Instead, you should use
   --  Gtk.Icon_Theme.Lookup_Icon_For_Scale, as the assets loaded for a given
   --  scaling factor may be different.
   --  Since: gtk+ 2.4
   --  "icon_name": the name of the icon to lookup
   --  "size": desired icon size
   --  "flags": flags modifying the behavior of the icon lookup

   function Lookup_Icon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info;
   --  Looks up a named icon for a particular window scale and returns a
   --  Gtk.Icon_Theme.Gtk_Icon_Info containing information such as the filename
   --  of the icon. The icon can then be rendered into a pixbuf using
   --  Gtk.Icon_Theme.Load_Icon. (gtk_icon_theme_load_icon combines these two
   --  steps if all you need is the pixbuf.)
   --  Since: gtk+ 3.10
   --  "icon_name": the name of the icon to lookup
   --  "size": desired icon size
   --  "scale": the desired scale
   --  "flags": flags modifying the behavior of the icon lookup

   procedure Prepend_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String);
   --  Prepends a directory to the search path. See
   --  Gtk.Icon_Theme.Set_Search_Path.
   --  Since: gtk+ 2.4
   --  "path": directory name to prepend to the icon path

   function Rescan_If_Needed
      (Icon_Theme : not null access Gtk_Icon_Theme_Record) return Boolean;
   --  Checks to see if the icon theme has changed; if it has, any currently
   --  cached information is discarded and will be reloaded next time
   --  Icon_Theme is accessed.
   --  Since: gtk+ 2.4

   procedure Set_Custom_Theme
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Theme_Name : UTF8_String := "");
   --  Sets the name of the icon theme that the Gtk.Icon_Theme.Gtk_Icon_Theme
   --  object uses overriding system configuration. This function cannot be
   --  called on the icon theme objects returned from
   --  Gtk.Icon_Theme.Get_Default and Gtk.Icon_Theme.Get_For_Screen.
   --  Since: gtk+ 2.4
   --  "theme_name": name of icon theme to use instead of configured theme, or
   --  null to unset a previously set custom theme

   procedure Set_Screen
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Screen     : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Sets the screen for an icon theme; the screen is used to track the
   --  user's currently configured icon theme, which might be different for
   --  different screens.
   --  Since: gtk+ 2.4
   --  "screen": a Gdk.Screen.Gdk_Screen

   function Copy
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gtk_Icon_Info;
   pragma Obsolescent (Copy);
   --  Make a copy of a Gtk.Icon_Theme.Gtk_Icon_Info.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.8, 1

   procedure Free (Icon_Info : not null access Gtk_Icon_Info_Record);
   pragma Obsolescent (Free);
   --  Free a Gtk.Icon_Theme.Gtk_Icon_Info and associated information
   --  Since: gtk+ 2.4
   --  Deprecated since 3.8, 1

   function Get_Attach_Points
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gdk.Types.Gdk_Points_Array;
   pragma Obsolescent (Get_Attach_Points);
   --  This function is deprecated and always returns False.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1

   function Get_Base_Scale
      (Icon_Info : not null access Gtk_Icon_Info_Record) return Glib.Gint;
   --  Gets the base scale for the icon. The base scale is a scale for the
   --  icon that was specified by the icon theme creator. For instance an icon
   --  drawn for a high-dpi screen with window scale 2 for a base size of 32
   --  will be 64 pixels tall and have a base scale of 2.
   --  Since: gtk+ 3.10

   function Get_Base_Size
      (Icon_Info : not null access Gtk_Icon_Info_Record) return Glib.Gint;
   --  Gets the base size for the icon. The base size is a size for the icon
   --  that was specified by the icon theme creator. This may be different than
   --  the actual size of image; an example of this is small emblem icons that
   --  can be attached to a larger icon. These icons will be given the same
   --  base size as the larger icons to which they are attached.
   --  Note that for scaled icons the base size does not include the base
   --  scale.
   --  Since: gtk+ 2.4

   function Get_Builtin_Pixbuf
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Get_Builtin_Pixbuf);
   --  Gets the built-in image for this icon, if any. To allow GTK+ to use
   --  built in icon images, you must pass the
   --  Gtk.Icon_Theme.Icon_Lookup_Use_Builtin to Gtk.Icon_Theme.Lookup_Icon.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1

   function Get_Display_Name
      (Icon_Info : not null access Gtk_Icon_Info_Record) return UTF8_String;
   pragma Obsolescent (Get_Display_Name);
   --  This function is deprecated and always returns null.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1

   procedure Get_Embedded_Rect
      (Icon_Info              : not null access Gtk_Icon_Info_Record;
       Rectangle              : out Gdk.Rectangle.Gdk_Rectangle;
       Has_Embedded_Rectangle : out Boolean);
   pragma Obsolescent (Get_Embedded_Rect);
   --  This function is deprecated and always returns False.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1
   --  "rectangle": Gdk.Rectangle.Gdk_Rectangle in which to store embedded
   --  rectangle coordinates; coordinates are only stored when this function
   --  returns True.

   function Get_Filename
      (Icon_Info : not null access Gtk_Icon_Info_Record) return UTF8_String;
   --  Gets the filename for the icon. If the
   --  Gtk.Icon_Theme.Icon_Lookup_Use_Builtin flag was passed to
   --  Gtk.Icon_Theme.Lookup_Icon, there may be no filename if a builtin icon
   --  is returned; in this case, you should use
   --  Gtk.Icon_Theme.Get_Builtin_Pixbuf.
   --  Since: gtk+ 2.4

   function Is_Symbolic
      (Icon_Info : not null access Gtk_Icon_Info_Record) return Boolean;
   --  Checks if the icon is symbolic or not. This currently uses only the
   --  file name and not the file contents for determining this. This behaviour
   --  may change in the future.
   --  Since: gtk+ 3.12

   function Load_Symbolic
      (Icon_Info     : not null access Gtk_Icon_Info_Record;
       Fg            : Gdk.RGBA.Gdk_RGBA;
       Success_Color : Gdk.RGBA.Gdk_RGBA;
       Warning_Color : Gdk.RGBA.Gdk_RGBA;
       Error_Color   : Gdk.RGBA.Gdk_RGBA;
       Was_Symbolic  : access Boolean;
       Error         : access Glib.Error.GError)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Loads an icon, modifying it to match the system colours for the
   --  foreground, success, warning and error colors provided. If the icon is
   --  not a symbolic one, the function will return the result from
   --  Gtk.Icon_Theme.Load_Icon.
   --  This allows loading symbolic icons that will match the system theme.
   --  Unless you are implementing a widget, you will want to use
   --  g_themed_icon_new_with_default_fallbacks to load the icon.
   --  As implementation details, the icon loaded needs to be of SVG type,
   --  contain the "symbolic" term as the last component of the icon name, and
   --  use the "fg", "success", "warning" and "error" CSS styles in the SVG
   --  file itself.
   --  See the [Symbolic Icons
   --  Specification](http://www.freedesktop.org/wiki/SymbolicIcons) for more
   --  information about symbolic icons.
   --  Since: gtk+ 3.0
   --  "fg": a Gdk.RGBA.Gdk_RGBA representing the foreground color of the icon
   --  "success_color": a Gdk.RGBA.Gdk_RGBA representing the warning color of
   --  the icon or null to use the default color
   --  "warning_color": a Gdk.RGBA.Gdk_RGBA representing the warning color of
   --  the icon or null to use the default color
   --  "error_color": a Gdk.RGBA.Gdk_RGBA representing the error color of the
   --  icon or null to use the default color (allow-none)
   --  "was_symbolic": a Boolean, returns whether the loaded icon was a
   --  symbolic one and whether the Fg color was applied to it.

   function Load_Symbolic_For_Context
      (Icon_Info    : not null access Gtk_Icon_Info_Record;
       Context      : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Was_Symbolic : access Boolean;
       Error        : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Loads an icon, modifying it to match the system colors for the
   --  foreground, success, warning and error colors provided. If the icon is
   --  not a symbolic one, the function will return the result from
   --  Gtk.Icon_Theme.Load_Icon. This function uses the regular foreground
   --  color and the symbolic colors with the names "success_color",
   --  "warning_color" and "error_color" from the context.
   --  This allows loading symbolic icons that will match the system theme.
   --  See Gtk.Icon_Theme.Load_Symbolic for more details.
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "was_symbolic": a Boolean, returns whether the loaded icon was a
   --  symbolic one and whether the Fg color was applied to it.

   function Load_Symbolic_For_Style
      (Icon_Info    : not null access Gtk_Icon_Info_Record;
       Style        : not null access Gtk.Style.Gtk_Style_Record'Class;
       State        : Gtk.Enums.Gtk_State_Type;
       Was_Symbolic : access Boolean;
       Error        : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Load_Symbolic_For_Style);
   --  Loads an icon, modifying it to match the system colours for the
   --  foreground, success, warning and error colors provided. If the icon is
   --  not a symbolic one, the function will return the result from
   --  Gtk.Icon_Theme.Load_Icon.
   --  This allows loading symbolic icons that will match the system theme.
   --  See Gtk.Icon_Theme.Load_Symbolic for more details.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.0, 1
   --  "style": a Gtk.Style.Gtk_Style to take the colors from
   --  "state": the widget state to use for colors
   --  "was_symbolic": a Boolean, returns whether the loaded icon was a
   --  symbolic one and whether the Fg color was applied to it.

   procedure Set_Raw_Coordinates
      (Icon_Info       : not null access Gtk_Icon_Info_Record;
       Raw_Coordinates : Boolean);
   pragma Obsolescent (Set_Raw_Coordinates);
   --  Sets whether the coordinates returned by
   --  Gtk.Icon_Theme.Get_Embedded_Rect and Gtk.Icon_Theme.Get_Attach_Points
   --  should be returned in their original form as specified in the icon
   --  theme, instead of scaled appropriately for the pixbuf returned by
   --  Gtk.Icon_Theme.Load_Icon.
   --  Raw coordinates are somewhat strange; they are specified to be with
   --  respect to the unscaled pixmap for PNG and XPM icons, but for SVG icons,
   --  they are in a 1000x1000 coordinate space that is scaled to the final
   --  size of the icon. You can determine if the icon is an SVG icon by using
   --  Gtk.Icon_Theme.Get_Filename, and seeing if it is non-null and ends in
   --  ".svg".
   --  This function is provided primarily to allow compatibility wrappers for
   --  older API's, and is not expected to be useful for applications.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1
   --  "raw_coordinates": whether the coordinates of embedded rectangles and
   --  attached points should be returned in their original (unscaled) form.

   ---------------
   -- Functions --
   ---------------

   procedure Add_Builtin_Icon
      (Icon_Name : UTF8_String;
       Size      : Glib.Gint;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   pragma Obsolescent (Add_Builtin_Icon);
   --  Registers a built-in icon for icon theme lookups. The idea of built-in
   --  icons is to allow an application or library that uses themed icons to
   --  function requiring files to be present in the file system. For instance,
   --  the default images for all of GTK+'s stock icons are registered as
   --  built-icons.
   --  In general, if you use Gtk.Icon_Theme.Add_Builtin_Icon you should also
   --  install the icon in the icon theme, so that the icon is generally
   --  available.
   --  This function will generally be used with pixbufs loaded via
   --  gdk_pixbuf_new_from_inline.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1
   --  "icon_name": the name of the icon to register
   --  "size": the size in pixels at which to register the icon (different
   --  images can be registered for the same icon name at different sizes.)
   --  "pixbuf": Gdk.Pixbuf.Gdk_Pixbuf that contains the image to use for
   --  Icon_Name

   function Get_Default return Gtk_Icon_Theme;
   --  Gets the icon theme for the default screen. See
   --  Gtk.Icon_Theme.Get_For_Screen.
   --  Since: gtk+ 2.4

   function Get_For_Screen
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
       return Gtk_Icon_Theme;
   --  Gets the icon theme object associated with Screen; if this function has
   --  not previously been called for the given screen, a new icon theme object
   --  will be created and associated with the screen. Icon theme objects are
   --  fairly expensive to create, so using this function is usually a better
   --  choice than calling than Gtk.Icon_Theme.Gtk_New and setting the screen
   --  yourself; by using this function a single icon theme object will be
   --  shared between users.
   --  Since: gtk+ 2.4
   --  "screen": a Gdk.Screen.Gdk_Screen

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
   --  Emitted when the current icon theme is switched or GTK+ detects that a
   --  change has occurred in the contents of the current icon theme.

end Gtk.Icon_Theme;
