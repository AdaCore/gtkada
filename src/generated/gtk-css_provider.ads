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

--  A style provider for CSS.
--
--  It is able to parse CSS-like input in order to style widgets.
--
--  An application can make GTK parse a specific CSS style sheet by calling
--  [methodGtk.CssProvider.load_from_file] or
--  [methodGtk.CssProvider.load_from_resource] and adding the provider with
--  [methodGtk.StyleContext.add_provider] or
--  [funcGtk.StyleContext.add_provider_for_display].
--
--  In addition, certain files will be read when GTK is initialized. First,
--  the file `$XDG_CONFIG_HOME/gtk-4.0/gtk.css` is loaded if it exists. Then,
--  GTK loads the first existing file among
--  `XDG_DATA_HOME/themes/THEME/gtk-VERSION/gtk-VARIANT.css`,
--  `$HOME/.themes/THEME/gtk-VERSION/gtk-VARIANT.css`,
--  `$XDG_DATA_DIRS/themes/THEME/gtk-VERSION/gtk-VARIANT.css` and
--  `DATADIR/share/themes/THEME/gtk-VERSION/gtk-VARIANT.css`, where `THEME` is
--  the name of the current theme (see the
--  [propertyGtk.Settings:gtk-theme-name] setting), `VARIANT` is the variant to
--  load (see the [propertyGtk.Settings:gtk-application-prefer-dark-theme]
--  setting), `DATADIR` is the prefix configured when GTK was compiled (unless
--  overridden by the `GTK_DATA_PREFIX` environment variable), and `VERSION` is
--  the GTK version number. If no file is found for the current version, GTK
--  tries older versions all the way back to 4.0.
--
--  To track errors while loading CSS, connect to the
--  [signalGtk.CssProvider::parsing-error] signal.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Bytes;  use Glib.Bytes;
with Glib.GFile;  use Glib.GFile;
with Glib.Object; use Glib.Object;
with Gtk.Enums;   use Gtk.Enums;

package Gtk.Css_Provider is

   type Gtk_Css_Provider_Record is new GObject_Record with null record;
   type Gtk_Css_Provider is access all Gtk_Css_Provider_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Css_Provider);
   procedure Initialize
      (Self : not null access Gtk_Css_Provider_Record'Class);
   --  Returns a newly created `GtkCssProvider`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Css_Provider_New return Gtk_Css_Provider;
   --  Returns a newly created `GtkCssProvider`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_css_provider_get_type");

   -------------
   -- Methods --
   -------------

   procedure Load_From_Bytes
      (Self : not null access Gtk_Css_Provider_Record;
       Data : Glib.Bytes.Gbytes);
   --  Loads Data into Css_Provider.
   --  This clears any previously loaded information.
   --  Since: gtk+ 4.12
   --  @param Data `GBytes` containing the data to load

   procedure Load_From_Data
      (Self : not null access Gtk_Css_Provider_Record;
       Data : UTF8_String);
   pragma Obsolescent (Load_From_Data);
   --  Loads Data into Css_Provider.
   --  This clears any previously loaded information.
   --  Deprecated since 4.12, 1
   --  @param Data CSS data to be parsed

   procedure Load_From_File
      (Self : not null access Gtk_Css_Provider_Record;
       File : Glib.GFile.Gfile);
   --  Loads the data contained in File into Css_Provider.
   --  This clears any previously loaded information.
   --  @param File `GFile` pointing to a file to load

   procedure Load_From_Path
      (Self : not null access Gtk_Css_Provider_Record;
       Path : UTF8_String);
   --  Loads the data contained in Path into Css_Provider.
   --  This clears any previously loaded information.
   --  @param Path the path of a filename to load, in the GLib filename
   --  encoding

   procedure Load_From_Resource
      (Self          : not null access Gtk_Css_Provider_Record;
       Resource_Path : UTF8_String);
   --  Loads the data contained in the resource at Resource_Path into the
   --  Css_Provider.
   --  This clears any previously loaded information.
   --  @param Resource_Path a `GResource` resource path

   procedure Load_From_String
      (Self   : not null access Gtk_Css_Provider_Record;
       String : UTF8_String);
   --  Loads String into Css_Provider.
   --  This clears any previously loaded information.
   --  Since: gtk+ 4.12
   --  @param String the CSS to load

   procedure Load_Named
      (Self    : not null access Gtk_Css_Provider_Record;
       Name    : UTF8_String;
       Variant : UTF8_String := "");
   pragma Obsolescent (Load_Named);
   --  Loads a theme from the usual theme paths.
   --  The actual process of finding the theme might change between releases,
   --  but it is guaranteed that this function uses the same mechanism to load
   --  the theme that GTK uses for loading its own theme.
   --  Deprecated since 4.20, 1
   --  @param Name A theme name
   --  @param Variant variant to load, for example, "dark", or null for the
   --  default

   function To_String
      (Self : not null access Gtk_Css_Provider_Record) return UTF8_String;
   --  Converts the Provider into a string representation in CSS format.
   --  Using [methodGtk.CssProvider.load_from_string] with the return value
   --  from this function on a new provider created with
   --  [ctorGtk.CssProvider.new] will basically create a duplicate of this
   --  Provider.
   --  @return a new string representing the Provider.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Prefers_Color_Scheme_Property : constant Gtk.Enums.Property_Gtk_Interface_Color_Scheme;
   --  Define the color scheme used for rendering the user interface.
   --
   --  The UI can be set to either [enumGtk.InterfaceColorScheme.LIGHT], or
   --  [enumGtk.InterfaceColorScheme.DARK] mode. Other values will be
   --  interpreted the same as [enumGtk.InterfaceColorScheme.LIGHT].
   --
   --  This setting is be available for media queries in CSS:
   --
   --  ```css Media (prefers-color-scheme: dark) { // some dark mode styling }
   --  ```
   --
   --  Changing this setting will reload the style sheet.

   Prefers_Contrast_Property : constant Gtk.Enums.Property_Gtk_Interface_Contrast;
   --  Define the contrast mode to use for the user interface.
   --
   --  When set to [enumGtk.InterfaceContrast.MORE] or
   --  [enumGtk.InterfaceContrast.LESS], the UI is rendered in high or low
   --  contrast.
   --
   --  When set to [enumGtk.InterfaceContrast.NO_PREFERENCE] (the default),
   --  the user interface will be rendered in default mode.
   --
   --  This setting is be available for media queries in CSS:
   --
   --  ```css Media (prefers-contrast: more) { // some style with high
   --  contrast } ```
   --
   --  Changing this setting will reload the style sheet.

   Prefers_Reduced_Motion_Property : constant Gtk.Enums.Property_Gtk_Reduced_Motion;
   --  Define the type of reduced motion to use for the user interface.
   --
   --  When set to [enumGtk.ReducedMotion.REDUCE] the UI is rendered in with
   --  reduced motion animations.
   --
   --  When set to [enumGtk.ReducedMotion.NO_PREFERENCE] (the default), the
   --  user interface will be rendered in default mode.
   --
   --  This setting is be available for media queries in CSS:
   --
   --  ```css Media (prefers-reduced-motion: reduce) { // some style with
   --  reduced motion } ```
   --
   --  Changing this setting will reload the style sheet.

   -------------
   -- Signals --
   -------------

   Signal_Parsing_Error : constant Glib.Signal_Name := "parsing-error";
   --  Signals that a parsing error occurred.
   --
   --  The expected error values are in the [errorGtk.CssParserError] and
   --  [enumGtk.CssParserWarning] enumerations.
   --
   --  The Path, Line and Position describe the actual location of the error
   --  as accurately as possible.
   --
   --  Parsing errors are never fatal, so the parsing will resume after the
   --  error. Errors may however cause parts of the given data or even all of
   --  it to not be parsed at all. So it is a useful idea to check that the
   --  parsing succeeds by connecting to this signal.
   --
   --  Errors in the [enumGtk.CssParserWarning] enumeration should not be
   --  treated as fatal errors.
   --
   --  Note that this signal may be emitted at any time as the css provider
   --  may opt to defer parsing parts or all of the input to a later time than
   --  when a loading function was called.
   --    procedure Handler
   --       (Self    : access Gtk_Css_Provider_Record'Class;
   --        Section : Gtk.Css_Section.Gtk_Css_Section;
   --        Error   : GLib.Error)
   -- 
   --  Callback parameters:
   --    --  @param Section section the error happened in
   --    --  @param Error The parsing error

private
   Prefers_Reduced_Motion_Property : constant Gtk.Enums.Property_Gtk_Reduced_Motion :=
     Gtk.Enums.Build ("prefers-reduced-motion");
   Prefers_Contrast_Property : constant Gtk.Enums.Property_Gtk_Interface_Contrast :=
     Gtk.Enums.Build ("prefers-contrast");
   Prefers_Color_Scheme_Property : constant Gtk.Enums.Property_Gtk_Interface_Color_Scheme :=
     Gtk.Enums.Build ("prefers-color-scheme");
end Gtk.Css_Provider;
