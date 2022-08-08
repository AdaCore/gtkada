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
--  GtkCssProvider is an object implementing the
--  Gtk.Style_Provider.Gtk_Style_Provider interface. It is able to parse
--  [CSS-like][css-overview] input in order to style widgets.
--
--  An application can make GTK+ parse a specific CSS style sheet by calling
--  gtk_css_provider_load_from_file or Gtk.Css_Provider.Load_From_Resource and
--  adding the provider with Gtk.Style_Context.Add_Provider or
--  Gtk.Style_Context.Add_Provider_For_Screen.
--
--  In addition, certain files will be read when GTK+ is initialized. First,
--  the file `$XDG_CONFIG_HOME/gtk-3.0/gtk.css` is loaded if it exists. Then,
--  GTK+ loads the first existing file among
--  `XDG_DATA_HOME/themes/THEME/gtk-VERSION/gtk.css`,
--  `$HOME/.themes/THEME/gtk-VERSION/gtk.css`,
--  `$XDG_DATA_DIRS/themes/THEME/gtk-VERSION/gtk.css` and
--  `DATADIR/share/themes/THEME/gtk-VERSION/gtk.css`, where `THEME` is the name
--  of the current theme (see the Gtk.Settings.Gtk_Settings:gtk-theme-name
--  setting), `DATADIR` is the prefix configured when GTK+ was compiled (unless
--  overridden by the `GTK_DATA_PREFIX` environment variable), and `VERSION` is
--  the GTK+ version number. If no file is found for the current version, GTK+
--  tries older versions all the way back to 3.0.
--
--  In the same way, GTK+ tries to load a gtk-keys.css file for the current
--  key theme, as defined by Gtk.Settings.Gtk_Settings:gtk-key-theme-name.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Error;         use Glib.Error;
with Glib.Object;        use Glib.Object;
with Glib.Types;         use Glib.Types;
with Glib.Values;        use Glib.Values;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Css_Provider is

   type Gtk_Css_Provider_Record is new GObject_Record with null record;
   type Gtk_Css_Provider is access all Gtk_Css_Provider_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Css_Provider);
   procedure Initialize
      (Self : not null access Gtk_Css_Provider_Record'Class);
   --  Returns a newly created Gtk.Css_Provider.Gtk_Css_Provider.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Css_Provider_New return Gtk_Css_Provider;
   --  Returns a newly created Gtk.Css_Provider.Gtk_Css_Provider.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_css_provider_get_type");

   -------------
   -- Methods --
   -------------

   function Load_From_Data
      (Self  : not null access Gtk_Css_Provider_Record;
       Data  : UTF8_String;
       Error : access Glib.Error.GError) return Boolean;
   --  Loads Data into Css_Provider, and by doing so clears any previously
   --  loaded information.
   --  "data": CSS data loaded in memory

   function Load_From_Path
      (Self  : not null access Gtk_Css_Provider_Record;
       Path  : UTF8_String;
       Error : access Glib.Error.GError) return Boolean;
   --  Loads the data contained in Path into Css_Provider, making it clear any
   --  previously loaded information.
   --  "path": the path of a filename to load, in the GLib filename encoding

   procedure Load_From_Resource
      (Self          : not null access Gtk_Css_Provider_Record;
       Resource_Path : UTF8_String);
   --  Loads the data contained in the resource at Resource_Path into the
   --  Gtk.Css_Provider.Gtk_Css_Provider, clearing any previously loaded
   --  information.
   --  To track errors while loading CSS, connect to the
   --  Gtk.Css_Provider.Gtk_Css_Provider::parsing-error signal.
   --  Since: gtk+ 3.16
   --  "resource_path": a Gresource.Gresource resource path

   function To_String
      (Self : not null access Gtk_Css_Provider_Record) return UTF8_String;
   --  Converts the Provider into a string representation in CSS format.
   --  Using Gtk.Css_Provider.Load_From_Data with the return value from this
   --  function on a new provider created with Gtk.Css_Provider.Gtk_New will
   --  basically create a duplicate of this Provider.
   --  Since: gtk+ 3.2

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Get_Style_Property
      (Self  : not null access Gtk_Css_Provider_Record;
       Path  : Gtk.Widget.Gtk_Widget_Path;
       State : Gtk.Enums.Gtk_State_Flags;
       Pspec : in out Glib.Param_Spec;
       Value : out Glib.Values.GValue;
       Found : out Boolean);

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gtk_Css_Provider;
   pragma Obsolescent (Get_Default);
   --  Returns the provider containing the style settings used as a fallback
   --  for all widgets.
   --  Deprecated since 3.24, 1

   function Get_Named
      (Name    : UTF8_String;
       Variant : UTF8_String := "") return Gtk_Css_Provider;
   --  Loads a theme from the usual theme paths
   --  "name": A theme name
   --  "variant": variant to load, for example, "dark", or null for the
   --  default

   -------------
   -- Signals --
   -------------

   Signal_Parsing_Error : constant Glib.Signal_Name := "parsing-error";
   --  Signals that a parsing error occurred. the Path, Line and Position
   --  describe the actual location of the error as accurately as possible.
   --
   --  Parsing errors are never fatal, so the parsing will resume after the
   --  error. Errors may however cause parts of the given data or even all of
   --  it to not be parsed at all. So it is a useful idea to check that the
   --  parsing succeeds by connecting to this signal.
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
   --    --  "section": section the error happened in
   --    --  "error": The parsing error

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "StyleProvider"

   package Implements_Gtk_Style_Provider is new Glib.Types.Implements
     (Gtk.Style_Provider.Gtk_Style_Provider, Gtk_Css_Provider_Record, Gtk_Css_Provider);
   function "+"
     (Widget : access Gtk_Css_Provider_Record'Class)
   return Gtk.Style_Provider.Gtk_Style_Provider
   renames Implements_Gtk_Style_Provider.To_Interface;
   function "-"
     (Interf : Gtk.Style_Provider.Gtk_Style_Provider)
   return Gtk_Css_Provider
   renames Implements_Gtk_Style_Provider.To_Object;

end Gtk.Css_Provider;
