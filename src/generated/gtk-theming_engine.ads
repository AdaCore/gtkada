------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  Gtk.Theming_Engine.Gtk_Theming_Engine is the object used for rendering
--  themed content in GTK+ widgets. Even though GTK+ has a default
--  implementation, it can be overridden in CSS files by enforcing a
--  Gtk.Theming_Engine.Gtk_Theming_Engine object to be loaded as a module.
--
--  In order to implement a theming engine, a
--  Gtk.Theming_Engine.Gtk_Theming_Engine subclass must be created, alongside
--  the CSS file that will reference it, the theming engine would be created as
--  an .so library, and installed in $(gtk-modules-dir)/theming-engines/.
--
--  Gtk.Theming_Engine.Gtk_Theming_Engine<!-- -->s have limited access to the
--  object they are rendering, the Gtk.Theming_Engine.Gtk_Theming_Engine API
--  has read-only accessors to the style information contained in the rendered
--  object's Gtk.Style_Context.Gtk_Style_Context.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;        use Gdk.RGBA;
with Gdk.Screen;      use Gdk.Screen;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Style;       use Gtk.Style;
with Gtk.Widget;      use Gtk.Widget;
with Pango.Font;      use Pango.Font;

package Gtk.Theming_Engine is

   type Gtk_Theming_Engine_Record is new GObject_Record with null record;
   type Gtk_Theming_Engine is access all Gtk_Theming_Engine_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_theming_engine_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Background_Color
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA);
   --  Gets the background color for a given state.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the color for
   --  "color": return value for the background color

   procedure Get_Border
      (Self   : not null access Gtk_Theming_Engine_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Border : out Gtk.Style.Gtk_Border);
   --  Gets the border for a given state as a Gtk.Style.Gtk_Border.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the border for
   --  "border": return value for the border settings

   procedure Get_Border_Color
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA);
   --  Gets the border color for a given state.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the color for
   --  "color": return value for the border color

   procedure Get_Color
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA);
   --  Gets the foreground color for a given state.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the color for
   --  "color": return value for the foreground color

   function Get_Direction
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Enums.Gtk_Text_Direction;
   pragma Obsolescent (Get_Direction);
   --  Returns the widget direction used for rendering.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, Use Gtk.Theming_Engine.Get_State and check for
   --  GTK_STATE_FLAG_DIR_LTR and GTK_STATE_FLAG_DIR_RTL instead.

   function Get_Font
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags)
       return Pango.Font.Pango_Font_Description;
   pragma Obsolescent (Get_Font);
   --  Returns the font description for a given state.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, Use gtk_theming_engine_get
   --  "state": state to retrieve the font for

   function Get_Junction_Sides
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Enums.Gtk_Junction_Sides;
   --  Returns the widget direction used for rendering.
   --  Since: gtk+ 3.0

   procedure Get_Margin
      (Self   : not null access Gtk_Theming_Engine_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Margin : out Gtk.Style.Gtk_Border);
   --  Gets the margin for a given state as a Gtk.Style.Gtk_Border.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the border for
   --  "margin": return value for the margin settings

   procedure Get_Padding
      (Self    : not null access Gtk_Theming_Engine_Record;
       State   : Gtk.Enums.Gtk_State_Flags;
       Padding : out Gtk.Style.Gtk_Border);
   --  Gets the padding for a given state as a Gtk.Style.Gtk_Border.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the padding for
   --  "padding": return value for the padding settings

   function Get_Path
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Widget.Gtk_Widget_Path;
   --  Returns the widget path used for style matching.
   --  Since: gtk+ 3.0

   procedure Get_Property
      (Self     : not null access Gtk_Theming_Engine_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : out Glib.Values.GValue);
   --  Gets a property value as retrieved from the style settings that apply
   --  to the currently rendered element.
   --  Since: gtk+ 3.0
   --  "property": the property name
   --  "state": state to retrieve the value for
   --  "value": return location for the property value, you must free this
   --  memory using g_value_unset once you are done with it.

   function Get_Screen
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gdk.Screen.Gdk_Screen;
   --  Returns the Gdk.Screen.Gdk_Screen to which Engine currently rendering
   --  to.

   function Get_State
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Enums.Gtk_State_Flags;
   --  returns the state used when rendering.
   --  Since: gtk+ 3.0

   procedure Get_Style_Property
      (Self          : not null access Gtk_Theming_Engine_Record;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Gets the value for a widget style property.
   --  Since: gtk+ 3.0
   --  "property_name": the name of the widget style property
   --  "value": Return location for the property value, free with
   --  g_value_unset after use.

   function Has_Class
      (Self        : not null access Gtk_Theming_Engine_Record;
       Style_Class : UTF8_String) return Boolean;
   --  Returns True if the currently rendered contents have defined the given
   --  class name.
   --  Since: gtk+ 3.0
   --  "style_class": class name to look up

   function Has_Region
      (Self         : not null access Gtk_Theming_Engine_Record;
       Style_Region : UTF8_String;
       Flags        : access Gtk.Enums.Gtk_Region_Flags) return Boolean;
   --  Returns True if the currently rendered contents have the region
   --  defined. If Flags_Return is not null, it is set to the flags affecting
   --  the region.
   --  Since: gtk+ 3.0
   --  "style_region": a region name
   --  "flags": return location for region flags

   function Lookup_Color
      (Self       : not null access Gtk_Theming_Engine_Record;
       Color_Name : UTF8_String;
       Color      : access Gdk.RGBA.Gdk_RGBA) return Boolean;
   --  Looks up and resolves a color name in the current style's color map.
   --  "color_name": color name to lookup
   --  "color": Return location for the looked up color

   function State_Is_Running
      (Self     : not null access Gtk_Theming_Engine_Record;
       State    : Gtk.Enums.Gtk_State_Type;
       Progress : access Gdouble) return Boolean;
   pragma Obsolescent (State_Is_Running);
   --  Returns True if there is a transition animation running for the current
   --  region (see Gtk.Style_Context.Push_Animatable_Region).
   --  If Progress is not null, the animation progress will be returned there,
   --  0.0 means the state is closest to being False, while 1.0 means it's
   --  closest to being True. This means transition animations will run from 0
   --  to 1 when State is being set to True and from 1 to 0 when it's being set
   --  to False.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, Always returns False
   --  "state": a widget state
   --  "progress": return location for the transition progress

   ---------------
   -- Functions --
   ---------------

   function Load (Name : UTF8_String) return Gtk_Theming_Engine;
   --  Loads and initializes a theming engine module from the standard
   --  directories.
   --  "name": Theme engine name to load

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Name_Property : constant Glib.Properties.Property_String;
   --  The theming engine name, this name will be used when registering custom
   --  properties, for a theming engine named "Clearlooks" registering a
   --  "glossy" custom property, it could be referenced in the CSS file as
   --
   --    -Clearlooks-glossy: true;

private
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
end Gtk.Theming_Engine;
