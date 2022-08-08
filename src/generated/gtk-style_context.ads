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
--  Gtk.Style_Context.Gtk_Style_Context is an object that stores styling
--  information affecting a widget defined by Gtk.Widget.Gtk_Widget_Path.
--
--  In order to construct the final style information,
--  Gtk.Style_Context.Gtk_Style_Context queries information from all attached
--  Gtk_Style_Providers. Style providers can be either attached explicitly to
--  the context through Gtk.Style_Context.Add_Provider, or to the screen
--  through Gtk.Style_Context.Add_Provider_For_Screen. The resulting style is a
--  combination of all providers' information in priority order.
--
--  For GTK+ widgets, any Gtk.Style_Context.Gtk_Style_Context returned by
--  gtk_widget_get_style_context will already have a
--  Gtk.Widget.Gtk_Widget_Path, a Gdk.Screen.Gdk_Screen and RTL/LTR information
--  set. The style context will also be updated automatically if any of these
--  settings change on the widget.
--
--  If you are using the theming layer standalone, you will need to set a
--  widget path and a screen yourself to the created style context through
--  Gtk.Style_Context.Set_Path and possibly Gtk.Style_Context.Set_Screen. See
--  the "Foreign drawing" example in gtk3-demo.
--
--  # Style Classes # {gtkstylecontext-classes}
--
--  Widgets can add style classes to their context, which can be used to
--  associate different styles by class. The documentation for individual
--  widgets lists which style classes it uses itself, and which style classes
--  may be added by applications to affect their appearance.
--
--  GTK+ defines macros for a number of style classes.
--
--  # Style Regions
--
--  Widgets can also add regions with flags to their context. This feature is
--  deprecated and will be removed in a future GTK+ update. Please use style
--  classes instead.
--
--  GTK+ defines macros for a number of style regions.
--
--  # Custom styling in UI libraries and applications
--
--  If you are developing a library with custom Gtk_Widgets that render
--  differently than standard components, you may need to add a
--  Gtk.Style_Provider.Gtk_Style_Provider yourself with the
--  GTK_STYLE_PROVIDER_PRIORITY_FALLBACK priority, either a
--  Gtk.Css_Provider.Gtk_Css_Provider or a custom object implementing the
--  Gtk.Style_Provider.Gtk_Style_Provider interface. This way themes may still
--  attempt to style your UI elements in a different way if needed so.
--
--  If you are using custom styling on an applications, you probably want then
--  to make your style information prevail to the theme's, so you must use a
--  Gtk.Style_Provider.Gtk_Style_Provider with the
--  GTK_STYLE_PROVIDER_PRIORITY_APPLICATION priority, keep in mind that the
--  user settings in `XDG_CONFIG_HOME/gtk-3.0/gtk.css` will still take
--  precedence over your changes, as it uses the
--  GTK_STYLE_PROVIDER_PRIORITY_USER priority.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Gdk;                     use Gdk;
with Gdk.Frame_Clock;         use Gdk.Frame_Clock;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Screen;              use Gdk.Screen;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Values;             use Glib.Values;
with Gtk.Css_Section;         use Gtk.Css_Section;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Style;               use Gtk.Style;
with Gtk.Style_Provider;      use Gtk.Style_Provider;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Font;              use Pango.Font;
with Pango.Layout;            use Pango.Layout;

package Gtk.Style_Context is

   type Gtk_Style_Context_Record is new GObject_Record with null record;
   type Gtk_Style_Context is access all Gtk_Style_Context_Record'Class;

   type Gtk_Style_Context_Print_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Style_Context_Print_Flags);
   --  Flags that modify the behavior of Gtk.Style_Context.To_String. New
   --  values may be added to this enumeration.

   Style_Context_Print_None : constant Gtk_Style_Context_Print_Flags := 0;
   Style_Context_Print_Recurse : constant Gtk_Style_Context_Print_Flags := 1;
   Style_Context_Print_Show_Style : constant Gtk_Style_Context_Print_Flags := 2;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Style_Context_Print_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Style_Context_Print_Flags);
   type Property_Gtk_Style_Context_Print_Flags is new Gtk_Style_Context_Print_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Style_Context);
   procedure Initialize
      (Self : not null access Gtk_Style_Context_Record'Class);
   --  Creates a standalone Gtk.Style_Context.Gtk_Style_Context, this style
   --  context won't be attached to any widget, so you may want to call
   --  Gtk.Style_Context.Set_Path yourself.
   --  This function is only useful when using the theming layer separated
   --  from GTK+, if you are using Gtk.Style_Context.Gtk_Style_Context to theme
   --  Gtk_Widgets, use gtk_widget_get_style_context in order to get a style
   --  context ready to theme the widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Style_Context_New return Gtk_Style_Context;
   --  Creates a standalone Gtk.Style_Context.Gtk_Style_Context, this style
   --  context won't be attached to any widget, so you may want to call
   --  Gtk.Style_Context.Set_Path yourself.
   --  This function is only useful when using the theming layer separated
   --  from GTK+, if you are using Gtk.Style_Context.Gtk_Style_Context to theme
   --  Gtk_Widgets, use gtk_widget_get_style_context in order to get a style
   --  context ready to theme the widget.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_context_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String);
   --  Adds a style class to Context, so posterior calls to
   --  gtk_style_context_get or any of the gtk_render_* functions will make use
   --  of this new class for styling.
   --  In the CSS file format, a Gtk.GEntry.Gtk_Entry defining a "search"
   --  class, would be matched by:
   --  |[ <!-- language="CSS" --> entry.search { ... } ]|
   --  While any widget defining a "search" class would be matched by: |[ <!--
   --  language="CSS" --> .search { ... } ]|
   --  Since: gtk+ 3.0
   --  "class_name": class name to use in styling

   procedure Add_Provider
      (Self     : not null access Gtk_Style_Context_Record;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint);
   --  Adds a style provider to Context, to be used in style construction.
   --  Note that a style provider added by this function only affects the style
   --  of the widget to which Context belongs. If you want to affect the style
   --  of all widgets, use Gtk.Style_Context.Add_Provider_For_Screen.
   --  Note: If both priorities are the same, a
   --  Gtk.Style_Provider.Gtk_Style_Provider added through this function takes
   --  precedence over another added through
   --  Gtk.Style_Context.Add_Provider_For_Screen.
   --  Since: gtk+ 3.0
   --  "provider": a Gtk.Style_Provider.Gtk_Style_Provider
   --  "priority": the priority of the style provider. The lower it is, the
   --  earlier it will be used in the style construction. Typically this will
   --  be in the range between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and
   --  GTK_STYLE_PROVIDER_PRIORITY_USER

   procedure Add_Region
      (Self        : not null access Gtk_Style_Context_Record;
       Region_Name : UTF8_String;
       Flags       : Gtk.Enums.Gtk_Region_Flags);
   pragma Obsolescent (Add_Region);
   --  Adds a region to Context, so posterior calls to gtk_style_context_get
   --  or any of the gtk_render_* functions will make use of this new region
   --  for styling.
   --  In the CSS file format, a Gtk.Tree_View.Gtk_Tree_View defining a "row"
   --  region, would be matched by:
   --  |[ <!-- language="CSS" --> treeview row { ... } ]|
   --  Pseudo-classes are used for matching Flags, so the two following rules:
   --  |[ <!-- language="CSS" --> treeview row:nth-child(even) { ... } treeview
   --  row:nth-child(odd) { ... } ]|
   --  would apply to even and odd rows, respectively.
   --  Region names must only contain lowercase letters and "-", starting
   --  always with a lowercase letter.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1
   --  "region_name": region name to use in styling
   --  "flags": flags that apply to the region

   procedure Cancel_Animations
      (Self      : not null access Gtk_Style_Context_Record;
       Region_Id : System.Address);
   pragma Obsolescent (Cancel_Animations);
   --  Stops all running animations for Region_Id and all animatable regions
   --  underneath.
   --  A null Region_Id will stop all ongoing animations in Context, when
   --  dealing with a Gtk.Style_Context.Gtk_Style_Context obtained through
   --  gtk_widget_get_style_context, this is normally done for you in all
   --  circumstances you would expect all widget to be stopped, so this should
   --  be only used in complex widgets with different animatable regions.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, 1
   --  "region_id": animatable region to stop, or null. See
   --  Gtk.Style_Context.Push_Animatable_Region

   procedure Get_Background_Color
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Get_Background_Color);
   --  Gets the background color for a given state.
   --  This function is far less useful than it seems, and it should not be
   --  used in newly written code. CSS has no concept of "background color", as
   --  a background can be an image, or a gradient, or any other pattern
   --  including solid colors.
   --  The only reason why you would call
   --  Gtk.Style_Context.Get_Background_Color is to use the returned value to
   --  draw the background with it; the correct way to achieve this result is
   --  to use Gtk.Style_Context.Render_Background instead, along with CSS style
   --  classes to modify the color to be rendered.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "state": state to retrieve the color for
   --  "color": return value for the background color

   procedure Get_Border
      (Self   : not null access Gtk_Style_Context_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Border : out Gtk.Style.Gtk_Border);
   --  Gets the border for a given state as a Gtk.Style.Gtk_Border.
   --  See Gtk.Style_Context.Get_Property and GTK_STYLE_PROPERTY_BORDER_WIDTH
   --  for details.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the border for
   --  "border": return value for the border settings

   procedure Get_Border_Color
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Get_Border_Color);
   --  Gets the border color for a given state.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "state": state to retrieve the color for
   --  "color": return value for the border color

   procedure Get_Color
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA);
   --  Gets the foreground color for a given state.
   --  See Gtk.Style_Context.Get_Property and GTK_STYLE_PROPERTY_COLOR for
   --  details.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the color for
   --  "color": return value for the foreground color

   function Get_Direction
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_Text_Direction;
   pragma Obsolescent (Get_Direction);
   --  Returns the widget direction used for rendering.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1

   procedure Set_Direction
      (Self      : not null access Gtk_Style_Context_Record;
       Direction : Gtk.Enums.Gtk_Text_Direction);
   pragma Obsolescent (Set_Direction);
   --  Sets the reading direction for rendering purposes.
   --  If you are using a Gtk.Style_Context.Gtk_Style_Context returned from
   --  gtk_widget_get_style_context, you do not need to call this yourself.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1
   --  "direction": the new direction.

   function Get_Font
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags)
       return Pango.Font.Pango_Font_Description;
   pragma Obsolescent (Get_Font);
   --  Returns the font description for a given state. The returned object is
   --  const and will remain valid until the
   --  Gtk.Style_Context.Gtk_Style_Context::changed signal happens.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1
   --  "state": state to retrieve the font for

   function Get_Frame_Clock
      (Self : not null access Gtk_Style_Context_Record)
       return Gdk.Frame_Clock.Gdk_Frame_Clock;
   --  Returns the Gdk.Frame_Clock.Gdk_Frame_Clock to which Context is
   --  attached.
   --  Since: gtk+ 3.8

   procedure Set_Frame_Clock
      (Self        : not null access Gtk_Style_Context_Record;
       Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class);
   --  Attaches Context to the given frame clock.
   --  The frame clock is used for the timing of animations.
   --  If you are using a Gtk.Style_Context.Gtk_Style_Context returned from
   --  gtk_widget_get_style_context, you do not need to call this yourself.
   --  Since: gtk+ 3.8
   --  "frame_clock": a Gdk.Frame_Clock.Gdk_Frame_Clock

   function Get_Junction_Sides
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_Junction_Sides;
   --  Returns the sides where rendered elements connect visually with others.
   --  Since: gtk+ 3.0

   procedure Set_Junction_Sides
      (Self  : not null access Gtk_Style_Context_Record;
       Sides : Gtk.Enums.Gtk_Junction_Sides);
   --  Sets the sides where rendered elements (mostly through
   --  Gtk.Style_Context.Render_Frame) will visually connect with other visual
   --  elements.
   --  This is merely a hint that may or may not be honored by themes.
   --  Container widgets are expected to set junction hints as appropriate for
   --  their children, so it should not normally be necessary to call this
   --  function manually.
   --  Since: gtk+ 3.0
   --  "sides": sides where rendered elements are visually connected to other
   --  elements

   procedure Get_Margin
      (Self   : not null access Gtk_Style_Context_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Margin : out Gtk.Style.Gtk_Border);
   --  Gets the margin for a given state as a Gtk.Style.Gtk_Border. See
   --  gtk_style_property_get and GTK_STYLE_PROPERTY_MARGIN for details.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the border for
   --  "margin": return value for the margin settings

   procedure Get_Padding
      (Self    : not null access Gtk_Style_Context_Record;
       State   : Gtk.Enums.Gtk_State_Flags;
       Padding : out Gtk.Style.Gtk_Border);
   --  Gets the padding for a given state as a Gtk.Style.Gtk_Border. See
   --  gtk_style_context_get and GTK_STYLE_PROPERTY_PADDING for details.
   --  Since: gtk+ 3.0
   --  "state": state to retrieve the padding for
   --  "padding": return value for the padding settings

   function Get_Parent
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk_Style_Context;
   --  Gets the parent context set via Gtk.Style_Context.Set_Parent. See that
   --  function for details.
   --  Since: gtk+ 3.4

   procedure Set_Parent
      (Self   : not null access Gtk_Style_Context_Record;
       Parent : access Gtk_Style_Context_Record'Class);
   --  Sets the parent style context for Context. The parent style context is
   --  used to implement
   --  [inheritance](http://www.w3.org/TR/css3-cascade/inheritance) of
   --  properties.
   --  If you are using a Gtk.Style_Context.Gtk_Style_Context returned from
   --  gtk_widget_get_style_context, the parent will be set for you.
   --  Since: gtk+ 3.4
   --  "parent": the new parent or null

   function Get_Path
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Widget.Gtk_Widget_Path;
   --  Returns the widget path used for style matching.
   --  Since: gtk+ 3.0

   procedure Set_Path
      (Self : not null access Gtk_Style_Context_Record;
       Path : Gtk.Widget.Gtk_Widget_Path);
   --  Sets the Gtk.Widget.Gtk_Widget_Path used for style matching. As a
   --  consequence, the style will be regenerated to match the new given path.
   --  If you are using a Gtk.Style_Context.Gtk_Style_Context returned from
   --  gtk_widget_get_style_context, you do not need to call this yourself.
   --  Since: gtk+ 3.0
   --  "path": a Gtk.Widget.Gtk_Widget_Path

   procedure Get_Property
      (Self     : not null access Gtk_Style_Context_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : out Glib.Values.GValue);
   --  Gets a style property from Context for the given state.
   --  Note that not all CSS properties that are supported by GTK+ can be
   --  retrieved in this way, since they may not be representable as
   --  Glib.Values.GValue. GTK+ defines macros for a number of properties that
   --  can be used with this function.
   --  Note that passing a state other than the current state of Context is
   --  not recommended unless the style context has been saved with
   --  Gtk.Style_Context.Save.
   --  When Value is no longer needed, g_value_unset must be called to free
   --  any allocated memory.
   --  Since: gtk+ 3.0
   --  "property": style property name
   --  "state": state to retrieve the property value for
   --  "value": return location for the style property value

   function Get_Scale
      (Self : not null access Gtk_Style_Context_Record) return Glib.Gint;
   --  Returns the scale used for assets.
   --  Since: gtk+ 3.10

   procedure Set_Scale
      (Self  : not null access Gtk_Style_Context_Record;
       Scale : Glib.Gint);
   --  Sets the scale to use when getting image assets for the style.
   --  Since: gtk+ 3.10
   --  "scale": scale

   function Get_Screen
      (Self : not null access Gtk_Style_Context_Record)
       return Gdk.Screen.Gdk_Screen;
   --  Returns the Gdk.Screen.Gdk_Screen to which Context is attached.

   procedure Set_Screen
      (Self   : not null access Gtk_Style_Context_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Attaches Context to the given screen.
   --  The screen is used to add style information from "global" style
   --  providers, such as the screen's Gtk.Settings.Gtk_Settings instance.
   --  If you are using a Gtk.Style_Context.Gtk_Style_Context returned from
   --  gtk_widget_get_style_context, you do not need to call this yourself.
   --  Since: gtk+ 3.0
   --  "screen": a Gdk.Screen.Gdk_Screen

   function Get_Section
      (Self     : not null access Gtk_Style_Context_Record;
       Property : UTF8_String) return Gtk.Css_Section.Gtk_Css_Section;
   --  Queries the location in the CSS where Property was defined for the
   --  current Context. Note that the state to be queried is taken from
   --  Gtk.Style_Context.Get_State.
   --  If the location is not available, null will be returned. The location
   --  might not be available for various reasons, such as the property being
   --  overridden, Property not naming a supported CSS property or tracking of
   --  definitions being disabled for performance reasons.
   --  Shorthand CSS properties cannot be queried for a location and will
   --  always return null.
   --  "property": style property name

   function Get_State
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_State_Flags;
   --  Returns the state used for style matching.
   --  This method should only be used to retrieve the
   --  Gtk.Enums.Gtk_State_Flags to pass to Gtk.Style_Context.Gtk_Style_Context
   --  methods, like Gtk.Style_Context.Get_Padding. If you need to retrieve the
   --  current state of a Gtk.Widget.Gtk_Widget, use
   --  Gtk.Widget.Get_State_Flags.
   --  Since: gtk+ 3.0

   procedure Set_State
      (Self  : not null access Gtk_Style_Context_Record;
       Flags : Gtk.Enums.Gtk_State_Flags);
   --  Sets the state to be used for style matching.
   --  Since: gtk+ 3.0
   --  "flags": state to represent

   procedure Get_Style_Property
      (Self          : not null access Gtk_Style_Context_Record;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Gets the value for a widget style property.
   --  When Value is no longer needed, g_value_unset must be called to free
   --  any allocated memory.
   --  "property_name": the name of the widget style property
   --  "value": Return location for the property value

   function Has_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String) return Boolean;
   --  Returns True if Context currently has defined the given class name.
   --  Since: gtk+ 3.0
   --  "class_name": a class name

   procedure Has_Region
      (Self         : not null access Gtk_Style_Context_Record;
       Region_Name  : UTF8_String;
       Flags_Return : out Gtk.Enums.Gtk_Region_Flags;
       Is_Defined   : out Boolean);
   pragma Obsolescent (Has_Region);
   --  Returns True if Context has the region defined. If Flags_Return is not
   --  null, it is set to the flags affecting the region.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1
   --  "region_name": a region name
   --  "flags_return": return location for region flags

   procedure Invalidate (Self : not null access Gtk_Style_Context_Record);
   pragma Obsolescent (Invalidate);
   --  Invalidates Context style information, so it will be reconstructed
   --  again. It is useful if you modify the Context and need the new
   --  information immediately.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.12, 1

   function List_Classes
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.String_List.Glist;
   --  Returns the list of classes currently defined in Context.
   --  Since: gtk+ 3.0

   function List_Regions
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.String_List.Glist;
   pragma Obsolescent (List_Regions);
   --  Returns the list of regions currently defined in Context.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1

   procedure Lookup_Color
      (Self       : not null access Gtk_Style_Context_Record;
       Color_Name : UTF8_String;
       Color      : out Gdk.RGBA.Gdk_RGBA;
       Found      : out Boolean);
   --  Looks up and resolves a color name in the Context color map.
   --  "color_name": color name to lookup
   --  "color": Return location for the looked up color

   procedure Notify_State_Change
      (Self        : not null access Gtk_Style_Context_Record;
       Window      : Gdk.Gdk_Window;
       Region_Id   : System.Address;
       State       : Gtk.Enums.Gtk_State_Type;
       State_Value : Boolean);
   pragma Obsolescent (Notify_State_Change);
   --  Notifies a state change on Context, so if the current style makes use
   --  of transition animations, one will be started so all rendered elements
   --  under Region_Id are animated for state State being set to value
   --  State_Value.
   --  The Window parameter is used in order to invalidate the rendered area
   --  as the animation runs, so make sure it is the same window that is being
   --  rendered on by the gtk_render_* functions.
   --  If Region_Id is null, all rendered elements using Context will be
   --  affected by this state transition.
   --  As a practical example, a Gtk.Button.Gtk_Button notifying a state
   --  transition on the prelight state: |[ <!-- language="C" -->
   --  gtk_style_context_notify_state_change (context, gtk_widget_get_window
   --  (widget), NULL, GTK_STATE_PRELIGHT, button->in_button); ]|
   --  Can be handled in the CSS file like this: |[ <!-- language="CSS" -->
   --  button { background-color: f00 }
   --  button:hover { background-color: fff; transition: 200ms linear } ]|
   --  This combination will animate the button background from red to white
   --  if a pointer enters the button, and back to red if the pointer leaves
   --  the button.
   --  Note that State is used when finding the transition parameters, which
   --  is why the style places the transition under the :hover pseudo-class.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, 1
   --  "window": a Gdk.Gdk_Window
   --  "region_id": animatable region to notify on, or null. See
   --  Gtk.Style_Context.Push_Animatable_Region
   --  "state": state to trigger transition for
   --  "state_value": True if State is the state we are changing to, False if
   --  we are changing away from it

   procedure Pop_Animatable_Region
      (Self : not null access Gtk_Style_Context_Record);
   pragma Obsolescent (Pop_Animatable_Region);
   --  Pops an animatable region from Context. See
   --  Gtk.Style_Context.Push_Animatable_Region.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, 1

   procedure Push_Animatable_Region
      (Self      : not null access Gtk_Style_Context_Record;
       Region_Id : System.Address);
   pragma Obsolescent (Push_Animatable_Region);
   --  Pushes an animatable region, so all further gtk_render_* calls between
   --  this call and the following Gtk.Style_Context.Pop_Animatable_Region will
   --  potentially show transition animations for this region if
   --  Gtk.Style_Context.Notify_State_Change is called for a given state, and
   --  the current theme/style defines transition animations for state changes.
   --  The Region_Id used must be unique in Context so the themes can uniquely
   --  identify rendered elements subject to a state transition.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, 1
   --  "region_id": unique identifier for the animatable region

   procedure Remove_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String);
   --  Removes Class_Name from Context.
   --  Since: gtk+ 3.0
   --  "class_name": class name to remove

   procedure Remove_Provider
      (Self     : not null access Gtk_Style_Context_Record;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider);
   --  Removes Provider from the style providers list in Context.
   --  Since: gtk+ 3.0
   --  "provider": a Gtk.Style_Provider.Gtk_Style_Provider

   procedure Remove_Region
      (Self        : not null access Gtk_Style_Context_Record;
       Region_Name : UTF8_String);
   pragma Obsolescent (Remove_Region);
   --  Removes a region from Context.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1
   --  "region_name": region name to unset

   procedure Restore (Self : not null access Gtk_Style_Context_Record);
   --  Restores Context state to a previous stage. See Gtk.Style_Context.Save.
   --  Since: gtk+ 3.0

   procedure Save (Self : not null access Gtk_Style_Context_Record);
   --  Saves the Context state, so temporary modifications done through
   --  Gtk.Style_Context.Add_Class, Gtk.Style_Context.Remove_Class,
   --  Gtk.Style_Context.Set_State, etc. can quickly be reverted in one go
   --  through Gtk.Style_Context.Restore.
   --  The matching call to Gtk.Style_Context.Restore must be done before GTK
   --  returns to the main loop.
   --  Since: gtk+ 3.0

   procedure Scroll_Animations
      (Self   : not null access Gtk_Style_Context_Record;
       Window : Gdk.Gdk_Window;
       Dx     : Glib.Gint;
       Dy     : Glib.Gint);
   pragma Obsolescent (Scroll_Animations);
   --  This function is analogous to Gdk.Window.Scroll, and should be called
   --  together with it so the invalidation areas for any ongoing animation are
   --  scrolled together with it.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, 1
   --  "window": a Gdk.Gdk_Window used previously in
   --  Gtk.Style_Context.Notify_State_Change
   --  "dx": Amount to scroll in the X axis
   --  "dy": Amount to scroll in the Y axis

   procedure Set_Background
      (Self   : not null access Gtk_Style_Context_Record;
       Window : Gdk.Gdk_Window);
   pragma Obsolescent (Set_Background);
   --  Sets the background of Window to the background pattern or color
   --  specified in Context for its current state.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.18, 1
   --  "window": a Gdk.Gdk_Window

   procedure State_Is_Running
      (Self       : not null access Gtk_Style_Context_Record;
       State      : Gtk.Enums.Gtk_State_Type;
       Progress   : out Gdouble;
       Is_Running : out Boolean);
   pragma Obsolescent (State_Is_Running);
   --  Returns True if there is a transition animation running for the current
   --  region (see Gtk.Style_Context.Push_Animatable_Region).
   --  If Progress is not null, the animation progress will be returned there,
   --  0.0 means the state is closest to being unset, while 1.0 means it's
   --  closest to being set. This means transition animation will run from 0 to
   --  1 when State is being set and from 1 to 0 when it's being unset.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.6, 1
   --  "state": a widget state
   --  "progress": return location for the transition progress

   function To_String
      (Self  : not null access Gtk_Style_Context_Record;
       Flags : Gtk_Style_Context_Print_Flags) return UTF8_String;
   --  Converts the style context into a string representation.
   --  The string representation always includes information about the name,
   --  state, id, visibility and style classes of the CSS node that is backing
   --  Context. Depending on the flags, more information may be included.
   --  This function is intended for testing and debugging of the CSS
   --  implementation in GTK+. There are no guarantees about the format of the
   --  returned string, it may change.
   --  Since: gtk+ 3.20
   --  "flags": Flags that determine what to print

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Style_Context
     (Widget : not null access Gtk_Widget_Record'Class)
   return Gtk_Style_Context;
   --  Returns the style context associated to Widget.
   --  must not be freed.

   ---------------
   -- Functions --
   ---------------

   procedure Add_Provider_For_Screen
      (Screen   : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint);
   --  Adds a global style provider to Screen, which will be used in style
   --  construction for all Gtk_Style_Contexts under Screen.
   --  GTK+ uses this to make styling information from
   --  Gtk.Settings.Gtk_Settings available.
   --  Note: If both priorities are the same, A
   --  Gtk.Style_Provider.Gtk_Style_Provider added through
   --  Gtk.Style_Context.Add_Provider takes precedence over another added
   --  through this function.
   --  Since: gtk+ 3.0
   --  "screen": a Gdk.Screen.Gdk_Screen
   --  "provider": a Gtk.Style_Provider.Gtk_Style_Provider
   --  "priority": the priority of the style provider. The lower it is, the
   --  earlier it will be used in the style construction. Typically this will
   --  be in the range between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and
   --  GTK_STYLE_PROVIDER_PRIORITY_USER

   procedure Remove_Provider_For_Screen
      (Screen   : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider);
   --  Removes Provider from the global style providers list in Screen.
   --  Since: gtk+ 3.0
   --  "screen": a Gdk.Screen.Gdk_Screen
   --  "provider": a Gtk.Style_Provider.Gtk_Style_Provider

   procedure Reset_Widgets
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  This function recomputes the styles for all widgets under a particular
   --  Gdk.Screen.Gdk_Screen. This is useful when some global parameter has
   --  changed that affects the appearance of all widgets, because when a
   --  widget gets a new style, it will both redraw and recompute any cached
   --  information about its appearance. As an example, it is used when the
   --  color scheme changes in the related Gtk.Settings.Gtk_Settings object.
   --  Since: gtk+ 3.0
   --  "screen": a Gdk.Screen.Gdk_Screen

   procedure Render_Handle
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders a handle (as in Gtk.Handle_Box.Gtk_Handle_Box,
   --  Gtk.Paned.Gtk_Paned and Gtk.Window.Gtk_Window's resize grip), in the
   --  rectangle determined by X, Y, Width, Height.
   --  Handles rendered for the paned and grip classes:
   --  ![](handles.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Check
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders a checkmark (as in a Gtk.Check_Button.Gtk_Check_Button).
   --  The Gtk.Enums.Gtk_State_Flag_Checked state determines whether the check
   --  is on or off, and Gtk.Enums.Gtk_State_Flag_Inconsistent determines
   --  whether it should be marked as undefined.
   --  Typical checkmark rendering:
   --  ![](checks.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Option
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders an option mark (as in a Gtk.Radio_Button.Gtk_Radio_Button), the
   --  Gtk.Enums.Gtk_State_Flag_Checked state will determine whether the option
   --  is on or off, and Gtk.Enums.Gtk_State_Flag_Inconsistent whether it
   --  should be marked as undefined.
   --  Typical option mark rendering:
   --  ![](options.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Arrow
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       Angle   : Gdouble;
       X       : Gdouble;
       Y       : Gdouble;
       Size    : Gdouble);
   --  Renders an arrow pointing to Angle.
   --  Typical arrow rendering at 0, 1⁄2 π;, π; and 3⁄2 π:
   --  ![](arrows.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "angle": arrow angle from 0 to 2 * G_PI, being 0 the arrow pointing to
   --  the north
   --  "x": X origin of the render area
   --  "y": Y origin of the render area
   --  "size": square side for render area

   procedure Render_Background
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders the background of an element.
   --  Typical background rendering, showing the effect of `background-image`,
   --  `border-width` and `border-radius`:
   --  ![](background.png)
   --  Since: gtk+ 3.0.
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Frame
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders a frame around the rectangle defined by X, Y, Width, Height.
   --  Examples of frame rendering, showing the effect of `border-image`,
   --  `border-color`, `border-width`, `border-radius` and junctions:
   --  ![](frames.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Expander
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders an expander (as used in Gtk.Tree_View.Gtk_Tree_View and
   --  Gtk.Expander.Gtk_Expander) in the area defined by X, Y, Width, Height.
   --  The state Gtk.Enums.Gtk_State_Flag_Checked determines whether the
   --  expander is collapsed or expanded.
   --  Typical expander rendering:
   --  ![](expanders.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Focus
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders a focus indicator on the rectangle determined by X, Y, Width,
   --  Height.
   --  Typical focus rendering:
   --  ![](focus.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Layout
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Layout  : not null access Pango.Layout.Pango_Layout_Record'Class);
   --  Renders Layout on the coordinates X, Y
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin
   --  "y": Y origin
   --  "layout": the Pango.Layout.Pango_Layout to render

   procedure Render_Line
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X0      : Gdouble;
       Y0      : Gdouble;
       X1      : Gdouble;
       Y1      : Gdouble);
   --  Renders a line from (x0, y0) to (x1, y1).
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x0": X coordinate for the origin of the line
   --  "y0": Y coordinate for the origin of the line
   --  "x1": X coordinate for the end of the line
   --  "y1": Y coordinate for the end of the line

   procedure Render_Slider
      (Context     : not null access Gtk_Style_Context_Record'Class;
       Cr          : Cairo.Cairo_Context;
       X           : Gdouble;
       Y           : Gdouble;
       Width       : Gdouble;
       Height      : Gdouble;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Renders a slider (as in Gtk.Scale.Gtk_Scale) in the rectangle defined
   --  by X, Y, Width, Height. Orientation defines whether the slider is
   --  vertical or horizontal.
   --  Typical slider rendering:
   --  ![](sliders.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height
   --  "orientation": orientation of the slider

   procedure Render_Frame_Gap
      (Context  : not null access Gtk_Style_Context_Record'Class;
       Cr       : Cairo.Cairo_Context;
       X        : Gdouble;
       Y        : Gdouble;
       Width    : Gdouble;
       Height   : Gdouble;
       Gap_Side : Gtk.Enums.Gtk_Position_Type;
       Xy0_Gap  : Gdouble;
       Xy1_Gap  : Gdouble);
   pragma Obsolescent (Render_Frame_Gap);
   --  Renders a frame around the rectangle defined by (X, Y, Width, Height),
   --  leaving a gap on one side. Xy0_Gap and Xy1_Gap will mean X coordinates
   --  for Gtk.Enums.Pos_Top and Gtk.Enums.Pos_Bottom gap sides, and Y
   --  coordinates for Gtk.Enums.Pos_Left and Gtk.Enums.Pos_Right.
   --  Typical rendering of a frame with a gap:
   --  ![](frame-gap.png)
   --  Since: gtk+ 3.0
   --  Deprecated since 3.24, 1
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height
   --  "gap_side": side where the gap is
   --  "xy0_gap": initial coordinate (X or Y depending on Gap_Side) for the
   --  gap
   --  "xy1_gap": end coordinate (X or Y depending on Gap_Side) for the gap

   procedure Render_Extension
      (Context  : not null access Gtk_Style_Context_Record'Class;
       Cr       : Cairo.Cairo_Context;
       X        : Gdouble;
       Y        : Gdouble;
       Width    : Gdouble;
       Height   : Gdouble;
       Gap_Side : Gtk.Enums.Gtk_Position_Type);
   --  Renders a extension (as in a Gtk.Notebook.Gtk_Notebook tab) in the
   --  rectangle defined by X, Y, Width, Height. The side where the extension
   --  connects to is defined by Gap_Side.
   --  Typical extension rendering:
   --  ![](extensions.png)
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height
   --  "gap_side": side where the gap is

   procedure Render_Activity
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble);
   --  Renders an activity indicator (such as in Gtk.Spinner.Gtk_Spinner). The
   --  state Gtk.Enums.Gtk_State_Flag_Checked determines whether there is
   --  activity going on.
   --  Since: gtk+ 3.0
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "x": X origin of the rectangle
   --  "y": Y origin of the rectangle
   --  "width": rectangle width
   --  "height": rectangle height

   procedure Render_Icon
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       Pixbuf  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
       X       : Gdouble;
       Y       : Gdouble);
   --  Renders the icon in Pixbuf at the specified X and Y coordinates.
   --  This function will render the icon in Pixbuf at exactly its size,
   --  regardless of scaling factors, which may not be appropriate when drawing
   --  on displays with high pixel densities.
   --  You probably want to use Gtk.Style_Context.Render_Icon_Surface instead,
   --  if you already have a Cairo surface.
   --  Since: gtk+ 3.2
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf containing the icon to draw
   --  "x": X position for the Pixbuf
   --  "y": Y position for the Pixbuf

   procedure Render_Icon_Surface
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       Surface : Cairo.Cairo_Surface;
       X       : Gdouble;
       Y       : Gdouble);
   --  Renders the icon in Surface at the specified X and Y coordinates.
   --  Since: gtk+ 3.10
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "cr": a cairo_t
   --  "surface": a cairo_surface_t containing the icon to draw
   --  "x": X position for the Icon
   --  "y": Y position for the Incon

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction;

   Paint_Clock_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Frame_Clock

   Parent_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Style_Context
   --  Sets or gets the style context's parent. See
   --  Gtk.Style_Context.Set_Parent for details.

   Screen_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Screen.Gdk_Screen

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Style_Context_Void is not null access procedure
     (Self : access Gtk_Style_Context_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Style_Context_Record;
       Call  : Cb_Gtk_Style_Context_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Style_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::changed signal is emitted when there is a change in the
   --  Gtk.Style_Context.Gtk_Style_Context.
   --
   --  For a Gtk.Style_Context.Gtk_Style_Context returned by
   --  gtk_widget_get_style_context, the Gtk.Widget.Gtk_Widget::style-updated
   --  signal/vfunc might be more convenient to use.
   --
   --  This signal is useful when using the theming layer standalone.

private
   Screen_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("screen");
   Parent_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("parent");
   Paint_Clock_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("paint-clock");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction :=
     Gtk.Enums.Build ("direction");
end Gtk.Style_Context;
