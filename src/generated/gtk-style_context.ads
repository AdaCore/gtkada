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

--  `GtkStyleContext` stores styling information affecting a widget.
--
--  In order to construct the final style information, `GtkStyleContext`
--  queries information from all attached `GtkStyleProviders`. Style providers
--  can be either attached explicitly to the context through
--  [methodGtk.StyleContext.add_provider], or to the display through
--  [funcGtk.StyleContext.add_provider_for_display]. The resulting style is a
--  combination of all providers' information in priority order.
--
--  For GTK widgets, any `GtkStyleContext` returned by
--  [methodGtk.Widget.get_style_context] will already have a `GdkDisplay` and
--  RTL/LTR information set. The style context will also be updated
--  automatically if any of these settings change on the widget.
--
--  ## Style Classes
--
--  Widgets can add style classes to their context, which can be used to
--  associate different styles by class. The documentation for individual
--  widgets lists which style classes it uses itself, and which style classes
--  may be added by applications to affect their appearance.
--
--  # Custom styling in UI libraries and applications
--
--  If you are developing a library with custom widgets that render
--  differently than standard components, you may need to add a
--  `GtkStyleProvider` yourself with the GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
--  priority, either a `GtkCssProvider` or a custom object implementing the
--  `GtkStyleProvider` interface. This way themes may still attempt to style
--  your UI elements in a different way if needed so.
--
--  If you are using custom styling on an applications, you probably want then
--  to make your style information prevail to the theme's, so you must use a
--  `GtkStyleProvider` with the GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
--  priority, keep in mind that the user settings in
--  `XDG_CONFIG_HOME/gtk-4.0/gtk.css` will still take precedence over your
--  changes, as it uses the GTK_STYLE_PROVIDER_PRIORITY_USER priority.

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                     use Gdk;
with Gdk.Display;
with Gdk.RGBA;                use Gdk.RGBA;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Style_Provider;      use Gtk.Style_Provider;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Style_Context is

   pragma Obsolescent;
   --  The relevant API has been moved to [class@Gtk.Widget] where applicable; otherwise, there is no replacement for querying the style machinery. Stylable UI elements should use widgets.

   type Gtk_Style_Context_Record is new GObject_Record with null record;
   type Gtk_Style_Context is access all Gtk_Style_Context_Record'Class;

   type Gtk_Style_Context_Print_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Style_Context_Print_Flags);
   --  Flags that modify the behavior of Gtk.Style_Context.To_String.
   --
   --  New values may be added to this enumeration.

   Style_Context_Print_None : constant Gtk_Style_Context_Print_Flags := 0;
   Style_Context_Print_Recurse : constant Gtk_Style_Context_Print_Flags := 1;
   Style_Context_Print_Show_Style : constant Gtk_Style_Context_Print_Flags := 2;
   Style_Context_Print_Show_Change : constant Gtk_Style_Context_Print_Flags := 4;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Style_Context_Print_Flags_Properties is
      new Generic_Internal_Flags_Property (Gtk_Style_Context_Print_Flags);
   type Property_Gtk_Style_Context_Print_Flags is new Gtk_Style_Context_Print_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_context_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String);
   pragma Obsolescent (Add_Class);
   --  Adds a style class to Context, so later uses of the style context will
   --  make use of this new class for styling.
   --  In the CSS file format, a `GtkEntry` defining a "search" class, would
   --  be matched by:
   --  ```css entry.search { ... } ```
   --  While any widget defining a "search" class would be matched by: ```css
   --  .search { ... } ```
   --  Deprecated since 4.10, 1
   --  @param Class_Name class name to use in styling

   procedure Add_Provider
      (Self     : not null access Gtk_Style_Context_Record;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint);
   pragma Obsolescent (Add_Provider);
   --  Adds a style provider to Context, to be used in style construction.
   --  Note that a style provider added by this function only affects the
   --  style of the widget to which Context belongs. If you want to affect the
   --  style of all widgets, use
   --  [funcGtk.StyleContext.add_provider_for_display].
   --  Note: If both priorities are the same, a `GtkStyleProvider` added
   --  through this function takes precedence over another added through
   --  [funcGtk.StyleContext.add_provider_for_display].
   --  Deprecated since 4.10, 1
   --  @param Provider a `GtkStyleProvider`
   --  @param Priority the priority of the style provider. The lower it is,
   --  the earlier it will be used in the style construction. Typically this
   --  will be in the range between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and
   --  GTK_STYLE_PROVIDER_PRIORITY_USER

   procedure Get_Color
      (Self  : not null access Gtk_Style_Context_Record;
       Color : out Gdk.RGBA.Gdk_RGBA);
   pragma Obsolescent (Get_Color);
   --  Gets the foreground color for a given state.
   --  Deprecated since 4.10, 1
   --  @param Color return value for the foreground color

   function Get_Display
      (Self : not null access Gtk_Style_Context_Record)
       return Gdk.Gdk_Display;
   pragma Obsolescent (Get_Display);
   --  Returns the `GdkDisplay` to which Context is attached.
   --  Deprecated since 4.10, 1
   --  @return a `GdkDisplay`.
   --  Return has transfer-ownership='none'

   procedure Set_Display
      (Self    : not null access Gtk_Style_Context_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   pragma Obsolescent (Set_Display);
   --  Attaches Context to the given display.
   --  The display is used to add style information from "global" style
   --  providers, such as the display's `GtkSettings` instance.
   --  If you are using a `GtkStyleContext` returned from
   --  [methodGtk.Widget.get_style_context], you do not need to call this
   --  yourself.
   --  Deprecated since 4.10, 1
   --  @param Display a `GdkDisplay`

   function Get_Scale
      (Self : not null access Gtk_Style_Context_Record) return Glib.Gint;
   pragma Obsolescent (Get_Scale);
   --  Returns the scale used for assets.
   --  Deprecated since 4.10, 1
   --  @return the scale

   procedure Set_Scale
      (Self  : not null access Gtk_Style_Context_Record;
       Scale : Glib.Gint);
   pragma Obsolescent (Set_Scale);
   --  Sets the scale to use when getting image assets for the style.
   --  Deprecated since 4.10, 1
   --  @param Scale scale

   function Get_State
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_State_Flags;
   pragma Obsolescent (Get_State);
   --  Returns the state used for style matching.
   --  This method should only be used to retrieve the `GtkStateFlags` to pass
   --  to `GtkStyleContext` methods, like [methodGtk.StyleContext.get_padding].
   --  If you need to retrieve the current state of a `GtkWidget`, use
   --  [methodGtk.Widget.get_state_flags].
   --  Deprecated since 4.10, 1
   --  @return the state flags

   procedure Set_State
      (Self  : not null access Gtk_Style_Context_Record;
       Flags : Gtk.Enums.Gtk_State_Flags);
   pragma Obsolescent (Set_State);
   --  Sets the state to be used for style matching.
   --  Deprecated since 4.10, 1
   --  @param Flags state to represent

   function Has_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String) return Boolean;
   pragma Obsolescent (Has_Class);
   --  Returns True if Context currently has defined the given class name.
   --  Deprecated since 4.10, 1
   --  @param Class_Name a class name
   --  @return True if Context has Class_Name defined

   procedure Lookup_Color
      (Self       : not null access Gtk_Style_Context_Record;
       Color_Name : UTF8_String;
       Color      : out Gdk.RGBA.Gdk_RGBA;
       Found      : out Boolean);
   pragma Obsolescent (Lookup_Color);
   --  Looks up and resolves a color name in the Context color map.
   --  Deprecated since 4.10, 1
   --  @param Color_Name color name to lookup
   --  @param Color Return location for the looked up color
   --  @return True if Color_Name was found and resolved, False otherwise

   procedure Remove_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String);
   pragma Obsolescent (Remove_Class);
   --  Removes Class_Name from Context.
   --  Deprecated since 4.10, 1
   --  @param Class_Name class name to remove

   procedure Remove_Provider
      (Self     : not null access Gtk_Style_Context_Record;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider);
   pragma Obsolescent (Remove_Provider);
   --  Removes Provider from the style providers list in Context.
   --  Deprecated since 4.10, 1
   --  @param Provider a `GtkStyleProvider`

   procedure Restore (Self : not null access Gtk_Style_Context_Record);
   pragma Obsolescent (Restore);
   --  Restores Context state to a previous stage.
   --  See [methodGtk.StyleContext.save].
   --  Deprecated since 4.10, 1

   procedure Save (Self : not null access Gtk_Style_Context_Record);
   pragma Obsolescent (Save);
   --  Saves the Context state.
   --  This allows temporary modifications done through
   --  [methodGtk.StyleContext.add_class],
   --  [methodGtk.StyleContext.remove_class],
   --  [methodGtk.StyleContext.set_state] to be quickly reverted in one go
   --  through [methodGtk.StyleContext.restore].
   --  The matching call to [methodGtk.StyleContext.restore] must be done
   --  before GTK returns to the main loop.
   --  Deprecated since 4.10, 1

   function To_String
      (Self  : not null access Gtk_Style_Context_Record;
       Flags : Gtk_Style_Context_Print_Flags) return UTF8_String;
   pragma Obsolescent (To_String);
   --  Converts the style context into a string representation.
   --  The string representation always includes information about the name,
   --  state, id, visibility and style classes of the CSS node that is backing
   --  Context. Depending on the flags, more information may be included.
   --  This function is intended for testing and debugging of the CSS
   --  implementation in GTK. There are no guarantees about the format of the
   --  returned string, it may change.
   --  Deprecated since 4.10, 1
   --  @param Flags Flags that determine what to print
   --  @return a newly allocated string representing Context

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Style_Context
     (Widget : not null access Gtk_Widget_Record'Class)
   return Gtk_Style_Context;
   --  Returns the style context associated with Widget.
   --  The returned context is owned by Widget and must not be freed.

   ---------------
   -- Functions --
   ---------------

   procedure Add_Provider_For_Display
      (Display  : not null access Gdk.Display.Gdk_Display_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint);
   --  Adds a global style provider to Display, which will be used in style
   --  construction for all `GtkStyleContexts` under Display.
   --  GTK uses this to make styling information from `GtkSettings` available.
   --  Note: If both priorities are the same, A `GtkStyleProvider` added
   --  through [methodGtk.StyleContext.add_provider] takes precedence over
   --  another added through this function.
   --  @param Display a `GdkDisplay`
   --  @param Provider a `GtkStyleProvider`
   --  @param Priority the priority of the style provider. The lower it is,
   --  the earlier it will be used in the style construction. Typically this
   --  will be in the range between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and
   --  GTK_STYLE_PROVIDER_PRIORITY_USER

   procedure Remove_Provider_For_Display
      (Display  : not null access Gdk.Display.Gdk_Display_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider);
   --  Removes Provider from the global style providers list in Display.
   --  @param Display a `GdkDisplay`
   --  @param Provider a `GtkStyleProvider`

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Display
   --  The display of the style context.

private
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gtk.Style_Context;
