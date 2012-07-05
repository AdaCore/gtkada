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

--  <description>
--  A Gtk.Style.Gtk_Style object encapsulates the information that provides
--  the look and feel for a widget.
--
--  <warning> In GTK+ 3.0, GtkStyle has been deprecated and replaced by
--  Gtk.Style_Context.Gtk_Style_Context. </warning>
--  Each Gtk.Widget.Gtk_Widget has an associated Gtk.Style.Gtk_Style object
--  that is used when rendering that widget. Also, a Gtk.Style.Gtk_Style holds
--  information for the five possible widget states though not every widget
--  supports all five states; see Gtk.Enums.Gtk_State_Type.
--
--  Usually the Gtk.Style.Gtk_Style for a widget is the same as the default
--  style that is set by GTK+ and modified the theme engine.
--
--  Usually applications should not need to use or modify the
--  Gtk.Style.Gtk_Style of their widgets.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;           use Cairo;
with Gdk.Color;       use Gdk.Color;
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;
with Gtk.Enums;       use Gtk.Enums;

package Gtk.Style is

   type Gtk_Style_Record is new GObject_Record with null record;
   type Gtk_Style is access all Gtk_Style_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Style : out Gtk_Style);
   --  Creates a new Gtk.Style.Gtk_Style.

   procedure Initialize (Style : not null access Gtk_Style_Record'Class);
   --  Creates a new Gtk.Style.Gtk_Style.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_get_type");

   -------------
   -- Methods --
   -------------

   procedure Apply_Default_Background
      (Style      : not null access Gtk_Style_Record;
       Cr         : Cairo.Cairo_Context;
       Window     : Gdk.Window.Gdk_Window;
       State_Type : Gtk.Enums.Gtk_State_Type;
       X          : Gint;
       Y          : Gint;
       Width      : Gint;
       Height     : Gint);
   --  Deprecated:3.0: Use Gtk.Style_Context.Gtk_Style_Context instead

   function Attach
      (Style  : not null access Gtk_Style_Record;
       Window : Gdk.Window.Gdk_Window) return Gtk_Style;
   --  Attaches a style to a window; this process allocates the colors and
   --  creates the GC's for the style - it specializes it to a particular
   --  visual. The process may involve the creation of a new style if the style
   --  has already been attached to a window with a different style and visual.
   --  Since this function may return a new object, you have to use it in the
   --  following way: 'style = gtk_style_attach (style, window)'
   --  If the style is newly created, the style parameter will be unref'ed,
   --  and the new style will have a reference count belonging to the caller.
   --  Deprecated:3.0: Use Gtk.Widget.Style_Attach instead
   --  "window": a Gdk.Window.Gdk_Window.

   function Copy (Style : not null access Gtk_Style_Record) return Gtk_Style;
   --  Creates a copy of the passed in Gtk.Style.Gtk_Style object.
   --  Deprecated:3.0: Use Gtk.Style_Context.Gtk_Style_Context instead

   procedure Detach (Style : not null access Gtk_Style_Record);
   --  Detaches a style from a window. If the style is not attached to any
   --  windows anymore, it is unrealized. See Gtk.Style.Attach.
   --  Deprecated:3.0: Use Gtk.Style_Context.Gtk_Style_Context instead

   procedure Get_Style_Property
      (Style         : not null access Gtk_Style_Record;
       Widget_Type   : GType;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Queries the value of a style property corresponding to a widget class
   --  is in the given style.
   --  Since: gtk+ 2.16
   --  "widget_type": the GType of a descendant of Gtk.Widget.Gtk_Widget
   --  "property_name": the name of the style property to get
   --  "value": a Glib.Values.GValue where the value of the property being
   --  queried will be stored

   function Has_Context
      (Style : not null access Gtk_Style_Record) return Boolean;
   --  Returns whether Style has an associated
   --  Gtk.Style_Context.Gtk_Style_Context.
   --  Since: gtk+ 3.0

   procedure Lookup_Color
      (Style      : not null access Gtk_Style_Record;
       Color_Name : UTF8_String;
       Color      : out Gdk.Color.Gdk_Color;
       Found      : out Boolean);
   --  Looks up Color_Name in the style's logical color mappings, filling in
   --  Color and returning True if found, otherwise returning False. Do not
   --  cache the found mapping, because it depends on the Gtk.Style.Gtk_Style
   --  and might change when a theme switch occurs.
   --  Deprecated:3.0: Use Gtk.Style_Context.Lookup_Color instead
   --  Since: gtk+ 2.10
   --  "color_name": the name of the logical color to look up
   --  "color": the Gdk.Color.Gdk_Color to fill in

   procedure Set_Background
      (Style      : not null access Gtk_Style_Record;
       Window     : Gdk.Window.Gdk_Window;
       State_Type : Gtk.Enums.Gtk_State_Type);
   --  Sets the background of Window to the background color or pixmap
   --  specified by Style for the given state.
   --  Deprecated:3.0: Use Gtk.Style_Context.Set_Background instead
   --  "window": a Gdk.Window.Gdk_Window
   --  "state_type": a state

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Gtk_Border_Record is record
      Left   : Gint := 0;
      Right  : Gint := 0;
      Top    : Gint := 0;
      Bottom : Gint := 0;
   end record;
   pragma Convention (C, Gtk_Border_Record);
   type Gtk_Border is access all Gtk_Border_Record;

   --  function Border_Copy (Border : access Gtk_Border_Record) return Gtk_Border;
   --  Copies a Gtk_Border structure.

   --  procedure Border_Free (Border : access Gtk_Border_Record);
   --  Frees a Gtk_Border structure.

   --  function Border_Get_Type return GType;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Context_Property
   --  Type: Gtk.Style_Context.Gtk_Style_Context
   --  Flags: read-write

   Context_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "realize"
   --     procedure Handler (Self : access Gtk_Style_Record'Class);
   --  Emitted when the style has been initialized for a particular visual.
   --  Connecting to this signal is probably seldom useful since most of the
   --  time applications and widgets only deal with styles that have been
   --  already realized.
   --
   --  "unrealize"
   --     procedure Handler (Self : access Gtk_Style_Record'Class);
   --  Emitted when the aspects of the style specific to a particular visual
   --  is being cleaned up. A connection to this signal can be useful if a
   --  widget wants to cache objects as object data on Gtk.Style.Gtk_Style.
   --  This signal provides a convenient place to free such cached objects.

   Signal_Realize : constant Glib.Signal_Name := "realize";
   Signal_Unrealize : constant Glib.Signal_Name := "unrealize";

private
   Context_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("context");
end Gtk.Style;
