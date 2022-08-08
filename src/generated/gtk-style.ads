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
--  A Gtk.Style.Gtk_Style object encapsulates the information that provides
--  the look and feel for a widget.
--
--  > In GTK+ 3.0, GtkStyle has been deprecated and replaced by >
--  Gtk.Style_Context.Gtk_Style_Context.
--
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
with Gdk;             use Gdk;
with Gdk.Color;       use Gdk.Color;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;
with Gtk.Enums;       use Gtk.Enums;

package Gtk.Style is

   type Gtk_Style_Record is new GObject_Record with null record;
   type Gtk_Style is access all Gtk_Style_Record'Class;

   type Gtk_Border is record
      Left : Gint16;
      Right : Gint16;
      Top : Gint16;
      Bottom : Gint16;
   end record;
   pragma Convention (C, Gtk_Border);

   function From_Object_Free (B : access Gtk_Border) return Gtk_Border;
   pragma Inline (From_Object_Free);
   --  A struct that specifies a border around a rectangular area that can be
   --  of different width on each side.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Style : out Gtk_Style);
   procedure Initialize (Style : not null access Gtk_Style_Record'Class);
   --  Creates a new Gtk.Style.Gtk_Style.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Style_New return Gtk_Style;
   --  Creates a new Gtk.Style.Gtk_Style.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_get_type");

   procedure Gtk_New (Self : out Gtk_Border);
   --  Allocates a new Gtk.Style.Gtk_Border-struct and initializes its
   --  elements to zero.
   --  Since: gtk+ 2.14

   function Gtk_Border_New return Gtk_Border;
   --  Allocates a new Gtk.Style.Gtk_Border-struct and initializes its
   --  elements to zero.
   --  Since: gtk+ 2.14

   function Border_Get_Type return Glib.GType;
   pragma Import (C, Border_Get_Type, "gtk_border_get_type");

   -------------
   -- Methods --
   -------------

   procedure Apply_Default_Background
      (Style      : not null access Gtk_Style_Record;
       Cr         : Cairo.Cairo_Context;
       Window     : Gdk.Gdk_Window;
       State_Type : Gtk.Enums.Gtk_State_Type;
       X          : Glib.Gint;
       Y          : Glib.Gint;
       Width      : Glib.Gint;
       Height     : Glib.Gint);
   pragma Obsolescent (Apply_Default_Background);
   --  Deprecated since 3.0, 1

   function Attach
      (Style  : not null access Gtk_Style_Record;
       Window : Gdk.Gdk_Window) return Gtk_Style;
   pragma Obsolescent (Attach);
   --  Attaches a style to a window; this process allocates the colors and
   --  creates the GC's for the style - it specializes it to a particular
   --  visual. The process may involve the creation of a new style if the style
   --  has already been attached to a window with a different style and visual.
   --  Since this function may return a new object, you have to use it in the
   --  following way: `style = gtk_style_attach (style, window)`
   --  Deprecated since 3.0, 1
   --  "window": a Gdk.Gdk_Window.

   function Copy (Style : not null access Gtk_Style_Record) return Gtk_Style;
   pragma Obsolescent (Copy);
   --  Creates a copy of the passed in Gtk.Style.Gtk_Style object.
   --  Deprecated since 3.0, 1

   procedure Detach (Style : not null access Gtk_Style_Record);
   pragma Obsolescent (Detach);
   --  Detaches a style from a window. If the style is not attached to any
   --  windows anymore, it is unrealized. See Gtk.Style.Attach.
   --  Deprecated since 3.0, 1

   procedure Get_Style_Property
      (Style         : not null access Gtk_Style_Record;
       Widget_Type   : GType;
       Property_Name : UTF8_String;
       Value         : out Glib.Values.GValue);
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
   pragma Obsolescent (Lookup_Color);
   --  Looks up Color_Name in the style's logical color mappings, filling in
   --  Color and returning True if found, otherwise returning False. Do not
   --  cache the found mapping, because it depends on the Gtk.Style.Gtk_Style
   --  and might change when a theme switch occurs.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.0, 1
   --  "color_name": the name of the logical color to look up
   --  "color": the Gdk.Color.Gdk_Color to fill in

   procedure Set_Background
      (Style      : not null access Gtk_Style_Record;
       Window     : Gdk.Gdk_Window;
       State_Type : Gtk.Enums.Gtk_State_Type);
   pragma Obsolescent (Set_Background);
   --  Sets the background of Window to the background color or pixmap
   --  specified by Style for the given state.
   --  Deprecated since 3.0, 1
   --  "window": a Gdk.Gdk_Window
   --  "state_type": a state

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Context_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Style_Context.Gtk_Style_Context

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Style_Void is not null access procedure (Self : access Gtk_Style_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Realize : constant Glib.Signal_Name := "realize";
   procedure On_Realize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_Gtk_Style_Void;
       After : Boolean := False);
   procedure On_Realize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the style has been initialized for a particular visual.
   --  Connecting to this signal is probably seldom useful since most of the
   --  time applications and widgets only deal with styles that have been
   --  already realized.

   Signal_Unrealize : constant Glib.Signal_Name := "unrealize";
   procedure On_Unrealize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_Gtk_Style_Void;
       After : Boolean := False);
   procedure On_Unrealize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the aspects of the style specific to a particular visual
   --  is being cleaned up. A connection to this signal can be useful if a
   --  widget wants to cache objects as object data on Gtk.Style.Gtk_Style.
   --  This signal provides a convenient place to free such cached objects.

private
   Context_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("context");
end Gtk.Style;
