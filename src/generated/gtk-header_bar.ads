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
--  GtkHeaderBar is similar to a horizontal Gtk.Box.Gtk_Box. It allows
--  children to be placed at the start or the end. In addition, it allows a
--  title and subtitle to be displayed. The title will be centered with respect
--  to the width of the box, even if the children at either side take up
--  different amounts of space. The height of the titlebar will be set to
--  provide sufficient space for the subtitle, even if none is currently set.
--  If a subtitle is not needed, the space reservation can be turned off with
--  Gtk.Header_Bar.Set_Has_Subtitle.
--
--  GtkHeaderBar can add typical window frame controls, such as minimize,
--  maximize and close buttons, or the window icon.
--
--  For these reasons, GtkHeaderBar is the natural choice for use as the
--  custom titlebar widget of a Gtk.Window.Gtk_Window (see
--  Gtk.Window.Set_Titlebar), as it gives features typical of titlebars while
--  allowing the addition of child widgets.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Header_Bar is

   type Gtk_Header_Bar_Record is new Gtk_Container_Record with null record;
   type Gtk_Header_Bar is access all Gtk_Header_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Header_Bar);
   procedure Initialize (Self : not null access Gtk_Header_Bar_Record'Class);
   --  Creates a new Gtk.Header_Bar.Gtk_Header_Bar widget.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Header_Bar_New return Gtk_Header_Bar;
   --  Creates a new Gtk.Header_Bar.Gtk_Header_Bar widget.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_header_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Custom_Title
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the custom title widget of the header. See
   --  Gtk.Header_Bar.Set_Custom_Title.
   --  Since: gtk+ 3.10

   procedure Set_Custom_Title
      (Self         : not null access Gtk_Header_Bar_Record;
       Title_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets a custom title for the Gtk.Header_Bar.Gtk_Header_Bar.
   --  The title should help a user identify the current view. This supersedes
   --  any title set by Gtk.Header_Bar.Set_Title or
   --  Gtk.Header_Bar.Set_Subtitle. To achieve the same style as the builtin
   --  title and subtitle, use the "title" and "subtitle" style classes.
   --  You should set the custom title to null, for the header title label to
   --  be visible again.
   --  Since: gtk+ 3.10
   --  "title_widget": a custom widget to use for a title

   function Get_Decoration_Layout
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String;
   --  Gets the decoration layout set with
   --  Gtk.Header_Bar.Set_Decoration_Layout.
   --  Since: gtk+ 3.12

   procedure Set_Decoration_Layout
      (Self   : not null access Gtk_Header_Bar_Record;
       Layout : UTF8_String := "");
   --  Sets the decoration layout for this header bar, overriding the
   --  Gtk.Settings.Gtk_Settings:gtk-decoration-layout setting.
   --  There can be valid reasons for overriding the setting, such as a header
   --  bar design that does not allow for buttons to take room on the right, or
   --  only offers room for a single close button. Split header bars are
   --  another example for overriding the setting.
   --  The format of the string is button names, separated by commas. A colon
   --  separates the buttons that should appear on the left from those on the
   --  right. Recognized button names are minimize, maximize, close, icon (the
   --  window icon) and menu (a menu button for the fallback app menu).
   --  For example, "menu:minimize,maximize,close" specifies a menu on the
   --  left, and minimize, maximize and close buttons on the right.
   --  Since: gtk+ 3.12
   --  "layout": a decoration layout, or null to unset the layout

   function Get_Has_Subtitle
      (Self : not null access Gtk_Header_Bar_Record) return Boolean;
   --  Retrieves whether the header bar reserves space for a subtitle,
   --  regardless if one is currently set or not.
   --  Since: gtk+ 3.12

   procedure Set_Has_Subtitle
      (Self    : not null access Gtk_Header_Bar_Record;
       Setting : Boolean);
   --  Sets whether the header bar should reserve space for a subtitle, even
   --  if none is currently set.
   --  Since: gtk+ 3.12
   --  "setting": True to reserve space for a subtitle

   function Get_Show_Close_Button
      (Self : not null access Gtk_Header_Bar_Record) return Boolean;
   --  Returns whether this header bar shows the standard window decorations.
   --  Since: gtk+ 3.10

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Header_Bar_Record;
       Setting : Boolean);
   --  Sets whether this header bar shows the standard window decorations,
   --  including close, maximize, and minimize.
   --  Since: gtk+ 3.10
   --  "setting": True to show standard window decorations

   function Get_Subtitle
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String;
   --  Retrieves the subtitle of the header. See Gtk.Header_Bar.Set_Subtitle.
   --  Since: gtk+ 3.10

   procedure Set_Subtitle
      (Self     : not null access Gtk_Header_Bar_Record;
       Subtitle : UTF8_String := "");
   --  Sets the subtitle of the Gtk.Header_Bar.Gtk_Header_Bar. The title
   --  should give a user an additional detail to help him identify the current
   --  view.
   --  Note that GtkHeaderBar by default reserves room for the subtitle, even
   --  if none is currently set. If this is not desired, set the
   --  Gtk.Header_Bar.Gtk_Header_Bar:has-subtitle property to False.
   --  Since: gtk+ 3.10
   --  "subtitle": a subtitle, or null

   function Get_Title
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String;
   --  Retrieves the title of the header. See Gtk.Header_Bar.Set_Title.
   --  Since: gtk+ 3.10

   procedure Set_Title
      (Self  : not null access Gtk_Header_Bar_Record;
       Title : UTF8_String := "");
   --  Sets the title of the Gtk.Header_Bar.Gtk_Header_Bar. The title should
   --  help a user identify the current view. A good title should not include
   --  the application name.
   --  Since: gtk+ 3.10
   --  "title": a title, or null

   procedure Pack_End
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Child to Bar, packed with reference to the end of the Bar.
   --  Since: gtk+ 3.10
   --  "child": the Gtk.Widget.Gtk_Widget to be added to Bar

   procedure Pack_Start
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Child to Bar, packed with reference to the start of the Bar.
   --  Since: gtk+ 3.10
   --  "child": the Gtk.Widget.Gtk_Widget to be added to Bar

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Custom_Title_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Decoration_Layout_Property : constant Glib.Properties.Property_String;
   --  The decoration layout for buttons. If this property is not set, the
   --  Gtk.Settings.Gtk_Settings:gtk-decoration-layout setting is used.
   --
   --  See Gtk.Header_Bar.Set_Decoration_Layout for information about the
   --  format of this string.

   Decoration_Layout_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Set to True if Gtk.Header_Bar.Gtk_Header_Bar:decoration-layout is set.

   Has_Subtitle_Property : constant Glib.Properties.Property_Boolean;
   --  If True, reserve space for a subtitle, even if none is currently set.

   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show window decorations.
   --
   --  Which buttons are actually shown and where is determined by the
   --  Gtk.Header_Bar.Gtk_Header_Bar:decoration-layout property, and by the
   --  state of the window (e.g. a close button will not be shown if the window
   --  can't be closed).

   Spacing_Property : constant Glib.Properties.Property_Int;

   Subtitle_Property : constant Glib.Properties.Property_String;

   Title_Property : constant Glib.Properties.Property_String;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Header_Bar_Record, Gtk_Header_Bar);
   function "+"
     (Widget : access Gtk_Header_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Header_Bar
   renames Implements_Gtk_Buildable.To_Object;

private
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Subtitle_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("subtitle");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-close-button");
   Has_Subtitle_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-subtitle");
   Decoration_Layout_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("decoration-layout-set");
   Decoration_Layout_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("decoration-layout");
   Custom_Title_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("custom-title");
end Gtk.Header_Bar;
