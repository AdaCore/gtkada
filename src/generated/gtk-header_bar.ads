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

--  Creates a custom titlebar for a window.
--
--  <picture> <source srcset="headerbar-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkHeaderBar"
--  src="headerbar.png"> </picture>
--  `GtkHeaderBar` is similar to a horizontal `GtkCenterBox`. It allows
--  children to be placed at the start or the end. In addition, it allows the
--  window title to be displayed. The title will be centered with respect to
--  the width of the box, even if the children at either side take up different
--  amounts of space.
--
--  `GtkHeaderBar` can add typical window frame controls, such as minimize,
--  maximize and close buttons, or the window icon.
--
--  For these reasons, `GtkHeaderBar` is the natural choice for use as the
--  custom titlebar widget of a `GtkWindow` (see
--  [methodGtk.Window.set_titlebar]), as it gives features typical of titlebars
--  while allowing the addition of child widgets.
--
--  ## GtkHeaderBar as GtkBuildable
--
--  The `GtkHeaderBar` implementation of the `GtkBuildable` interface supports
--  adding children at the start or end sides by specifying "start" or "end" as
--  the "type" attribute of a `<child>` element, or setting the title widget by
--  specifying "title" value.
--
--  By default the `GtkHeaderBar` uses a `GtkLabel` displaying the title of
--  the window it is contained in as the title widget, equivalent to the
--  following UI definition:
--
--  ```xml <object class="GtkHeaderBar"> <property name="title-widget">
--  <object class="GtkLabel"> <property name="label"
--  translatable="yes">Label</property> <property
--  name="single-line-mode">True</property> <property
--  name="ellipsize">end</property> <property name="width-chars">5</property>
--  <style> <class name="title"/> </style> </object> </property> </object> ```
--
--  # CSS nodes
--
--  ``` headerbar ╰── windowhandle ╰── box ├── box.start │ ├──
--  windowcontrols.start │ ╰── [other children] ├── [Title Widget] ╰── box.end
--  ├── [other children] ╰── windowcontrols.end ```
--
--  A `GtkHeaderBar`'s CSS node is called `headerbar`. It contains a
--  `windowhandle` subnode, which contains a `box` subnode, which contains two
--  `box` subnodes at the start and end of the header bar, as well as a center
--  node that represents the title.
--
--  Each of the boxes contains a `windowcontrols` subnode, see
--  [classGtk.WindowControls] for details, as well as other children.
--
--  # Accessibility
--
--  `GtkHeaderBar` uses the [enumGtk.AccessibleRole.group] role.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_header_bar.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Header_Bar is

   type Gtk_Header_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Header_Bar is access all Gtk_Header_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Header_Bar);
   procedure Initialize (Self : not null access Gtk_Header_Bar_Record'Class);
   --  Creates a new `GtkHeaderBar` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Header_Bar_New return Gtk_Header_Bar;
   --  Creates a new `GtkHeaderBar` widget.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_header_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Decoration_Layout
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String;
   --  Gets the decoration layout of the header bar.
   --  @return the decoration layout

   procedure Set_Decoration_Layout
      (Self   : not null access Gtk_Header_Bar_Record;
       Layout : UTF8_String := "");
   --  Sets the decoration layout for this header bar.
   --  This property overrides the
   --  [propertyGtk.Settings:gtk-decoration-layout] setting.
   --  There can be valid reasons for overriding the setting, such as a header
   --  bar design that does not allow for buttons to take room on the right, or
   --  only offers room for a single close button. Split header bars are
   --  another example for overriding the setting.
   --  The format of the string is button names, separated by commas. A colon
   --  separates the buttons that should appear on the left from those on the
   --  right. Recognized button names are minimize, maximize, close and icon
   --  (the window icon).
   --  For example, "icon:minimize,maximize,close" specifies an icon on the
   --  left, and minimize, maximize and close buttons on the right.
   --  @param Layout a decoration layout

   function Get_Show_Title_Buttons
      (Self : not null access Gtk_Header_Bar_Record) return Boolean;
   --  Returns whether this header bar shows the standard window title
   --  buttons.
   --  @return true if title buttons are shown

   procedure Set_Show_Title_Buttons
      (Self    : not null access Gtk_Header_Bar_Record;
       Setting : Boolean);
   --  Sets whether this header bar shows the standard window title buttons.
   --  @param Setting true to show standard title buttons

   function Get_Title_Widget
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the title widget of the header bar.
   --  See [methodGtk.HeaderBar.set_title_widget].
   --  @return the title widget. Has transfer-ownership='none'.

   procedure Set_Title_Widget
      (Self         : not null access Gtk_Header_Bar_Record;
       Title_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the title for the header bar.
   --  When set to `NULL`, the headerbar will display the title of the window
   --  it is contained in.
   --  The title should help a user identify the current view. To achieve the
   --  same style as the builtin title, use the "title" style class.
   --  You should set the title widget to `NULL`, for the window title label
   --  to be visible again.
   --  @param Title_Widget a widget to use for a title

   function Get_Use_Native_Controls
      (Self : not null access Gtk_Header_Bar_Record) return Boolean;
   --  Returns whether this header bar shows platform native window controls.
   --  Since: gtk+ 4.18
   --  @return true if native window controls are shown

   procedure Set_Use_Native_Controls
      (Self    : not null access Gtk_Header_Bar_Record;
       Setting : Boolean);
   --  Sets whether this header bar shows native window controls.
   --  This option shows the "stoplight" buttons on macOS. For Linux, this
   --  option has no effect.
   --  See also [Using GTK on Apple macOS](osx.html?native-window-controls).
   --  Since: gtk+ 4.18
   --  @param Setting true to show native window controls

   procedure Pack_End
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a child to the header bar, packed with reference to the end.
   --  @param Child the widget to be added to Bar

   procedure Pack_Start
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a child to the header bar, packed with reference to the start.
   --  @param Child the widget to be added to Bar

   procedure Remove
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child from the header bar.
   --  The child must have been added with [methodGtk.HeaderBar.pack_start],
   --  [methodGtk.HeaderBar.pack_end] or
   --  [methodGtk.HeaderBar.set_title_widget].
   --  @param Child the child to remove

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Header_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Header_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Header_Bar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Header_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Header_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Header_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Header_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Header_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Header_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Decoration_Layout_Property : constant Glib.Properties.Property_String;
   --  The decoration layout for buttons.
   --
   --  If this property is not set, the
   --  [propertyGtk.Settings:gtk-decoration-layout] setting is used.

   Show_Title_Buttons_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show title buttons like close, minimize, maximize.
   --
   --  Which buttons are actually shown and where is determined by the
   --  [propertyGtk.HeaderBar:decoration-layout] property, and by the state of
   --  the window (e.g. a close button will not be shown if the window can't be
   --  closed).

   Title_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The title widget to display.

   Use_Native_Controls_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show platform native close/minimize/maximize buttons.
   --
   --  For macOS, the [propertyGtk.HeaderBar:decoration-layout] property can
   --  be used to enable/disable controls.
   --
   --  On Linux, this option has no effect.
   --
   --  See also [Using GTK on Apple macOS](osx.html?native-window-controls).

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Header_Bar_Record, Gtk_Header_Bar);
   function "+"
     (Widget : access Gtk_Header_Bar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Header_Bar
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Header_Bar_Record, Gtk_Header_Bar);
   function "+"
     (Widget : access Gtk_Header_Bar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Header_Bar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Native_Controls_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-native-controls");
   Title_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("title-widget");
   Show_Title_Buttons_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-title-buttons");
   Decoration_Layout_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("decoration-layout");
end Gtk.Header_Bar;
