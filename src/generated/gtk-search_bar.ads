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

--  Reveals a search entry when search is started.
--
--  <picture> <source srcset="search-bar-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkSearchBar"
--  src="search-bar.png"> </picture>
--  It can also contain additional widgets, such as drop-down menus, or
--  buttons. The search bar would appear when a search is started through
--  typing on the keyboard, or the application's search mode is toggled on.
--
--  For keyboard presses to start a search, the search bar must be told of a
--  widget to capture key events from through
--  [methodGtk.SearchBar.set_key_capture_widget]. This widget will typically be
--  the top-level window, or a parent container of the search bar. Common
--  shortcuts such as Ctrl+F should be handled as an application action, or
--  through the menu items.
--
--  You will also need to tell the search bar about which entry you are using
--  as your search entry using [methodGtk.SearchBar.connect_entry].
--
--  ## Creating a search bar
--
--  The following example shows you how to create a more complex search entry.
--
--  [A simple
--  example](https://gitlab.gnome.org/GNOME/gtk/tree/main/examples/search-bar.c)
--
--  # Shortcuts and Gestures
--
--  `GtkSearchBar` supports the following keyboard shortcuts:
--
--  - <kbd>Escape</kbd> hides the search bar.
--
--  # CSS nodes
--
--  ``` searchbar ╰── revealer ╰── box ├── [child] ╰── [button.close] ```
--
--  `GtkSearchBar` has a main CSS node with name searchbar. It has a child
--  node with name revealer that contains a node with name box. The box node
--  contains both the CSS node of the child widget as well as an optional
--  button node which gets the .close style class applied.
--
--  # Accessibility
--
--  `GtkSearchBar` uses the [enumGtk.AccessibleRole.search] role.
--
--  <group>Numeric/Text Data Entry</group>
--  <gtkada_demo>create_entry.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Search_Bar is

   type Gtk_Search_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Search_Bar is access all Gtk_Search_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Search_Bar);
   procedure Initialize (Self : not null access Gtk_Search_Bar_Record'Class);
   --  Creates a `GtkSearchBar`.
   --  You will need to tell it about which widget is going to be your text
   --  entry using [methodGtk.SearchBar.connect_entry].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Search_Bar_New return Gtk_Search_Bar;
   --  Creates a `GtkSearchBar`.
   --  You will need to tell it about which widget is going to be your text
   --  entry using [methodGtk.SearchBar.connect_entry].

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_search_bar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Connect_Entry
      (Self   : not null access Gtk_Search_Bar_Record;
       GEntry : Gtk.Editable.Gtk_Editable);
   --  Connects the `GtkEditable` widget passed as the one to be used in this
   --  search bar.
   --  The entry should be a descendant of the search bar. Calling this
   --  function manually is only required if the entry isn't the direct child
   --  of the search bar (as in our main example).
   --  @param GEntry a `GtkEditable`

   function Get_Child
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Bar.
   --  @return the child widget of Bar
   --  Return has transfer-ownership='none'

   procedure Set_Child
      (Self  : not null access Gtk_Search_Bar_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Bar.
   --  @param Child the child widget

   function Get_Key_Capture_Widget
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the widget that Bar is capturing key events from.
   --  @return The key capture widget.
   --  Return has transfer-ownership='none'

   procedure Set_Key_Capture_Widget
      (Self   : not null access Gtk_Search_Bar_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets Widget as the widget that Bar will capture key events from.
   --  If key events are handled by the search bar, the bar will be shown, and
   --  the entry populated with the entered text.
   --  Note that despite the name of this function, the events are only
   --  'captured' in the bubble phase, which means that editable child widgets
   --  of Widget will receive text input before it gets captured. If that is
   --  not desired, you can capture and forward the events yourself with
   --  [methodGtk.EventControllerKey.forward].
   --  @param Widget a `GtkWidget`

   function Get_Search_Mode
      (Self : not null access Gtk_Search_Bar_Record) return Boolean;
   --  Returns whether the search mode is on or off.
   --  @return whether search mode is toggled on

   procedure Set_Search_Mode
      (Self        : not null access Gtk_Search_Bar_Record;
       Search_Mode : Boolean);
   --  Switches the search mode on or off.
   --  @param Search_Mode the new state of the search mode

   function Get_Show_Close_Button
      (Self : not null access Gtk_Search_Bar_Record) return Boolean;
   --  Returns whether the close button is shown.
   --  @return whether the close button is shown

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Search_Bar_Record;
       Visible : Boolean);
   --  Shows or hides the close button.
   --  Applications that already have a "search" toggle button should not show
   --  a close button in their search bar, as it duplicates the role of the
   --  toggle button.
   --  @param Visible whether the close button will be shown or not

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Search_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Search_Bar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Search_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Search_Bar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Search_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Search_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Search_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Search_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Search_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Search_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Search_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Key_Capture_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The key capture widget.

   Search_Mode_Enabled_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the search mode is on and the search bar shown.

   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to show the close button in the search bar.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Search_Bar_Record, Gtk_Search_Bar);
   function "+"
     (Widget : access Gtk_Search_Bar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Search_Bar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Search_Bar_Record, Gtk_Search_Bar);
   function "+"
     (Widget : access Gtk_Search_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Search_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Search_Bar_Record, Gtk_Search_Bar);
   function "+"
     (Widget : access Gtk_Search_Bar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Search_Bar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-close-button");
   Search_Mode_Enabled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("search-mode-enabled");
   Key_Capture_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("key-capture-widget");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Search_Bar;
