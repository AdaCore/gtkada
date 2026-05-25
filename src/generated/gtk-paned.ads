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

--  Arranges its children in two panes, horizontally or vertically.
--
--  <picture> <source srcset="panes-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkPaned" src="panes.png"> </picture>
--  The division between the two panes is adjustable by the user by dragging a
--  handle.
--
--  Child widgets are added to the panes of the widget with
--  [methodGtk.Paned.set_start_child] and [methodGtk.Paned.set_end_child]. The
--  division between the two children is set by default from the size requests
--  of the children, but it can be adjusted by the user.
--
--  A paned widget draws a separator between the two child widgets and a small
--  handle that the user can drag to adjust the division. It does not draw any
--  relief around the children or around the separator. (The space in which the
--  separator is called the gutter.) Often, it is useful to put each child
--  inside a [classGtk.Frame] so that the gutter appears as a ridge. No
--  separator is drawn if one of the children is missing.
--
--  Each child has two options that can be set, "resize" and "shrink". If
--  "resize" is true then, when the `GtkPaned` is resized, that child will
--  expand or shrink along with the paned widget. If "shrink" is true, then
--  that child can be made smaller than its requisition by the user. Setting
--  "shrink" to false allows the application to set a minimum size. If "resize"
--  is false for both children, then this is treated as if "resize" is true for
--  both children.
--
--  The application can set the position of the slider as if it were set by
--  the user, by calling [methodGtk.Paned.set_position].
--
--  # Shortcuts and Gestures
--
--  The following signals have default keybindings:
--
--  - [signalGtk.Paned::accept-position] - [signalGtk.Paned::cancel-position]
--  - [signalGtk.Paned::cycle-child-focus] -
--  [signalGtk.Paned::cycle-handle-focus] - [signalGtk.Paned::move-handle] -
--  [signalGtk.Paned::toggle-handle-focus]
--
--  # CSS nodes
--
--  ``` paned ├── <child> ├── separator[.wide] ╰── <child> ```
--
--  `GtkPaned` has a main CSS node with name paned, and a subnode for the
--  separator with name separator. The subnode gets a .wide style class when
--  the paned is supposed to be wide.
--
--  In horizontal orientation, the nodes are arranged based on the text
--  direction, so in left-to-right mode, :first-child will select the leftmost
--  child, while it will select the rightmost child in RTL layouts.
--
--  ## Creating a paned widget with minimum sizes.
--
--  ```c GtkWidget *hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
--  GtkWidget *frame1 = gtk_frame_new (NULL); GtkWidget *frame2 = gtk_frame_new
--  (NULL);
--
--  gtk_widget_set_size_request (hpaned, 200, -1);
--
--  gtk_paned_set_start_child (GTK_PANED (hpaned), frame1);
--  gtk_paned_set_resize_start_child (GTK_PANED (hpaned), TRUE);
--  gtk_paned_set_shrink_start_child (GTK_PANED (hpaned), FALSE);
--  gtk_widget_set_size_request (frame1, 50, -1);
--
--  gtk_paned_set_end_child (GTK_PANED (hpaned), frame2);
--  gtk_paned_set_resize_end_child (GTK_PANED (hpaned), FALSE);
--  gtk_paned_set_shrink_end_child (GTK_PANED (hpaned), FALSE);
--  gtk_widget_set_size_request (frame2, 50, -1); ```
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_paned.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Paned is

   type Gtk_Paned_Record is new Gtk_Widget_Record with null record;
   type Gtk_Paned is access all Gtk_Paned_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Paned       : out Gtk_Paned;
       Orientation : Gtk.Enums.Gtk_Orientation);
   procedure Initialize
      (Paned       : not null access Gtk_Paned_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Creates a new `GtkPaned` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Orientation the paned's orientation.

   function Gtk_Paned_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Paned;
   --  Creates a new `GtkPaned` widget.
   --  @param Orientation the paned's orientation.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_paned_get_type");

   -------------
   -- Methods --
   -------------

   function Get_End_Child
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the end child of the given `GtkPaned`.
   --  @return the end child widget

   procedure Set_End_Child
      (Paned : not null access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the end child of Paned to Child.
   --  If Child is `NULL`, the existing child will be removed.
   --  The end child is the bottom child of a vertical paned, or the right
   --  child of a horizontal paned.
   --  @param Child the widget to add

   function Get_Position
      (Paned : not null access Gtk_Paned_Record) return Glib.Gint;
   --  Obtains the position of the divider between the two panes.
   --  @return the position of the divider, in pixels

   procedure Set_Position
      (Paned    : not null access Gtk_Paned_Record;
       Position : Glib.Gint);
   --  Sets the position of the divider between the two panes.
   --  The position of the divider is expressed in pixels. A negative value
   --  restores the default behaviour, where the position is computed from the
   --  requisitions of the children.
   --  @param Position pixel position of divider, a negative value means that
   --  the position is unset

   function Get_Resize_End_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean;
   --  Returns whether the [propertyGtk.Paned:end-child] can be resized.
   --  @return true if the end child is resizable

   procedure Set_Resize_End_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean);
   --  Sets whether the [propertyGtk.Paned:end-child] can be resized.
   --  @param Resize true to let the end child be resized

   function Get_Resize_Start_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean;
   --  Returns whether the [propertyGtk.Paned:start-child] can be resized.
   --  @return true if the start child is resizable

   procedure Set_Resize_Start_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean);
   --  Sets whether the [propertyGtk.Paned:start-child] can be resized.
   --  @param Resize true to let the start child be resized

   function Get_Shrink_End_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean;
   --  Returns whether the [propertyGtk.Paned:end-child] can shrink.
   --  @return true if the end child is shrinkable

   procedure Set_Shrink_End_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean);
   --  Sets whether the [propertyGtk.Paned:end-child] can shrink.
   --  @param Resize true to let the end child be shrunk

   function Get_Shrink_Start_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean;
   --  Returns whether the [propertyGtk.Paned:start-child] can shrink.
   --  @return true if the start child is shrinkable

   procedure Set_Shrink_Start_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean);
   --  Sets whether the [propertyGtk.Paned:start-child] can shrink.
   --  @param Resize true to let the start child be shrunk

   function Get_Start_Child
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the start child of the given `GtkPaned`.
   --  @return the start child widget

   procedure Set_Start_Child
      (Paned : not null access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the start child of Paned to Child.
   --  If Child is `NULL`, the existing child will be removed.
   --  The start child is the top child of a vertical paned, or the left child
   --  of a horizontal paned.
   --  @param Child the widget to add

   function Get_Wide_Handle
      (Paned : not null access Gtk_Paned_Record) return Boolean;
   --  Gets whether the separator should be wide.
   --  @return True if the paned should have a wide handle

   procedure Set_Wide_Handle
      (Paned : not null access Gtk_Paned_Record;
       Wide  : Boolean);
   --  Sets whether the separator should be wide.
   --  @param Wide the new value for the [propertyGtk.Paned:wide-handle]
   --  property

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Paned_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Paned_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Paned_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Paned_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Paned_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Paned_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Paned_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Paned_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Paned_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Paned_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Paned_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   End_Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The second child.

   Max_Position_Property : constant Glib.Properties.Property_Int;
   --  The largest possible value for the [propertyGtk.Paned:position]
   --  property.
   --
   --  This property is derived from the size and shrinkability of the
   --  widget's children.

   Min_Position_Property : constant Glib.Properties.Property_Int;
   --  The smallest possible value for the [propertyGtk.Paned:position]
   --  property.
   --
   --  This property is derived from the size and shrinkability of the
   --  widget's children.

   Position_Property : constant Glib.Properties.Property_Int;
   --  Position of the separator in pixels, from the left/top.

   Position_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the [propertyGtk.Paned:position] property has been set.

   Resize_End_Child_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the second child expands and shrinks along with the
   --  paned widget.

   Resize_Start_Child_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the first child expands and shrinks along with the
   --  paned widget.

   Shrink_End_Child_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the second child can be made smaller than its
   --  requisition.

   Shrink_Start_Child_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the first child can be made smaller than its
   --  requisition.

   Start_Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The first child.

   Wide_Handle_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `GtkPaned` should provide a stronger visual separation.
   --
   --  For example, this could be set when a paned contains two
   --  [classGtk.Notebook]s, whose tab rows would otherwise merge visually.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Paned_Boolean is not null access function
     (Self : access Gtk_Paned_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Accept_Position : constant Glib.Signal_Name := "accept-position";
   procedure On_Accept_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False);
   procedure On_Accept_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to accept the current position of the handle when moving it
   --  using key bindings.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Return</kbd> or
   --  <kbd>Space</kbd>.
   -- 
   --  Callback parameters:

   Signal_Cancel_Position : constant Glib.Signal_Name := "cancel-position";
   procedure On_Cancel_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False);
   procedure On_Cancel_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to cancel moving the position of the handle using key bindings.
   --
   --  The position of the handle will be reset to the value prior to moving
   --  it.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Escape</kbd>.
   -- 
   --  Callback parameters:

   type Cb_Gtk_Paned_Boolean_Boolean is not null access function
     (Self     : access Gtk_Paned_Record'Class;
      Reversed : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self     : access Glib.Object.GObject_Record'Class;
      Reversed : Boolean) return Boolean;

   Signal_Cycle_Child_Focus : constant Glib.Signal_Name := "cycle-child-focus";
   procedure On_Cycle_Child_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Cycle_Child_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to cycle the focus between the children of the paned.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding is <kbd>F6</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Reversed whether cycling backward or forward

   Signal_Cycle_Handle_Focus : constant Glib.Signal_Name := "cycle-handle-focus";
   procedure On_Cycle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Cycle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to cycle whether the paned should grab focus to allow the user
   --  to change position of the handle by using key bindings.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>F8</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Reversed whether cycling backward or forward

   type Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean is not null access function
     (Self        : access Gtk_Paned_Record'Class;
      Scroll_Type : Gtk.Enums.Gtk_Scroll_Type) return Boolean;

   type Cb_GObject_Gtk_Scroll_Type_Boolean is not null access function
     (Self        : access Glib.Object.GObject_Record'Class;
      Scroll_Type : Gtk.Enums.Gtk_Scroll_Type) return Boolean;

   Signal_Move_Handle : constant Glib.Signal_Name := "move-handle";
   procedure On_Move_Handle
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After : Boolean := False);
   procedure On_Move_Handle
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to move the handle with key bindings.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>←</kbd>,
   --  <kbd>←</kbd>, <kbd>Ctrl</kbd>+<kbd>→</kbd>, <kbd>→</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>↑</kbd>, <kbd>↑</kbd>,
   --  <kbd>Ctrl</kbd>+<kbd>↓</kbd>, <kbd>↓</kbd>, <kbd>PgUp</kbd>,
   --  <kbd>PgDn</kbd>, <kbd>Home</kbd>, <kbd>End</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Scroll_Type a `GtkScrollType`

   Signal_Toggle_Handle_Focus : constant Glib.Signal_Name := "toggle-handle-focus";
   procedure On_Toggle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False);
   procedure On_Toggle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to accept the current position of the handle and then move
   --  focus to the next widget in the focus chain.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding is <kbd>Tab</kbd>.
   -- 
   --  Callback parameters:

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
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Paned
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Paned
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Paned
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Paned
   renames Implements_Gtk_Orientable.To_Object;

private
   Wide_Handle_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wide-handle");
   Start_Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("start-child");
   Shrink_Start_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("shrink-start-child");
   Shrink_End_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("shrink-end-child");
   Resize_Start_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resize-start-child");
   Resize_End_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resize-end-child");
   Position_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("position-set");
   Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("position");
   Min_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-position");
   Max_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-position");
   End_Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("end-child");
end Gtk.Paned;
