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

--  Shows one of its children at a time.
--
--  <picture> <source srcset="stack-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkStack" src="stack.png"> </picture>
--  In contrast to `GtkNotebook`, `GtkStack` does not provide a means for
--  users to change the visible child. Instead, a separate widget such as
--  [classGtk.StackSwitcher] or [classGtk.StackSidebar] can be used with
--  `GtkStack` to provide this functionality.
--
--  Transitions between pages can be animated as slides or fades. This can be
--  controlled with [methodGtk.Stack.set_transition_type]. These animations
--  respect the [propertyGtk.Settings:gtk-enable-animations] setting.
--
--  `GtkStack` maintains a [classGtk.StackPage] object for each added child,
--  which holds additional per-child properties. You obtain the `GtkStackPage`
--  for a child with [methodGtk.Stack.get_page] and you can obtain a
--  `GtkSelectionModel` containing all the pages with
--  [methodGtk.Stack.get_pages].
--
--  # GtkStack as GtkBuildable
--
--  To set child-specific properties in a .ui file, create `GtkStackPage`
--  objects explicitly, and set the child widget as a property on it:
--
--  ```xml <object class="GtkStack" id="stack"> <child> <object
--  class="GtkStackPage"> <property name="name">page1</property> <property
--  name="title">In the beginning…</property> <property name="child"> <object
--  class="GtkLabel"> <property name="label">It was dark</property> </object>
--  </property> </object> </child> ```
--
--  # CSS nodes
--
--  `GtkStack` has a single CSS node named stack.
--
--  # Accessibility
--
--  `GtkStack` uses the [enumGtk.AccessibleRole.tab_panel] role for the stack
--  pages, which are the accessible parent objects of the child widgets.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_stack.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Selection_Model;     use Gtk.Selection_Model;
with Gtk.Stack_Page;          use Gtk.Stack_Page;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Stack is

   type Gtk_Stack_Record is new Gtk_Widget_Record with null record;
   type Gtk_Stack is access all Gtk_Stack_Record'Class;

   type Gtk_Stack_Transition_Type is (
      Stack_Transition_Type_None,
      Stack_Transition_Type_Crossfade,
      Stack_Transition_Type_Slide_Right,
      Stack_Transition_Type_Slide_Left,
      Stack_Transition_Type_Slide_Up,
      Stack_Transition_Type_Slide_Down,
      Stack_Transition_Type_Slide_Left_Right,
      Stack_Transition_Type_Slide_Up_Down,
      Stack_Transition_Type_Over_Up,
      Stack_Transition_Type_Over_Down,
      Stack_Transition_Type_Over_Left,
      Stack_Transition_Type_Over_Right,
      Stack_Transition_Type_Under_Up,
      Stack_Transition_Type_Under_Down,
      Stack_Transition_Type_Under_Left,
      Stack_Transition_Type_Under_Right,
      Stack_Transition_Type_Over_Up_Down,
      Stack_Transition_Type_Over_Down_Up,
      Stack_Transition_Type_Over_Left_Right,
      Stack_Transition_Type_Over_Right_Left,
      Stack_Transition_Type_Rotate_Left,
      Stack_Transition_Type_Rotate_Right,
      Stack_Transition_Type_Rotate_Left_Right);
   pragma Convention (C, Gtk_Stack_Transition_Type);
   --  Possible transitions between pages in a `GtkStack` widget.
   --
   --  New values may be added to this enumeration over time.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Stack_Transition_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Stack_Transition_Type);
   type Property_Gtk_Stack_Transition_Type is new Gtk_Stack_Transition_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Stack);
   procedure Initialize (Self : not null access Gtk_Stack_Record'Class);
   --  Creates a new `GtkStack`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Stack_New return Gtk_Stack;
   --  Creates a new `GtkStack`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_stack_get_type");

   -------------
   -- Methods --
   -------------

   function Add_Child
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Stack_Page.Gtk_Stack_Page;
   --  Adds a child to Stack.
   --  @param Child the widget to add
   --  @return the `GtkStackPage` for Child. Has transfer-ownership='none'.

   function Add_Named
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Name  : UTF8_String := "") return Gtk.Stack_Page.Gtk_Stack_Page;
   --  Adds a child to Stack.
   --  The child is identified by the Name.
   --  @param Child the widget to add
   --  @param Name the name for Child
   --  @return the `GtkStackPage` for Child. Has transfer-ownership='none'.

   function Add_Titled
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Name  : UTF8_String := "";
       Title : UTF8_String) return Gtk.Stack_Page.Gtk_Stack_Page;
   --  Adds a child to Stack.
   --  The child is identified by the Name. The Title will be used by
   --  `GtkStackSwitcher` to represent Child in a tab bar, so it should be
   --  short.
   --  @param Child the widget to add
   --  @param Name the name for Child
   --  @param Title a human-readable title for Child
   --  @return the `GtkStackPage` for Child. Has transfer-ownership='none'.

   function Get_Child_By_Name
      (Self : not null access Gtk_Stack_Record;
       Name : UTF8_String) return Gtk.Widget.Gtk_Widget;
   --  Finds the child with the name given as the argument.
   --  Returns null if there is no child with this name.
   --  @param Name the name of the child to find
   --  @return the requested child of the `GtkStack`. Has
   --  transfer-ownership='none'.

   function Get_Hhomogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Gets whether Stack is horizontally homogeneous.
   --  @return whether Stack is horizontally homogeneous.

   procedure Set_Hhomogeneous
      (Self         : not null access Gtk_Stack_Record;
       Hhomogeneous : Boolean);
   --  Sets the `GtkStack` to be horizontally homogeneous or not.
   --  If it is homogeneous, the `GtkStack` will request the same width for
   --  all its children. If it isn't, the stack may change width when a
   --  different child becomes visible.
   --  @param Hhomogeneous True to make Stack horizontally homogeneous

   function Get_Interpolate_Size
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Returns whether the `GtkStack` is set up to interpolate between the
   --  sizes of children on page switch.
   --  @return True if child sizes are interpolated

   procedure Set_Interpolate_Size
      (Self             : not null access Gtk_Stack_Record;
       Interpolate_Size : Boolean);
   --  Sets whether or not Stack will interpolate its size when changing the
   --  visible child.
   --  If the [propertyGtk.Stack:interpolate-size] property is set to True,
   --  Stack will interpolate its size between the current one and the one
   --  it'll take after changing the visible child, according to the set
   --  transition duration.
   --  @param Interpolate_Size the new value

   function Get_Page
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Stack_Page.Gtk_Stack_Page;
   --  Returns the `GtkStackPage` object for Child.
   --  @param Child a child of Stack
   --  @return the `GtkStackPage` for Child. Has transfer-ownership='none'.

   function Get_Pages
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model;
   --  Returns a `GListModel` that contains the pages of the stack.
   --  This can be used to keep an up-to-date view. The model also implements
   --  [ifaceGtk.SelectionModel] and can be used to track and modify the
   --  visible page.
   --  The caller owns a reference on the result and must Unref it.
   --  @return a `GtkSelectionModel` for the stack's children

   function Get_Transition_Duration
      (Self : not null access Gtk_Stack_Record) return Guint;
   --  Returns the amount of time (in milliseconds) that transitions between
   --  pages in Stack will take.
   --  @return the transition duration

   procedure Set_Transition_Duration
      (Self     : not null access Gtk_Stack_Record;
       Duration : Guint);
   --  Sets the duration that transitions between pages in Stack will take.
   --  @param Duration the new duration, in milliseconds

   function Get_Transition_Running
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Returns whether the Stack is currently in a transition from one page to
   --  another.
   --  @return True if the transition is currently running, False otherwise.

   function Get_Transition_Type
      (Self : not null access Gtk_Stack_Record)
       return Gtk_Stack_Transition_Type;
   --  Gets the type of animation that will be used for transitions between
   --  pages in Stack.
   --  @return the current transition type of Stack

   procedure Set_Transition_Type
      (Self       : not null access Gtk_Stack_Record;
       Transition : Gtk_Stack_Transition_Type);
   --  Sets the type of animation that will be used for transitions between
   --  pages in Stack.
   --  Available types include various kinds of fades and slides.
   --  The transition type can be changed without problems at runtime, so it
   --  is possible to change the animation based on the page that is about to
   --  become current.
   --  @param Transition the new transition type

   function Get_Vhomogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Gets whether Stack is vertically homogeneous.
   --  @return whether Stack is vertically homogeneous.

   procedure Set_Vhomogeneous
      (Self         : not null access Gtk_Stack_Record;
       Vhomogeneous : Boolean);
   --  Sets the `GtkStack` to be vertically homogeneous or not.
   --  If it is homogeneous, the `GtkStack` will request the same height for
   --  all its children. If it isn't, the stack may change height when a
   --  different child becomes visible.
   --  @param Vhomogeneous True to make Stack vertically homogeneous

   function Get_Visible_Child
      (Self : not null access Gtk_Stack_Record) return Gtk.Widget.Gtk_Widget;
   --  Gets the currently visible child of Stack.
   --  Returns null if there are no visible children.
   --  @return the visible child of the `GtkStack`. Has
   --  transfer-ownership='none'.

   procedure Set_Visible_Child
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Makes Child the visible child of Stack.
   --  If Child is different from the currently visible child, the transition
   --  between the two will be animated with the current transition type of
   --  Stack.
   --  Note that the Child widget has to be visible itself (see
   --  [methodGtk.Widget.show]) in order to become the visible child of Stack.
   --  @param Child a child of Stack

   function Get_Visible_Child_Name
      (Self : not null access Gtk_Stack_Record) return UTF8_String;
   --  Returns the name of the currently visible child of Stack.
   --  Returns null if there is no visible child.
   --  @return the name of the visible child of the `GtkStack`

   procedure Set_Visible_Child_Name
      (Self : not null access Gtk_Stack_Record;
       Name : UTF8_String);
   --  Makes the child with the given name visible.
   --  If Child is different from the currently visible child, the transition
   --  between the two will be animated with the current transition type of
   --  Stack.
   --  Note that the child widget has to be visible itself (see
   --  [methodGtk.Widget.show]) in order to become the visible child of Stack.
   --  @param Name the name of the child to make visible

   procedure Remove
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child widget from Stack.
   --  @param Child the child to remove

   procedure Set_Visible_Child_Full
      (Self       : not null access Gtk_Stack_Record;
       Name       : UTF8_String;
       Transition : Gtk_Stack_Transition_Type);
   --  Makes the child with the given name visible.
   --  Note that the child widget has to be visible itself (see
   --  [methodGtk.Widget.show]) in order to become the visible child of Stack.
   --  @param Name the name of the child to make visible
   --  @param Transition the transition type to use

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Stack_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Stack_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Stack_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Stack_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Stack_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Stack_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Stack_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Stack_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Stack_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Stack_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Hhomogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  True if the stack allocates the same width for all children.

   Interpolate_Size_Property : constant Glib.Properties.Property_Boolean;
   --  Whether or not the size should smoothly change during the transition.

   Pages_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Selection_Model.Gtk_Selection_Model
   --  A selection model with the stack pages.

   Transition_Duration_Property : constant Glib.Properties.Property_Uint;
   --  The animation duration, in milliseconds.

   Transition_Running_Property : constant Glib.Properties.Property_Boolean;
   --  Whether or not the transition is currently running.

   Transition_Type_Property : constant Gtk.Stack.Property_Gtk_Stack_Transition_Type;
   --  Type: Gtk_Stack_Transition_Type
   --  The type of animation used to transition.

   Vhomogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  True if the stack allocates the same height for all children.

   Visible_Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget currently visible in the stack.

   Visible_Child_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the widget currently visible in the stack.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Stack_Record, Gtk_Stack);
   function "+"
     (Widget : access Gtk_Stack_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Stack
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Stack_Record, Gtk_Stack);
   function "+"
     (Widget : access Gtk_Stack_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Stack
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Stack_Record, Gtk_Stack);
   function "+"
     (Widget : access Gtk_Stack_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Stack
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Visible_Child_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("visible-child-name");
   Visible_Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("visible-child");
   Vhomogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("vhomogeneous");
   Transition_Type_Property : constant Gtk.Stack.Property_Gtk_Stack_Transition_Type :=
     Gtk.Stack.Build ("transition-type");
   Transition_Running_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("transition-running");
   Transition_Duration_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("transition-duration");
   Pages_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("pages");
   Interpolate_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("interpolate-size");
   Hhomogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hhomogeneous");
end Gtk.Stack;
