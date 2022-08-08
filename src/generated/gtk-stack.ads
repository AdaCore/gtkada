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
--  The GtkStack widget is a container which only shows one of its children at
--  a time. In contrast to GtkNotebook, GtkStack does not provide a means for
--  users to change the visible child. Instead, the
--  Gtk.Stack_Switcher.Gtk_Stack_Switcher widget can be used with GtkStack to
--  provide this functionality.
--
--  Transitions between pages can be animated as slides or fades. This can be
--  controlled with Gtk.Stack.Set_Transition_Type. These animations respect the
--  Gtk.Settings.Gtk_Settings:gtk-enable-animations setting.
--
--  The GtkStack widget was added in GTK+ 3.10.
--
--  # CSS nodes
--
--  GtkStack has a single CSS node named stack.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Container;           use Gtk.Container;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Stack is

   type Gtk_Stack_Record is new Gtk_Container_Record with null record;
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
      Stack_Transition_Type_Over_Right_Left);
   pragma Convention (C, Gtk_Stack_Transition_Type);
   --  These enumeration values describe the possible transitions between
   --  pages in a Gtk.Stack.Gtk_Stack widget.
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
   --  Creates a new Gtk.Stack.Gtk_Stack container.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Stack_New return Gtk_Stack;
   --  Creates a new Gtk.Stack.Gtk_Stack container.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_stack_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Named
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Name  : UTF8_String);
   --  Adds a child to Stack. The child is identified by the Name.
   --  Since: gtk+ 3.10
   --  "child": the widget to add
   --  "name": the name for Child

   procedure Add_Titled
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Name  : UTF8_String;
       Title : UTF8_String);
   --  Adds a child to Stack. The child is identified by the Name. The Title
   --  will be used by Gtk.Stack_Switcher.Gtk_Stack_Switcher to represent Child
   --  in a tab bar, so it should be short.
   --  Since: gtk+ 3.10
   --  "child": the widget to add
   --  "name": the name for Child
   --  "title": a human-readable title for Child

   function Get_Child_By_Name
      (Self : not null access Gtk_Stack_Record;
       Name : UTF8_String) return Gtk.Widget.Gtk_Widget;
   --  Finds the child of the Gtk.Stack.Gtk_Stack with the name given as the
   --  argument. Returns null if there is no child with this name.
   --  Since: gtk+ 3.12
   --  "name": the name of the child to find

   function Get_Hhomogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Gets whether Stack is horizontally homogeneous. See
   --  Gtk.Stack.Set_Hhomogeneous.
   --  Since: gtk+ 3.16

   procedure Set_Hhomogeneous
      (Self         : not null access Gtk_Stack_Record;
       Hhomogeneous : Boolean);
   --  Sets the Gtk.Stack.Gtk_Stack to be horizontally homogeneous or not. If
   --  it is homogeneous, the Gtk.Stack.Gtk_Stack will request the same width
   --  for all its children. If it isn't, the stack may change width when a
   --  different child becomes visible.
   --  Since: gtk+ 3.16
   --  "hhomogeneous": True to make Stack horizontally homogeneous

   function Get_Homogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Gets whether Stack is homogeneous. See Gtk.Stack.Set_Homogeneous.
   --  Since: gtk+ 3.10

   procedure Set_Homogeneous
      (Self        : not null access Gtk_Stack_Record;
       Homogeneous : Boolean);
   --  Sets the Gtk.Stack.Gtk_Stack to be homogeneous or not. If it is
   --  homogeneous, the Gtk.Stack.Gtk_Stack will request the same size for all
   --  its children. If it isn't, the stack may change size when a different
   --  child becomes visible.
   --  Since 3.16, homogeneity can be controlled separately for horizontal and
   --  vertical size, with the Gtk.Stack.Gtk_Stack:hhomogeneous and
   --  Gtk.Stack.Gtk_Stack:vhomogeneous.
   --  Since: gtk+ 3.10
   --  "homogeneous": True to make Stack homogeneous

   function Get_Interpolate_Size
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Returns wether the Gtk.Stack.Gtk_Stack is set up to interpolate between
   --  the sizes of children on page switch.
   --  Since: gtk+ 3.18

   procedure Set_Interpolate_Size
      (Self             : not null access Gtk_Stack_Record;
       Interpolate_Size : Boolean);
   --  Sets whether or not Stack will interpolate its size when changing the
   --  visible child. If the Gtk.Stack.Gtk_Stack:interpolate-size property is
   --  set to True, Stack will interpolate its size between the current one and
   --  the one it'll take after changing the visible child, according to the
   --  set transition duration.
   --  Since: gtk+ 3.18
   --  "interpolate_size": the new value

   function Get_Transition_Duration
      (Self : not null access Gtk_Stack_Record) return Guint;
   --  Returns the amount of time (in milliseconds) that transitions between
   --  pages in Stack will take.
   --  Since: gtk+ 3.10

   procedure Set_Transition_Duration
      (Self     : not null access Gtk_Stack_Record;
       Duration : Guint);
   --  Sets the duration that transitions between pages in Stack will take.
   --  Since: gtk+ 3.10
   --  "duration": the new duration, in milliseconds

   function Get_Transition_Running
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Returns whether the Stack is currently in a transition from one page to
   --  another.
   --  Since: gtk+ 3.12

   function Get_Transition_Type
      (Self : not null access Gtk_Stack_Record)
       return Gtk_Stack_Transition_Type;
   --  Gets the type of animation that will be used for transitions between
   --  pages in Stack.
   --  Since: gtk+ 3.10

   procedure Set_Transition_Type
      (Self       : not null access Gtk_Stack_Record;
       Transition : Gtk_Stack_Transition_Type);
   --  Sets the type of animation that will be used for transitions between
   --  pages in Stack. Available types include various kinds of fades and
   --  slides.
   --  The transition type can be changed without problems at runtime, so it
   --  is possible to change the animation based on the page that is about to
   --  become current.
   --  Since: gtk+ 3.10
   --  "transition": the new transition type

   function Get_Vhomogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean;
   --  Gets whether Stack is vertically homogeneous. See
   --  Gtk.Stack.Set_Vhomogeneous.
   --  Since: gtk+ 3.16

   procedure Set_Vhomogeneous
      (Self         : not null access Gtk_Stack_Record;
       Vhomogeneous : Boolean);
   --  Sets the Gtk.Stack.Gtk_Stack to be vertically homogeneous or not. If it
   --  is homogeneous, the Gtk.Stack.Gtk_Stack will request the same height for
   --  all its children. If it isn't, the stack may change height when a
   --  different child becomes visible.
   --  Since: gtk+ 3.16
   --  "vhomogeneous": True to make Stack vertically homogeneous

   function Get_Visible_Child
      (Self : not null access Gtk_Stack_Record) return Gtk.Widget.Gtk_Widget;
   --  Gets the currently visible child of Stack, or null if there are no
   --  visible children.
   --  Since: gtk+ 3.10

   procedure Set_Visible_Child
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Makes Child the visible child of Stack.
   --  If Child is different from the currently visible child, the transition
   --  between the two will be animated with the current transition type of
   --  Stack.
   --  Note that the Child widget has to be visible itself (see
   --  Gtk.Widget.Show) in order to become the visible child of Stack.
   --  Since: gtk+ 3.10
   --  "child": a child of Stack

   function Get_Visible_Child_Name
      (Self : not null access Gtk_Stack_Record) return UTF8_String;
   --  Returns the name of the currently visible child of Stack, or null if
   --  there is no visible child.
   --  Since: gtk+ 3.10

   procedure Set_Visible_Child_Name
      (Self : not null access Gtk_Stack_Record;
       Name : UTF8_String);
   --  Makes the child with the given name visible.
   --  If Child is different from the currently visible child, the transition
   --  between the two will be animated with the current transition type of
   --  Stack.
   --  Note that the child widget has to be visible itself (see
   --  Gtk.Widget.Show) in order to become the visible child of Stack.
   --  Since: gtk+ 3.10
   --  "name": the name of the child to make visible

   procedure Set_Visible_Child_Full
      (Self       : not null access Gtk_Stack_Record;
       Name       : UTF8_String;
       Transition : Gtk_Stack_Transition_Type);
   --  Makes the child with the given name visible.
   --  Note that the child widget has to be visible itself (see
   --  Gtk.Widget.Show) in order to become the visible child of Stack.
   --  Since: gtk+ 3.10
   --  "name": the name of the child to make visible
   --  "transition": the transition type to use

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Hhomogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  True if the stack allocates the same width for all children.

   Homogeneous_Property : constant Glib.Properties.Property_Boolean;

   Interpolate_Size_Property : constant Glib.Properties.Property_Boolean;

   Transition_Duration_Property : constant Glib.Properties.Property_Uint;

   Transition_Running_Property : constant Glib.Properties.Property_Boolean;

   Transition_Type_Property : constant Gtk.Stack.Property_Gtk_Stack_Transition_Type;
   --  Type: Gtk_Stack_Transition_Type

   Vhomogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  True if the stack allocates the same height for all children.

   Visible_Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Visible_Child_Name_Property : constant Glib.Properties.Property_String;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

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
   Interpolate_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("interpolate-size");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   Hhomogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hhomogeneous");
end Gtk.Stack;
