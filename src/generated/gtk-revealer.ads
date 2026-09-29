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

--  Animates the transition of its child from invisible to visible.
--
--  The style of transition can be controlled with
--  [methodGtk.Revealer.set_transition_type].
--
--  These animations respect the [propertyGtk.Settings:gtk-enable-animations]
--  setting.
--
--  # CSS nodes
--
--  `GtkRevealer` has a single CSS node with name revealer. When styling
--  `GtkRevealer` using CSS, remember that it only hides its contents, not
--  itself. That means applied margin, padding and borders will be visible even
--  when the [propertyGtk.Revealer:reveal-child] property is set to False.
--
--  # Accessibility
--
--  `GtkRevealer` uses the [enumGtk.AccessibleRole.group] role.
--
--  The child of `GtkRevealer`, if set, is always available in the
--  accessibility tree, regardless of the state of the revealer widget.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_revealer.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Revealer is

   type Gtk_Revealer_Record is new Gtk_Widget_Record with null record;
   type Gtk_Revealer is access all Gtk_Revealer_Record'Class;

   type Gtk_Revealer_Transition_Type is (
      None,
      Crossfade,
      Slide_Right,
      Slide_Left,
      Slide_Up,
      Slide_Down,
      Swing_Right,
      Swing_Left,
      Swing_Up,
      Swing_Down,
      Fade_Slide_Right,
      Fade_Slide_Left,
      Fade_Slide_Up,
      Fade_Slide_Down);
   pragma Convention (C, Gtk_Revealer_Transition_Type);
   --  These enumeration values describe the possible transitions when the
   --  child of a `GtkRevealer` widget is shown or hidden.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Revealer_Transition_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Revealer_Transition_Type);
   type Property_Gtk_Revealer_Transition_Type is new Gtk_Revealer_Transition_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Revealer);
   procedure Initialize (Self : not null access Gtk_Revealer_Record'Class);
   --  Creates a new `GtkRevealer`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Revealer_New return Gtk_Revealer;
   --  Creates a new `GtkRevealer`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_revealer_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Revealer_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Revealer.
   --  @return the child widget of Revealer. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Revealer_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Revealer.
   --  @param Child the child widget

   function Get_Child_Revealed
      (Self : not null access Gtk_Revealer_Record) return Boolean;
   --  Returns whether the child is fully revealed.
   --  In other words, this returns whether the transition to the revealed
   --  state is completed.
   --  @return True if the child is fully revealed

   function Get_Reveal_Child
      (Self : not null access Gtk_Revealer_Record) return Boolean;
   --  Returns whether the child is currently revealed.
   --  This function returns True as soon as the transition is to the revealed
   --  state is started. To learn whether the child is fully revealed (ie the
   --  transition is completed), use [methodGtk.Revealer.get_child_revealed].
   --  @return True if the child is revealed.

   procedure Set_Reveal_Child
      (Self         : not null access Gtk_Revealer_Record;
       Reveal_Child : Boolean);
   --  Tells the `GtkRevealer` to reveal or conceal its child.
   --  The transition will be animated with the current transition type of
   --  Revealer.
   --  @param Reveal_Child True to reveal the child

   function Get_Transition_Duration
      (Self : not null access Gtk_Revealer_Record) return Guint;
   --  Returns the amount of time (in milliseconds) that transitions will
   --  take.
   --  @return the transition duration

   procedure Set_Transition_Duration
      (Self     : not null access Gtk_Revealer_Record;
       Duration : Guint);
   --  Sets the duration that transitions will take.
   --  @param Duration the new duration, in milliseconds

   function Get_Transition_Type
      (Self : not null access Gtk_Revealer_Record)
       return Gtk_Revealer_Transition_Type;
   --  Gets the type of animation that will be used for transitions in
   --  Revealer.
   --  @return the current transition type of Revealer

   procedure Set_Transition_Type
      (Self       : not null access Gtk_Revealer_Record;
       Transition : Gtk_Revealer_Transition_Type);
   --  Sets the type of animation that will be used for transitions in
   --  Revealer.
   --  Available types include various kinds of fades and slides.
   --  @param Transition the new transition type

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Revealer_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Revealer_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Revealer_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Revealer_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Revealer_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Revealer_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Revealer_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Revealer_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Revealer_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Revealer_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Revealer_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Revealer_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Revealer_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Revealer_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Revealer_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Child_Revealed_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the child is revealed and the animation target reached.

   Reveal_Child_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the revealer should reveal the child.

   Transition_Duration_Property : constant Glib.Properties.Property_Uint;
   --  The animation duration, in milliseconds.

   Transition_Type_Property : constant Gtk.Revealer.Property_Gtk_Revealer_Transition_Type;
   --  Type: Gtk_Revealer_Transition_Type
   --  The type of animation used to transition.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Revealer_Record, Gtk_Revealer);
   function "+"
     (Widget : access Gtk_Revealer_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Revealer
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Revealer_Record, Gtk_Revealer);
   function "+"
     (Widget : access Gtk_Revealer_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Revealer
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Revealer_Record, Gtk_Revealer);
   function "+"
     (Widget : access Gtk_Revealer_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Revealer
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Transition_Type_Property : constant Gtk.Revealer.Property_Gtk_Revealer_Transition_Type :=
     Gtk.Revealer.Build ("transition-type");
   Transition_Duration_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("transition-duration");
   Reveal_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reveal-child");
   Child_Revealed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("child-revealed");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Revealer;
