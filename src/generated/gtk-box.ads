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

--  Arranges child widgets into a single row or column.
--
--  <picture> <source srcset="box-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkBox" src="box.png"> </picture>
--  Whether it is a row or column depends on the value of its
--  [propertyGtk.Orientable:orientation] property. Within the other dimension,
--  all children are allocated the same size. The [propertyGtk.Widget:halign]
--  and [propertyGtk.Widget:valign] properties can be used on the children to
--  influence their allocation.
--
--  Use repeated calls to [methodGtk.Box.append] to pack widgets into a
--  `GtkBox` from start to end. Use [methodGtk.Box.remove] to remove widgets
--  from the `GtkBox`. [methodGtk.Box.insert_child_after] can be used to add a
--  child at a particular position.
--
--  Use [methodGtk.Box.set_homogeneous] to specify whether or not all children
--  of the `GtkBox` are forced to get the same amount of space.
--
--  Use [methodGtk.Box.set_spacing] to determine how much space will be
--  minimally placed between all children in the `GtkBox`. Note that spacing is
--  added *between* the children.
--
--  Use [methodGtk.Box.reorder_child_after] to move a child to a different
--  place in the box.
--
--  # CSS nodes
--
--  `GtkBox` uses a single CSS node with name box.
--
--  # Accessibility
--
--  Until GTK 4.10, `GtkBox` used the [enumGtk.AccessibleRole.group] role.
--
--  Starting from GTK 4.12, `GtkBox` uses the [enumGtk.AccessibleRole.generic]
--  role.
--
--  See the testgtk example in the GtkAda distribution to see concrete
--  examples on how all the parameters for the boxes work.
--
--  <screenshot>gtk-box</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_box.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Box is

   type Gtk_Box_Record is new Gtk_Widget_Record with null record;
   type Gtk_Box is access all Gtk_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Box;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Glib.Gint);
   procedure Initialize
      (Self        : not null access Gtk_Box_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Glib.Gint);
   --  Creates a new box.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Orientation the box's orientation
   --  @param Spacing the number of pixels to place between children

   function Gtk_Box_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Glib.Gint) return Gtk_Box;
   --  Creates a new box.
   --  @param Orientation the box's orientation
   --  @param Spacing the number of pixels to place between children

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_box_get_type");
   --  Used with Glib.Object.G_New, this creates a horizontal box

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self  : not null access Gtk_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a child at the end.
   --  @param Child the widget to append

   function Get_Baseline_Child
      (Self : not null access Gtk_Box_Record) return Glib.Gint;
   --  Gets the value set by [methodGtk.Box.set_baseline_child].
   --  Since: gtk+ 4.12
   --  @return the baseline child

   procedure Set_Baseline_Child
      (Self  : not null access Gtk_Box_Record;
       Child : Glib.Gint);
   --  Sets the baseline child of a box.
   --  This affects only vertical boxes.
   --  Since: gtk+ 4.12
   --  @param Child a child position, or -1

   function Get_Baseline_Position
      (Self : not null access Gtk_Box_Record)
       return Gtk.Enums.Gtk_Baseline_Position;
   --  Gets the value set by [methodGtk.Box.set_baseline_position].
   --  @return the baseline position

   procedure Set_Baseline_Position
      (Self     : not null access Gtk_Box_Record;
       Position : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets the baseline position of a box.
   --  This affects only horizontal boxes with at least one baseline aligned
   --  child. If there is more vertical space available than requested, and the
   --  baseline is not allocated by the parent then Position is used to
   --  allocate the baseline with respect to the extra space available.
   --  @param Position the baseline position

   function Get_Homogeneous
      (Self : not null access Gtk_Box_Record) return Boolean;
   --  Returns whether the box is homogeneous.
   --  In a homogeneous box all children are the same size.
   --  @return true if the box is homogeneous

   procedure Set_Homogeneous
      (Self        : not null access Gtk_Box_Record;
       Homogeneous : Boolean);
   --  Sets whether or not all children are given equal space in the box.
   --  @param Homogeneous true to create equal allotments, false for variable
   --  allotments

   function Get_Spacing
      (Self : not null access Gtk_Box_Record) return Glib.Gint;
   --  Gets the value set by [methodGtk.Box.set_spacing].
   --  @return spacing between children

   procedure Set_Spacing
      (Self    : not null access Gtk_Box_Record;
       Spacing : Glib.Gint);
   --  Sets the number of pixels to place between children.
   --  @param Spacing the number of pixels to put between children

   procedure Insert_Child_After
      (Self    : not null access Gtk_Box_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Sibling : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Inserts a child at a specific position.
   --  The child is added after Sibling in the list of Box children.
   --  If Sibling is `NULL`, the Child is placed at the beginning.
   --  @param Child the widget to insert
   --  @param Sibling the sibling after which to insert Child

   procedure Prepend
      (Self  : not null access Gtk_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a child at the beginning.
   --  @param Child the widget to prepend

   procedure Remove
      (Self  : not null access Gtk_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child widget from the box.
   --  The child must have been added before with [methodGtk.Box.append],
   --  [methodGtk.Box.prepend], or [methodGtk.Box.insert_child_after].
   --  @param Child the child to remove

   procedure Reorder_Child_After
      (Self    : not null access Gtk_Box_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Sibling : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Moves a child to a different position.
   --  The child is moved to the position after Sibling in the list of Box
   --  children.
   --  If Sibling is `NULL`, the child is placed at the beginning.
   --  @param Child the widget to move, must be a child of Box
   --  @param Sibling the sibling to move Child after

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Child
     (Self : not null access Gtk_Box_Record;
      Num  : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the Num-th child of the box, or null if there is no such child
   --  Since: gtk+ GtkAda 1.0

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Box_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Box_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Box_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Box_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Box_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Box_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Box_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Box_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Box_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Box_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Baseline_Child_Property : constant Glib.Properties.Property_Int;
   --  The position of the child that determines the baseline.
   --
   --  This is only relevant if the box is in vertical orientation.

   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position;
   --  How to position baseline-aligned widgets if extra space is available.

   Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the children should all be the same size.

   Spacing_Property : constant Glib.Properties.Property_Int;
   --  The amount of space between children.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "ConstraintTarget"
   --
   --  - "Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Box
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Box
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Box
   renames Implements_Gtk_Orientable.To_Object;

private
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position :=
     Gtk.Enums.Build ("baseline-position");
   Baseline_Child_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("baseline-child");
end Gtk.Box;
