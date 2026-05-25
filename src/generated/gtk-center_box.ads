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

--  Arranges three children in a row, keeping the middle child centered as
--  well as possible.
--
--  <picture> <source srcset="centerbox-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkCenterBox"
--  src="centerbox.png"> </picture>
--  To add children to `GtkCenterBox`, use
--  [methodGtk.CenterBox.set_start_widget],
--  [methodGtk.CenterBox.set_center_widget] and
--  [methodGtk.CenterBox.set_end_widget].
--
--  The sizing and positioning of children can be influenced with the align
--  and expand properties of the children.
--
--  # GtkCenterBox as GtkBuildable
--
--  The `GtkCenterBox` implementation of the `GtkBuildable` interface supports
--  placing children in the 3 positions by specifying "start", "center" or
--  "end" as the "type" attribute of a `<child>` element.
--
--  # CSS nodes
--
--  `GtkCenterBox` uses a single CSS node with the name "box",
--
--  The first child of the `GtkCenterBox` will be allocated depending on the
--  text direction, i.e. in left-to-right layouts it will be allocated on the
--  left and in right-to-left layouts on the right.
--
--  In vertical orientation, the nodes of the children are arranged from top
--  to bottom.
--
--  # Accessibility
--
--  Until GTK 4.10, `GtkCenterBox` used the [enumGtk.AccessibleRole.group]
--  role.
--
--  Starting from GTK 4.12, `GtkCenterBox` uses the
--  [enumGtk.AccessibleRole.generic] role.

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

package Gtk.Center_Box is

   type Gtk_Center_Box_Record is new Gtk_Widget_Record with null record;
   type Gtk_Center_Box is access all Gtk_Center_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Center_Box);
   procedure Initialize (Self : not null access Gtk_Center_Box_Record'Class);
   --  Creates a new `GtkCenterBox`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Center_Box_New return Gtk_Center_Box;
   --  Creates a new `GtkCenterBox`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_center_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Baseline_Position
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Enums.Gtk_Baseline_Position;
   --  Gets the baseline position of the center box.
   --  See [methodGtk.CenterBox.set_baseline_position].
   --  @return the baseline position

   procedure Set_Baseline_Position
      (Self     : not null access Gtk_Center_Box_Record;
       Position : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets the baseline position of a center box.
   --  This affects only horizontal boxes with at least one baseline aligned
   --  child. If there is more vertical space available than requested, and the
   --  baseline is not allocated by the parent then Position is used to
   --  allocate the baseline with respect to the extra space available.
   --  @param Position the baseline position

   function Get_Center_Widget
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the center widget.
   --  @return the center widget

   procedure Set_Center_Widget
      (Self  : not null access Gtk_Center_Box_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the center widget.
   --  To remove the existing center widget, pass `NULL`.
   --  @param Child the new center widget

   function Get_End_Widget
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the end widget.
   --  @return the end widget

   procedure Set_End_Widget
      (Self  : not null access Gtk_Center_Box_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the end widget.
   --  To remove the existing end widget, pass `NULL`.
   --  @param Child the new end widget

   function Get_Shrink_Center_Last
      (Self : not null access Gtk_Center_Box_Record) return Boolean;
   --  Gets whether the center widget shrinks after other children.
   --  Since: gtk+ 4.12
   --  @return whether to shrink the center widget after others

   procedure Set_Shrink_Center_Last
      (Self               : not null access Gtk_Center_Box_Record;
       Shrink_Center_Last : Boolean);
   --  Sets whether to shrink the center widget after other children.
   --  By default, when there's no space to give all three children their
   --  natural widths, the start and end widgets start shrinking and the center
   --  child keeps natural width until they reach minimum width.
   --  If Shrink_Center_Last is false, start and end widgets keep natural
   --  width and the center widget starts shrinking instead.
   --  Since: gtk+ 4.12
   --  @param Shrink_Center_Last whether to shrink the center widget after
   --  others

   function Get_Start_Widget
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the start widget.
   --  @return the start widget

   procedure Set_Start_Widget
      (Self  : not null access Gtk_Center_Box_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the start widget.
   --  To remove the existing start widget, pass `NULL`.
   --  @param Child the new start widget

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Center_Box_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Center_Box_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Center_Box_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Center_Box_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Center_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Center_Box_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Center_Box_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Center_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Center_Box_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Center_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Center_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position;
   --  The position of the baseline aligned widget if extra space is
   --  available.

   Center_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget that is placed at the center position.

   End_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget that is placed at the end position.
   --
   --  In vertical orientation, the end position is at the bottom. In
   --  horizontal orientation, the end position is at the trailing edge with
   --  respect to the text direction.

   Shrink_Center_Last_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to shrink the center widget after other children.
   --
   --  By default, when there's no space to give all three children their
   --  natural widths, the start and end widgets start shrinking and the center
   --  child keeps natural width until they reach minimum width.
   --
   --  If false, start and end widgets keep natural width and the center
   --  widget starts shrinking instead.

   Start_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget that is placed at the start position.
   --
   --  In vertical orientation, the start position is at the top. In
   --  horizontal orientation, the start position is at the leading edge with
   --  respect to the text direction.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Center_Box_Record, Gtk_Center_Box);
   function "+"
     (Widget : access Gtk_Center_Box_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Center_Box
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Center_Box_Record, Gtk_Center_Box);
   function "+"
     (Widget : access Gtk_Center_Box_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Center_Box
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Center_Box_Record, Gtk_Center_Box);
   function "+"
     (Widget : access Gtk_Center_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Center_Box
   renames Implements_Gtk_Orientable.To_Object;

private
   Start_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("start-widget");
   Shrink_Center_Last_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("shrink-center-last");
   End_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("end-widget");
   Center_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("center-widget");
   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position :=
     Gtk.Enums.Build ("baseline-position");
end Gtk.Center_Box;
