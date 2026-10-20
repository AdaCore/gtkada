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

--  Uses constraints to describe relations between widgets.
--
--  `GtkConstraintLayout` is a layout manager that uses relations between
--  widget attributes, expressed via [classGtk.Constraint] instances, to
--  measure and allocate widgets.
--
--  ### How do constraints work
--
--  Constraints are objects defining the relationship between attributes of a
--  widget; you can read the description of the [classGtk.Constraint] class to
--  have a more in depth definition.
--
--  By taking multiple constraints and applying them to the children of a
--  widget using `GtkConstraintLayout`, it's possible to describe complex
--  layout policies; each constraint applied to a child or to the parent
--  widgets contributes to the full description of the layout, in terms of
--  parameters for resolving the value of each attribute.
--
--  It is important to note that a layout is defined by the totality of
--  constraints; removing a child, or a constraint, from an existing layout
--  without changing the remaining constraints may result in an unstable or
--  unsolvable layout.
--
--  Constraints have an implicit "reading order"; you should start describing
--  each edge of each child, as well as their relationship with the parent
--  container, from the top left (or top right, in RTL languages), horizontally
--  first, and then vertically.
--
--  A constraint-based layout with too few constraints can become "unstable",
--  that is: have more than one solution. The behavior of an unstable layout is
--  undefined.
--
--  A constraint-based layout with conflicting constraints may be unsolvable,
--  and lead to an unstable layout. You can use the
--  [propertyGtk.Constraint:strength] property of [classGtk.Constraint] to
--  "nudge" the layout towards a solution.
--
--  ### GtkConstraintLayout as GtkBuildable
--
--  `GtkConstraintLayout` implements the [ifaceGtk.Buildable] interface and
--  has a custom "constraints" element which allows describing constraints in a
--  [classGtk.Builder] UI file.
--
--  An example of a UI definition fragment specifying a constraint:
--
--  ```xml <object class="GtkConstraintLayout"> <constraints> <constraint
--  target="button" target-attribute="start" relation="eq" source="super"
--  source-attribute="start" constant="12" strength="required" /> <constraint
--  target="button" target-attribute="width" relation="ge" constant="250"
--  strength="strong" /> </constraints> </object> ```
--
--  The definition above will add two constraints to the GtkConstraintLayout:
--
--  - a required constraint between the leading edge of "button" and the
--  leading edge of the widget using the constraint layout, plus 12 pixels - a
--  strong, constant constraint making the width of "button" greater than, or
--  equal to 250 pixels
--
--  The "target" and "target-attribute" attributes are required.
--
--  The "source" and "source-attribute" attributes of the "constraint" element
--  are optional; if they are not specified, the constraint is assumed to be a
--  constant.
--
--  The "relation" attribute is optional; if not specified, the constraint is
--  assumed to be an equality.
--
--  The "strength" attribute is optional; if not specified, the constraint is
--  assumed to be required.
--
--  The "source" and "target" attributes can be set to "super" to indicate
--  that the constraint target is the widget using the GtkConstraintLayout.
--
--  There can be "constant" and "multiplier" attributes.
--
--  Additionally, the "constraints" element can also contain a description of
--  the `GtkConstraintGuides` used by the layout:
--
--  ```xml <constraints> <guide min-width="100" max-width="500"
--  name="hspace"/> <guide min-height="64" nat-height="128" name="vspace"
--  strength="strong"/> </constraints> ```
--
--  The "guide" element has the following optional attributes:
--
--  - "min-width", "nat-width", and "max-width", describe the minimum,
--  natural, and maximum width of the guide, respectively - "min-height",
--  "nat-height", and "max-height", describe the minimum, natural, and maximum
--  height of the guide, respectively - "strength" describes the strength of
--  the constraint on the natural size of the guide; if not specified, the
--  constraint is assumed to have a medium strength - "name" describes a name
--  for the guide, useful when debugging
--
--  ### Using the Visual Format Language
--
--  Complex constraints can be described using a compact syntax called VFL, or
--  *Visual Format Language*.
--
--  The Visual Format Language describes all the constraints on a row or
--  column, typically starting from the leading edge towards the trailing one.
--  Each element of the layout is composed by "views", which identify a
--  [ifaceGtk.ConstraintTarget].
--
--  For instance:
--
--  ``` [button]-[textField] ```
--
--  Describes a constraint that binds the trailing edge of "button" to the
--  leading edge of "textField", leaving a default space between the two.
--
--  Using VFL is also possible to specify predicates that describe constraints
--  on attributes like width and height:
--
--  ``` // Width must be greater than, or equal to 50 [button(>=50)]
--
--  // Width of button1 must be equal to width of button2 [button1(==button2)]
--  ```
--
--  The default orientation for a VFL description is horizontal, unless
--  otherwise specified:
--
--  ``` // horizontal orientation, default attribute: width H:[button(>=150)]
--
--  // vertical orientation, default attribute: height V:[button1(==button2)]
--  ```
--
--  It's also possible to specify multiple predicates, as well as their
--  strength:
--
--  ``` // minimum width of button must be 150 // natural width of button can
--  be 250 [button(>=150Required, ==250Medium)] ```
--
--  Finally, it's also possible to use simple arithmetic operators:
--
--  ``` // width of button1 must be equal to width of button2 // divided by 2
--  plus 12 [button1(button2 / 2 + 12)] ```
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_constraints.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Ada.Strings.Unbounded;
with GNAT.Strings;
with Glib;                  use Glib;
with Glib.Error;            use Glib.Error;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Types;            use Glib.Types;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint;        use Gtk.Constraint;
with Gtk.Constraint_Guide;  use Gtk.Constraint_Guide;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Layout_Manager;    use Gtk.Layout_Manager;

package Gtk.Constraint_Layout is

   type Gtk_Constraint_Layout_Record is new Gtk_Layout_Manager_Record with null record;
   type Gtk_Constraint_Layout is access all Gtk_Constraint_Layout_Record'Class;

   type Gtk_Constraint_Layout_Child_Record is new Gtk_Layout_Child_Record with null record;
   type Gtk_Constraint_Layout_Child is access all Gtk_Constraint_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Layout : out Gtk_Constraint_Layout);
   procedure Initialize
      (Layout : not null access Gtk_Constraint_Layout_Record'Class);
   --  Creates a new `GtkConstraintLayout` layout manager.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Constraint_Layout_New return Gtk_Constraint_Layout;
   --  Creates a new `GtkConstraintLayout` layout manager.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_constraint_layout_get_type");

   function Constraint_Layout_Child_Get_Type return Glib.GType;
   pragma Import (C, Constraint_Layout_Child_Get_Type, "gtk_constraint_layout_child_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Constraint
      (Layout     : not null access Gtk_Constraint_Layout_Record;
       Constraint : not null access Gtk.Constraint.Gtk_Constraint_Record'Class);
   --  Adds a constraint to the layout manager.
   --  The [propertyGtk.Constraint:source] and [propertyGtk.Constraint:target]
   --  properties of `constraint` can be:
   --  - set to `NULL` to indicate that the constraint refers to the widget
   --  using `layout` - set to the [classGtk.Widget] using `layout` - set to a
   --  child of the [classGtk.Widget] using `layout` - set to a
   --  [classGtk.ConstraintGuide] that is part of `layout`
   --  The Layout acquires the ownership of Constraint after calling this
   --  function.
   --  @param Constraint a [classGtk.Constraint]. Has
   --  transfer-ownership='full'.

   procedure Add_Guide
      (Layout : not null access Gtk_Constraint_Layout_Record;
       Guide  : not null access Gtk.Constraint_Guide.Gtk_Constraint_Guide_Record'Class);
   --  Adds a guide to `layout`.
   --  A guide can be used as the source or target of constraints, like a
   --  widget, but it is not visible.
   --  The `layout` acquires the ownership of `guide` after calling this
   --  function.
   --  @param Guide a [classGtk.ConstraintGuide] object. Has
   --  transfer-ownership='full'.

   function Observe_Constraints
      (Layout : not null access Gtk_Constraint_Layout_Record)
       return Glib.List_Model.Glist_Model;
   --  Returns a `GListModel` to track the constraints that are part of the
   --  layout.
   --  Calling this function will enable extra internal bookkeeping to track
   --  constraints and emit signals on the returned listmodel. It may slow down
   --  operations a lot.
   --  Applications should try hard to avoid calling this function because of
   --  the slowdowns.
   --  @return a `GListModel` tracking the layout's constraints

   function Observe_Guides
      (Layout : not null access Gtk_Constraint_Layout_Record)
       return Glib.List_Model.Glist_Model;
   --  Returns a `GListModel` to track the guides that are part of the layout.
   --  Calling this function will enable extra internal bookkeeping to track
   --  guides and emit signals on the returned listmodel. It may slow down
   --  operations a lot.
   --  Applications should try hard to avoid calling this function because of
   --  the slowdowns.
   --  @return a `GListModel` tracking the layout's guides

   procedure Remove_All_Constraints
      (Layout : not null access Gtk_Constraint_Layout_Record);
   --  Removes all constraints from the layout manager.

   procedure Remove_Constraint
      (Layout     : not null access Gtk_Constraint_Layout_Record;
       Constraint : not null access Gtk.Constraint.Gtk_Constraint_Record'Class);
   --  Removes `constraint` from the layout manager, so that it no longer
   --  influences the layout.
   --  The constraint must be attached: the layout's widget must be rooted.
   --  @param Constraint a [classGtk.Constraint]

   procedure Remove_Guide
      (Layout : not null access Gtk_Constraint_Layout_Record;
       Guide  : not null access Gtk.Constraint_Guide.Gtk_Constraint_Guide_Record'Class);
   --  Removes `guide` from the layout manager, so that it no longer
   --  influences the layout.
   --  @param Guide a [classGtk.ConstraintGuide] object

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Named_View is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Target : Gtk.Constraint_Target.Gtk_Constraint_Target;
   end record;
   type Named_View_Array is array (Positive range <>) of Named_View;

   function Add_Constraints_From_Description
     (Layout : not null access Gtk_Constraint_Layout_Record;
      Lines : GNAT.Strings.String_List;
      Views : Named_View_Array;
      Error : out Glib.Error.GError;
      Hspacing : Glib.Gint := -1;
      Vspacing : Glib.Gint := -1)
   return Glib.Object.Object_Simple_List.Glist;
   --  Parse VFL lines and add their constraints to Layout. Names in Views
   --  refer to children or guides already added to the layout. A null entry
   --  in Lines is treated as an empty line. Arbitrary array bounds are allowed.
   --  Free the returned list nodes with Object_Simple_List.Free; its objects
   --  belong to Layout and must not be unreferenced by the caller.
   --  On a parse error, Error is set (free it with Glib.Error.Error_Free).
   --  GTK can add constraints from preceding valid lines before an error;
   --  the returned list includes those constraints, so they can be removed.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Constraint_Layout_Record, Gtk_Constraint_Layout);
   function "+"
     (Widget : access Gtk_Constraint_Layout_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Constraint_Layout
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Constraint_Layout;
