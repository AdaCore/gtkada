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

--  Describes a constraint between attributes of two widgets, expressed as a
--  linear equation.
--
--  The typical equation for a constraint is:
--
--  ``` target.target_attr = source.source_attr × multiplier + constant ```
--
--  Each `GtkConstraint` is part of a system that will be solved by a
--  [classGtk.ConstraintLayout] in order to allocate and position each child
--  widget or guide.
--
--  The source and target, as well as their attributes, of a `GtkConstraint`
--  instance are immutable after creation.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;

package Gtk.Constraint is

   type Gtk_Constraint_Record is new GObject_Record with null record;
   type Gtk_Constraint is access all Gtk_Constraint_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self             : out Gtk_Constraint;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       Source           : System.Address;
       Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Multiplier       : Gdouble;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint);
   procedure Initialize
      (Self             : not null access Gtk_Constraint_Record'Class;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       Source           : System.Address;
       Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Multiplier       : Gdouble;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint);
   --  Creates a new constraint representing a relation between a layout
   --  attribute on a source and a layout attribute on a target.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Target the target of the constraint
   --  @param Target_Attribute the attribute of `target` to be set
   --  @param Relation the relation equivalence between `target_attribute` and
   --  `source_attribute`
   --  @param Source the source of the constraint
   --  @param Source_Attribute the attribute of `source` to be read
   --  @param Multiplier a multiplication factor to be applied to
   --  `source_attribute`
   --  @param The_Constant a constant factor to be added to `source_attribute`
   --  @param Strength the strength of the constraint

   function Gtk_Constraint_New
      (Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       Source           : System.Address;
       Source_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Multiplier       : Gdouble;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint) return Gtk_Constraint;
   --  Creates a new constraint representing a relation between a layout
   --  attribute on a source and a layout attribute on a target.
   --  @param Target the target of the constraint
   --  @param Target_Attribute the attribute of `target` to be set
   --  @param Relation the relation equivalence between `target_attribute` and
   --  `source_attribute`
   --  @param Source the source of the constraint
   --  @param Source_Attribute the attribute of `source` to be read
   --  @param Multiplier a multiplication factor to be applied to
   --  `source_attribute`
   --  @param The_Constant a constant factor to be added to `source_attribute`
   --  @param Strength the strength of the constraint

   procedure Gtk_New_Constant
      (Self             : out Gtk_Constraint;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint);
   procedure Initialize_Constant
      (Self             : not null access Gtk_Constraint_Record'Class;
       Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint);
   --  Creates a new constraint representing a relation between a layout
   --  attribute on a target and a constant value.
   --  Initialize_Constant does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param Target a the target of the constraint
   --  @param Target_Attribute the attribute of `target` to be set
   --  @param Relation the relation equivalence between `target_attribute` and
   --  `constant`
   --  @param The_Constant a constant factor to be set on `target_attribute`
   --  @param Strength the strength of the constraint

   function Gtk_Constraint_New_Constant
      (Target           : System.Address;
       Target_Attribute : Gtk.Enums.Gtk_Constraint_Attribute;
       Relation         : Gtk.Enums.Gtk_Constraint_Relation;
       The_Constant     : Gdouble;
       Strength         : Glib.Gint) return Gtk_Constraint;
   --  Creates a new constraint representing a relation between a layout
   --  attribute on a target and a constant value.
   --  @param Target a the target of the constraint
   --  @param Target_Attribute the attribute of `target` to be set
   --  @param Relation the relation equivalence between `target_attribute` and
   --  `constant`
   --  @param The_Constant a constant factor to be set on `target_attribute`
   --  @param Strength the strength of the constraint

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_constraint_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Constant
      (Self : not null access Gtk_Constraint_Record) return Gdouble;
   --  Retrieves the constant factor added to the source attributes' value.
   --  @return a constant factor

   function Get_Multiplier
      (Self : not null access Gtk_Constraint_Record) return Gdouble;
   --  Retrieves the multiplication factor applied to the source attribute's
   --  value.
   --  @return a multiplication factor

   function Get_Relation
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Enums.Gtk_Constraint_Relation;
   --  The order relation between the terms of the constraint.
   --  @return a relation type

   function Get_Source
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Constraint_Target.Gtk_Constraint_Target;
   --  Retrieves the [ifaceGtk.ConstraintTarget] used as the source for the
   --  constraint.
   --  If the source is set to `NULL` at creation, the constraint will use the
   --  widget using the [classGtk.ConstraintLayout] as the source.
   --  @return the source of the constraint

   function Get_Source_Attribute
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Enums.Gtk_Constraint_Attribute;
   --  Retrieves the attribute of the source to be read by the constraint.
   --  @return the source's attribute

   function Get_Strength
      (Self : not null access Gtk_Constraint_Record) return Glib.Gint;
   --  Retrieves the strength of the constraint.
   --  @return the strength value

   function Get_Target
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Constraint_Target.Gtk_Constraint_Target;
   --  Retrieves the [ifaceGtk.ConstraintTarget] used as the target for the
   --  constraint.
   --  If the targe is set to `NULL` at creation, the constraint will use the
   --  widget using the [classGtk.ConstraintLayout] as the target.
   --  @return a `GtkConstraintTarget`

   function Get_Target_Attribute
      (Self : not null access Gtk_Constraint_Record)
       return Gtk.Enums.Gtk_Constraint_Attribute;
   --  Retrieves the attribute of the target to be set by the constraint.
   --  @return the target's attribute

   function Is_Attached
      (Self : not null access Gtk_Constraint_Record) return Boolean;
   --  Checks whether the constraint is attached to a
   --  [classGtk.ConstraintLayout], and it is contributing to the layout.
   --  @return `TRUE` if the constraint is attached

   function Is_Constant
      (Self : not null access Gtk_Constraint_Record) return Boolean;
   --  Checks whether the constraint describes a relation between an attribute
   --  on the [propertyGtk.Constraint:target] and a constant value.
   --  @return `TRUE` if the constraint is a constant relation

   function Is_Required
      (Self : not null access Gtk_Constraint_Record) return Boolean;
   --  Checks whether the constraint is a required relation for solving the
   --  constraint layout.
   --  @return True if the constraint is required

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Multiplier_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The multiplication factor to be applied to the
   --  [propertyGtk.Constraint:source-attribute].

   Relation_Property : constant Gtk.Enums.Property_Gtk_Constraint_Relation;
   --  The order relation between the terms of the constraint.

   Source_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Constraint_Target.Gtk_Constraint_Target
   --  The source of the constraint.
   --
   --  The constraint will set the [propertyGtk.Constraint:target-attribute]
   --  property of the target using the
   --  [propertyGtk.Constraint:source-attribute] property of the source.

   Source_Attribute_Property : constant Gtk.Enums.Property_Gtk_Constraint_Attribute;
   --  The attribute of the [propertyGtk.Constraint:source] read by the
   --  constraint.

   Strength_Property : constant Glib.Properties.Property_Int;
   --  The strength of the constraint.
   --
   --  The strength can be expressed either using one of the symbolic values
   --  of the [enumGtk.ConstraintStrength] enumeration, or any positive integer
   --  value.

   Target_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Constraint_Target.Gtk_Constraint_Target
   --  The target of the constraint.
   --
   --  The constraint will set the [propertyGtk.Constraint:target-attribute]
   --  property of the target using the
   --  [propertyGtk.Constraint:source-attribute] property of the source widget.

   Target_Attribute_Property : constant Gtk.Enums.Property_Gtk_Constraint_Attribute;
   --  The attribute of the [propertyGtk.Constraint:target] set by the
   --  constraint.

   The_Constant_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The constant value to be added to the
   --  [propertyGtk.Constraint:source-attribute].

private
   The_Constant_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("constant");
   Target_Attribute_Property : constant Gtk.Enums.Property_Gtk_Constraint_Attribute :=
     Gtk.Enums.Build ("target-attribute");
   Target_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("target");
   Strength_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("strength");
   Source_Attribute_Property : constant Gtk.Enums.Property_Gtk_Constraint_Attribute :=
     Gtk.Enums.Build ("source-attribute");
   Source_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("source");
   Relation_Property : constant Gtk.Enums.Property_Gtk_Constraint_Relation :=
     Gtk.Enums.Build ("relation");
   Multiplier_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("multiplier");
end Gtk.Constraint;
