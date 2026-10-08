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

--  An invisible layout element in a `GtkConstraintLayout`.
--
--  The `GtkConstraintLayout` treats guides like widgets. They can be used as
--  the source or target of a `GtkConstraint`.
--
--  Guides have a minimum, maximum and natural size. Depending on the
--  constraints that are applied, they can act like a guideline that widgets
--  can be aligned to, or like *flexible space*.
--
--  Unlike a `GtkWidget`, a `GtkConstraintGuide` will not be drawn.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;

package Gtk.Constraint_Guide is

   type Gtk_Constraint_Guide_Record is new GObject_Record with null record;
   type Gtk_Constraint_Guide is access all Gtk_Constraint_Guide_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Constraint_Guide);
   procedure Initialize
      (Self : not null access Gtk_Constraint_Guide_Record'Class);
   --  Creates a new `GtkConstraintGuide` object.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Constraint_Guide_New return Gtk_Constraint_Guide;
   --  Creates a new `GtkConstraintGuide` object.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_constraint_guide_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Max_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the maximum size of Guide.
   --  @param Width return location for the maximum width
   --  @param Height return location for the maximum height

   procedure Set_Max_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Sets the maximum size of Guide.
   --  If Guide is attached to a `GtkConstraintLayout`, the constraints will
   --  be updated to reflect the new size.
   --  @param Width the new maximum width, or -1 to not change it
   --  @param Height the new maximum height, or -1 to not change it

   procedure Get_Min_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the minimum size of Guide.
   --  @param Width return location for the minimum width
   --  @param Height return location for the minimum height

   procedure Set_Min_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Sets the minimum size of Guide.
   --  If Guide is attached to a `GtkConstraintLayout`, the constraints will
   --  be updated to reflect the new size.
   --  @param Width the new minimum width, or -1 to not change it
   --  @param Height the new minimum height, or -1 to not change it

   function Get_Name
      (Self : not null access Gtk_Constraint_Guide_Record)
       return UTF8_String;
   --  Retrieves the name set using Gtk.Constraint_Guide.Set_Name.
   --  @return the name of the guide

   procedure Set_Name
      (Self : not null access Gtk_Constraint_Guide_Record;
       Name : UTF8_String := "");
   --  Sets a name for the given `GtkConstraintGuide`.
   --  The name is useful for debugging purposes.
   --  @param Name a name for the Guide

   procedure Get_Nat_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the natural size of Guide.
   --  @param Width return location for the natural width
   --  @param Height return location for the natural height

   procedure Set_Nat_Size
      (Self   : not null access Gtk_Constraint_Guide_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Sets the natural size of Guide.
   --  If Guide is attached to a `GtkConstraintLayout`, the constraints will
   --  be updated to reflect the new size.
   --  @param Width the new natural width, or -1 to not change it
   --  @param Height the new natural height, or -1 to not change it

   function Get_Strength
      (Self : not null access Gtk_Constraint_Guide_Record)
       return Gtk.Enums.Gtk_Constraint_Strength;
   --  Retrieves the strength set using Gtk.Constraint_Guide.Set_Strength.
   --  @return the strength of the constraint on the natural size

   procedure Set_Strength
      (Self     : not null access Gtk_Constraint_Guide_Record;
       Strength : Gtk.Enums.Gtk_Constraint_Strength);
   --  Sets the strength of the constraint on the natural size of the given
   --  `GtkConstraintGuide`.
   --  @param Strength the strength of the constraint

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Max_Height_Property : constant Glib.Properties.Property_Int;
   --  The maximum height of the guide.

   Max_Width_Property : constant Glib.Properties.Property_Int;
   --  The maximum width of the guide.

   Min_Height_Property : constant Glib.Properties.Property_Int;
   --  The minimum height of the guide.

   Min_Width_Property : constant Glib.Properties.Property_Int;
   --  The minimum width of the guide.

   Name_Property : constant Glib.Properties.Property_String;
   --  A name that identifies the `GtkConstraintGuide`, for debugging.

   Nat_Height_Property : constant Glib.Properties.Property_Int;
   --  The preferred, or natural, height of the guide.

   Nat_Width_Property : constant Glib.Properties.Property_Int;
   --  The preferred, or natural, width of the guide.

   Strength_Property : constant Gtk.Enums.Property_Gtk_Constraint_Strength;
   --  The `GtkConstraintStrength` to be used for the constraint on the
   --  natural size of the guide.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Constraint_Guide_Record, Gtk_Constraint_Guide);
   function "+"
     (Widget : access Gtk_Constraint_Guide_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Constraint_Guide
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Strength_Property : constant Gtk.Enums.Property_Gtk_Constraint_Strength :=
     Gtk.Enums.Build ("strength");
   Nat_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("nat-width");
   Nat_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("nat-height");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Min_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-width");
   Min_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-height");
   Max_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width");
   Max_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-height");
end Gtk.Constraint_Guide;
