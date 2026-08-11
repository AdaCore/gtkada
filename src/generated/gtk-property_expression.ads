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

--  A `GObject` property value in a `GtkExpression`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.Expression; use Gtk.Expression;

package Gtk.Property_Expression is

   type Gtk_Property_Expression_Record is new Gtk_Expression_Record with null record;
   type Gtk_Property_Expression is access all Gtk_Property_Expression_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self          : out Gtk_Property_Expression;
       This_Type     : GType;
       Expression    : Gtk.Expression.Gtk_Expression;
       Property_Name : UTF8_String);
   procedure Initialize
      (Self          : not null access Gtk_Property_Expression_Record'Class;
       This_Type     : GType;
       Expression    : Gtk.Expression.Gtk_Expression;
       Property_Name : UTF8_String);
   --  Creates an expression that looks up a property.
   --  The object to use is found by evaluating the `expression`, or using the
   --  `this` argument when `expression` is `NULL`.
   --  If the resulting object conforms to `this_type`, its property named
   --  `property_name` will be queried. Otherwise, this expression's evaluation
   --  will fail.
   --  The given `this_type` must have a property with `property_name`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param This_Type The type to expect for the this type
   --  @param Expression Expression to evaluate to get the object to query or
   --  `NULL` to query the `this` object
   --  @param Property_Name name of the property

   function Gtk_Property_Expression_New
      (This_Type     : GType;
       Expression    : Gtk.Expression.Gtk_Expression;
       Property_Name : UTF8_String) return Gtk_Property_Expression;
   --  Creates an expression that looks up a property.
   --  The object to use is found by evaluating the `expression`, or using the
   --  `this` argument when `expression` is `NULL`.
   --  If the resulting object conforms to `this_type`, its property named
   --  `property_name` will be queried. Otherwise, this expression's evaluation
   --  will fail.
   --  The given `this_type` must have a property with `property_name`.
   --  @param This_Type The type to expect for the this type
   --  @param Expression Expression to evaluate to get the object to query or
   --  `NULL` to query the `this` object
   --  @param Property_Name name of the property

   procedure Gtk_New_For_Pspec
      (Self       : out Gtk_Property_Expression;
       Expression : Gtk.Expression.Gtk_Expression;
       Pspec      : Glib.Param_Spec);
   procedure Initialize_For_Pspec
      (Self       : not null access Gtk_Property_Expression_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression;
       Pspec      : Glib.Param_Spec);
   --  Creates an expression that looks up a property.
   --  The object to use is found by evaluating the `expression`, or using the
   --  `this` argument when `expression` is `NULL`.
   --  If the resulting object conforms to `this_type`, its property specified
   --  by `pspec` will be queried. Otherwise, this expression's evaluation will
   --  fail.
   --  Initialize_For_Pspec does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Expression Expression to evaluate to get the object to query or
   --  `NULL` to query the `this` object
   --  @param Pspec the `GParamSpec` for the property to query

   function Gtk_Property_Expression_New_For_Pspec
      (Expression : Gtk.Expression.Gtk_Expression;
       Pspec      : Glib.Param_Spec) return Gtk_Property_Expression;
   --  Creates an expression that looks up a property.
   --  The object to use is found by evaluating the `expression`, or using the
   --  `this` argument when `expression` is `NULL`.
   --  If the resulting object conforms to `this_type`, its property specified
   --  by `pspec` will be queried. Otherwise, this expression's evaluation will
   --  fail.
   --  @param Expression Expression to evaluate to get the object to query or
   --  `NULL` to query the `this` object
   --  @param Pspec the `GParamSpec` for the property to query

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_property_expression_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expression
      (Self : Gtk_Property_Expression) return Gtk.Expression.Gtk_Expression;
   --  Gets the expression specifying the object of a property expression.
   --  @return the object expression

   function Get_Pspec
      (Self : Gtk_Property_Expression) return Glib.Param_Spec;
   --  Gets the `GParamSpec` specifying the property of a property expression.
   --  @return the `GParamSpec` for the property

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Property_Expression_Record;

private
   for Gtk_Property_Expression_Record'External_Tag use "GtkPropertyExpression";
end Gtk.Property_Expression;
