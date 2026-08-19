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

--  Sorts items by comparing strings.
--
--  To obtain the strings to compare, this sorter evaluates a
--  [classGtk.Expression].
--
--  It does the comparison in a linguistically correct way using the current
--  locale by normalizing Unicode strings and possibly case-folding them before
--  performing the comparison.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Gtk.Expression;          use Gtk.Expression;
with Gtk.Sorter;              use Gtk.Sorter;

package Gtk.String_Sorter is

   type Gtk_String_Sorter_Record is new Gtk_Sorter_Record with null record;
   type Gtk_String_Sorter is access all Gtk_String_Sorter_Record'Class;

   type Gtk_Collation is (
      Collation_None,
      Collation_Unicode,
      Collation_Filename);
   pragma Convention (C, Gtk_Collation);
   --  Describes how a [classGtk.StringSorter] turns strings into sort keys to
   --  compare them.
   --
   --  Note that the result of sorting will in general depend on the current
   --  locale unless the mode is Gtk_Collation_None.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Collation_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Collation);
   type Property_Gtk_Collation is new Gtk_Collation_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_String_Sorter;
       Expression : Gtk.Expression.Gtk_Expression);
   procedure Initialize
      (Self       : not null access Gtk_String_Sorter_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Creates a new string sorter that compares items using the given
   --  Expression.
   --  Unless an expression is set on it, this sorter will always compare
   --  items as invalid.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Expression The expression to evaluate

   function Gtk_String_Sorter_New
      (Expression : Gtk.Expression.Gtk_Expression) return Gtk_String_Sorter;
   --  Creates a new string sorter that compares items using the given
   --  Expression.
   --  Unless an expression is set on it, this sorter will always compare
   --  items as invalid.
   --  @param Expression The expression to evaluate

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_string_sorter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Collation
      (Self : not null access Gtk_String_Sorter_Record) return Gtk_Collation;
   --  Gets which collation method the sorter uses.
   --  Since: gtk+ 4.10
   --  @return The collation method

   procedure Set_Collation
      (Self      : not null access Gtk_String_Sorter_Record;
       Collation : Gtk_Collation);
   --  Sets the collation method to use for sorting.
   --  Since: gtk+ 4.10
   --  @param Collation the collation method

   function Get_Expression
      (Self : not null access Gtk_String_Sorter_Record)
       return Gtk.Expression.Gtk_Expression;
   --  Gets the expression that is evaluated to obtain strings from items.
   --  @return a `GtkExpression`
   --  Return has transfer-ownership='none'

   procedure Set_Expression
      (Self       : not null access Gtk_String_Sorter_Record;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Sets the expression that is evaluated to obtain strings from items.
   --  The expression must have the type G_TYPE_STRING.
   --  @param Expression a `GtkExpression`

   function Get_Ignore_Case
      (Self : not null access Gtk_String_Sorter_Record) return Boolean;
   --  Gets whether the sorter ignores case differences.
   --  @return True if Self is ignoring case differences

   procedure Set_Ignore_Case
      (Self        : not null access Gtk_String_Sorter_Record;
       Ignore_Case : Boolean);
   --  Sets whether the sorter will ignore case differences.
   --  @param Ignore_Case True to ignore case differences

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Collation_Property : constant Gtk.String_Sorter.Property_Gtk_Collation;
   --  Type: Gtk_Collation
   --  The collation method to use for sorting.
   --
   --  The `GTK_COLLATION_NONE` value is useful when the expression already
   --  returns collation keys, or strings that need to be compared
   --  byte-by-byte.
   --
   --  The default value, `GTK_COLLATION_UNICODE`, compares strings according
   --  to the [Unicode collation
   --  algorithm](https://www.unicode.org/reports/tr10/).

   Ignore_Case_Property : constant Glib.Properties.Property_Boolean;
   --  If sorting is case sensitive.

private
   Ignore_Case_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("ignore-case");
   Collation_Property : constant Gtk.String_Sorter.Property_Gtk_Collation :=
     Gtk.String_Sorter.Build ("collation");
end Gtk.String_Sorter;
