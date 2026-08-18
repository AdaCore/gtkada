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

--  Sorts items numerically.
--
--  To obtain the numbers to compare, this sorter evaluates a
--  [classGtk.Expression].

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Expression; use Gtk.Expression;
with Gtk.Sorter;     use Gtk.Sorter;

package Gtk.Numeric_Sorter is

   type Gtk_Numeric_Sorter_Record is new Gtk_Sorter_Record with null record;
   type Gtk_Numeric_Sorter is access all Gtk_Numeric_Sorter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Numeric_Sorter;
       Expression : Gtk.Expression.Gtk_Expression);
   procedure Initialize
      (Self       : not null access Gtk_Numeric_Sorter_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Creates a new numeric sorter using the given Expression.
   --  Smaller numbers will be sorted first. You can call
   --  [methodGtk.NumericSorter.set_sort_order] to change this.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Expression The expression to evaluate

   function Gtk_Numeric_Sorter_New
      (Expression : Gtk.Expression.Gtk_Expression) return Gtk_Numeric_Sorter;
   --  Creates a new numeric sorter using the given Expression.
   --  Smaller numbers will be sorted first. You can call
   --  [methodGtk.NumericSorter.set_sort_order] to change this.
   --  @param Expression The expression to evaluate

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_numeric_sorter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expression
      (Self : not null access Gtk_Numeric_Sorter_Record)
       return Gtk.Expression.Gtk_Expression;
   --  Gets the expression that is evaluated to obtain numbers from items.
   --  @return a `GtkExpression`
   --  Return has transfer-ownership='none'

   procedure Set_Expression
      (Self       : not null access Gtk_Numeric_Sorter_Record;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Sets the expression that is evaluated to obtain numbers from items.
   --  Unless an expression is set on Self, the sorter will always compare
   --  items as invalid.
   --  The expression must have a return type that can be compared
   --  numerically, such as G_TYPE_INT or G_TYPE_DOUBLE.
   --  @param Expression a `GtkExpression`

   function Get_Sort_Order
      (Self : not null access Gtk_Numeric_Sorter_Record)
       return Gtk.Enums.Gtk_Sort_Type;
   --  Gets whether this sorter will sort smaller numbers first.
   --  @return the order of the numbers

   procedure Set_Sort_Order
      (Self       : not null access Gtk_Numeric_Sorter_Record;
       Sort_Order : Gtk.Enums.Gtk_Sort_Type);
   --  Sets whether to sort smaller numbers before larger ones.
   --  @param Sort_Order whether to sort smaller numbers first

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Sort_Order_Property : constant Gtk.Enums.Property_Gtk_Sort_Type;
   --  Whether the sorter will sort smaller numbers first.

private
   Sort_Order_Property : constant Gtk.Enums.Property_Gtk_Sort_Type :=
     Gtk.Enums.Build ("sort-order");
end Gtk.Numeric_Sorter;
