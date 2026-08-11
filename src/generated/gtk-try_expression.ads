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

--  A `GtkExpression` that tries to evaluate each of its expressions until it
--  succeeds.
--
--  If all expressions fail to evaluate, the `GtkTryExpression`'s evaluation
--  fails as well.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.Expression; use Gtk.Expression;

package Gtk.Try_Expression is

   type Gtk_Try_Expression_Record is new Gtk_Expression_Record with null record;
   type Gtk_Try_Expression is access all Gtk_Try_Expression_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_try_expression_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self        : out Gtk_Try_Expression;
      Expressions : Gtk.Expression.Gtk_Expression_Array);
   procedure Initialize
     (Self        : not null access Gtk_Try_Expression_Record'Class;
      Expressions : Gtk.Expression.Gtk_Expression_Array);
   --  Creates a `GtkExpression` with an array of expressions.
   --  When evaluated, the `GtkTryExpression` tries to evaluate each of its
   --  expressions until it succeeds. If all expressions fail to evaluate, the
   --  `GtkTryExpression`'s evaluation fails as well.
   --  The value type of the expressions in the array must match.
   --  Since: gtk+ 4.22
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Expressions The array of expressions

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Try_Expression_Record;

private
   for Gtk_Try_Expression_Record'External_Tag use "GtkTryExpression";
end Gtk.Try_Expression;
