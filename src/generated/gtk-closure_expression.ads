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

--  An expression using a custom `GClosure` to compute the value from its
--  parameters.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.Expression; use Gtk.Expression;

package Gtk.Closure_Expression is

   type Gtk_Closure_Expression_Record is new Gtk_Expression_Record with null record;
   type Gtk_Closure_Expression is access all Gtk_Closure_Expression_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_closure_expression_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self       : out Gtk_Closure_Expression;
      Value_Type : GType;
      Closure    : System.Address;
      Params     : Gtk.Expression.Gtk_Expression_Array);
   procedure Initialize
     (Self       : not null access Gtk_Closure_Expression_Record'Class;
      Value_Type : GType;
      Closure    : System.Address;
      Params     : Gtk.Expression.Gtk_Expression_Array);
   --  Creates a `GtkExpression` that calls `closure` when it is evaluated.
   --  `closure` is called with the `this` object and the results of
   --  evaluating the `params` expressions.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Value_Type the type of the value that this expression evaluates
   --  to
   --  @param Closure closure to call when evaluating this expression. If
   --  closure is floating, it is adopted
   --  @param Params expressions for each parameter

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Closure_Expression_Record;

private
   for Gtk_Closure_Expression_Record'External_Tag use "GtkClosureExpression";
end Gtk.Closure_Expression;
