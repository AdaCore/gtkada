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

--  An opaque structure representing a watched `GtkExpression`.
--
--  The contents of `GtkExpressionWatch` should only be accessed through the
--  provided API.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Values; use Glib.Values;

package Gtk.Expression_Watch is

   type Gtk_Expression_Watch is new Glib.C_Boxed with null record;
   Null_Gtk_Expression_Watch : constant Gtk_Expression_Watch;

   function From_Object (Object : System.Address) return Gtk_Expression_Watch;
   function From_Object_Free (B : access Gtk_Expression_Watch'Class) return Gtk_Expression_Watch;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_expression_watch_get_type");

   -------------
   -- Methods --
   -------------

   function Evaluate
      (Self  : Gtk_Expression_Watch;
       Value : in out Glib.Values.GValue) return Boolean;
   --  Evaluates the watched expression and on success stores the result in
   --  `value`.
   --  This is equivalent to calling [methodGtk.Expression.evaluate] with the
   --  expression and this pointer originally used to create `watch`.
   --  @param Value an empty `GValue` to be set
   --  @return `TRUE` if the expression could be evaluated and `value` was set

   function Ref (Self : Gtk_Expression_Watch) return Gtk_Expression_Watch;
   --  Acquires a reference on the given `GtkExpressionWatch`.
   --  @return the `GtkExpressionWatch` with an additional reference

   procedure Unref (Self : Gtk_Expression_Watch);
   --  Releases a reference on the given `GtkExpressionWatch`.
   --  If the reference was the last, the resources associated to `self` are
   --  freed.

   procedure Unwatch (Self : Gtk_Expression_Watch);
   --  Stops watching an expression.
   --  See [methodGtk.Expression.watch] for how the watch was established.

private
   Null_Gtk_Expression_Watch : constant Gtk_Expression_Watch :=
      (Glib.C_Boxed with null record);

end Gtk.Expression_Watch;
