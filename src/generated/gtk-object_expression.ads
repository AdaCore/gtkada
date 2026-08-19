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

--  A `GObject` value in a `GtkExpression`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Gtk.Expression; use Gtk.Expression;

package Gtk.Object_Expression is

   type Gtk_Object_Expression_Record is new Gtk_Expression_Record with null record;
   type Gtk_Object_Expression is access all Gtk_Object_Expression_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Object_Expression;
       Object : not null access Glib.Object.GObject_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Object_Expression_Record'Class;
       Object : not null access Glib.Object.GObject_Record'Class);
   --  Creates an expression evaluating to the given `object` with a weak
   --  reference.
   --  Once the `object` is disposed, it will fail to evaluate.
   --  This expression is meant to break reference cycles.
   --  If you want to keep a reference to `object`, use
   --  [ctorGtk.ConstantExpression.new].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Object object to watch

   function Gtk_Object_Expression_New
      (Object : not null access Glib.Object.GObject_Record'Class)
       return Gtk_Object_Expression;
   --  Creates an expression evaluating to the given `object` with a weak
   --  reference.
   --  Once the `object` is disposed, it will fail to evaluate.
   --  This expression is meant to break reference cycles.
   --  If you want to keep a reference to `object`, use
   --  [ctorGtk.ConstantExpression.new].
   --  @param Object object to watch

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_object_expression_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Object
      (Self : Gtk_Object_Expression) return Glib.Object.GObject;
   --  Gets the object that the expression evaluates to.
   --  @return the object, or `NULL`
   --  Return has transfer-ownership='none'

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Object_Expression_Record;

private
   for Gtk_Object_Expression_Record'External_Tag use "GtkObjectExpression";
end Gtk.Object_Expression;
