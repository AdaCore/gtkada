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

--  A constant value in a `GtkExpression`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Values;    use Glib.Values;
with Gtk.Expression; use Gtk.Expression;

package Gtk.Constant_Expression is

   type Gtk_Constant_Expression_Record is new Gtk_Expression_Record with null record;
   type Gtk_Constant_Expression is access all Gtk_Constant_Expression_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_For_Value
      (Self  : out Gtk_Constant_Expression;
       Value : in out Glib.Values.GValue);
   procedure Initialize_For_Value
      (Self  : not null access Gtk_Constant_Expression_Record'Class;
       Value : in out Glib.Values.GValue);
   --  Creates an expression that always evaluates to the given `value`.
   --  Initialize_For_Value does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Value a `GValue`

   function Gtk_Constant_Expression_New_For_Value
      (Value : in out Glib.Values.GValue) return Gtk_Constant_Expression;
   --  Creates an expression that always evaluates to the given `value`.
   --  @param Value a `GValue`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_constant_expression_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Value
      (Self : Gtk_Constant_Expression) return access constant GValue;
   --  Gets the value that a constant expression evaluates to.

   ----------------------
   -- GtkAda additions --
   ----------------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Constant_Expression_Record;

private
   for Gtk_Constant_Expression_Record'External_Tag use "GtkConstantExpression";
end Gtk.Constant_Expression;
