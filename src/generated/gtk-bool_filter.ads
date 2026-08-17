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

--  Evaluates a boolean expression to determine whether to include items.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Gtk.Expression;  use Gtk.Expression;
with Gtk.Filter;      use Gtk.Filter;

package Gtk.Bool_Filter is

   type Gtk_Bool_Filter_Record is new Gtk_Filter_Record with null record;
   type Gtk_Bool_Filter is access all Gtk_Bool_Filter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self       : out Gtk_Bool_Filter;
       Expression : Gtk.Expression.Gtk_Expression);
   procedure Initialize
      (Self       : not null access Gtk_Bool_Filter_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Creates a new bool filter.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Expression the expression to evaluate

   function Gtk_Bool_Filter_New
      (Expression : Gtk.Expression.Gtk_Expression) return Gtk_Bool_Filter;
   --  Creates a new bool filter.
   --  @param Expression the expression to evaluate

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bool_filter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expression
      (Self : not null access Gtk_Bool_Filter_Record)
       return Gtk.Expression.Gtk_Expression;
   --  Gets the expression that the filter evaluates for each item.
   --  @return the expression

   procedure Set_Expression
      (Self       : not null access Gtk_Bool_Filter_Record;
       Expression : Gtk.Expression.Gtk_Expression);
   --  Sets the expression that the filter uses to check if items should be
   --  filtered.
   --  The expression must have a value type of `G_TYPE_BOOLEAN`.
   --  @param Expression the expression

   function Get_Invert
      (Self : not null access Gtk_Bool_Filter_Record) return Boolean;
   --  Returns whether the filter inverts the expression.
   --  @return true if the filter inverts

   procedure Set_Invert
      (Self   : not null access Gtk_Bool_Filter_Record;
       Invert : Boolean);
   --  Sets whether the filter should invert the expression.
   --  @param Invert true to invert

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Invert_Property : constant Glib.Properties.Property_Boolean;
   --  If the expression result should be inverted.

private
   Invert_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invert");
end Gtk.Bool_Filter;
