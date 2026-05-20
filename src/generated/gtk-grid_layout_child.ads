------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  `GtkLayoutChild` subclass for children in a `GtkGridLayout`.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Gtk.Layout_Child; use Gtk.Layout_Child;

package Gtk.Grid_Layout_Child is

   pragma Elaborate_Body;

   type Gtk_Grid_Layout_Child_Record is new Gtk_Layout_Child_Record with null record;
   type Gtk_Grid_Layout_Child is access all Gtk_Grid_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_grid_layout_child_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Column
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the column number to which Child attaches its left side.
   --  @return the column number

   procedure Set_Column
      (Child  : not null access Gtk_Grid_Layout_Child_Record;
       Column : Glib.Gint);
   --  Sets the column number to attach the left side of Child.
   --  @param Column the attach point for Child

   function Get_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the number of columns that Child spans to.
   --  @return the number of columns

   procedure Set_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint);
   --  Sets the number of columns Child spans to.
   --  @param Span the span of Child

   function Get_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the row number to which Child attaches its top side.
   --  @return the row number

   procedure Set_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Row   : Glib.Gint);
   --  Sets the row to place Child in.
   --  @param Row the row for Child

   function Get_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint;
   --  Retrieves the number of rows that Child spans to.
   --  @return the number of row

   procedure Set_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint);
   --  Sets the number of rows Child spans to.
   --  @param Span the span of Child

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Column_Property : constant Glib.Properties.Property_Int;
   --  The column to place the child in.

   Column_Span_Property : constant Glib.Properties.Property_Int;
   --  The number of columns the child spans to.

   Row_Property : constant Glib.Properties.Property_Int;
   --  The row to place the child in.

   Row_Span_Property : constant Glib.Properties.Property_Int;
   --  The number of rows the child spans to.

private
   Row_Span_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-span");
   Row_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row");
   Column_Span_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-span");
   Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column");
end Gtk.Grid_Layout_Child;
