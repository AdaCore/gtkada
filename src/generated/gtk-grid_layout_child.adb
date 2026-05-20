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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Grid_Layout_Child is

   package Type_Conversion_Gtk_Grid_Layout_Child is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Grid_Layout_Child_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Grid_Layout_Child);

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_column");
   begin
      return Internal (Get_Object (Child));
   end Get_Column;

   ---------------------
   -- Get_Column_Span --
   ---------------------

   function Get_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_column_span");
   begin
      return Internal (Get_Object (Child));
   end Get_Column_Span;

   -------------
   -- Get_Row --
   -------------

   function Get_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_row");
   begin
      return Internal (Get_Object (Child));
   end Get_Row;

   ------------------
   -- Get_Row_Span --
   ------------------

   function Get_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_row_span");
   begin
      return Internal (Get_Object (Child));
   end Get_Row_Span;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column
      (Child  : not null access Gtk_Grid_Layout_Child_Record;
       Column : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_column");
   begin
      Internal (Get_Object (Child), Column);
   end Set_Column;

   ---------------------
   -- Set_Column_Span --
   ---------------------

   procedure Set_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Span : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_column_span");
   begin
      Internal (Get_Object (Child), Span);
   end Set_Column_Span;

   -------------
   -- Set_Row --
   -------------

   procedure Set_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Row   : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Row : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_row");
   begin
      Internal (Get_Object (Child), Row);
   end Set_Row;

   ------------------
   -- Set_Row_Span --
   ------------------

   procedure Set_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Span : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_row_span");
   begin
      Internal (Get_Object (Child), Span);
   end Set_Row_Span;

end Gtk.Grid_Layout_Child;
