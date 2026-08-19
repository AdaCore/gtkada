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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Column_View_Sorter is

   package Type_Conversion_Gtk_Column_View_Sorter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Column_View_Sorter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Column_View_Sorter);

   ------------------------
   -- Get_N_Sort_Columns --
   ------------------------

   function Get_N_Sort_Columns
      (Self : not null access Gtk_Column_View_Sorter_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_column_view_sorter_get_n_sort_columns");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Sort_Columns;

   -------------------------
   -- Get_Nth_Sort_Column --
   -------------------------

   function Get_Nth_Sort_Column
      (Self       : not null access Gtk_Column_View_Sorter_Record;
       Position   : Guint;
       Sort_Order : out Gtk.Enums.Gtk_Sort_Type)
       return Gtk.Column_View_Column.Gtk_Column_View_Column
   is
      function Internal
         (Self           : System.Address;
          Position       : Guint;
          Acc_Sort_Order : access Gtk.Enums.Gtk_Sort_Type)
          return System.Address;
      pragma Import (C, Internal, "gtk_column_view_sorter_get_nth_sort_column");
      Acc_Sort_Order              : aliased Gtk.Enums.Gtk_Sort_Type;
      Stub_Gtk_Column_View_Column : Gtk.Column_View_Column.Gtk_Column_View_Column_Record;
      Tmp_Return                  : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Position, Acc_Sort_Order'Access);
      Sort_Order := Acc_Sort_Order;
      return Gtk.Column_View_Column.Gtk_Column_View_Column (Get_User_Data (Tmp_Return, Stub_Gtk_Column_View_Column));
   end Get_Nth_Sort_Column;

   -----------------------------
   -- Get_Primary_Sort_Column --
   -----------------------------

   function Get_Primary_Sort_Column
      (Self : not null access Gtk_Column_View_Sorter_Record)
       return Gtk.Column_View_Column.Gtk_Column_View_Column
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_sorter_get_primary_sort_column");
      Stub_Gtk_Column_View_Column : Gtk.Column_View_Column.Gtk_Column_View_Column_Record;
   begin
      return Gtk.Column_View_Column.Gtk_Column_View_Column (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Column_View_Column));
   end Get_Primary_Sort_Column;

   ----------------------------
   -- Get_Primary_Sort_Order --
   ----------------------------

   function Get_Primary_Sort_Order
      (Self : not null access Gtk_Column_View_Sorter_Record)
       return Gtk.Enums.Gtk_Sort_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Sort_Type;
      pragma Import (C, Internal, "gtk_column_view_sorter_get_primary_sort_order");
   begin
      return Internal (Get_Object (Self));
   end Get_Primary_Sort_Order;

end Gtk.Column_View_Sorter;
