------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2025, AdaCore                     --
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

with System;      use System;
with Glib.Object; use Glib.Object;

package body Pango.Cairo is

   ---------------------------
   -- Set_Default_Font_Type --
   ---------------------------

   procedure Set_Default_Font_Type
     (Font_Type : Standard.Cairo.Cairo_Font_Type)
   is
      function pango_cairo_font_map_new_for_font_type
        (fonttype : Standard.Cairo.Cairo_Font_Type) return System.Address with
        Import, Convention => C,
        External_Name      => "pango_cairo_font_map_new_for_font_type";

      procedure pango_cairo_font_map_set_default
        (fontmap : System.Address) with
        Import, Convention => C,
        External_Name      => "pango_cairo_font_map_set_default";

   begin
      pango_cairo_font_map_set_default
        (pango_cairo_font_map_new_for_font_type (Font_Type));
   end Set_Default_Font_Type;

   -----------------
   -- Show_Layout --
   -----------------

   procedure Show_Layout (Cr : Cairo_Context; Layout : Pango_Layout) is
      procedure Internal (Cr : Cairo_Context; Layout : System.Address);
      pragma Import (C, Internal, "pango_cairo_show_layout");
   begin
      Internal (Cr, Get_Object (Layout));
   end Show_Layout;

end Pango.Cairo;
