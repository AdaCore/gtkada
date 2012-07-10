------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Cairo is

   ------------------------
   -- Should_Draw_Window --
   ------------------------

   function Should_Draw_Window
      (Cr     : Cairo.Cairo_Context;
       Window : Gdk.Gdk_Window) return Boolean
   is
      function Internal
         (Cr     : Cairo.Cairo_Context;
          Window : Gdk.Gdk_Window) return Integer;
      pragma Import (C, Internal, "gtk_cairo_should_draw_window");
   begin
      return Boolean'Val (Internal (Cr, Window));
   end Should_Draw_Window;

   -------------------------
   -- Transform_To_Window --
   -------------------------

   procedure Transform_To_Window
      (Cr     : Cairo.Cairo_Context;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal
         (Cr     : Cairo.Cairo_Context;
          Widget : System.Address;
          Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_cairo_transform_to_window");
   begin
      Internal (Cr, Get_Object (Widget), Window);
   end Transform_To_Window;

end Gtk.Cairo;
