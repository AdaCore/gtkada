------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

package body Pango.Font_Metrics is

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Pango_Font_Metrics) is
      function Internal return Pango_Font_Metrics;
      pragma Import (C, Internal, "pango_font_metrics_new");
   begin
      Self := Internal;
   end Gdk_New;

   ----------------------------
   -- Pango_Font_Metrics_New --
   ----------------------------

   function Pango_Font_Metrics_New return Pango_Font_Metrics is
      function Internal return Pango_Font_Metrics;
      pragma Import (C, Internal, "pango_font_metrics_new");
      Self : Pango_Font_Metrics;
   begin
      Self := Internal;
      return Self;
   end Pango_Font_Metrics_New;

end Pango.Font_Metrics;
