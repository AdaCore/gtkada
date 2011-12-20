------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Glib; use Glib;

package body Pango.Tabs is

   ---------------
   -- Pango_New --
   ---------------

   procedure Pango_New
     (Tab_Array           : out Pango_Tab_Array;
      Initial_Size        : Glib.Gint;
      Positions_In_Pixels : Boolean := True)
   is
      function Internal
        (Size : Glib.Gint; Pixels : Glib.Gboolean) return Pango_Tab_Array;
      pragma Import (C, Internal, "pango_tab_array_new");
   begin
      Tab_Array := Internal (Initial_Size, Boolean'Pos (Positions_In_Pixels));
   end Pango_New;

   -----------------------------
   -- Get_Positions_In_Pixels --
   -----------------------------

   function Get_Positions_In_Pixels
     (Tab_Array : Pango_Tab_Array) return Boolean
   is
      function Internal (Tab : Pango_Tab_Array) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_tab_array_get_positions_in_pixels");
   begin
      return Boolean'Val (Internal (Tab_Array));
   end Get_Positions_In_Pixels;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Src    : Pango_Tab_Array;
      Target : out Pango_Tab_Array)
   is
      function Internal (Src : Pango_Tab_Array) return Pango_Tab_Array;
      pragma Import (C, Internal, "pango_tab_array_copy");
   begin
      Target := Internal (Src);
   end Copy;

end Pango.Tabs;
