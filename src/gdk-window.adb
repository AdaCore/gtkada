-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package body Gdk.Window is

   ----------------
   -- Clear_Area --
   ----------------

   procedure Clear_Area (Window : in Gdk_Window;
                         X      : in Gint;
                         Y      : in Gint;
                         Width  : in Gint;
                         Height : in Gint) is
      procedure Internal (Window : in System.Address;
                          X, Y, Width, Height : in Gint);
      pragma Import (C, Internal, "gdk_window_clear_area");
   begin
      Internal (Get_Object (Window), X, Y, Width, Height);
   end Clear_Area;

   ------------------
   -- Get_Colormap --
   ------------------

   function Get_Colormap (Window : in Gdk_Window)
                          return Gdk.Color.Gdk_Colormap
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_colormap");
      Cmap : Gdk.Color.Gdk_Colormap;
   begin
      Set_Object (Cmap, Internal (Get_Object (Window)));
      return Cmap;
   end Get_Colormap;

end Gdk.Window;
