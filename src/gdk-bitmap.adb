------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

package body Gdk.Bitmap is

   ----------------------
   -- Create_From_Data --
   ----------------------

   procedure Create_From_Data
     (Bitmap : out Gdk_Bitmap;
      Window : Gdk.Window.Gdk_Window;
      Data   : String;
      Width  : Gint;
      Height : Gint)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Data   : String;
         Width  : Gint;
         Height : Gint) return Gdk_Bitmap;
      pragma Import (C, Internal, "gdk_bitmap_create_from_data");

   begin
      Bitmap := Internal (Window, Data & ASCII.NUL, Width, Height);
   end Create_From_Data;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Bitmap : out Gdk_Bitmap;
      Window : Gdk.Window.Gdk_Window;
      Width  : Gint;
      Height : Gint)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Width  : Gint;
         Height : Gint;
         Depth  : Gint) return Gdk_Bitmap;
      pragma Import (C, Internal, "gdk_pixmap_new");

   begin
      Bitmap := Internal (Window, Width, Height, 1);
   end Gdk_New;

end Gdk.Bitmap;
