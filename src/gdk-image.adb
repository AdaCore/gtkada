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

package body Gdk.Image is

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Image      : out Gdk_Image;
      Image_Type : Gdk_Image_Type;
      Visual     : Gdk.Gdk_Visual;
      Width      : Gint;
      Height     : Gint)
   is
      function Internal
        (Image_Type : Gdk_Image_Type;
         Visual     : Gdk.Gdk_Visual;
         Width      : Gint;
         Height     : Gint) return Gdk_Image;
      pragma Import (C, Internal, "gdk_image_new");

   begin
      Image := Internal (Image_Type, Visual, Width, Height);
   end Gdk_New;

   ---------
   -- Get --
   ---------

   procedure Get
     (Image    : out Gdk_Image;
      Drawable : Gdk.Gdk_Drawable;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint)
   is
      function Internal
        (Drawable : Gdk.Gdk_Drawable;
         X        : Gint;
         Y        : Gint;
         Width    : Gint;
         Height   : Gint) return Gdk_Image;
      pragma Import (C, Internal, "gdk_image_get");

   begin
      Image := Internal (Drawable, X, Y, Width, Height);
   end Get;

end Gdk.Image;
