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
--         General Public License for more details.                  --
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

with System;
with Gdk; use Gdk;

package body Gtk.Image is

   ---------
   -- Get --
   ---------

   procedure Get
      (Image : in Gtk_Image'Class;
       Val   : in Gdk.Image.Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Image : in System.Address;
          Val   : in System.Address;
          Mask  : in System.Address);
      pragma Import (C, Internal, "gtk_image_get");
   begin
      Internal (Get_Object (Image),
                Get_Object (Val),
                Get_Object (Mask));
   end Get;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Image;
       Val    : in Gdk.Image.Gdk_Image'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      function Internal
         (Val    : in System.Address;
          Mask   : in System.Address)
          return      System.Address;
      pragma Import (C, Internal, "gtk_image_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Val),
                                    Get_Object (Mask)));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image : in Gtk_Image'Class;
       Val   : in Gdk.Image.Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Image : in System.Address;
          Val   : in System.Address;
          Mask  : in System.Address);
      pragma Import (C, Internal, "gtk_image_set");
   begin
      Internal (Get_Object (Image),
                Get_Object (Val),
                Get_Object (Mask));
   end Set;

end Gtk.Image;
