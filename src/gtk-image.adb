-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with System;
with Gdk; use Gdk;
with Gdk.Visual;
with Gtk.Util; use Gtk.Util;

package body Gtk.Image is

   ---------
   -- Get --
   ---------

   procedure Get
     (Image : access Gtk_Image_Record;
      Val   : in Gdk.Image.Gdk_Image'Class;
      Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
        (Image : in System.Address;
         Val   : in System.Address;
         Mask  : in System.Address);
      pragma Import (C, Internal, "gtk_image_get");

   begin
      Internal (Get_Object (Image), Get_Object (Val), Get_Object (Mask));
   end Get;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Image : out Gtk_Image;
      Val   : in Gdk.Image.Gdk_Image'Class;
      Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Val, Mask);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Image : access Gtk_Image_Record'Class;
      Val    : in Gdk.Image.Gdk_Image'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      function Internal
        (Val    : in System.Address;
         Mask   : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_image_new");

   begin
      Set_Object (Image, Internal (Get_Object (Val), Get_Object (Mask)));
      Initialize_User_Data (Image);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
     (Image : access Gtk_Image_Record;
      Val   : in Gdk.Image.Gdk_Image'Class;
      Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
        (Image : in System.Address;
         Val   : in System.Address;
         Mask  : in System.Address);
      pragma Import (C, Internal, "gtk_image_set");

   begin
      Internal (Get_Object (Image), Get_Object (Val), Get_Object (Mask));
   end Set;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
   begin
      if not N.Specific_Data.Created then
         Add_Package ("Gdk.Image");
         Add_Package ("Gdk.Bitmap");
         Add_Package ("Gdk.Visual");

         Put_Line (File, "   Get_System (The_Visual);");
         Put_Line (File, "   Gdk_New (The_Image, " &
           To_Ada (Get_Field (N, "image_type").all) & ", The_Visual, " &
           Get_Field (N, "image_width").all & ", " &
           Get_Field (N, "image_height").all & ");");
         Gen_New (N, "Image", "The_Image", "Null_Bitmap", File => File);
      end if;

      Misc.Generate (N, File);
   end Generate;

   procedure Generate (Image : in out Object.Gtk_Object; N : in Node_Ptr) is
      use Gdk.Image, Gdk.Bitmap, Gdk.Visual;

      Img    : Gdk_Image;
      Visual : Gdk_Visual;
      S      : String_Ptr;

   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "image_type");
         Get_System (Visual);
         Gdk_New (Img,
           Gdk_Image_Type'Value (S (S'First + 4 .. S'Last)), Visual,
           Gint'Value (Get_Field (N, "image_width").all),
           Gint'Value (Get_Field (N, "image_height").all));
         Gtk_New (Gtk_Image (Image), Img, Null_Bitmap);
         Set_Object (Get_Field (N, "name"), Image);
         N.Specific_Data.Created := True;
      end if;

      Misc.Generate (Image, N);
   end Generate;

end Gtk.Image;
