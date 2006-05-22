-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

with Gtk.Enums;             use Gtk.Enums;
with System;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body Gtk.Image is

   ---------
   -- Get --
   ---------

   procedure Get
     (Image : access Gtk_Image_Record;
      Val   : out Gdk.Image.Gdk_Image;
      Mask  : out Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Image : System.Address;
         Val   : out Gdk.Image.Gdk_Image;
         Mask  : out Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_get_image");

   begin
      Internal (Get_Object (Image), Val, Mask);
   end Get;

   procedure Get
     (Image  : access Gtk_Image_Record;
      Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
      Mask   : out Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Image  : System.Address;
         Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
         Mask   : out Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_get_pixmap");

   begin
      Internal (Get_Object (Image), Pixmap, Mask);
   end Get;

   function Get
     (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Image : System.Address) return Gdk.Pixbuf.Gdk_Pixbuf;
      pragma Import (C, Internal, "gtk_image_get_pixbuf");

   begin
      return Internal (Get_Object (Image));
   end Get;

   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String
   is
      procedure Internal
        (Image    : System.Address;
         Stock_Id : out Interfaces.C.Strings.chars_ptr;
         Size     : out Gint);
      pragma Import (C, Internal, "gtk_image_get_stock");

      Stock : Interfaces.C.Strings.chars_ptr;
      Sze   : Gint;

   begin
      Internal (Get_Object (Image), Stock, Sze);
      Size.all := Gtk.Enums.Gtk_Icon_Size'Val (Sze);
      return Interfaces.C.Strings.Value (Stock);
   end Get;

   procedure Get
     (Image    : access Gtk_Image_Record;
      Icon_Set : out Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : out Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
        (Image    : System.Address;
         Icon_Set : out Gtk.Icon_Factory.Gtk_Icon_Set;
         Size     : out Gint);
      pragma Import (C, Internal, "gtk_image_get_icon_set");

      Sze : Gint;

   begin
      Internal (Get_Object (Image), Icon_Set, Sze);
      Size := Gtk.Enums.Gtk_Icon_Size'Val (Sze);
   end Get;

   function Get
     (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf_Animation
   is
      function Internal
        (Image : System.Address) return Gdk.Pixbuf.Gdk_Pixbuf_Animation;
      pragma Import (C, Internal, "gtk_image_get_animation");
   begin
      return Internal (Get_Object (Image));
   end Get;

   ----------------------
   -- Get_Storage_Type --
   ----------------------

   function Get_Storage_Type
     (Image : access Gtk_Image_Record) return Gtk_Image_Type
   is
      function Internal (Image : System.Address) return Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_image_get_storage_type");

   begin
      return Internal (Get_Object (Image));
   end Get_Storage_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Image : out Gtk_Image;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Val, Mask);
   end Gtk_New;

   procedure Gtk_New
     (Image  : out Gtk_Image;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Pixmap, Mask);
   end Gtk_New;

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Filename : String) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Filename);
   end Gtk_New;

   procedure Gtk_New
     (Image  : out Gtk_Image;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Pixbuf);
   end Gtk_New;

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Stock_Id, Size);
   end Gtk_New;

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Icon_Set, Size);
   end Gtk_New;

   procedure Gtk_New
     (Image     : out Gtk_Image;
      Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation) is
   begin
      Image := new Gtk_Image_Record;
      Initialize (Image, Animation);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Image : access Gtk_Image_Record'Class;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
        (Val    : Gdk.Image.Gdk_Image;
         Mask   : Gdk.Bitmap.Gdk_Bitmap) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_image");

   begin
      Set_Object (Image, Internal (Val, Mask));
   end Initialize;

   procedure Initialize
     (Image  : access Gtk_Image_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
        (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
         Mask   : Gdk.Bitmap.Gdk_Bitmap) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_pixmap");

   begin
      Set_Object (Image, Internal (Pixmap, Mask));
   end Initialize;

   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Filename : String)
   is
      function Internal (Filename : String) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_file");

   begin
      Set_Object (Image, Internal (Filename & ASCII.NUL));
   end Initialize;

   procedure Initialize
     (Image  : access Gtk_Image_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      function Internal (Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_pixbuf");

   begin
      Set_Object (Image, Internal (Pixbuf));
   end Initialize;

   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
        (Stock_Id : String;
         Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_stock");

   begin
      Set_Object (Image, Internal (Stock_Id & ASCII.NUL, Size));
   end Initialize;

   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
        (Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
         Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_set");

   begin
      Set_Object (Image, Internal (Icon_Set, Size));
   end Initialize;

   procedure Initialize
     (Image     : access Gtk_Image_Record'Class;
      Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
      function Internal
        (Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_animation");
   begin
      Set_Object (Image, Internal (Animation));
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
     (Image : access Gtk_Image_Record;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Image : System.Address;
         Val   : Gdk.Image.Gdk_Image;
         Mask  : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_set_from_image");

   begin
      Internal (Get_Object (Image), Val, Mask);
   end Set;

   procedure Set
     (Image  : access Gtk_Image_Record;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Image  : System.Address;
         Pixmap : Gdk.Pixmap.Gdk_Pixmap;
         Mask   : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_set_from_pixmap");

   begin
      Internal (Get_Object (Image), Pixmap, Mask);
   end Set;

   procedure Set (Image : access Gtk_Image_Record; File : String) is
      procedure Internal
        (Image : System.Address;
         File  : String);
      pragma Import (C, Internal, "gtk_image_set_from_file");

   begin
      Internal (Get_Object (Image), File & ASCII.NUL);
   end Set;

   procedure Set
     (Image : access Gtk_Image_Record; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Image  : System.Address;
         Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
      pragma Import (C, Internal, "gtk_image_set_from_pixbuf");

   begin
      Internal (Get_Object (Image), Pixbuf);
   end Set;

   procedure Set
     (Image    : access Gtk_Image_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
        (Image    : System.Address;
         Stock_Id : String;
         Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_stock");

   begin
      Internal (Get_Object (Image), Stock_Id & ASCII.NUL, Size);
   end Set;

   procedure Set
     (Image    : access Gtk_Image_Record;
      Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
        (Image : System.Address;
         Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
         Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_icon_set");

   begin
      Internal (Get_Object (Image), Icon_Set, Size);
   end Set;

   procedure Set
     (Image     : access Gtk_Image_Record;
      Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
      procedure Internal
        (Image     : System.Address;
         Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gtk_image_set_from_animation");
   begin
      Internal (Get_Object (Image), Animation);
   end Set;

   ------------------------
   -- Set_From_Icon_Name --
   ------------------------

   procedure Set_From_Icon_Name
     (Image     : access Gtk_Image_Record;
      Icon_Name : String;
      Size      : Gtk_Icon_Size)
   is
      procedure Internal
        (Image     : System.Address;
         Icon_Name : String;
         Size      : Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_icon_name");
   begin
      Internal (Get_Object (Image), Icon_Name & ASCII.NUL, Size);
   end Set_From_Icon_Name;

   --------------------
   -- Set_Pixel_Size --
   --------------------

   procedure Set_Pixel_Size
     (Image : access Gtk_Image_Record; Pixel_Size : Gint) is
      procedure Internal (Image : System.Address; Pixel_Size : Gint);
      pragma Import (C, Internal, "gtk_image_set_pixel_size");
   begin
      Internal (Get_Object (Image), Pixel_Size);
   end Set_Pixel_Size;

   ----------------------------
   -- Gtk_New_From_Icon_Name --
   ----------------------------

   procedure Gtk_New_From_Icon_Name
     (Image     : out Gtk_Image;
      Icon_Name : String;
      Size      : Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Initialize_From_Icon_Name (Image, Icon_Name, Size);
   end Gtk_New_From_Icon_Name;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
     (Image     : access Gtk_Image_Record'Class;
      Icon_Name : String;
      Size      : Gtk_Icon_Size)
   is
      function Internal
        (Icon_Name : String;
         Size      : Gtk_Icon_Size)
         return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_name");
   begin
      Set_Object (Image, Internal (Icon_Name & ASCII.NUL, Size));
   end Initialize_From_Icon_Name;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Image : out Gtk_Image) is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Image : access Gtk_Image_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_image_new");
   begin
      Set_Object (Image, Internal);
   end Initialize;

   -------------------
   -- Get_Icon_Name --
   -------------------

   procedure Get_Icon_Name
     (Image : access Gtk_Image_Record;
      Name  : out GNAT.Strings.String_Access;
      Size  : out Gtk_Icon_Size)
   is
      procedure Internal
        (Image : System.Address;
         Name  : out Interfaces.C.Strings.chars_ptr;
         Size  : out Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_icon_name");
      Str : chars_ptr;
   begin
      Internal (Get_Object (Image), Str, Size);
      Name := new String'(Value (Str));
   end Get_Icon_Name;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   function Get_Pixel_Size (Image : access Gtk_Image_Record) return Gint is
      function Internal (Image : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_image_get_pixel_size");
   begin
      return Internal (Get_Object (Image));
   end Get_Pixel_Size;

   -----------
   -- Clear --
   -----------

   procedure Clear (Image : access Gtk_Image_Record) is
      procedure Internal (Image : System.Address);
      pragma Import (C, Internal, "gtk_image_clear");
   begin
      Internal (Get_Object (Image));
   end Clear;

end Gtk.Image;
