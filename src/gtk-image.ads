-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

--  <description>
--  The Gtk_Image widget displays a graphical image. The image is typically
--  created using Gdk.Image.Gdk_New.
--
--  The pixels in a Gtk_Image may be manipulated by the application after
--  creation, as Gtk_Image store the pixel data on the client side. If you wish
--  to store the pixel data on the server side (thus not allowing manipulation
--  of the data after creation) you should use Gtk_Pixmap.
--  </description>
--  <c_version>1.3.4</c_version>

with Gdk.Bitmap;
with Gdk.Pixbuf;
with Gdk.Pixmap;
with Gdk.Image;
with Gtk.Enums;
with Gtk.Misc;

package Gtk.Image is

   type Gtk_Image_Record is new Gtk.Misc.Gtk_Misc_Record with private;
   type Gtk_Image is access all Gtk_Image_Record'Class;

   type Gtk_Icon_Set is new Glib.C_Proxy;
   --  Move this declaration in Gtk.Icon_Factory when it is bound ???

   type Gtk_Image_Type is
     (Image_Empty,
      Image_Pixmap,
      Image_Image,
      Image_Pixbuf,
      Image_Stock,
      Image_Icon_Set);

   procedure Gtk_New
     (Image : out Gtk_Image;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap);

   procedure Gtk_New
     (Image  : out Gtk_Image;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Filename : String);

   procedure Gtk_New
     (Image  : out Gtk_Image;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Icon_Set : Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size);

   procedure Initialize
     (Image : access Gtk_Image_Record'Class;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Image  : access Gtk_Image_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Internal initialization function.

   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Filename : String);
   --  Internal initialization function.

   procedure Initialize
     (Image  : access Gtk_Image_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Internal initialization function.

   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Internal initialization function.

   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Icon_Set : Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Internal initialization function.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Image.

   procedure Set
     (Image  : access Gtk_Image_Record;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Set the value of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   procedure Set
     (Image : access Gtk_Image_Record;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap);
   --  Set the value of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   procedure Set (Image : access Gtk_Image_Record; File : String);

   procedure Set
     (Image : access Gtk_Image_Record; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Set
     (Image    : access Gtk_Image_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);

   procedure Set
     (Image    : access Gtk_Image_Record;
      Icon_Set : Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size);

   function Get_Storage_Type
     (Image : access Gtk_Image_Record) return Gtk_Image_Type;

   procedure Get
     (Image  : access Gtk_Image_Record;
      Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
      Mask   : out Gdk.Bitmap.Gdk_Bitmap);
   --  Get the values of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   procedure Get
     (Image : access Gtk_Image_Record;
      Val   : out Gdk.Image.Gdk_Image;
      Mask  : out Gdk.Bitmap.Gdk_Bitmap);
   --  Get the values of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   function Get (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf;

   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String;

   procedure Get
     (Image    : access Gtk_Image_Record;
      Icon_Set : out Gtk_Icon_Set;
      Size     : out Gtk.Enums.Gtk_Icon_Size);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Image_Record is new Gtk.Misc.Gtk_Misc_Record with null record;

   pragma Import (C, Get_Type, "gtk_image_get_type");

end Gtk.Image;
