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

--  <description>
--  The Gtk_Image widget displays a graphical image. The image is typically
--  created using Gdk.Image.Gdk_New.
--
--  The pixels in a Gtk_Image may be manipulated by the application after
--  creation, as Gtk_Image store the pixel data on the client side. If you wish
--  to store the pixel data on the server side (thus not allowing manipulation
--  of the data after creation) you should use Gtk_Pixmap.
--  </description>
--  <c_version>1.2.8</c_version>

with Gdk.Bitmap;
with Gdk.Image;
with Gtk.Misc;

package Gtk.Image is

   type Gtk_Image_Record is new Gtk.Misc.Gtk_Misc_Record with private;
   type Gtk_Image is access all Gtk_Image_Record'Class;

   procedure Gtk_New
     (Image : out Gtk_Image;
      Val   : in Gdk.Image.Gdk_Image;
      Mask  : in Gdk.Bitmap.Gdk_Bitmap);

   procedure Initialize
     (Image : access Gtk_Image_Record'Class;
      Val   : in Gdk.Image.Gdk_Image;
      Mask  : in Gdk.Bitmap.Gdk_Bitmap);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Image.

   procedure Get
     (Image : access Gtk_Image_Record;
      Val   : out Gdk.Image.Gdk_Image;
      Mask  : out Gdk.Bitmap.Gdk_Bitmap);
   --  Get the values of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   procedure Set
     (Image : access Gtk_Image_Record;
      Val   : in Gdk.Image.Gdk_Image;
      Mask  : in Gdk.Bitmap.Gdk_Bitmap);
   --  Set the values of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

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
