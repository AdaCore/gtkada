-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------


with Gdk.Bitmap;
with Gdk.Image;
with Gtk.Misc;

package Gtk.Image is

   type Gtk_Image is new Gtk.Misc.Gtk_Misc with private;

   procedure Get
      (Image : in Gtk_Image'Class;
       Val   : in Gdk.Image.Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Gtk_New
      (Widget : out Gtk_Image;
       Val    : in Gdk.Image.Gdk_Image'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set
      (Image : in Gtk_Image'Class;
       Val   : in Gdk.Image.Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class);

private
   type Gtk_Image is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: Get gtkimage.h gtk_image_get
   --  mapping: NOT_IMPLEMENTED gtkimage.h gtk_image_get_type
   --  mapping: Gtk_New gtkimage.h gtk_image_new
   --  mapping: Set gtkimage.h gtk_image_set
end Gtk.Image;
