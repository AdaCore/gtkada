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
with Gdk.Pixmap;
with Gtk.Misc;

package Gtk.Pixmap is

   type Gtk_Pixmap is new Gtk.Misc.Gtk_Misc with private;

   procedure Get
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   function Get_Mask (Widget : in Gtk_Pixmap'Class)
                      return      Gdk.Bitmap.Gdk_Bitmap'Class;
   function Get_Pixmap (Widget : in Gtk_Pixmap'Class)
                        return      Gdk.Pixmap.Gdk_Pixmap'Class;
   procedure Gtk_New
      (Widget : out Gtk_Pixmap;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);

private
   type Gtk_Pixmap is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: Get gtkpixmap.h gtk_pixmap_get
   --  mapping: Get_Mask gtkpixmap.h GtkPixmap->mask
   --  mapping: Get_Pixmap gtkpixmap.h GtkPixmap->pixmap
   --  mapping: NOT_IMPLEMENTED gtkpixmap.h gtk_pixmap_get_type
   --  mapping: Gtk_New gtkpixmap.h gtk_pixmap_new
   --  mapping: Set gtkpixmap.h gtk_pixmap_set
end Gtk.Pixmap;
