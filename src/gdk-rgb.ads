-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Glib;
with Gdk.Color;
with Gdk.Visual;
with Gdk.GC;
with Gdk.Drawable;
with System;

package Gdk.Rgb is

   --  This package implements a client-side pixmap. As opposed to the pixmaps
   --  found in Gdk.Pixmap, this one simply implements a local buffer, which
   --  can be manipulated at the pixel level easily. This buffer then needs to
   --  be sent to the server.
   --  The major efficiency difference is that the same amount of data needs
   --  to be sent to the server no matter how much things were modified.
   --  Gdk.Pixmaps requires one communication with the server per drawing
   --  function.
   --  Some X servers are also optimized so that the buffers in this package
   --  can be implemented in shared memory with the server, which of course
   --  makes it much faster to transfer the data.
   --  This package is basically an implementation of XImage (on X-Window),
   --  which means that it handles transparently different depths, byte
   --  ordering,... It also provides some color dithering functions.
   --
   --  Note that you need to initialize this package before using it by
   --  calling the Init procedure below.
   --
   --  See the commands Get_Visual and Get_Cmap below on how to use the
   --  colormaps and visual with this package
   --
   --  Dithering simulates a higher number of colors than what is available on
   --  the current visual (only for 8-bit and 16-bit displays).


   procedure Init;
   --  This function must be called before using any of the functions below. It
   --  initializes internal data, such as the visual and the colormap that will
   --  be used.


   function Get_Visual return Gdk.Visual.Gdk_Visual;
   function Get_Cmap return Gdk.Color.Gdk_Colormap;
   --  Returns the visual and the color map used internally in this package.
   --  Note that these are not the same as returned by Gtk.Widget or
   --  Gdk.Window, and you should use these if you are using this package.
   --
   --  The drawable you intend to copy the RBG buffer to must use this visual
   --  and this colormap. Therefore, before creating the widget, you need to do
   --  the following:
   --      Gtk.Widget.Push_Visual (Gdk.Rgb.Get_Visual);
   --      Gtk.Widget.Push_Colormap (Gdk.Rgb.Get_Cmap);
   --      Gtk_New (....)
   --      Gtk.Widget.Pop_Visual (Gdk.Rgb.Get_Visual);
   --      Gtk.Widget.Pop_Colormap (Gdk.Rgb.Get_Cmap);


   type Rgb_Buffer is array (Natural range <>) of Glib.Guchar;
   --  This is the buffer that will contain the image. You can manipulate each
   --  byte in it independantly, although there is no high level routine
   --  to draw lines, circles, ...
   --  Once you are done drawing into this buffer, you can copy it to any
   --  drawable on the screen, *if* the widget was created with the correct
   --  visual and colormap (see above).


   type Gdk_Rgb_Dither is (Dither_None, Dither_Normal, Dither_Max);
   --  The three kinds of dithering that are implemented in this package:
   --  Dither_None: No dithering will be done
   --  Dither_Normal: Specifies dithering on 8 bit displays, but not 16-bit.
   --                 Usually the best choice.
   --  Dither_Max: Specifies dithering on every kind of display

   --------------------------
   --  Color manipulation  --
   --------------------------

   subtype Rgb_Item is Glib.Guint32;
   --  This represents the coding for a rbg value. The exact encoding depends
   --  on the visual used and its depth (pseudo-color, true-color, ...)

   function Xpixel_From_Rgb (Value : in Rgb_Item) return Glib.Gulong;
   --  Converts the Rgb representation to the usual one found in Gdk.Color

   procedure GC_Set_Foreground
     (GC : in out Gdk.GC.Gdk_GC; Value : in Rgb_Item);
   procedure GC_Set_Background
     (GC : in out Gdk.GC.Gdk_GC; Value : in Rgb_Item);
   --  Modify the foreground and the background of a graphic context with a
   --  value. These are exactly the same functions has found in Gdk.Gc, but do
   --  not used the same parameters.


   -----------------------------
   --  Colormap manipulation  --
   -----------------------------

   type Gdk_Rgb_Cmap is private;
   --  This is the full colormap, ie a set of 256 Rbg items.
   --  You can extract values using the functions Get or Set below.

   type Rgb_Cmap_Index is new Natural range 0 .. 255;

   function Get (Cmap : Gdk_Rgb_Cmap; Index : Rgb_Cmap_Index) return Rgb_Item;
   function Get_8 (Cmap : Gdk_Rgb_Cmap; Index : Rgb_Cmap_Index)
                  return Glib.Guchar;
   procedure Set
     (Cmap : in out Gdk_Rgb_Cmap; Index : Rgb_Cmap_Index; Value : Rgb_Item);
   procedure Set_8
     (Cmap : in out Gdk_Rgb_Cmap; Index : Rgb_Cmap_Index; Value : Glib.Guchar);
   --  Access items of the colormap
   --  The *_8 functions should be used with 8-bit displays

   procedure Gtk_New
     (Cmap : in out Gdk_Rgb_Cmap; Colors : in Glib.Guint32_Array);

   procedure Free (Cmap : in out Gdk_Rgb_Cmap);


   ----------------------
   --  Drawing Images  --
   ----------------------

   procedure Draw_Rgb_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in Rgb_Buffer;
      Rowstride     : in Glib.Gint);
   --  Renders a Gdb buffer with 24 bit Data. Such a buffer is a one
   --  dimensional array of bytes, where every byte triplet makes up a pixel
   --  (byte 0 is red, byte 1 is green and byte 2 is blue).
   --  Width: Number of pixels (byte triplets) per row of the image
   --  Height: Number of rows in the image
   --  RowStride: Number of bytes between rows... (row n+1 will start at byte
   --     row n + Rowstride). Gdk.Rgb is faster is both the source pointer and
   --     the rowstride are aligned to a 4 byte boundary.
   --  X,Y,Width,Height: Defines a region in the target to copy the buffer to.


   procedure Draw_Rgb_Image_Dithalign
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in Rgb_Buffer;
      Rowstride     : in Glib.Gint;
      Xdith, Ydith  : in Glib.Gint);
   --  Same kind of function as above, but for different buffer types (???)

   procedure Draw_Rgb_32_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in Rgb_Buffer;
      Rowstride     : in Glib.Gint);
   --  Same kind of function as above, but for different buffer types (???)

   procedure Draw_Gray_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in Rgb_Buffer;
      Rowstride     : in Glib.Gint);
   --  Same kind of function as above, but for different buffer types (???)

   procedure Draw_Indexed_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in Rgb_Buffer;
      Rowstride     : in Glib.Gint;
      Cmap          : in Gdk_Rgb_Cmap);
   --  Same kind of function as above, but for different buffer types (???)


private
   type Gdk_Rgb_Cmap is new System.Address;

   pragma Inline (Get);
   pragma Inline (Set);

   pragma Import (C, Init, "gdk_rgb_init");

end Gdk.Rgb;
