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

package Gdk.Rgb is

   type Gdk_Rgb_Cmap is private;
   --  This is the full colormap, ie a set of 256 Rbg items.
   --  You can extract values using the functions Get or Set below.

   subtype Rgb_Item  is Glib.Guint32;
   --  This is a single Rbg value, that can be extracted from the colormap

   type Gdk_Rgb_Dither is (Dither_None, Dither_Normal, Dither_Max);


   function Get (Cmap : Gdk_Rgb_Cmap; Index : Natural) return Rgb_Item;
   procedure Set
     (Cmap : in out Gdk_Rgb_Cmap; Index : Natural; Value : Rgb_Item);
   --  Access items of the colormap


   procedure Init;
   --  Must be called once at the beginning to initialize internal data

   function Xpixel_From_Rgb (Value : in Rgb_Item) return Glib.Gulong;

   procedure GC_Set_Foreground
     (GC : in out Gdk.GC.Gdk_GC; Value : in Rgb_Item);

   procedure GC_Set_Background
     (GC : in out Gdk.GC.Gdk_GC; Value : in Rgb_Item);

   procedure Draw_Rgb_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in String;
      Rowstride     : in Glib.Gint);

   procedure Draw_Rgb_Image_Dithalign
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in String;
      Rowstride     : in Glib.Gint;
      Xdith, Ydith  : in Glib.Gint);

   procedure Draw_Rgb_32_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in String;
      Rowstride     : in Glib.Gint);

   procedure Draw_Gray_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in String;
      Rowstride     : in Glib.Gint);

   procedure Gtk_New
     (Cmap : in out Gdk_Rgb_Cmap; Colors : in Glib.Guint32_Array);

   procedure Free
     (Cmap : in out Gdk_Rgb_Cmap);

   procedure Draw_Indexed_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in String;
      Rowstride     : in Glib.Gint;
      Cmap          : in Gdk_Rgb_Cmap);

   function Get_Cmap return Gdk.Color.Gdk_Colormap;

   function Get_Visual return Gdk.Visual.Gdk_Visual;

private
   type Gdk_Rgb_Cmap is
      record
         Colors : Glib.Guint32_Array (1 .. 256);
         Lut    : Glib.Guchar_Array (1 .. 256);   -- for 8-bit modes
      end record;

   pragma Inline (Get);
   pragma Inline (Set);

end Gdk.Rgb;
