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

with System;
with Gtk.Widget;

package body Gdk.Rgb is

   ---------------------
   -- Draw_Gray_Image --
   ---------------------

   procedure Draw_Gray_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in Glib.Gint;
      Width, Height : in Glib.Gint;
      Dith          : in Gdk_Rgb_Dither;
      Rgb_Buf       : in String;
      Rowstride     : in Glib.Gint)
   is
      procedure Internal (Drawable            : System.Address;
                          GC                  : System.Address;
                          X, Y, Width, Height : Glib.Gint;
                          Dith                : Integer;
                          Rgb_Buf             : System.Address;
                          Rowstride           : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_gray_image");
   begin
      Internal (Get_Object (Drawable), Get_Object (GC), X, Y,
                Width, Height,
                Gdk_Rgb_Dither'Pos (Dith), Rgb_Buf'Address, Rowstride);
   end Draw_Gray_Image;

   ------------------------
   -- Draw_Indexed_Image --
   ------------------------

   procedure Draw_Indexed_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in     Glib.Gint;
      Width, Height : in     Glib.Gint;
      Dith          : in     Gdk_Rgb_Dither;
      Rgb_Buf       : in     String;
      Rowstride     : in     Glib.Gint;
      Cmap          : in     Gdk_Rgb_Cmap)
   is
      procedure Internal (Drawable            : System.Address;
                          GC                  : System.Address;
                          X, Y, Width, Height : Glib.Gint;
                          Dith                : Integer;
                          Rgb_Buf             : System.Address;
                          Rowstride           : Glib.Gint;
                          Cmap                : System.Address);
      pragma Import (C, Internal, "gdk_draw_indexed_image");
   begin
      Internal (Get_Object (Drawable), Get_Object (GC), X, Y, Width, Height,
                Gdk_Rgb_Dither'Pos (Dith),
                Rgb_Buf'Address, Rowstride, Cmap'Address);
   end Draw_Indexed_Image;

   -----------------------
   -- Draw_Rgb_32_Image --
   -----------------------

   procedure Draw_Rgb_32_Image
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in     Glib.Gint;
      Width, Height : in     Glib.Gint;
      Dith          : in     Gdk_Rgb_Dither;
      Rgb_Buf       : in     String;
      Rowstride     : in     Glib.Gint)
   is
      procedure Internal (Drawable            : System.Address;
                          GC                  : System.Address;
                          X, Y, Width, Height : Glib.Gint;
                          Dith                : Integer;
                          Rgb_Buf             : System.Address;
                          Rowstride           : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_32_image");
   begin
      Internal (Get_Object (Drawable), Get_Object (GC), X, Y,
                Width, Height,
                Gdk_Rgb_Dither'Pos (Dith), Rgb_Buf'Address, Rowstride);
   end Draw_Rgb_32_Image;

   --------------------
   -- Draw_Rgb_Image --
   --------------------

   procedure Draw_Rgb_Image (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
                             GC            : in out Gdk.GC.Gdk_GC;
                             X, Y          : in     Glib.Gint;
                             Width, Height : in     Glib.Gint;
                             Dith          : in     Gdk_Rgb_Dither;
                             Rgb_Buf       : in     String;
                             Rowstride     : in     Glib.Gint)
   is
      procedure Internal (Drawable            : System.Address;
                          GC                  : System.Address;
                          X, Y, Width, Height : Glib.Gint;
                          Dith                : Integer;
                          Rgb_Buf             : System.Address;
                          Rowstride           : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_image");
   begin
      Internal (Get_Object (Drawable), Get_Object (GC), X, Y,
                Width, Height, Gdk_Rgb_Dither'Pos (Dith),
                Rgb_Buf'Address, Rowstride);
   end Draw_Rgb_Image;

   ------------------------------
   -- Draw_Rgb_Image_Dithalign --
   ------------------------------

   procedure Draw_Rgb_Image_Dithalign
     (Drawable      : in out Gdk.Drawable.Gdk_Drawable;
      GC            : in out Gdk.GC.Gdk_GC;
      X, Y          : in     Glib.Gint;
      Width, Height : in     Glib.Gint;
      Dith          : in     Gdk_Rgb_Dither;
      Rgb_Buf       : in     String;
      Rowstride     : in     Glib.Gint;
      Xdith, Ydith  : in     Glib.Gint)
   is
      procedure Internal (Drawable            : System.Address;
                          GC                  : System.Address;
                          X, Y, Width, Height : Glib.Gint;
                          Dith                : Integer;
                          Rgb_Buf             : System.Address;
                          Rowstride           : Glib.Gint;
                          Xdith, Ydith        : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_image_dithalign");
   begin
      Internal (Get_Object (Drawable), Get_Object (GC), X, Y, Width,
                Height, Gdk_Rgb_Dither'Pos (Dith), Rgb_Buf'Address,
                Rowstride, Xdith, Ydith);
   end Draw_Rgb_Image_Dithalign;

   ----------
   -- Free --
   ----------

   procedure Free (Cmap : in out Gdk_Rgb_Cmap) is
      procedure Internal (Cmap : in out Gdk_Rgb_Cmap);
      pragma Import (C, Internal, "gdk_rgb_cmap_free");
   begin
      Internal (Cmap);
   end Free;

   -----------------------
   -- GC_Set_Background --
   -----------------------

   procedure GC_Set_Background
     (GC : in out Gdk.GC.Gdk_GC; Value : in Rgb_Item)
   is
      procedure Internal (GC : System.Address; Value : Rgb_Item);
      pragma Import (C, Internal, "gdk_rgb_gc_set_background");
   begin
      Internal (Get_Object (GC), Value);
   end GC_Set_Background;

   -----------------------
   -- GC_Set_Foreground --
   -----------------------

   procedure GC_Set_Foreground
     (GC : in out Gdk.GC.Gdk_GC; Value : in Rgb_Item)
   is
      procedure Internal (GC : System.Address; Value : Rgb_Item);
      pragma Import (C, Internal, "gdk_rgb_gc_set_foreground");
   begin
      Internal (Get_Object (GC), Value);
   end GC_Set_Foreground;

   ---------
   -- Get --
   ---------

   function Get (Cmap : Gdk_Rgb_Cmap; Index : Natural) return Rgb_Item is
   begin
      return Cmap.Colors (Index);
   end Get;

   --------------
   -- Get_Cmap --
   --------------

   function Get_Cmap return Gdk.Color.Gdk_Colormap is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_rgb_get_cmap");
      Cmap : Gdk.Color.Gdk_Colormap;
   begin
      Set_Object (Cmap, Internal);
      return Cmap;
   end Get_Cmap;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual return Gdk.Visual.Gdk_Visual is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_rgb_get_visual");
      Visual : Gdk.Visual.Gdk_Visual;
   begin
      Set_Object (Visual, Internal);
      return Visual;
   end Get_Visual;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Cmap   : in out Gdk_Rgb_Cmap;
                      Colors : in     Glib.Guint32_Array)
   is
      procedure Internal (Cmap     : in out Gdk_Rgb_Cmap;
                          Colors   :        System.Address;
                          N_Colors :        Integer);
      pragma Import (C, Internal, "gdk_rgb_cmap_new");
   begin
      Internal (Cmap, Colors'Address, Colors'Length);
   end Gtk_New;

   ----------
   -- Init --
   ----------

   procedure Init is
      procedure Internal;
      pragma Import (C, Internal, "gdk_rgb_init");
   begin
      Internal;
      Gtk.Widget.Set_Default_Colormap (Get_Cmap);
      Gtk.Widget.Set_Default_Visual (Get_Visual);
   end Init;

   ---------
   -- Set --
   ---------

   procedure Set
     (Cmap : in out Gdk_Rgb_Cmap; Index : Natural; Value : Rgb_Item)
   is
   begin
      Cmap.Colors (Index) := Value;
   end Set;

   ---------------------
   -- Xpixel_From_Rgb --
   ---------------------

   function Xpixel_From_Rgb (Value : in Rgb_Item) return Glib.Gulong is
      function Internal (Value : Rgb_Item) return Glib.Gulong;
      pragma Import (C, Internal, "gdk_rgb_xpixel_from_rgb");
   begin
      return Internal (Value);
   end Xpixel_From_Rgb;

end Gdk.Rgb;
