-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Gdk.Art.Pixbuf; use Gdk.Art.Pixbuf;
with Gdk.Bitmap;     use Gdk.Bitmap;
with Gdk.Drawable;   use Gdk.Drawable;
with Gdk.GC;         use Gdk.GC;

package body Gdk.Pixbuf is

   -------------------
   -- Get_Has_Alpha --
   -------------------

   function Get_Has_Alpha (Pixbuf : in Gdk_Pixbuf) return Boolean is
      function Internal (Pixbuf : Gdk_Pixbuf) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_has_alpha");
   begin
      return Boolean'Val (Internal (Pixbuf));
   end Get_Has_Alpha;

   -------------
   -- Gdk_New --
   -------------

   function Gdk_New (Width           : in Gint;
                     Height          : in Gint;
                     Format          : in Gdk.Art.Pixbuf.Art_Pix_Format :=
                       Gdk.Art.Pixbuf.Art_Pix_RGB;
                     Has_Alpha       : in Boolean := False;
                     Bits_Per_Sample : in Gint := 8)
                    return Gdk_Pixbuf
   is
      function Internal (Format          : in Art_Pix_Format;
                         Has_Alpha       : in Gint;
                         Bits_Per_Sample : in Gint;
                         Width           : in Gint;
                         Height          : in Gint)
                        return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_new");
   begin
      return Internal (Format, Boolean'Pos (Has_Alpha),
                       Bits_Per_Sample, Width, Height);
   end Gdk_New;

   -------------------
   -- New_From_File --
   -------------------

   function New_From_File (Filename : in String) return Gdk_Pixbuf is
      function Internal (Filename : String) return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_file");
   begin
      return Internal (Filename & ASCII.Nul);
   end New_From_File;

   ---------------
   -- Add_Alpha --
   ---------------

   function Add_Alpha (Pixbuf           : in Gdk_Pixbuf;
                       Substitute_Color : in Boolean;
                       Red              : in Guchar := 0;
                       Green            : in Guchar := 0;
                       Blue             : in Guchar := 0)
                      return Gdk_Pixbuf
   is
      function Internal (Pixbuf           : in Gdk_Pixbuf;
                         Substitute_Color : in Gboolean;
                         Red              : in Guchar;
                         Green            : in Guchar;
                         Blue             : in Guchar)
                        return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_add_alpha");
   begin
      return Internal (Pixbuf, Boolean'Pos (Substitute_Color),
                       Red, Green, Blue);
   end Add_Alpha;

   ------------------------
   -- Render_To_Drawable --
   ------------------------

   procedure Render_To_Drawable
     (Pixbuf   : in Gdk_Pixbuf;
      Drawable : in Gdk.Drawable.Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Src_X    : in Gint;
      Src_Y    : in Gint;
      Dest_X   : in Gint;
      Dest_Y   : in Gint;
      Width    : in Gint;
      Height   : in Gint;
      Dither   : in Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither : in Gint := 0;
      Y_Dither : in Gint := 0)
   is
      procedure Internal (Pixbuf   : in Gdk_Pixbuf;
                          Drawable : in Gdk.Drawable.Gdk_Drawable;
                          Gc       : in Gdk.GC.Gdk_GC;
                          Src_X    : in Gint;
                          Src_Y    : in Gint;
                          Dest_X   : in Gint;
                          Dest_Y   : in Gint;
                          Width    : in Gint;
                          Height   : in Gint;
                          Dither   : in Gint;
                          X_Dither : in Gint;
                          Y_Dither : in Gint);
      pragma Import (C, Internal, "gdk_pixbuf_render_to_drawable");
   begin
      Internal (Pixbuf, Drawable, Gc,
                Src_X, Src_Y, Dest_X, Dest_Y, Width, Height,
                Gdk.Rgb.Gdk_Rgb_Dither'Pos (Dither),
                X_Dither, Y_Dither);
   end Render_To_Drawable;

   ------------------------------
   -- Render_To_Drawable_Alpha --
   ------------------------------

   procedure Render_To_Drawable_Alpha
     (Pixbuf          : in Gdk_Pixbuf;
      Drawable        : in Gdk.Drawable.Gdk_Drawable;
      Src_X           : in Gint;
      Src_Y           : in Gint;
      Dest_X          : in Gint;
      Dest_Y          : in Gint;
      Width           : in Gint;
      Height          : in Gint;
      Alpha           : in Alpha_Mode;
      Alpha_Threshold : in Gint;
      Dither          : in Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither        : in Gint := 0;
      Y_Dither        : in Gint := 0)
   is
      procedure Internal (Pixbuf          : in Gdk_Pixbuf;
                          Drawable        : in Gdk.Drawable.Gdk_Drawable;
                          Src_X           : in Gint;
                          Src_Y           : in Gint;
                          Dest_X          : in Gint;
                          Dest_Y          : in Gint;
                          Width           : in Gint;
                          Height          : in Gint;
                          Alpha           : in Gint;
                          Alpha_Threshold : in Gint;
                          Dither          : in Gint;
                          X_Dither        : in Gint;
                          Y_Dither        : in Gint);
      pragma Import (C, Internal, "gdk_pixbuf_render_to_drawable_alpha");
   begin
      Internal (Pixbuf, Drawable,
                Src_X, Src_Y, Dest_X, Dest_Y, Width, Height,
                Alpha_Mode'Pos (Alpha),
                Alpha_Threshold,
                Gdk.Rgb.Gdk_Rgb_Dither'Pos (Dither),
                X_Dither, Y_Dither);
   end Render_To_Drawable_Alpha;

end Gdk.Pixbuf;
