-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Font;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gdk.Region;
with Gdk.Types;
with Gdk.Window;

package Gdk.GC is

   type Gdk_GC is new Root_Type with private;

   type Gdk_GC_Values is new Root_Type with private;


   procedure Gdk_New (GC     :    out Gdk_GC;
                      Window : in     Gdk.Window.Gdk_Window'Class);
   --  mapping: Gdk_New gdk.h gdk_gc_new

   procedure Gdk_New (GC          :    out Gdk_GC;
                      Window      : in     Gdk.Window.Gdk_Window;
                      Values      : in     Gdk_GC_Values'Class;
                      Values_Mask : in     Types.Gdk_Values_Mask);
   --  mapping: Gdk_New gdk.h gdk_gc_new_with_values

   procedure Destroy (GC : in out Gdk_GC);
   --  mapping: Destroy gdk.h gdk_gc_destroy

   procedure Get_Values (GC     : in     Gdk_GC'Class;
                         Values :    out Gdk_GC_Values);
   --  mapping: Get_Values gdk.h gdk_gc_get_values

   procedure Set_Foreground (GC    : in out Gdk_GC;
                             Color : in     Gdk.Color.Gdk_Color);
   --  mapping: Set_Foreground gdk.h gdk_gc_set_foreground

   procedure Set_Background (GC     : in out Gdk_GC;
                             Color  : in     Gdk.Color.Gdk_Color);
   --  mapping: Set_Background gdk.h gdk_gc_set_background

   procedure Set_Font (GC   : in out Gdk_GC;
                       Font : in     Gdk.Font.Gdk_Font);
   --  mapping: Set_Font gdk.h gdk_gc_set_font

   procedure Set_Function (GC   : in out Gdk_GC;
                           Func : in     Types.Gdk_Function);
   --  mapping: Set_Function gdk.h gdk_gc_set_function

   procedure Set_Fill (GC   : in out Gdk_GC;
                       Fill : in     Types.Gdk_Fill);
   --  mapping: Set_Fill gdk.h gdk_gc_set_fill

   procedure Set_Tile (GC   : in out Gdk_GC;
                       Tile : in     Pixmap.Gdk_Pixmap);
   --  mapping: Set_Tile gdk.h gdk_gc_set_tile

   procedure Set_Stipple (GC      : in out Gdk_GC;
                          Stipple : in     Pixmap.Gdk_Pixmap);
   --  mapping: Set_Stipple gdk.h gdk_gc_set_stipple

   procedure Set_Ts_Origin (GC   : in out Gdk_GC;
                            X, Y : in     Gint);
   --  mapping: Set_Ts_Origin gdk.h gdk_gc_set_ts_origin

   procedure Set_Clip_Origin (GC   : in out Gdk_GC;
                              X, Y : in     Gint);
   --  mapping: Set_Clip_Origin gdk.h gdk_gc_set_clip_origin

   procedure Set_Clip_Mask (GC    : in out Gdk_GC;
                            Mask  : in     Bitmap.Gdk_Bitmap);
   --  mapping: Set_Clip_Mask gdk.h gdk_gc_set_clip_mask

   procedure Set_Clip_Rectangle
     (GC        : in out Gdk_GC;
      Rectangle : in     Gdk.Rectangle.Gdk_Rectangle);
   --  mapping: Set_Clip_Rectangle gdk.h gdk_gc_set_clip_rectangle

   procedure Set_Clip_Region (GC     : in out Gdk_GC;
                              Region : in     Gdk.Region.Gdk_Region);
   --  mapping: Set_Clip_Region gdk.h gdk_gc_set_clip_region

   procedure Set_Subwindow (GC   : in out Gdk_GC;
                            Mode : in     Types.Gdk_Subwindow_Mode);
   --  mapping: Set_Subwindow gdk.h gdk_gc_set_subwindow

   procedure Set_Exposures (GC        : in out Gdk_GC;
                            Exposures : in     Boolean);
   --  mapping: Set_Exposures gdk.h gdk_gc_set_exposures

   procedure Set_Line_Attributes (GC         : in out Gdk_GC;
                                  Line_Width : in     Gint;
                                  Line_Style : in     Types.Gdk_Line_Style;
                                  Cap_Style  : in     Types.Gdk_Cap_Style;
                                  Join_Style : in     Types.Gdk_Join_Style);
   --  mapping: Set_Line_Attributes gdk.h gdk_gc_set_line_attributes

   procedure Copy (Dst_GC :    out Gdk_GC;
                   Src_GC : in     Gdk_GC);
   --  mapping: Copy gdk.h gdk_gc_copy

private

   type Gdk_GC is new Root_Type with null record;
   type Gdk_GC_Values is new Root_Type with null record;

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_gc_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_gc_unref

end Gdk.GC;
