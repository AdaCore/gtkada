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

package body Gdk.GC is


   ------------
   --  Copy  --
   ------------

   procedure Copy (Dst_GC :    out Gdk_GC;
                   Src_GC : in     Gdk_GC) is
      procedure Internal (Dst_GC :    out System.Address;
                          Src_GC : in     System.Address);
      pragma Import (C, Internal, "gdk_gc_copy");
      Temp : System.Address := System.Null_Address;
   begin
      Internal (Temp, Get_Object (Src_GC));
      Set_Object (Dst_GC, Temp);
   end Copy;


   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (GC : in out Gdk_GC) is
      procedure Internal (GC : in System.Address);
      pragma Import (C, Internal, "gdk_gc_destroy");
   begin
      Internal (Get_Object (GC));
      Set_Object (GC, System.Null_Address);
   end Destroy;


   ------------------
   --  Get_Values  --
   ------------------

   procedure Get_Values (GC     : in     Gdk_GC'Class;
                         Values :    out Gdk_GC_Values) is
      procedure Internal (GC : in System.Address;
                          Values : out System.Address);
      pragma Import (C, Internal, "gdk_gc_get_values");
      Temp : System.Address := System.Null_Address;
   begin
      Internal (Get_Object (GC), Temp);
      Set_Object (Values, Temp);
   end Get_Values;


   --------------
   -- Gdk_New  --
   --------------

   procedure Gdk_New (GC     :    out Gdk_GC;
                      Window : in     Gdk.Window.Gdk_Window) is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gc_new");
   begin
      Set_Object (GC, Internal (Get_Object (Window)));
   end Gdk_New;


   --------------
   -- Gdk_New  --
   --------------

   procedure Gdk_New (GC          :    out Gdk_GC;
                      Window      : in     Gdk.Window.Gdk_Window;
                      Values      : in     Gdk_GC_Values'Class;
                      Values_Mask : in     Types.Gdk_Values_Mask) is
      function Internal (Window : in System.Address;
                         Values : in System.Address;
                         Values_Mask : in Types.Gdk_Values_Mask)
                         return System.Address;
      pragma Import (C, Internal, "gdk_gc_new_with_values");
   begin
      Set_Object (GC, Internal (Get_Object (Window), Get_Object (Values),
                                Values_Mask));
   end Gdk_New;


   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (GC     : in out Gdk_GC;
                             Color  : in     Gdk.Color.Gdk_Color) is
      procedure Internal (GC, Color : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_background");
   begin
      Internal (Get_Object (GC), Get_Object (Color));
   end Set_Background;


   ---------------------
   --  Set_Clip_Mask  --
   ---------------------

   procedure Set_Clip_Mask (GC    : in out Gdk_GC;
                            Mask  : in     Bitmap.Gdk_Bitmap) is
      procedure Internal (GC, Mask : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_mask");
   begin
      Internal (Get_Object (GC), Get_Object (Mask));
   end Set_Clip_Mask;


   -----------------------
   --  Set_Clip_Origin  --
   -----------------------

   procedure Set_Clip_Origin (GC   : in out Gdk_GC;
                              X, Y : in     Gint) is
      procedure Internal (GC : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_clip_origin");
   begin
      Internal (Get_Object (GC), X, Y);
   end Set_Clip_Origin;


   --------------------------
   --  Set_Clip_Rectangle  --
   --------------------------

   procedure Set_Clip_Rectangle
     (GC        : in out Gdk_GC;
      Rectangle : in     Gdk.Rectangle.Gdk_Rectangle) is
      procedure Internal (GC, Rectangle : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_rectangle");
   begin
      Internal (Get_Object (GC), Get_Object (Rectangle));
   end Set_Clip_Rectangle;


   -----------------------
   --  Set_Clip_Region  --
   -----------------------

   procedure Set_Clip_Region (GC     : in out Gdk_GC;
                              Region : in     Gdk.Region.Gdk_Region) is
      procedure Internal (GC, Region : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_region");
   begin
      Internal (Get_Object (GC), Get_Object (Region));
   end Set_Clip_Region;


   ---------------------
   --  Set_Exposures  --
   ---------------------

   procedure Set_Exposures (GC        : in out Gdk_GC;
                            Exposures : in     Gint) is
      procedure Internal (GC : in System.Address; Exposures : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_exposures");
   begin
      Internal (Get_Object (GC), Exposures);
   end Set_Exposures;


   ----------------
   --  Set_Fill  --
   ----------------

   procedure Set_Fill (GC   : in out Gdk_GC;
                       Fill : in     Types.Gdk_Fill) is
      procedure Internal (GC : in System.Address; Fill : in Types.Gdk_Fill);
      pragma Import (C, Internal, "gdk_gc_set_fill");
   begin
      Internal (Get_Object (GC), Fill);
   end Set_Fill;


   ----------------
   --  Set_Font  --
   ----------------

   procedure Set_Font (GC   : in out Gdk_GC;
                       Font : in     Gdk.Font.Gdk_Font) is
      procedure Internal (GC, Font : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_font");
   begin
      Internal (Get_Object (GC), Get_Object (Font));
   end Set_Font;


   ----------------------
   --  Set_Foreground  --
   ----------------------

   procedure Set_Foreground (GC    : in out Gdk_GC;
                             Color : in     Gdk.Color.Gdk_Color) is
      procedure Internal (GC, Color : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_foreground");
   begin
      Internal (Get_Object (GC), Get_Object (Color));
   end Set_Foreground;


   --------------------
   --  Set_Function  --
   --------------------

   procedure Set_Function (GC   : in out Gdk_GC;
                           Func : in     Types.Gdk_Function) is
      procedure Internal (GC   : in System.Address;
                          Func : in Types.Gdk_Function);
      pragma Import (C, Internal, "gdk_gc_set_function");
   begin
      Internal (Get_Object (GC), Func);
   end Set_Function;


   ---------------------------
   --  Set_Line_Attributes  --
   ---------------------------

   procedure Set_Line_Attributes (GC         : in out Gdk_GC;
                                  Line_Width : in     Gint;
                                  Line_Style : in     Types.Gdk_Line_Style;
                                  Cap_Style  : in     Types.Gdk_Cap_Style;
                                  Join_Style : in     Types.Gdk_Join_Style) is
      procedure Internal (GC : in System.Address;
                          Line_Width : in     Gint;
                          Line_Style : in     Types.Gdk_Line_Style;
                          Cap_Style  : in     Types.Gdk_Cap_Style;
                          Join_Style : in     Types.Gdk_Join_Style);
      pragma Import (C, Internal, "gdk_gc_set_line_attributes");
   begin
      Internal (Get_Object (GC), Line_Width, Line_Style,
                Cap_Style, Join_Style);
   end Set_Line_Attributes;


   -------------------
   --  Set_Stipple  --
   -------------------

   procedure Set_Stipple (GC      : in out Gdk_GC;
                          Stipple : in     Pixmap.Gdk_Pixmap) is
      procedure Internal (GC, Stipple : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_stipple");
   begin
      Internal (Get_Object (GC), Get_Object (Stipple));
   end Set_Stipple;


   ---------------------
   --  Set_Subwindow  --
   ---------------------

   procedure Set_Subwindow (GC   : in out Gdk_GC;
                            Mode : in     Types.Gdk_Subwindow_Mode) is
      procedure Internal (GC : in System.Address;
                          Mode : in Types.Gdk_Subwindow_Mode);
      pragma Import (C, Internal, "gdk_gc_set_subwindow");
   begin
      Internal (Get_Object (GC), Mode);
   end Set_Subwindow;


   ----------------
   --  Set_Tile  --
   ----------------

   procedure Set_Tile (GC   : in out Gdk_GC;
                       Tile : in     Pixmap.Gdk_Pixmap) is
      procedure Internal (GC, Tile : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_tile");
   begin
      Internal (Get_Object (GC), Get_Object (Tile));
   end Set_Tile;


   ---------------------
   --  Set_Ts_Origin  --
   ---------------------

   procedure Set_Ts_Origin (GC   : in out Gdk_GC;
                            X, Y : in     Gint) is
      procedure Internal (GC : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_ts_origin");
   begin
      Internal (Get_Object (GC), X, Y);
   end Set_Ts_Origin;


   --  //  --


end Gdk.GC;
