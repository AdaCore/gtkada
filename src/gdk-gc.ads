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

with Glib; use Glib;
with Gdk.Color;
with Gdk.Font;
with Gdk.Rectangle;
with Gdk.Region;
with Gdk.Types;
with Gdk.Window;

package Gdk.GC is

   type Gdk_GC is new Root_Type with private;

   type Gdk_GC_Values is new Root_Type with private;


   procedure Gdk_New (GC     :    out Gdk_GC;
                      Window : in     Gdk.Window.Gdk_Window'Class);

   procedure Gdk_New (GC          :    out Gdk_GC;
                      Window      : in     Gdk.Window.Gdk_Window'Class;
                      Values      : in     Gdk_GC_Values'Class;
                      Values_Mask : in     Types.Gdk_GC_Values_Mask);

   procedure Destroy (GC : in out Gdk_GC);

   procedure Ref (GC : in out Gdk_GC);

   procedure Unref (GC : in out Gdk_GC);

   procedure Get_Values (GC     : in     Gdk_GC'Class;
                         Values :    out Gdk_GC_Values);

   procedure Set_Foreground (GC    : in Gdk_GC;
                             Color : in Gdk.Color.Gdk_Color);

   procedure Set_Background (GC     : in Gdk_GC;
                             Color  : in Gdk.Color.Gdk_Color);

   procedure Set_Font (GC   : in Gdk_GC;
                       Font : in Gdk.Font.Gdk_Font'Class);

   procedure Set_Function (GC   : in Gdk_GC;
                           Func : in Types.Gdk_Function);

   procedure Set_Fill (GC   : in Gdk_GC;
                       Fill : in Types.Gdk_Fill);

   --  procedure Set_Tile
   --  procedure Set_Stipple
   --
   --  Have been moved to the Gdk.GC.Pixmap child package for
   --  circular dependency reasons.

   procedure Set_Ts_Origin (GC   : in Gdk_GC;
                            X, Y : in Gint);

   procedure Set_Clip_Origin (GC   : in Gdk_GC;
                              X, Y : in Gint);

   procedure Set_Clip_Rectangle
     (GC        : in Gdk_GC;
      Rectangle : in Gdk.Rectangle.Gdk_Rectangle);

   procedure Set_Clip_Region (GC     : in Gdk_GC;
                              Region : in Gdk.Region.Gdk_Region'Class);

   procedure Set_Subwindow (GC   : in Gdk_GC;
                            Mode : in Types.Gdk_Subwindow_Mode);

   procedure Set_Exposures (GC        : in Gdk_GC;
                            Exposures : in Boolean);

   procedure Set_Line_Attributes (GC         : in Gdk_GC;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style);

   procedure Set_Dashes (Gc          : in Gdk_GC;
                         Dash_Offset : in Gint;
                         Dash_List   : in Gint_Array);

   procedure Copy (Dst_GC :    out Gdk_GC;
                   Src_GC : in     Gdk_GC);

private

   type Gdk_GC is new Root_Type with null record;
   type Gdk_GC_Values is new Root_Type with null record;


end Gdk.GC;
