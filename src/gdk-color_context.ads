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

with Gdk.Color;
with Gdk.Visual;
with Glib;

package Gdk.Color_Context is

   type Gdk_Color_Context is new Gdk.C_Proxy;
   Null_Color_Context : constant Gdk_Color_Context;

   type Color_Description is
      record
         Red   : Glib.Gushort;
         Green : Glib.Gushort;
         Blue  : Glib.Gushort;
      end record;

   type Color_Description_Array is
      array (Positive range <>) of Color_Description;


   procedure Gdk_New (CC       :    out Gdk_Color_Context;
                      Visual   : in     Gdk.Visual.Gdk_Visual;
                      Colormap : in     Gdk.Color.Gdk_Colormap);

   procedure Gdk_New_Mono (CC       :    out Gdk_Color_Context;
                           Visual   : in     Gdk.Visual.Gdk_Visual;
                           Colormap : in     Gdk.Color.Gdk_Colormap);

   procedure Free (CC : in out Gdk_Color_Context);

   procedure Get_Pixel (CC     : in     Gdk_Color_Context;
                        Color  : in     Color_Description;
                        Failed :    out Boolean;
                        Pixel  :    out Glib.Gulong);

   function Get_Pixels (CC     : in     Gdk_Color_Context;
                        Colors : in     Color_Description_Array)
                        return Glib.Gulong_Array;


   type Get_Pixel_Incremental_Data (Number_Of_Colors : Positive) is
      record
         Colors : Color_Description_Array (1 .. Number_Of_Colors);
         Used   : Glib.Boolean_Array (1 .. Number_Of_Colors);
         Pixels : Glib.Gulong_Array (1 .. Number_Of_Colors);
         Number_Of_Colors_Allocated : Natural;
         --
         --  This value reflects the number of valid pixels that
         --  have been allocated and can acutally be read in the
         --  Pixels array.
      end record;

   procedure Get_Pixels_Incremental
     (CC   : in     Gdk_Color_Context;
      Data : in out Get_Pixel_Incremental_Data);
   --
   --  FIXME: This procedure needs to be reviewed by Manu to get approval
   --  FIXME: that it has been properly bound. I don't see how it is used,
   --  FIXME: especially for the Used parameter.
   --  FIXME:
   --  FIXME: Manu, once you've approved this procedure (or adapted it
   --  FIXME: to your needs), please remove this comment.

   procedure Query_Color (CC    : in     Gdk_Color_Context;
                          Color :    out Gdk.Color.Gdk_Color);


   procedure Query_Colors (CC     : in     Gdk_Color_Context;
                           Colors : in out Gdk.Color.Gdk_Color_Array);

   procedure Add_Palette (CC           : in     Gdk_Color_Context;
                          Palette      : in     Gdk.Color.Gdk_Color_Array;
                          Palette_Size :    out Glib.Gint);
   --
   --  Palette_Size corresponds to the number of colors actually allocated
   --  in the Color_Context palette. It can be equal or less than the
   --  length of the Palette array.

   procedure Init_Dither (CC : in Gdk_Color_Context);

   procedure Free_Dither (CC : in Gdk_Color_Context);

   procedure Get_Pixel_From_Palette (CC     : in     Gdk_Color_Context;
                                     Color  : in out Color_Description;
                                     Failed :    out Boolean;
                                     Pixel  :    out Glib.Gulong);

   procedure Get_Index_From_Palette (CC     : in     Gdk_Color_Context;
                                     Color  : in out Color_Description;
                                     Failed :    out Boolean;
                                     Index  :    out Glib.Gint);
   --
   --  In the interface, the type returned is guchar, but the code returns
   --  a Gint, which seems more reasonable to me. So, let's return a Gint.

private
   Null_Color_Context : constant Gdk_Color_Context := null;
   pragma Import (C, Free_Dither, "gdk_color_context_free_dither");
   pragma Import (C, Init_Dither, "gdk_color_context_init_dither");
   pragma Import (C, Query_Color, "gdk_color_context_query_color");
end Gdk.Color_Context;
