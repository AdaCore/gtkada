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

package body Gdk.Color_Context is

   -------------------
   --  Add_Palette  --
   -------------------

   procedure Add_Palette (CC           : in  Gdk_Color_Context;
                          Palette      : in  Gdk.Color.Gdk_Color_Array;
                          Palette_Size : out Glib.Gint) is
      function Internal  (CC          : in System.Address;
                          Palette     : in System.Address;
                          Num_Palette : in Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gdk_color_context_add_palette");
   begin
      Palette_Size := Internal (CC => Get_Object (CC),
                                Palette => Palette'Address,
                                Num_Palette => Palette'Length);
   end Add_Palette;


   ------------
   --  Free  --
   ------------

   procedure Free (CC : in out Gdk_Color_Context) is
      procedure Internal (CC : in System.Address);
      pragma Import (C, Internal, "gdk_color_context_free");
   begin
      Internal (Get_Object (CC));
      Set_Object (CC, System.Null_Address);
   end Free;


   -------------------
   --  Free_Dither  --
   -------------------

   procedure Free_Dither (CC : in out Gdk_Color_Context) is
      procedure Internal (CC : in System.Address);
      pragma Import (C, Internal, "gdk_color_context_free_dither");
   begin
      Internal (Get_Object (CC));
   end Free_Dither;

   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (CC       :    out Gdk_Color_Context;
                      Visual   : in     Gdk.Visual.Gdk_Visual'Class;
                      Colormap : in     Gdk.Color.Gdk_Colormap'Class) is
      function Internal (Visual, Colormap : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "gdk_color_context_new");
   begin
      Set_Object (CC, Internal (Get_Object (Visual),
                                Get_Object (Colormap)));
   end Gdk_New;


   --------------------
   --  Gdk_New_Mono  --
   --------------------

   procedure Gdk_New_Mono (CC       :    out Gdk_Color_Context;
                           Visual   : in     Gdk.Visual.Gdk_Visual'Class;
                           Colormap : in     Gdk.Color.Gdk_Colormap'Class) is
      function Internal (Visual, Colormap : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_color_context_new_mono");
      Result : Gdk_Color_Context;
   begin
      Set_Object (Result, Internal (Get_Object (Visual),
                                    Get_Object (Colormap)));
      CC := Result;
   end Gdk_New_Mono;


   ------------------------------
   --  Get_Index_From_Palette  --
   ------------------------------

   procedure Get_Index_From_Palette (CC     : in     Gdk_Color_Context;
                                     Color  : in out Color_Description;
                                     Failed :    out Boolean;
                                     Index  :    out Glib.Gint) is
      function Internal (CC : in System.Address;
                         Red : in System.Address;
                         Green : in System.Address;
                         Blue : in System.Address;
                         Failed : in System.Address) return Glib.Guchar;
      pragma Import (C, Internal, "gdk_color_context_get_index_from_palette");
      Local_Failed : aliased Glib.Gint;

      Col : aliased Color_Description := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      Index := Glib.Gint (Internal (CC => Get_Object (CC),
                                    Red => Col.Red'Address,
                                    Green => Col.Green'Address,
                                    Blue => Col.Blue'Address,
                                    Failed => Local_Failed'Address));
      Color := Col;
      Failed := Glib.To_Boolean (Local_Failed);
   end Get_Index_From_Palette;


   -----------------
   --  Get_Pixel  --
   -----------------

   procedure Get_Pixel (CC     : in     Gdk_Color_Context;
                        Color  : in     Color_Description;
                        Failed :    out Boolean;
                        Pixel  :    out Glib.Gulong) is
      function Internal (CC : in System.Address;
                         Red, Green, Blue : in Glib.Gushort;
                         Failed : in System.Address)
                         return Glib.Gulong;
      pragma Import (C, Internal, "gdk_color_context_get_pixel");
      Result : Integer;
   begin
      Pixel := Internal (CC => Get_Object (CC),
                         Red => Color.Red,
                         Green => Color.Green,
                         Blue => Color.Blue,
                         Failed => Result'Address);
      Failed := Result /= 0;
   end Get_Pixel;


   ------------------------------
   --  Get_Pixel_From_Palette  --
   ------------------------------

   procedure Get_Pixel_From_Palette (CC     : in     Gdk_Color_Context;
                                     Color  : in out Color_Description;
                                     Failed :    out Boolean;
                                     Pixel  :    out Glib.Gulong) is
      function Internal (CC : in System.Address;
                         Red : in System.Address;
                         Green : in System.Address;
                         Blue : in System.Address;
                         Failed : in System.Address) return Glib.Gulong;
      pragma Import (C, Internal, "gdk_color_context_get_pixel_from_palette");
      Local_Failed : aliased Glib.Gint;
   begin
      Pixel := Internal (CC => Get_Object (CC),
                         Red => Color.Red'Address,
                         Green => Color.Green'Address,
                         Blue => Color.Blue'Address,
                         Failed => Local_Failed'Address);
      Failed := Glib.To_Boolean (Local_Failed);
   end Get_Pixel_From_Palette;


   ------------------
   --  Get_Pixels  --
   ------------------

   function Get_Pixels (CC     : in     Gdk_Color_Context;
                        Colors : in     Color_Description_Array)
                        return Glib.Gulong_Array is
      procedure Internal (CC         : in     System.Address;
                          Reds       : in     Glib.Gushort_Array;
                          Greens     : in     Glib.Gushort_Array;
                          Blues      : in     Glib.Gushort_Array;
                          Ncolors    : in     Glib.Gint;
                          Colors     : in out Glib.Gulong_Array;
                          Nallocated :    out Glib.Gint);
      pragma Import (C, Internal, "gdk_color_context_get_pixels");
      Reds   : Glib.Gushort_Array (1 .. Colors'Length);
      Greens : Glib.Gushort_Array (1 .. Colors'Length);
      Blues  : Glib.Gushort_Array (1 .. Colors'Length);
      Result : Glib.Gulong_Array (1 .. Colors'Length);
      --
      --  The max colors returned is the number of colors asked,
      --  which is the length of the "Colors" table.
      Nallocated : Glib.Gint;
   begin

      for Index in Colors'Range loop
         Reds (Index) := Colors (Index).Red;
         Greens (Index) := Colors (Index).Green;
         Blues (Index) := Colors (Index).Blue;
      end loop;

      Internal (CC => Get_Object (CC),
                Reds => Reds,
                Greens => Greens,
                Blues => Blues,
                Ncolors => Colors'Length,
                Colors => Result,
                Nallocated => Nallocated);

      return Result (1 .. Natural (Nallocated));

   end Get_Pixels;


   ------------------------------
   --  Get_Pixels_Incremental  --
   ------------------------------

   procedure Get_Pixels_Incremental
     (CC   : in     Gdk_Color_Context;
      Data : in out Get_Pixel_Incremental_Data) is
      procedure Internal (CC         : in     System.Address;
                          Reds       : in     Glib.Gushort_Array;
                          Greens     : in     Glib.Gushort_Array;
                          Blues      : in     Glib.Gushort_Array;
                          Ncolors    : in     Glib.Gint;
                          Used       : in     Glib.Gint_Array;
                          Colors     : in out Glib.Gulong_Array;
                          Nallocated :    out Glib.Gint);
      pragma Import (C, Internal, "gdk_color_context_get_pixels_incremental");
      Reds   : Glib.Gushort_Array (1 .. Data.Number_Of_Colors);
      Greens : Glib.Gushort_Array (1 .. Data.Number_Of_Colors);
      Blues  : Glib.Gushort_Array (1 .. Data.Number_Of_Colors);
      Used   : Glib.Gint_Array (1 .. Data.Number_Of_Colors);
      Nallocated : Glib.Gint;
   begin

      for Index in 1 .. Data.Number_Of_Colors loop
         Reds (Index) := Data.Colors (Index).Red;
         Greens (Index) := Data.Colors (Index).Green;
         Blues (Index) := Data.Colors (Index).Blue;
         Used (Index) := Glib.To_Gint (Data.Used (Index));
      end loop;

      Internal (CC => Get_Object (CC),
                Reds => Reds,
                Greens => Greens,
                Blues => Blues,
                Ncolors => Glib.Gint (Data.Number_Of_Colors),
                Used => Used,
                Colors => Data.Pixels,
                Nallocated => Nallocated);

      Data.Number_Of_Colors_Allocated := Natural (Nallocated);

   end Get_Pixels_Incremental;


   -------------------
   --  Init_Dither  --
   -------------------

   procedure Init_Dither (CC : in out Gdk_Color_Context) is
      procedure Internal (CC : in System.Address);
      pragma Import (C, Internal, "gdk_color_context_init_dither");
   begin
      Internal (Get_Object (CC));
   end Init_Dither;


   -------------------
   --  Query_Color  --
   -------------------

   procedure Query_Color (CC    : in     Gdk_Color_Context;
                          Color :    out Gdk.Color.Gdk_Color) is
      procedure Internal (CC    : in     System.Address;
                          Color :    out Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gdk_color_context_query_color");
   begin
      Internal (Get_Object (CC), Color);
   end Query_Color;


   --------------------
   --  Query_Colors  --
   --------------------

   procedure Query_Colors (CC     : in     Gdk_Color_Context;
                           Colors : in out Gdk.Color.Gdk_Color_Array) is
      procedure Internal (CC         : in     System.Address;
                          Colors     : in out Gdk.Color.Gdk_Color_Array;
                          Num_Colors : in     Glib.Gint);
      pragma Import (C, Internal, "gdk_color_context_query_colors");
   begin
      Internal (Get_Object (CC), Colors, Colors'Length);
   end Query_Colors;

end Gdk.Color_Context;
