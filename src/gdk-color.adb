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

package body Gdk.Color is

   ---------
   -- "=" --
   ---------

   function "=" (Colora, Colorb : in Gdk_Color) return Boolean is
   begin
      return Colora.Red = Colorb.Red
        and then Colora.Blue = Colorb.Blue
        and then Colora.Green = Colorb.Green;
   end "=";

   -----------
   -- Alloc --
   -----------

   procedure Alloc (Colormap   : in out Gdk_Colormap;
                    Contiguous : in     Boolean;
                    Planes     : in     Gulong_Array;
                    Pixels     : in     Gulong_Array;
                    Succeeded  :    out Boolean) is
      function Internal (Colormap   : in System.Address;
                         Contiguous : in Gint;
                         Planes     : in Gulong_Array;
                         Nplanes    : in Gint;
                         Pixels     : in Gulong_Array;
                         Npixels    : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_colors_alloc");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         To_Gint (Contiguous),
                                         Planes, Planes'Length,
                                         Pixels, Pixels'Length));
   end Alloc;

   -----------
   -- Alloc --
   -----------

   procedure Alloc (Colormap  : in Gdk_Colormap;
                    Color     : in out Gdk_Color)
   is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_alloc");
   begin
      if not To_Boolean (Internal (Get_Object (Colormap), Color'Address)) then
         raise Wrong_Color;
      end if;
   end Alloc;

   -----------
   -- Black --
   -----------

   function Black (Colormap  : in Gdk_Colormap) return Gdk_Color is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_black");
      Color : Gdk_Color;
   begin
      if not To_Boolean (Internal (Get_Object (Colormap), Color'Address)) then
         raise Wrong_Color;
      end if;
      return Color;
   end Black;

   ----------
   -- Blue --
   ----------

   function Blue (Color : in Gdk_Color) return Gushort is
   begin
      return Color.Blue;
   end Blue;

   ------------
   -- Change --
   ------------

   procedure Change (Colormap : in Gdk_Colormap;
                     Ncolors  : in Gint) is
      procedure Internal (Colormap : in System.Address;
                          Ncolors : in Gint);
      pragma Import (C, Internal, "gdk_colormap_change");
   begin
      Internal (Get_Object (Colormap), Ncolors);
   end Change;

   ------------
   -- Change --
   ------------

   procedure Change (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color;
                    Succeeded :    out Boolean) is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_change");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         Color'Address));
   end Change;

   ----------
   -- Free --
   ----------

   procedure Free (Colormap : in out Gdk_Colormap;
                   Pixels   : in     Gulong_Array;
                   Planes   : in     Gulong) is
      procedure Internal (Colormap : in System.Address;
                          Pixels   : in Gulong_Array;
                          NPixels  : in Gint;
                          Planes   : in Gulong);
      pragma Import (C, Internal, "gdk_colors_free");
   begin
      Internal (Get_Object (Colormap), Pixels, Pixels'Length, Planes);
   end Free;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Colormap     :    out Gdk_Colormap;
                      Visual       : in     Gdk.Visual.Gdk_Visual;
                      Private_Cmap : in     Gint) is
      function Internal (Visual   : in System.Address;
                         Private_Cmap : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_colormap_new");
   begin
      Set_Object (Colormap, Internal (Get_Object (Visual), Private_Cmap));
   end Gdk_New;

   -----------
   -- Green --
   -----------

   function Green (Color : in Gdk_Color) return Gushort is
   begin
      return Color.Green;
   end Green;

   -----------
   -- Parse --
   -----------

   function Parse (Spec : in String) return Gdk_Color is
      function Internal (Spec  : in String;
                         Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_parse");
      Color : Gdk_Color;
   begin
      if not To_Boolean (Internal (Spec & ASCII.NUL, Color'Address)) then
         raise Wrong_Color;
      end if;
      return Color;
   end Parse;

   ---------
   -- Red --
   ---------

   function Red (Color : in Gdk_Color) return Gushort is
   begin
      return Color.Red;
   end Red;

   -------------
   -- Set_Rgb --
   -------------

   procedure Set_Rgb (Color   : out Gdk_Color;
                      R, G, B : in Gushort)
   is
   begin
      Color.Red := R;
      Color.Green := G;
      Color.Blue := B;
   end Set_Rgb;

   -----------
   -- Store --
   -----------

   procedure Store (Colormap : in out Gdk_Colormap;
                    Colors   : in     Gdk_Color_Array)
   is
      procedure Internal (Colormap : in System.Address;
                          Colors : in System.Address;
                          Ncolors : in Gint);
      pragma Import (C, Internal, "gdk_colors_store");
   begin
      Internal (Get_Object (Colormap), Colors'Address, Colors'Length);
   end Store;

   -----------
   -- White --
   -----------

   function White (Colormap  : in Gdk_Colormap) return Gdk_Color is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_white");
      Color     : Gdk_Color;
   begin
      if not To_Boolean (Internal (Get_Object (Colormap), Color'Address)) then
         raise Wrong_Color;
      end if;
      return Color;
   end White;

   function Ada_Gdk_Color_Size return Guint;
   pragma Import (C, Ada_Gdk_Color_Size);
begin
   pragma Assert (Gdk_Color'Size / 8 = Ada_Gdk_Color_Size);
   null;
end Gdk.Color;
