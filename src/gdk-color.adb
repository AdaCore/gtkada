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


   --------------------
   --  Alloc_Colors  --
   --------------------

   procedure Alloc_Color (Colormap   : in out Gdk_Colormap;
                          Color      : in     Gdk_Color;
                          Writeable  : in     Boolean;
                          Best_Match : in     Boolean;
                          Success    :    out Boolean) is
      function Internal (Colormap   : in System.Address;
                         Color      : in System.Address;
                         Writeable  : in Gboolean;
                         Best_Match : in Gboolean) return Gboolean;
      pragma Import (C, Internal, "gdk_colormap_alloc_color");
   begin
      Success := To_Boolean (Internal (Get_Object (Colormap),
                                       Color'Address,
                                       To_Gboolean (Writeable),
                                       To_Gboolean (Best_Match)));
   end Alloc_Color;


   --------------------
   --  Alloc_Colors  --
   --------------------

   procedure Alloc_Colors (Colormap   : in out Gdk_Colormap;
                           Colors     : in     Gdk_Color_Array;
                           Writeable  : in     Boolean;
                           Best_Match : in     Boolean;
                           Success    :    out Boolean_Array;
                           Result     :    out Gint) is
      function Internal (Colormap   : in System.Address;
                         Colors     : in Gdk_Color_Array;
                         N_Colors   : in Gint;
                         Writeable  : in Gboolean;
                         Best_Match : in Gboolean;
                         Success    : in System.Address)
                         return Gint;
      pragma Import (C, Internal, "gdk_colormap_alloc_colors");
      Tmp : Gboolean_Array (1 .. Colors'Length);
   begin
      Result := Internal (Get_Object (Colormap),
                          Colors,
                          Colors'Length,
                          To_Gboolean (Writeable),
                          To_Gboolean (Best_Match),
                          Tmp (Tmp'First)'Address);
      Success := To_Boolean_Array (Tmp);
   end Alloc_Colors;


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
   -- Copy --
   ----------

   procedure Copy (Source      : in     Gdk_Color;
                   Destination :    out Gdk_Color) is
      function Internal (Source : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_color_copy");
      Result : Gdk_Color;
      for Result'Address use Internal (Source'Address);
   begin
      Destination := Result;
   end Copy;

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


   -----------------
   -- Free_Colors --
   -----------------

   procedure Free_Colors (Colormap : in Gdk_Colormap;
                          Colors   : in Gdk_Color_Array) is
      procedure Internal (Colormap : in System.Address;
                          Colors   : in Gdk_Color_Array;
                          Ncolors  : in Gint);
      pragma Import (C, Internal, "gdk_colormap_free_colors");
   begin
      Internal (Get_Object (Colormap),
                Colors,
                Colors'Length);
   end Free_Colors;


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

   ------------------
   --  Get_System  --
   ------------------

   procedure Get_System (Colormap : out Gdk_Colormap) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_colormap_get_system");
   begin
      Set_Object (Colormap, Internal);
   end Get_System;

   ------------------
   --  Get_Visual  --
   ------------------

   procedure Get_Visual (Colormap : in     Gdk_Colormap;
                         Visual   :    out Gdk.Visual.Gdk_Visual) is
      function Internal (Colormap : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_colormap_get_visual");
   begin
      Set_Object (Visual, Internal (Get_Object (Colormap)));
   end Get_Visual;

   -----------
   -- Green --
   -----------

   function Green (Color : in Gdk_Color) return Gushort is
   begin
      return Color.Green;
   end Green;


   ------------
   --  Hash  --
   ------------

   function Hash (Color_A : in Gdk_Color;
                  Color_B : in Gdk_Color) return Guint is
      function Internal (Color_A, Color_B : in System.Address) return Guint;
      pragma Import (C, Internal, "gdk_color_hash");
   begin
      return Internal (Color_A'Address, Color_B'Address);
   end Hash;


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

   -----------
   -- Pixel --
   -----------

   function Pixel (Color : in Gdk_Color) return Gulong is
   begin
      return Color.Pixel;
   end Pixel;

   ---------
   -- Red --
   ---------

   function Red (Color : in Gdk_Color) return Gushort is
   begin
      return Color.Red;
   end Red;


   -----------
   --  Ref  --
   -----------

   procedure Ref (Colormap : in out Gdk_Colormap) is
      function Internal (Colormap : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_colormap_ref");
      S : System.Address;
   begin
      S := Internal (Get_Object (Colormap));
   end Ref;


   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (Color : in out Gdk_Color; Pixel : Gulong) is
   begin
      Color.Pixel := Pixel;
   end Set_Pixel;

   -------------
   -- Set_Rgb --
   -------------

   procedure Set_Rgb (Color   : out Gdk_Color;
                      Red, Green, Blue : in Gushort)
   is
   begin
      Color.Red := Red;
      Color.Green := Green;
      Color.Blue := Blue;
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

   -------------
   --  Unref  --
   -------------

   procedure Unref (Colormap : in out Gdk_Colormap) is
      procedure Internal (Colormap : in System.Address);
      pragma Import (C, Internal, "gdk_colormap_unref");
   begin
      Internal (Get_Object (Colormap));
      Set_Object (Colormap, System.Null_Address);
   end Unref;


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
   --   pragma Assert (Gdk_Color'Size / 8 = Ada_Gdk_Color_Size);
   null;
end Gdk.Color;
