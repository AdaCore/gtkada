-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Gdk.Color is

   -----------
   -- Equal --
   -----------

   function Equal (Colora, Colorb : in Gdk_Color) return Boolean is
   begin
      return Colora.Red = Colorb.Red
        and then Colora.Blue = Colorb.Blue
        and then Colora.Green = Colorb.Green;
   end Equal;

   -----------
   -- Alloc --
   -----------

   procedure Alloc
     (Colormap   : in Gdk_Colormap;
      Contiguous : in Boolean;
      Planes     : in Gulong_Array;
      Pixels     : in Gulong_Array;
      Succeeded  :    out Boolean)
   is
      function Internal
        (Colormap   : in Gdk_Colormap;
         Contiguous : in Gint;
         Planes     : in Gulong_Array;
         Nplanes    : in Gint;
         Pixels     : in Gulong_Array;
         Npixels    : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_colors_alloc");

   begin
      Succeeded :=
        Boolean'Val
          (Internal
            (Colormap, To_Gint (Contiguous), Planes, Planes'Length,
             Pixels, Pixels'Length));
   end Alloc;

   -----------
   -- Alloc --
   -----------

   procedure Alloc
     (Colormap  : in Gdk_Colormap;
      Color     : in out Gdk_Color)
   is
      function Internal
        (Colormap : in Gdk_Colormap;
         Color    : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_alloc");

      Col : aliased Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      if not Boolean'Val (Internal (Colormap, Col'Address)) then
         raise Wrong_Color;
      end if;

      Color := Col;
   end Alloc;

   -----------------
   -- Alloc_Color --
   -----------------

   procedure Alloc_Color
     (Colormap   : in Gdk_Colormap;
      Color      : in out Gdk_Color;
      Writeable  : in Boolean;
      Best_Match : in Boolean;
      Success    :    out Boolean)
   is
      function Internal
        (Colormap   : in Gdk_Colormap;
         Color      : in System.Address;
         Writeable  : in Gboolean;
         Best_Match : in Gboolean) return Gboolean;
      pragma Import (C, Internal, "gdk_colormap_alloc_color");

      Col : aliased Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Success :=
        Boolean'Val
          (Internal
            (Colormap, Col'Address,
             To_Gboolean (Writeable), To_Gboolean (Best_Match)));
      Color := Col;
   end Alloc_Color;

   ------------------
   -- Alloc_Colors --
   ------------------

   procedure Alloc_Colors
     (Colormap   : in     Gdk_Colormap;
      Colors     : in out Gdk_Color_Array;
      Writeable  : in     Boolean;
      Best_Match : in     Boolean;
      Success    :    out Boolean_Array;
      Result     :    out Gint)
   is
      function Internal
        (Colormap   : in Gdk_Colormap;
         Colors     : in System.Address;
         N_Colors   : in Gint;
         Writeable  : in Gboolean;
         Best_Match : in Gboolean;
         Success    : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_colormap_alloc_colors");

      Tmp : Gboolean_Array (Colors'Range);

   begin
      Result :=
        Internal
          (Colormap,
           Colors'Address,
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
      function Internal
        (Colormap : in Gdk_Colormap;
         Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_black");

      Color : aliased Gdk_Color;

   begin
      if not To_Boolean (Internal (Colormap, Color'Address)) then
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

   procedure Change
     (Colormap : in Gdk_Colormap;
      Ncolors  : in Gint)
   is
      procedure Internal
        (Colormap : in Gdk_Colormap;
         Ncolors  : in Gint);
      pragma Import (C, Internal, "gdk_colormap_change");

   begin
      Internal (Colormap, Ncolors);
   end Change;

   ------------
   -- Change --
   ------------

   procedure Change
     (Colormap  : in Gdk_Colormap;
      Color     : in out Gdk_Color;
      Succeeded :    out Boolean)
   is
      function Internal
        (Colormap : in Gdk_Colormap;
         Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_change");

      Col : aliased Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Succeeded := To_Boolean (Internal (Colormap, Col'Address));
      Color := Col;
   end Change;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source      : in     Gdk_Color;
      Destination :    out Gdk_Color)
   is
      function Internal (Source : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_color_copy");

      Result : aliased Gdk_Color;
      for Result'Address use Internal (Source'Address);

   begin
      Destination := Result;
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free
     (Colormap : in Gdk_Colormap;
      Pixels   : in Gulong_Array;
      Planes   : in Gulong)
   is
      procedure Internal
        (Colormap : in Gdk_Colormap;
         Pixels   : in Gulong_Array;
         NPixels  : in Gint;
         Planes   : in Gulong);
      pragma Import (C, Internal, "gdk_colors_free");

   begin
      Internal (Colormap, Pixels, Pixels'Length, Planes);
   end Free;

   -----------------
   -- Free_Colors --
   -----------------

   procedure Free_Colors
     (Colormap : in Gdk_Colormap;
      Colors   : in Gdk_Color_Array)
   is
      procedure Internal
        (Colormap : in Gdk_Colormap;
         Colors   : in Gdk_Color_Array;
         Ncolors  : in Gint);
      pragma Import (C, Internal, "gdk_colormap_free_colors");

   begin
      Internal (Colormap, Colors, Colors'Length);
   end Free_Colors;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Colormap     :    out Gdk_Colormap;
      Visual       : in     Gdk.Visual.Gdk_Visual;
      Private_Cmap : in     Boolean)
   is
      function Internal
        (Visual   : in Gdk.Visual.Gdk_Visual;
         Private_Cmap : in Gint) return Gdk_Colormap;
      pragma Import (C, Internal, "gdk_colormap_new");

   begin
      Colormap := Internal (Visual, Boolean'Pos (Private_Cmap));
   end Gdk_New;

   ----------------
   -- Get_Visual --
   ----------------

   procedure Get_Visual
     (Colormap : in     Gdk_Colormap;
      Visual   :    out Gdk.Visual.Gdk_Visual)
   is
      function Internal
        (Colormap : in Gdk_Colormap) return Gdk.Visual.Gdk_Visual;
      pragma Import (C, Internal, "gdk_colormap_get_visual");

   begin
      Visual := Internal (Colormap);
   end Get_Visual;

   -----------
   -- Green --
   -----------

   function Green (Color : in Gdk_Color) return Gushort is
   begin
      return Color.Green;
   end Green;

   ----------
   -- Hash --
   ----------

   function Hash
     (Color_A : in Gdk_Color;
      Color_B : in Gdk_Color) return Guint
   is
      function Internal (Color_A, Color_B : in System.Address) return Guint;
      pragma Import (C, Internal, "gdk_color_hash");

      ColA : aliased Gdk_Color := Color_A;
      ColB : aliased Gdk_Color := Color_B;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      return Internal (ColA'Address, ColB'Address);
   end Hash;

   -----------
   -- Parse --
   -----------

   function Parse (Spec : in String) return Gdk_Color is
      function Internal
        (Spec  : in String;
         Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_parse");

      Color : aliased Gdk_Color;

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

   procedure Set_Rgb
     (Color : out Gdk_Color;
      Red,
      Green,
      Blue  : in Gushort) is
   begin
      Color.Red := Red;
      Color.Green := Green;
      Color.Blue := Blue;
   end Set_Rgb;

   -----------
   -- Store --
   -----------

   procedure Store
     (Colormap : in Gdk_Colormap;
      Colors   : in Gdk_Color_Array)
   is
      procedure Internal
        (Colormap : in Gdk_Colormap;
         Colors : in System.Address;
         Ncolors : in Gint);
      pragma Import (C, Internal, "gdk_colors_store");

   begin
      Internal (Colormap, Colors'Address, Colors'Length);
   end Store;

   -----------
   -- White --
   -----------

   function White (Colormap  : in Gdk_Colormap) return Gdk_Color is
      function Internal
        (Colormap : in Gdk_Colormap;
         Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_white");

      Color : aliased Gdk_Color;

   begin
      if not To_Boolean (Internal (Colormap, Color'Address)) then
         raise Wrong_Color;
      end if;

      return Color;
   end White;

end Gdk.Color;
