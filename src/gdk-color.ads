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

with Glib; use Glib;

with Gdk.Visual;

package Gdk.Color is

   type Gdk_Color is private;
   type Gdk_Color_Array is array (Natural range <>) of Gdk_Color;

   Null_Color : constant Gdk_Color;

   type Gdk_Colormap is new Root_Type with private;

   Null_Colormap : constant Gdk_Colormap;

   Wrong_Color : exception;


   procedure Gdk_New (Colormap     :    out Gdk_Colormap;
                      Visual       : in     Gdk.Visual.Gdk_Visual;
                      Private_Cmap : in     Gint);

   procedure Ref (Colormap : in out Gdk_Colormap);

   procedure Unref (Colormap : in out Gdk_Colormap);

   procedure Get_System (Colormap : out Gdk_Colormap);

   function Get_System_Size return Gint;

   procedure Change (Colormap : in Gdk_Colormap;
                     Ncolors  : in Gint);

   procedure Alloc_Colors (Colormap   : in out Gdk_Colormap;
                           Colors     : in     Gdk_Color_Array;
                           Writeable  : in     Boolean;
                           Best_Match : in     Boolean;
                           Success    :    out Boolean_Array;
                           Result     :    out Gint);
   --
   --  The size of the Boolean_Array is equal to the length of the
   --  Colors_Array. USAGE OF AN ARRAY OF A DIFFERENT SIZE WILL
   --  PROBABLY LEAD TO A CONSTRAINT_ERROR.

   procedure Alloc_Color (Colormap   : in out Gdk_Colormap;
                          Color      : in     Gdk_Color;
                          Writeable  : in     Boolean;
                          Best_Match : in     Boolean;
                          Success    :    out Boolean);

   procedure Free_Colors (Colormap : in Gdk_Colormap;
                          Colors   : in Gdk_Color_Array);

   procedure Store (Colormap : in out Gdk_Colormap;
                    Colors   : in     Gdk_Color_Array);
   --  Stores the colors in the colormap

   procedure Alloc (Colormap   : in out Gdk_Colormap;
                    Contiguous : in     Boolean;
                    Planes     : in     Gulong_Array;
                    Pixels     : in     Gulong_Array;
                    Succeeded  :    out Boolean);

   procedure Free (Colormap : in out Gdk_Colormap;
                   Pixels   : in     Gulong_Array;
                   Planes   : in     Gulong);

   function White (Colormap  : in Gdk_Colormap) return Gdk_Color;
   function Black (Colormap  : in Gdk_Colormap) return Gdk_Color;
   function Parse (Spec      : in String) return Gdk_Color;
   procedure Alloc (Colormap  : in Gdk_Colormap;
                    Color     : in out Gdk_Color);
   --
   --  The four previous functions raise Wrong_Color if the color could not
   --  be created
   --
   --  The usual way to allocate a new color is :
   --  Alloc (Get_Default_Colormap (Widget), Parse ("colorname"));


   function Hash (Color_A : in Gdk_Color;
                  Color_B : in Gdk_Color) return Guint;

   procedure Change (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color;
                    Succeeded :    out Boolean);

   procedure Get_Visual (Colormap : in     Gdk_Colormap;
                         Visual   :    out Gdk.Visual.Gdk_Visual);

   procedure Copy (Source      : in     Gdk_Color;
                   Destination :    out Gdk_Color);

   function "=" (Colora, Colorb : in Gdk_Color) return Boolean;

   --------------------------------------
   --  Getting the fields of Gdk_Color --
   --------------------------------------

   procedure Set_Rgb (Color   : out Gdk_Color;
                      R, G, B : in Gushort);
   procedure Set_Pixel (Color : in out Gdk_Color; Pixel : Gulong);
   function Red (Color : in Gdk_Color) return Gushort;
   function Green (Color : in Gdk_Color) return Gushort;
   function Blue (Color : in Gdk_Color) return Gushort;
   function Pixel (Color : in Gdk_Color) return Gulong;

private

   type Gdk_Colormap is new Root_Type with null record;

   Null_Colormap : constant Gdk_Colormap := (Ptr => System.Null_Address);

   type Gdk_Color is
      record
         Pixel : Gulong;
         Red   : Gushort;
         Green : Gushort;
         Blue  : Gushort;
      end record;
   --  The fields are to be chosen between 0 and 65535, not 0 and 255!!!

   Null_Color : constant Gdk_Color := (Gulong'Last, 0, 0, 0);

   pragma Inline (Set_Rgb);
   pragma Inline (Set_Pixel);
   pragma Inline (Red);
   pragma Inline (Green);
   pragma Inline (Blue);
   pragma Inline (Pixel);

   pragma Import (C, Get_System_Size, "gdk_colormap_get_system_size");

end Gdk.Color;
