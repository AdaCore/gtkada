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

--  <description>
--
--  This package provides an interface to the color handling facilities in
--  gtk+. It is able to handle any kind of visual (monochrome, greyscale,
--  color with different depths, ...), but provides a common and easy
--  interface for all of them.
--  Some of these functions expect a Colormap. There are two ways you can
--  get such a colormap, either a system default colormap or a per-widget
--  colormap. It is recommended, unless you are writing your own new widget,
--  to always use the system default Colormap. All the functions to get
--  these colormap are found in Gtk.Widget.
--
--  Here is an example how you can allocate a new color, if you already know
--  its red/green/blue components:
--     Color   : Gdk_Color;
--     Success : Boolean;
--     Set_Rbg (Color, 255, 255, 255);
--     Alloc_Color (Colormap   => Gtk.Widget.Get_Default_Colormap,
--                  Color      => Color,
--                  Writeable  => False,
--                  Best_Match => True,
--                  Success    => Success);
--     if not Success then
--         ...;  --  allocation failed
--     end if;
--
--  Getting the Red/Green/Blue components can be done through Parse, and is
--  actually recommended, since the exact color generally depends on the
--  visual your application is running on.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Glib; use Glib;
with Gdk.Visual;

package Gdk.Color is

   type Gdk_Color is private;
   type Gdk_Color_Array is array (Natural range <>) of Gdk_Color;
   Null_Color : constant Gdk_Color;
   --  No color. For most functions, this will select the default color in the
   --  context, although this exact specification depends on the function you
   --  want to use.

   type Gdk_Colormap is new Root_Type with private;
   Null_Colormap : constant Gdk_Colormap;

   Wrong_Color : exception;
   --  Exception raised when some functions below could not find or allocate
   --  a color on the user's system.

   ------------------------------------
   -- Creating and Destroying colors --
   ------------------------------------

   function Parse (Spec : in String) return Gdk_Color;
   --  Parses the string Spec, and get its Red/Green/Blue components. The color
   --  is not allocated, and you need to call Alloc_Color.
   --  It the string could not be parsed to an existing color, Wrong_Color is
   --  raised.
   --  The string can be one of :
   --  * "RBG: FF/FF/FF" where the "FF" substrings are respectively the value
   --    of the red, green and blue components. Some other prefixes than RBG
   --    are defined in the X11 definition, please see some X11 documentation
   --    (or the man page XParseColor on unix systems).
   --  * "color_name" which can be any color name defined in the file rgb.txt
   --    the user system. You should always check that Wrong_Color was not
   --    raised, in case the color was not known on the user's system.

   procedure Alloc_Color (Colormap   : in     Gdk_Colormap;
                          Color      : in out Gdk_Color;
                          Writeable  : in     Boolean;
                          Best_Match : in     Boolean;
                          Success    :    out Boolean);
   --  Allocate a new color. The fields RGB should have been set before calling
   --  this function.
   --  If Writeable is True, the color will be allocated read/write, that can
   --  be changed at any time. Not all visulals support this. On modern systems
   --  this usage has become less useful than before, since redrawing the
   --  screen with a new color is about as fast.
   --  If Best_Match is True, and the exact color can not be allocated, GtkAda
   --  will find the closest possible match, and modify the fields Red, Green
   --  and Blue of Color.
   --  Note that the allocation has more chances to succeed if Writeable is
   --  False and Best_Match is True.
   --  When you no longer use a color, you should call Free.

   procedure Alloc_Colors (Colormap   : in     Gdk_Colormap;
                           Colors     : in out Gdk_Color_Array;
                           Writeable  : in     Boolean;
                           Best_Match : in     Boolean;
                           Success    :    out Boolean_Array;
                           Result     :    out Gint);
   --  Allocate a set of colors. The parameters are the same as for Alloc_Color
   --  Result is the number of colors not successfully allocated.
   --
   --  The size of the Boolean_Array is equal to the length of the
   --  Colors_Array. USAGE OF AN ARRAY OF A DIFFERENT SIZE WILL
   --  PROBABLY LEAD TO A CONSTRAINT_ERROR.

   procedure Alloc (Colormap  : in Gdk_Colormap;
                    Color     : in out Gdk_Color);
   --  Same function as Alloc_Colors above, but for a single color.
   --  The color is allocated non-writeable, and the best-match is taken.
   --  Raises Wrong_Color if the color could not be allocated

   function White (Colormap  : in Gdk_Colormap) return Gdk_Color;
   --  Returns the default white color for the colormap. If this color was not
   --  found or could not be allocated, Wrong_Color is raised.

   function Black (Colormap  : in Gdk_Colormap) return Gdk_Color;
   --  Returns the default black colors for the colormap. If these colors were
   --  not found or could not be allocated, Wrong_Color is raised.

   function Get_System return Gdk_Colormap;
   --  Get the default colormap for the system. This is the same function as
   --  Gtk.Widget.Get_Default_Colormap.

   procedure Get_Visual (Colormap : in     Gdk_Colormap;
                         Visual   :    out Gdk.Visual.Gdk_Visual);
   --  Get the visual associated with a colormap. The main information you can
   --  get from there is the depth of the display.

   procedure Gdk_New (Colormap     :    out Gdk_Colormap;
                      Visual       : in     Gdk.Visual.Gdk_Visual;
                      Private_Cmap : in     Boolean);
   --  Creates a new colormap for the visual. If Private_Cmap is true, then the
   --  colormap won't be modifiable outside this scope. This might result in
   --  some strange colors on the display...

   procedure Unref (Colormap : in out Gdk_Colormap);
   --  Unref is the only way to destroy a colormap once you no longer need it.
   --  Note that because gtk+ use reference counts, the colormap will not
   --  be actually destroyed while at least one object is using it.

   procedure Ref (Colormap : in Gdk_Colormap);
   --  Increments the ref-count for the color.
   --  You should not have to use this function.

   function Get_System_Size return Gint;
   --  Returns the number of entries in the default colormap on the system.

   procedure Free_Colors (Colormap : in Gdk_Colormap;
                          Colors   : in Gdk_Color_Array);
   --  Free COLORS, assuming they are allocated in COLORMAP.

   procedure Store (Colormap : in Gdk_Colormap;
                    Colors   : in Gdk_Color_Array);
   --  Stores the colors in the colormap

   procedure Alloc (Colormap   : in Gdk_Colormap;
                    Contiguous : in Boolean;
                    Planes     : in     Gulong_Array;
                    Pixels     : in     Gulong_Array;
                    Succeeded  :    out Boolean);
   --  Allocate some Read/Write color cells, whoes value can be changed
   --  dynamically. The pixels allocated are returned in PIXELS.
   --  See XAllocColorCells(3) on Unix systems.
   --  The PLANES parameter can be used to nondestructively overlay one
   --  set of graphics over another. See the X11 manual for more info.
   --  Note that this is a low-level function which you should rarely
   --  have to use.

   procedure Free (Colormap : in Gdk_Colormap;
                   Pixels   : in Gulong_Array;
                   Planes   : in Gulong);
   --  Free some colors in the colormap.
   --  See XFreeColors(3) on Unix systems.

   --  <doc_ignore>
   function Hash (Color_A : in Gdk_Color;
                  Color_B : in Gdk_Color) return Guint;
   --  Returns a hash code for COLOR_A.
   --  This seems to be a internal function for gtk+ only.
   --  </doc_ignore>

   procedure Change (Colormap  : in Gdk_Colormap;
                     Color     : in out Gdk_Color;
                     Succeeded :    out Boolean);
   --  Changes the Read/Write colormap cell corresponding to COLOR.
   --  The new value is the one contained in the Red, Green and Blue
   --  fields of COLOR.

   procedure Change (Colormap : in Gdk_Colormap;
                     Ncolors  : in Gint);
   --  Changes the first NCOLORS defined in COLORMAP.

   procedure Copy (Source      : in     Gdk_Color;
                   Destination :    out Gdk_Color);
   --  Copy the Source color to Destination.

   function Equal (Colora, Colorb : in Gdk_Color) return Boolean;
   --  This function returns True if the Red, Green and Blue components
   --  of both colors are equal.

   ----------------------------------------------
   --  Setting/Getting the fields of Gdk_Color --
   ----------------------------------------------

   procedure Set_Rgb (Color   : out Gdk_Color;
                      Red, Green, Blue : in Gushort);
   --  Modifies the fields of the color.
   --  You then have to allocate the color with one of the Alloc* functions
   --  above.

   procedure Set_Pixel (Color : in out Gdk_Color; Pixel : Gulong);
   --  This function should almost never be used. Instead, use Alloc_Color

   function Red (Color : in Gdk_Color) return Gushort;
   --  Returns the RED field of COLOR.

   function Green (Color : in Gdk_Color) return Gushort;
   --  Returns the GREEN field of COLOR.

   function Blue (Color : in Gdk_Color) return Gushort;
   --  Returns the BLUE field of COLOR.

   function Pixel (Color : in Gdk_Color) return Gulong;
   --  RETURN the PIXEL field of COLOR.

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

   Null_Color : constant Gdk_Color := (Gulong'Last, 1, 0, 0);
   --  Note: in the implementation of GtkAda, everytime a color is used, it
   --  is important to test whether this is Null_Color or not. If it is, then
   --  System.Null_Address should be passed to C instead of Null_Color'Address
   --  so that gtk+ can provide a default value for colors.

   pragma Inline (Set_Rgb);
   pragma Inline (Set_Pixel);
   pragma Inline (Red);
   pragma Inline (Green);
   pragma Inline (Blue);
   pragma Inline (Pixel);
   pragma Import (C, Get_System_Size, "gdk_colormap_get_system_size");
end Gdk.Color;
