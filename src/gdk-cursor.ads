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
--  This package provides the capability to create predefined mouse cursors
--  as well as user defined ones.
--  </description>
--  <c_version>1.2.8</c_version>

with Glib;
with Gdk.Types;
with Gdk.Color;

package Gdk.Cursor is

   type Gdk_Cursor is new Gdk.C_Proxy;
   Null_Cursor : constant Gdk_Cursor;

   procedure Gdk_New
     (Widget      : out Gdk_Cursor;
      Cursor_Type : in  Gdk.Types.Gdk_Cursor_Type);
   --  Create a new standard cursor.

   procedure Gdk_New
     (Widget : out Gdk_Cursor;
      Source : in Gdk.Gdk_Pixmap;
      Mask   : in Gdk.Gdk_Pixmap;
      Fg     : in Gdk.Color.Gdk_Color;
      Bg     : in Gdk.Color.Gdk_Color;
      X      : in Glib.Gint;
      Y      : in Glib.Gint);
   --  Create a new cursor from a given pixmap and mask.
   --  Both the pixmap and mask must have a depth of 1 (i.e. each pixel has
   --  only 2 values - on or off). The standard cursor size is 16 by 16 pixels.
   --   - Source is the pixmap specifying the cursor.
   --   - Mask is the pixmap specifying the mask, which must be the same size
   --   as source.
   --   - Fg is the foreground color, used for the bits in the source which are
   --   enabled. The color does not have to be allocated first.
   --   - Bg is the background color, used for the bits in the source which are
   --   disabled. The color does not have to be allocated first.
   --   - X is the horizontal offset of the 'hotspot' of the cursor.
   --   - Y is the vertical offset of the 'hotspot' of the cursor.

   procedure Destroy (Cursor : in Gdk_Cursor);
   --  Destroy a cursor, freeing any resources allocated for it.

private
   Null_Cursor : constant Gdk_Cursor := null;
   pragma Import (C, Destroy, "gdk_cursor_destroy");
end Gdk.Cursor;
