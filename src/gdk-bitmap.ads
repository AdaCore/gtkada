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

with Glib; use Glib;

with Gdk.Window;

package Gdk.Bitmap is

   type Gdk_Bitmap is new Root_Type with private;

   procedure Create_From_Data (Bitmap :    out Gdk_Bitmap;
                               Window : in     Gdk.Window.Gdk_Window;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint);
   --  mapping: Create_From_Data gdk.h gdk_bitmap_create_from_data


private

   type Gdk_Bitmap is new Root_Type with null record;

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_bitmap_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_bitmap_unref

end Gdk.Bitmap;
