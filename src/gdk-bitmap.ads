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

with Gdk.Drawable;
with Gdk.GC;
with Gdk.Window;

package Gdk.Bitmap is

   type Gdk_Bitmap is new Gdk.Drawable.Gdk_Drawable with private;

   function Null_Bitmap return Gdk_Bitmap;

   procedure Gdk_New (Bitmap : out Gdk_Bitmap;
                      Window : in  Gdk.Window.Gdk_Window'Class;
                      Width  : in  Gint;
                      Height : in  Gint);

   procedure Unref_Bitmap (Bitmap : in out Gdk_Bitmap);
   --  This is the usual way to destroy a bitmap. The memory is freed when
   --  there is no more reference

   procedure Ref (Bitmap : in out Gdk_Bitmap);

   procedure Unref (Bitmap : in out Gdk_Bitmap);

   procedure Create_From_Data (Bitmap :    out Gdk_Bitmap;
                               Window : in     Gdk.Window.Gdk_Window'Class;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint);

   procedure Set_Clip_Mask (GC    : in out Gdk.GC.Gdk_GC'Class;
                            Mask  : in     Gdk_Bitmap);
   --  If MASK is set to Null_Bitmap, then no clip_mask is used for drawing

private

   type Gdk_Bitmap is new Gdk.Drawable.Gdk_Drawable with null record;

end Gdk.Bitmap;
