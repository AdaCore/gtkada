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
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Window;
with Interfaces.C.Strings;

package Gdk.Pixmap is

   type Gdk_Pixmap is new Root_Type with private;

   procedure Gtk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint);
   --  mapping: Gtk_New gdk.h gdk_pixmap_new

   procedure Create_From_Data (Pixmap :    out Gdk_Pixmap;
                               Window : in     Gdk.Window.Gdk_Window'Class;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint;
                               Depth  : in     Gint;
                               Fg     : in     Color.Gdk_Color'Class;
                               Bg     : in     Color.Gdk_Color'Class);
   --  mapping: Create_From_Data gdk.h gdk_pixmap_create_from_data

   procedure Create_From_Xpm (Pixmap      : out Gdk_Pixmap;
                              Window      : in  Gdk.Window.Gdk_Window'Class;
                              Mask        : out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in  Gdk.Color.Gdk_Color'Class;
                              Filename    : in  String);
   --  mapping: Create_From_Xpm gdk.h gdk_pixmap_create_from_xpm

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : in  Gdk.Window.Gdk_Window'Class;
      Mask        : out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in  Gdk.Color.Gdk_Color'Class;
      Data        : in  Interfaces.C.Strings.chars_ptr_array);
   --  mapping: Create_From_Xpm_D gdk.h gdk_pixmap_create_from_xpm_d

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_colormap_create_from_xpm_d

private

   type Gdk_Pixmap is new Root_Type with null record;

   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_ref
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pixmap_unref

end Gdk.Pixmap;
