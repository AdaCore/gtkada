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

package Gdk.Rectangle is

   type Gdk_Rectangle is private;

   Full_Area : constant Gdk_Rectangle;
   --  The constant above can be used in Gtk.Widget.Draw when you want to
   --  redraw the whole widget

   procedure Set_X (Rectangle : in out Gdk_Rectangle;
                    X         : in     Gint16);

   procedure Set_Y (Rectangle : in out Gdk_Rectangle;
                    Y         : in     Gint16);

   procedure Set_Width (Rectangle : in out Gdk_Rectangle;
                        Width     : in     Guint16);

   procedure Set_Height (Rectangle : in out Gdk_Rectangle;
                         Height    : in     Guint16);

   procedure Set_Values (Rectangle : in out Gdk_Rectangle;
                         X         : in     Gint16;
                         Y         : in     Gint16;
                         Width     : in     Guint16;
                         Height    : in     Guint16);

   procedure Intersect (Src1      : in     Gdk_Rectangle;
                        Src2      : in     Gdk_Rectangle;
                        Dest      :    out Gdk_Rectangle;
                        Intersect :    out Boolean);

   procedure Union (Src1 : in     Gdk_Rectangle;
                    Src2 : in     Gdk_Rectangle;
                    Dest :    out Gdk_Rectangle);

   ----------------------
   --  Gets the fields --
   ----------------------

   function Get_X (Rect : Gdk_Rectangle) return Gint16;
   function Get_Y (Rect : Gdk_Rectangle) return Gint16;
   function Get_Width (Rect : Gdk_Rectangle) return Guint16;
   function Get_Height (Rect : Gdk_Rectangle) return Guint16;

private

   type Gdk_Rectangle is
      record
         X      : Gint16;
         Y      : Gint16;
         Width  : Guint16;
         Height : Guint16;
      end record;
   pragma Pack (Gdk_Rectangle);
   for Gdk_Rectangle'Size use 64;

   Full_Area : constant Gdk_Rectangle := (Gint16'First,
                                          Gint16'First,
                                          Guint16'Last,
                                          Guint16'Last);

   pragma Inline (Get_X);
   pragma Inline (Get_Y);
   pragma Inline (Get_Width);
   pragma Inline (Get_Height);

end Gdk.Rectangle;
