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

package Gdk.Rectangle is

   type Gdk_Rectangle is new Root_Type with private;

   Full_Area : constant Gdk_Rectangle;
   --  The constant above can be used in Gtk.Widget.Draw when you want to
   --  redraw the whole widget

   procedure Gdk_New (Rectangle : out Gdk_Rectangle);

   procedure Gdk_New (Rectangle :    out Gdk_Rectangle;
                      X         : in     Gint16;
                      Y         : in     Gint16;
                      Width     : in     Guint16;
                      Height    : in     Guint16);

   procedure Destroy (Rectangle : in out Gdk_Rectangle'Class);


   procedure Get_Values (Rectangle : in     Gdk_Rectangle;
                         X         :    out Gint16;
                         Y         :    out Gint16;
                         Width     :    out Guint16;
                         Height    :    out Guint16);

   function Get_X (Rectangle : in Gdk_Rectangle) return Gint16;

   function Get_Y (Rectangle : in Gdk_Rectangle) return Gint16;

   function Get_Width (Rectangle : in Gdk_Rectangle) return Guint16;

   function Get_Height (Rectangle : in Gdk_Rectangle) return Guint16;


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
                        Dest      : in out Gdk_Rectangle;
                        Intersect :    out Boolean);
   --  mapping: Intersect gdk.h gdk_rectangle_intersect
   --
   --  NOTE : Dest needs to be allocated first.
private

   type Gdk_Rectangle is new Root_Type with null record;
   Full_Area : constant Gdk_Rectangle := (Ptr => System.Null_Address);

end Gdk.Rectangle;
