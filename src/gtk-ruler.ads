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
--         General Public License for more details.                  --
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


with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Ruler is

   type Gtk_Ruler is new Gtk.Widget.Gtk_Widget with private;

   procedure Draw_Pos (Ruler : in Gtk_Ruler'Class);
   procedure Draw_Ticks (Ruler : in Gtk_Ruler'Class);
   function Get_Lower (Widget : in Gtk_Ruler'Class)
                       return      Gfloat;
   function Get_Max_Size (Widget : in Gtk_Ruler'Class)
                          return      Gfloat;
   function Get_Position (Widget : in Gtk_Ruler'Class)
                          return      Gfloat;
   function Get_Upper (Widget : in Gtk_Ruler'Class)
                       return      Gfloat;
   procedure Gtk_New_Hruler (Widget : out Gtk_Ruler);
   procedure Gtk_New_Vruler (Widget : out Gtk_Ruler);
   procedure Set_Metric
      (Ruler  : in Gtk_Ruler'Class;
       Metric : in Gtk_Metric_Type);
   procedure Set_Range
      (Ruler    : in Gtk_Ruler'Class;
       Lower    : in Gfloat;
       Upper    : in Gfloat;
       Position : in Gfloat;
       Max_Size : in Gfloat);

private
   type Gtk_Ruler is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: Draw_Pos gtkruler.h gtk_ruler_draw_pos
   --  mapping: Draw_Ticks gtkruler.h gtk_ruler_draw_ticks
   --  mapping: Get_Lower gtkruler.h GtkRuler->lower
   --  mapping: Get_Max_Size gtkruler.h GtkRuler->max_size
   --  mapping: Get_Position gtkruler.h GtkRuler->position
   --  mapping: NOT_IMPLEMENTED gtkruler.h gtk_ruler_get_type
   --  mapping: Get_Upper gtkruler.h GtkRuler->upper
   --  mapping: Set_Metric gtkruler.h gtk_ruler_set_metric
   --  mapping: Set_Range gtkruler.h gtk_ruler_set_range
   --  mapping: Gtk_New_Hruler gtkhruler.h gtk_hruler_new
   --  mapping: Gtk_New_Vruler gtkvruler.h gtk_vruler_new
end Gtk.Ruler;
