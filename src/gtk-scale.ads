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


with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GRange;

package Gtk.Scale is

   type Gtk_Scale is new Gtk.GRange.Gtk_Range with private;

   procedure Draw_Value (Scale : in Gtk_Scale'Class);
   procedure Gtk_New_Hscale
     (Widget     : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Gtk_New_Vscale
     (Widget     : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Digits
      (Scale      : in Gtk_Scale'Class;
       The_Digits : in Gint);
   procedure Set_Draw_Value
      (Scale      : in Gtk_Scale'Class;
       Draw_Value : in Boolean);
   procedure Set_Value_Pos
      (Scale : in Gtk_Scale'Class;
       Pos   : in Gtk_Position_Type);
   function Value_Width (Scale  : in Gtk_Scale'Class)
                         return      Gint;

private
   type Gtk_Scale is new Gtk.GRange.Gtk_Range with null record;

   --  mapping: Draw_Value gtkscale.h gtk_scale_draw_value
   --  mapping: NOT_IMPLEMENTED gtkscale.h gtk_scale_get_type
   --  mapping: Set_Digits gtkscale.h gtk_scale_set_digits
   --  mapping: Set_Draw_Value gtkscale.h gtk_scale_set_draw_value
   --  mapping: Set_Value_Pos gtkscale.h gtk_scale_set_value_pos
   --  mapping: Value_Width gtkscale.h gtk_scale_value_width
end Gtk.Scale;
