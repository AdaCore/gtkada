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


with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.GRange is

   type Gtk_Range is new Gtk.Widget.Gtk_Widget with private;

   procedure Default_Hmotion
      (The_Range : in Gtk_Range'Class;
       Xdelta    : in Gint;
       Ydelta    : in Gint);
   procedure Default_Hslider_Update (The_Range : in Gtk_Range'Class);

   procedure Default_Htrough_Click
     (The_Range : in Gtk_Range'Class;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    : out Gint);
   --  Was a function in C

   procedure Default_Vmotion
      (The_Range : in Gtk_Range'Class;
       Xdelta    : in Gint;
       Ydelta    : in Gint);
   procedure Default_Vslider_Update (The_Range : in Gtk_Range'Class);

   procedure Default_Vtrough_Click
      (The_Range : in Gtk_Range'Class;
       X         : in Gint;
       Y         : in Gint;
       Jump_Perc : in out Gfloat;
       Result    :    out Gint);
   --  Was a function in C

   procedure Draw_Background (The_Range : in Gtk_Range'Class);
   procedure Draw_Slider (The_Range : in Gtk_Range'Class);
   procedure Draw_Step_Back (The_Range : in Gtk_Range'Class);
   procedure Draw_Step_Forw (The_Range : in Gtk_Range'Class);
   procedure Draw_Trough (The_Range : in Gtk_Range'Class);
   function Get_Adjustment (The_Range  : in Gtk_Range'Class)
                            return      Gtk.Adjustment.Gtk_Adjustment'Class;
   procedure Set_Adjustment
      (The_Range  : in Gtk_Range'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Update_Policy
      (The_Range : in Gtk_Range'Class;
       Policy    : in Gtk_Update_Type);
   procedure Slider_Update (The_Range : in Gtk_Range'Class);

   procedure Trough_Click
      (The_Range : in Gtk_Range'Class;
       X         : in Gint;
       Y         : in Gint;
       Jump_Perc : in out Gfloat;
       Result    :    out Gint);
   --  Was a function in C

private
   type Gtk_Range is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: Default_Hmotion gtkrange.h gtk_range_default_hmotion
   --  mapping: Default_Hslider_Update gtkrange.h \
   --  mapping:    gtk_range_default_hslider_update
   --  mapping: Default_Htrough_Click gtkrange.h  \
   --  mapping:    gtk_range_default_htrough_click
   --  mapping: Default_Vmotion gtkrange.h gtk_range_default_vmotion
   --  mapping: Default_Vslider_Update gtkrange.h \
   --  mapping:    gtk_range_default_vslider_update
   --  mapping: Default_Vtrough_Click gtkrange.h \
   --  mapping:    gtk_range_default_vtrough_click
   --  mapping: Draw_Background gtkrange.h gtk_range_draw_background
   --  mapping: Draw_Slider gtkrange.h gtk_range_draw_slider
   --  mapping: Draw_Step_Back gtkrange.h gtk_range_draw_step_back
   --  mapping: Draw_Step_Forw gtkrange.h gtk_range_draw_step_forw
   --  mapping: Draw_Trough gtkrange.h gtk_range_draw_trough
   --  mapping: Get_Adjustment gtkrange.h gtk_range_get_adjustment
   --  mapping: NOT_IMPLEMENTED gtkrange.h gtk_range_get_type
   --  mapping: Set_Adjustment gtkrange.h gtk_range_set_adjustment
   --  mapping: Set_Update_Policy gtkrange.h gtk_range_set_update_policy
   --  mapping: Slider_Update gtkrange.h gtk_range_slider_update
   --  mapping: Trough_Click gtkrange.h gtk_range_trough_click
end Gtk.GRange;
