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

with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object;
with Gtk.Widget;

package Gtk.GRange is

   type Gtk_Range_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Range is access all Gtk_Range_Record'Class;
   subtype Gtk_GRange is Gtk_Range;

   procedure Default_Hmotion
     (The_Range : access Gtk_Range_Record;
      Xdelta    : in Gint;
      Ydelta    : in Gint);

   procedure Default_Hslider_Update (The_Range : access Gtk_Range_Record);

   procedure Default_Htrough_Click
     (The_Range : access Gtk_Range_Record;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    : out Gint);
   --  Was a function in C

   procedure Default_Vmotion
     (The_Range : access Gtk_Range_Record;
      Xdelta    : in Gint;
      Ydelta    : in Gint);

   procedure Default_Vslider_Update (The_Range : access Gtk_Range_Record);

   procedure Default_Vtrough_Click
     (The_Range : access Gtk_Range_Record;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    :    out Gint);
   --  Was a function in C

   procedure Draw_Background (The_Range : access Gtk_Range_Record);

   procedure Draw_Slider (The_Range : access Gtk_Range_Record);

   procedure Draw_Step_Back (The_Range : access Gtk_Range_Record);

   procedure Draw_Step_Forw (The_Range : access Gtk_Range_Record);

   procedure Draw_Trough (The_Range : access Gtk_Range_Record);

   function Get_Adjustment (The_Range  : access Gtk_Range_Record)
     return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Adjustment
     (The_Range  : access Gtk_Range_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Update_Policy
     (The_Range : access Gtk_Range_Record;
      Policy    : in Gtk_Update_Type);

   procedure Slider_Update (The_Range : access Gtk_Range_Record);

   procedure Trough_Click
     (The_Range : access Gtk_Range_Record;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    :    out Gint);
   --  Was a function in C

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (The_Range : in out Object.Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Range_Record is new Gtk.Widget.Gtk_Widget_Record with null record;

end Gtk.GRange;
