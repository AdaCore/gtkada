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

with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Ruler is

   type Gtk_Ruler_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   subtype Gtk_Hruler_Record is Gtk_Ruler_Record;
   subtype Gtk_Vruler_Record is Gtk_Ruler_Record;

   type Gtk_Ruler is access all Gtk_Ruler_Record'Class;
   subtype Gtk_Hruler is Gtk_Ruler;
   subtype Gtk_Vruler is Gtk_Ruler;

   procedure Gtk_New_Hruler (Ruler : out Gtk_Ruler);

   procedure Gtk_New_Vruler (Ruler : out Gtk_Ruler);

   procedure Initialize_Hruler (Ruler : access Gtk_Ruler_Record'Class);

   procedure Initialize_Vruler (Ruler : access Gtk_Ruler_Record'Class);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Ruler.

   procedure Draw_Pos (Ruler : access Gtk_Ruler_Record);

   procedure Draw_Ticks (Ruler : access Gtk_Ruler_Record);

   function Get_Lower (Ruler : access Gtk_Ruler_Record) return Gfloat;

   function Get_Max_Size (Ruler : access Gtk_Ruler_Record) return Gfloat;

   function Get_Position (Ruler : access Gtk_Ruler_Record) return Gfloat;

   function Get_Upper (Ruler : access Gtk_Ruler_Record) return Gfloat;

   procedure Set_Metric
     (Ruler  : access Gtk_Ruler_Record;
      Metric : in Gtk_Metric_Type);

   procedure Set_Range
     (Ruler    : access Gtk_Ruler_Record;
      Lower    : in Gfloat;
      Upper    : in Gfloat;
      Position : in Gfloat;
      Max_Size : in Gfloat);

private
   type Gtk_Ruler_Record is new Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gtk_ruler_get_type");

end Gtk.Ruler;
