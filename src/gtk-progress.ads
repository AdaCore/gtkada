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
with Gtk.Widget;
with Gtk.Object;

package Gtk.Progress is

   type Gtk_Progress_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Progress is access all Gtk_Progress_Record'Class;

   function Get_Current_Percentage
     (Progress : access Gtk_Progress_Record) return Gfloat;

   function Get_Current_Text
     (Progress : access Gtk_Progress_Record) return String;

   function Get_Percentage_From_Value
     (Progress : access Gtk_Progress_Record;
      Value    : in Gfloat) return Gfloat;

   function Get_Text_From_Value
     (Progress : access Gtk_Progress_Record;
      Value    : in Gfloat) return String;

   function Get_Value (Progress : access Gtk_Progress_Record) return Gfloat;

   function Get_Adjustment
     (Widget : access Gtk_Progress_Record)
      return Gtk.Adjustment.Gtk_Adjustment;

   procedure Configure
     (Progress : access Gtk_Progress_Record;
      Value    : in Gfloat;
      Min      : in Gfloat;
      Max      : in Gfloat);

   procedure Set_Activity_Mode
     (Progress      : access Gtk_Progress_Record;
      Activity_Mode : in Boolean);

   function Get_Activity_Mode
     (Progress : access Gtk_Progress_Record) return Boolean;

   procedure Set_Adjustment
     (Progress   : access Gtk_Progress_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Format_String
     (Progress : access Gtk_Progress_Record;
      Format   : in String);

   procedure Set_Percentage
     (Progress   : access Gtk_Progress_Record;
      Percentage : in Gfloat);

   procedure Set_Show_Text
     (Progress  : access Gtk_Progress_Record;
      Show_Text : in Boolean);

   procedure Set_Text_Alignment
     (Progress : access Gtk_Progress_Record;
      X_Align  : in Gfloat;
      Y_Align  : in Gfloat);

   procedure Set_Value
     (Progress : access Gtk_Progress_Record;
      Value    : in Gfloat);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (Progress : in out Object.Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Progress_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

end Gtk.Progress;
