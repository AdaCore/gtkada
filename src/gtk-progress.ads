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

with Gtk.Adjustment;
with Gtk.Widget;

package Gtk.Progress is

   type Gtk_Progress is new Gtk.Widget.Gtk_Widget with private;

   function Get_Current_Percentage (Progress : in Gtk_Progress)
                                    return        Gfloat;
   function Get_Current_Text (Progress : in Gtk_Progress)
                              return        String;
   function Get_Percentage_From_Value
     (Progress : in Gtk_Progress;
      Value    : in Gfloat)
      return        Gfloat;
   function Get_Text_From_Value
     (Progress : in Gtk_Progress;
      Value    : in Gfloat)
      return        String;
   function Get_Value (Progress : in Gtk_Progress)
                       return        Gfloat;

   function Get_Adjustment (Widget : in Gtk_Progress)
                            return Gtk.Adjustment.Gtk_Adjustment;

   procedure Configure
     (Progress : in Gtk_Progress;
      Value    : in Gfloat;
      Min      : in Gfloat;
      Max      : in Gfloat);

   procedure Set_Activity_Mode
     (Progress      : in Gtk_Progress;
      Activity_Mode : in Boolean);
   function Get_Activity_Mode (Progress : in Gtk_Progress) return Boolean;

   procedure Set_Adjustment
      (Progress   : in Gtk_Progress;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Set_Format_String
      (Progress : in Gtk_Progress;
       Format   : in String);
   procedure Set_Percentage
      (Progress   : in Gtk_Progress;
       Percentage : in Gfloat);
   procedure Set_Show_Text
      (Progress  : in Gtk_Progress;
       Show_Text : in Boolean);
   procedure Set_Text_Alignment
      (Progress : in Gtk_Progress;
       X_Align  : in Gfloat;
       Y_Align  : in Gfloat);
   procedure Set_Value
      (Progress : in Gtk_Progress;
       Value    : in Gfloat);

private
   type Gtk_Progress is new Gtk.Widget.Gtk_Widget with null record;

end Gtk.Progress;
