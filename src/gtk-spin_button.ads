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
with Gtk.GEntry;
with Gtk.Object;

package Gtk.Spin_Button is

   type Gtk_Spin_Button_Record is new Gtk.GEntry.Gtk_Entry_Record with private;
   type Gtk_Spin_Button is access all Gtk_Spin_Button_Record'Class;

   procedure Configure
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint);

   function Get_Adjustment (Spin_Button : access Gtk_Spin_Button_Record)
     return Gtk.Adjustment.Gtk_Adjustment;

   function Get_Value_As_Float (Spin_Button : access Gtk_Spin_Button_Record)
     return Gfloat;

   function Get_Value_As_Int (Spin_Button : access Gtk_Spin_Button_Record)
     return Gint;

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint);

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint);

   procedure Set_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Set_Digits
     (Spin_Button : access Gtk_Spin_Button_Record;
      The_Digits  : in Gint);

   procedure Set_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record;
      Numeric     : in Boolean);

   procedure Set_Snap_To_Ticks
    (Spin_Button   : in Gtk_Spin_Button;
     Snap_To_Ticks : in Boolean);

   procedure Set_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record;
      Policy      : in Gtk_Spin_Button_Update_Policy);

   procedure Set_Value
     (Spin_Button : access Gtk_Spin_Button_Record;
      Value       : in Gfloat);

   procedure Set_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record;
      Wrap        : in Boolean);

   procedure Spin
     (Spin_Button : access Gtk_Spin_Button_Record;
      Direction   : in Gtk.Enums.Gtk_Arrow_Type;
      Step        : in Gfloat);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate
     (Spin_Button : in out Object.Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Spin_Button_Record is new Gtk.GEntry.Gtk_Entry_Record
     with null record;

end Gtk.Spin_Button;
