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

package Gtk.Spin_Button is

   type Gtk_Spin_Button is new Gtk.GEntry.Gtk_Entry with private;

   procedure Configure
      (Spin_Button : in Gtk_Spin_Button;
       Adjustment  : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Climb_Rate  : in Gfloat;
       The_Digits  : in Gint);
   function Get_Adjustment (Spin_Button : in Gtk_Spin_Button)
                            return     Gtk.Adjustment.Gtk_Adjustment'Class;
   function Get_Value_As_Float (Spin_Button : in Gtk_Spin_Button)
                                return           Gfloat;
   function Get_Value_As_Int (Spin_Button : in Gtk_Spin_Button)
                              return           Gint;
   procedure Gtk_New
      (Widget     : out Gtk_Spin_Button;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Climb_Rate : in Gfloat;
       The_Digits : in Gint);
   procedure Set_Adjustment
      (Spin_Button : in Gtk_Spin_Button;
       Adjustment  : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Digits
      (Spin_Button : in Gtk_Spin_Button;
       The_Digits  : in Gint);
   procedure Set_Numeric
      (Spin_Button : in Gtk_Spin_Button;
       Numeric     : in Boolean);
   procedure Set_Update_Policy
      (Spin_Button : in Gtk_Spin_Button;
       Policy      : in Gtk_Spin_Button_Update_Policy);
   procedure Set_Value
      (Spin_Button : in Gtk_Spin_Button;
       Value       : in Gfloat);
   procedure Set_Wrap
      (Spin_Button : in Gtk_Spin_Button;
       Wrap        : in Boolean);
   procedure Spin
      (Spin_Button : in Gtk_Spin_Button;
       Direction   : in Gtk.Enums.Gtk_Arrow_Type;
       Step        : in Gfloat);

private
   type Gtk_Spin_Button is new Gtk.GEntry.Gtk_Entry with null record;

end Gtk.Spin_Button;
