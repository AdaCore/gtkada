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


with Gtk.Drawing_Area;

package Gtk.Curve is

   type Gtk_Curve is new Gtk.Drawing_Area.Gtk_Drawing_Area with private;
   type Gtk_Curve_Type is (Gtk_Curve_Type_Linear,
                           Gtk_Curve_Type_Spline,
                           Gtk_Curve_Type_Free);

   procedure Get_Vector (Curve  : in Gtk_Curve'Class;
                         Vector : in out Gfloat_Array);
   procedure Gtk_New (Widget : out Gtk_Curve);
   procedure Reset (Curve : in Gtk_Curve'Class);
   procedure Set_Curve_Type (Curve      : in Gtk_Curve'Class;
                             Curve_Type : in Gtk_Curve_Type);
   procedure Set_Gamma (Curve : in Gtk_Curve'Class; Gamma : in Gfloat);
   procedure Set_Range
     (Curve : in Gtk_Curve'Class;
      Min_X : in Gfloat;
      Max_X : in Gfloat;
      Min_Y : in Gfloat;
      Max_Y : in Gfloat);
   procedure Set_Vector (Curve  : in Gtk_Curve'Class;
                         Vector : in Gfloat_Array);

private

   type Gtk_Curve is new Gtk.Drawing_Area.Gtk_Drawing_Area with null record;

   --  mapping: NOT_IMPLEMENTED gtkcurve.h gtk_curve_get_type
   --  mapping: Gtk_New  gtkcurve.h gtk_curve_new
   --  mapping: Reset gtkcurve.h gtk_curve_reset
   --  mapping: Set_Gamma gtkcurve.h gtk_curve_set_gamma
   --  mapping: Set_Range gtkcurve.h gtk_curve_set_range
   --  mapping: Get_Vector gtkcurve.h gtk_curve_get_vector
   --  mapping: Set_Vector gtkcurve.h gtk_curve_set_vector
   --  mapping: Set_Curve_Type gtkcurve.h gtk_curve_set_curve_type

end Gtk.Curve;
