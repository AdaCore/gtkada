-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Gtk.Drawing_Area;
with Gtk.Object;

package Gtk.Curve is

   type Gtk_Curve_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with private;
   type Gtk_Curve is access all Gtk_Curve_Record'Class;

   type Gtk_Curve_Type is (Curve_Type_Linear,
                           Curve_Type_Spline,
                           Curve_Type_Free);

   procedure Get_Vector (Curve  : access Gtk_Curve_Record;
                         Vector : in out Gfloat_Array);
   procedure Gtk_New (Curve : out Gtk_Curve);
   procedure Initialize (Curve : access Gtk_Curve_Record'Class);
   procedure Reset (Curve : access Gtk_Curve_Record);
   procedure Set_Curve_Type (Curve      : access Gtk_Curve_Record;
                             Curve_Type : in Gtk_Curve_Type);
   procedure Set_Gamma (Curve : access Gtk_Curve_Record; Gamma : in Gfloat);
   procedure Set_Range
     (Curve : access Gtk_Curve_Record;
      Min_X : in Gfloat;
      Max_X : in Gfloat;
      Min_Y : in Gfloat;
      Max_Y : in Gfloat);
   procedure Set_Vector (Curve  : access Gtk_Curve_Record;
                         Vector : in Gfloat_Array);

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type);
   --  Gate internal function

   procedure Generate (Curve : in out Gtk.Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private

   type Gtk_Curve_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with null record;

end Gtk.Curve;
