-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with System;

package body Gtk.Curve is

   ----------------
   -- Get_Vector --
   ----------------

   procedure Get_Vector
     (Curve  : access Gtk_Curve_Record;
      Vector : out Gfloat_Array)
   is
      procedure Internal
        (Curve  : System.Address;
         Veclen : Integer;
         Vector : System.Address);
      pragma Import (C, Internal, "gtk_curve_get_vector");

   begin
      Internal (Get_Object (Curve), Vector'Length, Vector'Address);
   end Get_Vector;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Curve : out Gtk_Curve) is
   begin
      Curve := new Gtk_Curve_Record;
      Gtk.Curve.Initialize (Curve);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Curve : access Gtk_Curve_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_curve_new");

   begin
      Set_Object (Curve, Internal);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset (Curve : access Gtk_Curve_Record) is
      procedure Internal (Curve : System.Address);
      pragma Import (C, Internal, "gtk_curve_reset");

   begin
      Internal (Get_Object (Curve));
   end Reset;

   --------------------
   -- Set_Curve_Type --
   --------------------

   procedure Set_Curve_Type
     (Curve      : access Gtk_Curve_Record;
      Curve_Type : Gtk_Curve_Type)
   is
      procedure Internal
        (Curve : System.Address; Curve_Type : Gtk_Curve_Type);
      pragma Import (C, Internal, "gtk_curve_set_curve_type");

   begin
      Internal (Get_Object (Curve), Curve_Type);
   end Set_Curve_Type;

   ---------------
   -- Set_Gamma --
   ---------------

   procedure Set_Gamma (Curve : access Gtk_Curve_Record; Gamma : Gfloat) is
      procedure Internal (Curve : System.Address; Gamma : Gfloat);
      pragma Import (C, Internal, "gtk_curve_set_gamma");

   begin
      Internal (Get_Object (Curve), Gamma);
   end Set_Gamma;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Curve : access Gtk_Curve_Record;
      Min_X : Gfloat;
      Max_X : Gfloat;
      Min_Y : Gfloat;
      Max_Y : Gfloat)
   is
      procedure Internal
        (Curve : System.Address;
         Min_X : Gfloat;
         Max_X : Gfloat;
         Min_Y : Gfloat;
         Max_Y : Gfloat);
      pragma Import (C, Internal, "gtk_curve_set_range");

   begin
      Internal (Get_Object (Curve), Min_X, Max_X, Min_Y, Max_Y);
   end Set_Range;

   ----------------
   -- Set_Vector --
   ----------------

   procedure Set_Vector
     (Curve  : access Gtk_Curve_Record;
      Vector : Gfloat_Array)
   is
      procedure Internal
        (Curve  : System.Address;
         Veclen : Integer;
         Vector : System.Address);
      pragma Import (C, Internal, "gtk_curve_set_vector");

   begin
      Internal (Get_Object (Curve), Vector'Length,
                Vector (Vector'First)'Address);
   end Set_Vector;

end Gtk.Curve;
