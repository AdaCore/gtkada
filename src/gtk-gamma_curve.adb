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

with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;

package body Gtk.Gamma_Curve is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Gamma_Curve : out Gtk_Gamma_Curve) is
   begin
      Gamma_Curve := new Gtk_Gamma_Curve_Record;
      Initialize (Gamma_Curve);
   end Gtk_New;

   ---------------
   -- Get_Curve --
   ---------------

   function Get_Curve (Gamma_Curve : access Gtk_Gamma_Curve_Record)
                       return Gtk.Curve.Gtk_Curve
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gamma_curve_get_curve");

      Stub : Gtk.Curve.Gtk_Curve_Record;
   begin
      return Gtk.Curve.Gtk_Curve
        (Get_User_Data (Internal (Get_Object (Gamma_Curve)), Stub));
   end Get_Curve;

   ---------------
   -- Get_Gamma --
   ---------------

   function Get_Gamma (Gamma_Curve : access Gtk_Gamma_Curve_Record)
                       return Gfloat
   is
      function Internal (Widget : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_gamma_curve_get_gamma");
   begin
      return Internal (Get_Object (Gamma_Curve));
   end Get_Gamma;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Gamma_Curve : access Gtk_Gamma_Curve_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_gamma_curve_new");
   begin
      Set_Object (Gamma_Curve, Internal);
      Initialize_User_Data (Gamma_Curve);
   end Initialize;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
   begin
      Gen_New (N, "Gamma_Curve", File => File);
      Box.Generate (N, File);
      Add_Package ("Curve");
      Put_Line (File, "   Set_Range (Get_Curve (" &
        To_Ada (Get_Field (Find_Top_Widget (N), "name").all) & "." &
        To_Ada (Get_Field (N, "name").all) & "), " &
        To_Float (Get_Field (N, "min_x").all) & ", " &
        To_Float (Get_Field (N, "max_x").all) & ", " &
        To_Float (Get_Field (N, "min_y").all) & ", " &
        To_Float (Get_Field (N, "max_y").all) & ");");
   end Generate;

   procedure Generate
     (Gamma_Curve : in out Object.Gtk_Object;
      N           : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Gamma_Curve (Gamma_Curve));
         Set_Object (Get_Field (N, "name"), Gamma_Curve);
         N.Specific_Data.Created := True;
      end if;

      Box.Generate (Gamma_Curve, N);

      Curve.Set_Range (Get_Curve (Gtk_Gamma_Curve (Gamma_Curve)),
        Gfloat'Value (Get_Field (N, "min_x").all),
        Gfloat'Value (Get_Field (N, "max_x").all),
        Gfloat'Value (Get_Field (N, "min_y").all),
        Gfloat'Value (Get_Field (N, "max_y").all));
   end Generate;

end Gtk.Gamma_Curve;
