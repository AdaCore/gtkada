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

with Gtk.Object;
with Gtk.Box;
with Gtk.Curve;

package Gtk.Gamma_Curve is

   type Gtk_Gamma_Curve_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Gamma_Curve is access all Gtk_Gamma_Curve_Record'Class;

   procedure Gtk_New (Gamma_Curve : out Gtk_Gamma_Curve);
   procedure Initialize (Gamma_Curve : access Gtk_Gamma_Curve_Record'Class);

   function Get_Curve (Gamma_Curve : access Gtk_Gamma_Curve_Record)
     return Gtk.Curve.Gtk_Curve;

   function Get_Gamma (Gamma_Curve : access Gtk_Gamma_Curve_Record)
     return Gfloat;

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type);
   --  Gate internal function

   procedure Generate
     (Gamma_Curve : in out Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private

   type Gtk_Gamma_Curve_Record is new Gtk.Box.Gtk_Box_Record with null record;

end Gtk.Gamma_Curve;
