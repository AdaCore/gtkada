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

with Gtk.Data;

package Gtk.Adjustment is

   type Gtk_Adjustment is new Data.Gtk_Data with private;

   function Null_Adjustment return Gtk_Adjustment;

   procedure Gtk_New (Adjustment : out Gtk_Adjustment;
                      Value          : in Gfloat;
                      Lower          : in Gfloat;
                      Upper          : in Gfloat;
                      Step_Increment : in Gfloat;
                      Page_Increment : in Gfloat;
                      Page_Size      : in Gfloat);

   function Get_Value (Adjustment : in Gtk_Adjustment) return Gfloat;
   function Get_Lower (Adjustment : in Gtk_Adjustment) return Gfloat;
   function Get_Upper (Adjustment : in Gtk_Adjustment) return Gfloat;

   function Get_Step_Increment (Adjustment : in Gtk_Adjustment) return Gfloat;

   procedure Set_Upper (Adjustment : in out Gtk_Adjustment; Upper : Gfloat);
   procedure Set_Lower (Adjustment : in out Gtk_Adjustment; Lower : Gfloat);

   procedure Set_Value (Adjustment : in out Gtk_Adjustment; Value : in Gfloat);

   procedure Clamp_Page (Adjustment : in out Gtk_Adjustment;
                         Lower      : in     Gfloat;
                         Upper      : in     Gfloat);

   procedure Set_Page_Size (Adjustment : in out Gtk_Adjustment;
                            Page_Size  : in Gfloat);

   procedure Set_Page_Increment (Adjustment : in out Gtk_Adjustment;
                                 Page_Increment : in Gfloat);

private

   type Gtk_Adjustment is new Data.Gtk_Data with null record;

end Gtk.Adjustment;
