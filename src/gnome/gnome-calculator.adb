-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk; use Gtk;
with System;

package body Gnome.Calculator is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Calculator) is
   begin
      Widget := new Gnome_Calculator_Record;
      Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gnome_Calculator_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_calculator_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Gc    : access Gnome_Calculator_Record;
      Reset : Boolean)
   is
      procedure Internal
        (Gc    : System.Address;
         Reset : Gint);
      pragma Import (C, Internal, "gnome_calculator_clear");
   begin
      Internal (Get_Object (Gc),
                Boolean'Pos (Reset));
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set
     (Gc     : access Gnome_Calculator_Record;
      Result : Gdouble)
   is
      procedure Internal
        (Gc     : System.Address;
         Result : Gdouble);
      pragma Import (C, Internal, "gnome_calculator_set");
   begin
      Internal (Get_Object (Gc),
                Result);
   end Set;

end Gnome.Calculator;
