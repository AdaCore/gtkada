-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000 Helix Code, Inc.                 --
--               Copyright (C) 2000-2001 ACT-Europe                  --
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
with Gtk; use Gtk;

package body Gnome.Less is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Less : out Gnome_Less) is
   begin
      Less := new Gnome_Less_Record;
      Initialize (Less);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Less : access Gnome_Less_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_less_new");
   begin
      Set_Object (Less, Internal);
      Initialize_User_Data (Less);
   end Initialize;

   ------------
   -- Reshow --
   ------------

   procedure Reshow (Less : Gnome_Less) is
      procedure Internal (Less : System.Address);
      pragma Import (C, Internal, "gnome_less_reshow");
   begin
      Internal (Get_Object (Less));
   end Reshow;

   --------------------
   -- Set_Fixed_Font --
   --------------------

   procedure Set_Fixed_Font (Less : Gnome_Less; Fixed : Boolean := True) is
      procedure Internal (Less : System.Address; Fixed : Integer);
      pragma Import (C, Internal, "gnome_less_set_fixed_font");

   begin
      Internal (Get_Object (Less), Boolean'Pos (Fixed));
   end Set_Fixed_Font;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (Less : Gnome_Less; Font : Gdk.Font.Gdk_Font) is
      procedure Internal (Less : System.Address; Font : Gdk.Font.Gdk_Font);
      pragma Import (C, Internal, "gnome_less_set_font");

   begin
      Internal (Get_Object (Less), Font);
   end Set_Font;

   ------------------
   -- Show_Command --
   ------------------

   function Show_Command
     (Less : Gnome_Less; Command : String) return Boolean
   is
      function Internal
         (Less : System.Address; Command : String) return Integer;
      pragma Import (C, Internal, "gnome_less_show_command");

   begin
      return Internal (Get_Object (Less), Command & ASCII.NUL) /= 0;
   end Show_Command;

   ---------------
   -- Show_File --
   ---------------

   function Show_File (Less : Gnome_Less; Path : String) return Boolean is
      function Internal (Less : System.Address; Path : String) return Integer;
      pragma Import (C, Internal, "gnome_less_show_file");

   begin
      return Internal (Get_Object (Less), Path & ASCII.NUL) /= 0;
   end Show_File;

   -----------------
   -- Show_String --
   -----------------

   function Show_String (Less : Gnome_Less; S : String) return Boolean is
      function Internal (Less : System.Address; S : String) return Integer;
      pragma Import (C, Internal, "gnome_less_show_string");

   begin
      return Internal (Get_Object (Less), S & ASCII.NUL) /= 0;
   end Show_String;

   ----------------
   -- Write_File --
   ----------------

   function Write_File (Less : Gnome_Less; Path : String) return Boolean is
      function Internal (Less : System.Address; Path : String) return Integer;
      pragma Import (C, Internal, "gnome_less_write_file");

   begin
      return Internal (Get_Object (Less), Path & ASCII.NUL) /= 0;
   end Write_File;

end Gnome.Less;
