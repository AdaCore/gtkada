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

with Gtk.Enums;
with Gtk.Box;

package Gtk.Color_Selection is

   type Gtk_Color_Selection is new Gtk.Box.Gtk_Box with private;

   type Color_Index is (Red, Green, Blue, Opacity);
   type Color_Array is array (Color_Index'Range) of Gdouble;

   procedure Get_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : out Color_Array);

   procedure Gtk_New (Widget : out Gtk_Color_Selection);

   procedure Set_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : in Color_Array);

   procedure Set_Opacity (Colorsel    : in Gtk_Color_Selection'Class;
                          Use_Opacity : in Boolean);

   procedure Set_Update_Policy (Colorsel : in Gtk_Color_Selection'Class;
                                Policy   : in Enums.Gtk_Update_Type);

private

   type Gtk_Color_Selection is new Gtk.Box.Gtk_Box with null record;


end Gtk.Color_Selection;
