-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with Gtk.Enums;
with Gtk.Box;

package Gtk.Color_Selection is

   type Gtk_Color_Selection is new Gtk.Box.Gtk_Box with private;

   type Color_Index is (Red, Green, Blue, Opacity);
   type Color_Array is array (Color_Index'Range) of Gdouble;

   procedure Get_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : out Color_Array);
   --  mapping: Get_Color gtkcolorsel.h gtk_color_selection_get_color

   procedure Gtk_New (Widget : out Gtk_Color_Selection);
   --  mapping: Gtk_New gtkcolorsel.h gtk_color_selection_new

   procedure Set_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : in Color_Array);
   --  mapping: Set_Color gtkcolorsel.h gtk_color_selection_set_color

   procedure Set_Opacity (Colorsel    : in Gtk_Color_Selection'Class;
                          Use_Opacity : in Boolean);
   --  mapping: Set_Opacity gtkcolorsel.h gtk_color_selection_set_opacity

   procedure Set_Update_Policy (Colorsel : in Gtk_Color_Selection'Class;
                                Policy   : in Enums.Gtk_Update_Type);
   --  mapping: Set_Update_Policy gtkcolorsel.h \
   --  mapping: gtk_color_selection_set_update_policy

private

   type Gtk_Color_Selection is new Gtk.Box.Gtk_Box with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkcolorsel.h gtk_color_selection_get_type

end Gtk.Color_Selection;
