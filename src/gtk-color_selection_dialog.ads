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


with Gtk.Window;
with Gtk.Button;
with Gtk.Color_Selection;

package Gtk.Color_Selection_Dialog is

   type Gtk_Color_Selection_Dialog is new
     Gtk.Window.Gtk_Window with private;

   procedure Gtk_New (Widget : out Gtk_Color_Selection_Dialog;
                      Title  : in String);
   --  mapping: Gtk_New gtkcolorsel.h gtk_color_selection_dialog_new

   function Get_Colorsel (Dialog : in Gtk_Color_Selection_Dialog'Class)
                          return Gtk.Color_Selection.Gtk_Color_Selection;
   function Get_OK_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                           return Gtk.Button.Gtk_Button;
   function Get_Reset_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                              return Gtk.Button.Gtk_Button;
   function Get_Cancel_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                               return Gtk.Button.Gtk_Button;
   function Get_Help_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                             return Gtk.Button.Gtk_Button;
   --  Functions to get the fields of the dialog

   --  mapping: NOT_IMPLEMENTED gtkcolorsel.h \
   --  mapping:    gtk_color_selection_dialog_get_type


private

   type Gtk_Color_Selection_Dialog is new
     Gtk.Window.Gtk_Window with null record;

end Gtk.Color_Selection_Dialog;
