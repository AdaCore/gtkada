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

with Gtk.Object; use Gtk.Object;
with Gtk.Window;
with Gtk.Button;
with Gtk.Color_Selection;

package Gtk.Color_Selection_Dialog is

   type Gtk_Color_Selection_Dialog_Record is new Gtk.Window.Gtk_Window_Record
     with private;
   type Gtk_Color_Selection_Dialog
     is access all Gtk_Color_Selection_Dialog_Record'Class;

   procedure Gtk_New (Color_Selection_Dialog : out Gtk_Color_Selection_Dialog;
                      Title  : in String);

   procedure Initialize
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record'Class;
      Title  : in String);

   --  Functions to get the fields of the dialog

   function Get_Colorsel
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Color_Selection.Gtk_Color_Selection;
   function Get_OK_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   function Get_Reset_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   function Get_Cancel_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   function Get_Help_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate
     (Color_Selection_Dialog : in out Gtk_Object;
      N                      : in Node_Ptr);

private

   type Gtk_Color_Selection_Dialog_Record is new
     Gtk.Window.Gtk_Window_Record with null record;

end Gtk.Color_Selection_Dialog;
