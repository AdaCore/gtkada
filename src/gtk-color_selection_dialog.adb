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
-- Library General Public License for more details.                  --
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

package body Gtk.Color_Selection_Dialog is

   ------------------
   -- Get_Colorsel --
   ------------------

   function Get_Colorsel (Dialog : in Gtk_Color_Selection_Dialog'Class)
                          return Gtk.Color_Selection.Gtk_Color_Selection
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_colorsel");
      Tmp : Gtk.Color_Selection.Gtk_Color_Selection;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Colorsel;

   -------------------
   -- Get_OK_Button --
   -------------------

   function Get_OK_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                           return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_ok_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_OK_Button;

   ----------------------
   -- Get_Reset_Button --
   ----------------------

   function Get_Reset_Button (Dialog : in Gtk_Color_Selection_Dialog'Class)
                              return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_reset_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Reset_Button;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                               return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_cancel_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Cancel_Button;

   ---------------------
   -- Get_Help_Button --
   ---------------------

   function Get_Help_Button (Dialog : in  Gtk_Color_Selection_Dialog'Class)
                             return Gtk.Button.Gtk_Button
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_colorsel_dialog_get_help_button");
      Tmp : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Tmp, Internal (Get_Object (Dialog)));
      return Tmp;
   end Get_Help_Button;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Color_Selection_Dialog;
      Title  : in String)
   is
      function Internal (S : String) return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_dialog_new");
   begin
      Set_Object (Widget, Internal (Title & Ascii.NUL));
   end Gtk_New;

end Gtk.Color_Selection_Dialog;
