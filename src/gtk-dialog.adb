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

package body Gtk.Dialog is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area (Widget : in Gtk_Dialog'Class)
                             return      Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_dialog_get_action_area");
      Tmp : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Action_Area;

   --------------
   -- Get_Vbox --
   --------------

   function Get_Vbox (Widget : in Gtk_Dialog'Class)
                      return      Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_dialog_get_vbox");
      Tmp : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Vbox;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Dialog)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.Dialog;

