-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gtk.Dialog is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_dialog_get_action_area");

      Stub : Gtk.Box.Gtk_Box_Record;

   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Action_Area;

   --------------
   -- Get_Vbox --
   --------------

   function Get_Vbox
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_dialog_get_vbox");

      Stub : Gtk.Box.Gtk_Box_Record;

   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Vbox;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Dialog : out Gtk_Dialog) is
   begin
      Dialog := new Gtk_Dialog_Record;
      Initialize (Dialog);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dialog : access Gtk_Dialog_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");

   begin
      Set_Object (Dialog, Internal);
      Initialize_User_Data (Dialog);
   end Initialize;

end Gtk.Dialog;
