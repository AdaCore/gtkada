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

with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;

package body Gtk.Dialog is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area (Dialog : access Gtk_Dialog_Record)
                             return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_dialog_get_action_area");
      Stub : Gtk.Box.Gtk_Box_Record;
   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Action_Area;

   --------------
   -- Get_Vbox --
   --------------

   function Get_Vbox (Dialog : access Gtk_Dialog_Record)
                      return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : in System.Address) return System.Address;
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

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
   begin
      Gen_New (N, "Dialog", File => File);
      Window.Generate (N, File);
   end Generate;

   procedure Generate (Dialog : in out Gtk_Object;
                       N      : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Dialog (Dialog));
         Set_Object (Get_Field (N, "name"), Dialog);
         N.Specific_Data.Created := True;
      end if;

      Window.Generate (Dialog, N);
   end Generate;

end Gtk.Dialog;
