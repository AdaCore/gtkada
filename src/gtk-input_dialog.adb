-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
with Gtk.Util; use Gtk.Util;

package body Gtk.Input_Dialog is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Input_Dialog : out Gtk_Input_Dialog) is
   begin
      Input_Dialog := new Gtk_Input_Dialog_Record;
      Initialize (Input_Dialog);
   end Gtk_New;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
   begin
      Gen_New (N, "Input_Dialog", File => File);
      Dialog.Generate (N, File);
   end Generate;

   procedure Generate (Input_Dialog : in out Gtk_Object;
                       N            : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Input_Dialog (Input_Dialog));
         Set_Object (Get_Field (N, "name"), Input_Dialog);
         N.Specific_Data.Created := True;
      end if;

      Dialog.Generate (Input_Dialog, N);
   end Generate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Input_Dialog : access Gtk_Input_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_input_dialog_new");
   begin
      Set_Object (Input_Dialog, Internal);
      Initialize_User_Data (Input_Dialog);
   end Initialize;

end Gtk.Input_Dialog;
