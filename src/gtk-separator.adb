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

package body Gtk.Separator is

   ------------------------
   -- Gtk_New_Hseparator --
   ------------------------

   procedure Gtk_New_Hseparator (Separator : out Gtk_Separator) is
   begin
      Separator := new Gtk_Separator_Record;
      Initialize_Hseparator (Separator);
   end Gtk_New_Hseparator;

   ------------------------
   -- Gtk_New_Vseparator --
   ------------------------

   procedure Gtk_New_Vseparator (Separator : out Gtk_Separator) is
   begin
      Separator := new Gtk_Separator_Record;
      Initialize_Vseparator (Separator);
   end Gtk_New_Vseparator;

   ---------------------------
   -- Initialize_Hseparator --
   ---------------------------

   procedure Initialize_Hseparator
     (Separator : access Gtk_Separator_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hseparator_new");

   begin
      Set_Object (Separator, Internal);
      Initialize_User_Data (Separator);
   end Initialize_Hseparator;

   ------------------------
   -- Initialize_Vseparator --
   ------------------------

   procedure Initialize_Vseparator
     (Separator : access Gtk_Separator_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vseparator_new");

   begin
      Set_Object (Separator, Internal);
      Initialize_User_Data (Separator);
   end Initialize_Vseparator;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Class : String_Ptr := Get_Field (N, "class");
   begin
      Gen_New (N, "Separator", "", "",
        Class (Class'First + 3) & "separator", File);
      Widget.Generate (N, File);
   end Generate;

   procedure Generate
     (Separator : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      Class : String_Ptr := Get_Field (N, "class");
   begin
      if not N.Specific_Data.Created then
         if Class (Class'First + 3) = 'H' then
            Gtk_New_Hseparator (Gtk_Separator (Separator));
         else
            Gtk_New_Vseparator (Gtk_Separator (Separator));
         end if;

         Set_Object (Get_Field (N, "name"), Separator);
         N.Specific_Data.Created := True;
      end if;

      Widget.Generate (Separator, N);
   end Generate;

end Gtk.Separator;
