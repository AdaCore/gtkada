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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Gtk.Option_Menu is

   --------------
   -- Get_Menu --
   --------------

   function Get_Menu (Option_Menu : access Gtk_Option_Menu_Record)
                      return Gtk.Menu.Gtk_Menu
   is
      function Internal (Option_Menu : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_option_menu_get_menu");
      Stub : Gtk.Menu.Gtk_Menu_Record;
   begin
      return Gtk.Menu.Gtk_Menu
        (Get_User_Data (Internal (Get_Object (Option_Menu)), Stub));
   end Get_Menu;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Option_Menu : out Gtk_Option_Menu) is
   begin
      Option_Menu := new Gtk_Option_Menu_Record;
      Initialize (Option_Menu);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Option_Menu : access Gtk_Option_Menu_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_option_menu_new");
   begin
      Set_Object (Option_Menu, Internal);
      Initialize_User_Data (Option_Menu);
   end Initialize;

   -----------------
   -- Remove_Menu --
   -----------------

   procedure Remove_Menu (Option_Menu : access Gtk_Option_Menu_Record;
                          Menu        : access Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Option_Menu, Menu : in System.Address);
      pragma Import (C, Internal, "gtk_option_menu_remove_menu");
   begin
      Internal (Get_Object (Option_Menu), Get_Object (Menu));
   end Remove_Menu;

   -----------------
   -- Set_History --
   -----------------

   procedure Set_History (Option_Menu : access Gtk_Option_Menu_Record;
                          Index       : in     Gint) is
      procedure Internal (Option_Menu : in System.Address; Index : in Gint);
      pragma Import (C, Internal, "gtk_option_menu_set_history");
   begin
      Internal (Get_Object (Option_Menu), Index);
   end Set_History;

   --------------
   -- Set_Menu --
   --------------

   procedure Set_Menu (Option_Menu : access Gtk_Option_Menu_Record;
                       Menu        : access Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Option_Menu, Menu : in System.Address);
      pragma Import (C, Internal, "gtk_option_menu_set_menu");
   begin
      Internal (Get_Object (Option_Menu), Get_Object (Menu));
   end Set_Menu;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      S   : String_Ptr;
      First, Last : Natural;

   begin
      Gen_New (N, "Option_Menu", File => File);
      Button.Generate (N, File);

      S := Get_Field (N, "items");

      if S /= null then
         First := S'First;

         Add_Package ("Menu");
         Add_Package ("Menu_Item");
         Put_Line (File, "   Menu.Gtk_New (" &
           To_Ada (Get_Field (N, "name").all) & "_Menu);");

         loop
            Last := Index (S (First .. S'Last), ASCII.LF & "");

            if Last = 0 then
               Last := S'Last + 1;
            end if;

            Put (File, "   Menu_Item.Gtk_New (The_Menu_Item, ");

            if Gettext_Support (N) then
               Put (File, '-');
            end if;

            Put_Line (File, '"' & S (First .. Last - 1) & """);");
            Put_Line (File, "   Menu.Append (" &
              To_Ada (Get_Field (N, "name").all) & "_Menu, The_Menu_Item);");

            exit when Last >= S'Last;

            First := Last + 1;
         end loop;

         Put_Line (File, "   Option_Menu.Set_Menu");
         Put_Line (File, "     (Gtk_Option_Menu (" &
           To_Ada (Get_Field (Find_Top_Widget (N), "name").all) & "." &
           To_Ada (Get_Field (N, "name").all) & "),");
         Put_Line (File, "      " & To_Ada (Get_Field (N, "name").all) &
           "_Menu);");
      end if;
   end Generate;

   procedure Generate
     (Option_Menu : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S             : String_Ptr;
      The_Menu      : Menu.Gtk_Menu;
      The_Menu_Item : Menu_Item.Gtk_Menu_Item;
      First, Last   : Natural;

   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Option_Menu (Option_Menu));
         Set_Object (Get_Field (N, "name"), Option_Menu);
         N.Specific_Data.Created := True;
      end if;

      Button.Generate (Option_Menu, N);

      S := Get_Field (N, "items");

      if S /= null then
         First := S'First;

         Menu.Gtk_New (The_Menu);

         loop
            Last := Index (S (First .. S'Last), ASCII.LF & "");

            if Last = 0 then
               Last := S'Last + 1;
            end if;

            Menu_Item.Gtk_New (The_Menu_Item, S (First .. Last - 1));
            Menu.Append (The_Menu, The_Menu_Item);

            exit when Last >= S'Last;

            First := Last + 1;
         end loop;

         Set_Menu (Gtk_Option_Menu (Option_Menu), The_Menu);
      end if;
   end Generate;

end Gtk.Option_Menu;
