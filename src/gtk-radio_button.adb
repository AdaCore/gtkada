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

package body Gtk.Radio_Button is

   use Widget_SList;

   -----------
   -- Group --
   -----------

   function Group (Button : in Gtk_Radio_Button) return Widget_SList.GSlist is
      function Internal (Button : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_group");
      Group : Widget_SList.GSlist;
   begin
      Set_Object (Group, Internal (Get_Object (Button)));
      return Group;
   end Group;

   -------------
   -- GtK_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Widget_SList.GSlist)
   is
      function Internal (Group : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new");
   begin
      Set_Object (Button, Internal (Get_Object (Group)));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in  Widget_SList.GSlist;
      Label  : in String)
   is
      function Internal (Group : in System.Address;
                         Label : in String) return System.Address;
      pragma Import (C, Internal,
                     "gtk_radio_button_new_with_label");
   begin
      Set_Object (Button, Internal (Get_Object (Group),
                                    Label & ASCII.NUL));
   end Gtk_New;

   -------------
   -- GtK_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button)
   is
      function Internal (Group : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_from_widget");
   begin
      Set_Object (Button, Internal (Get_Object (Group)));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button;
      Label  : in String)
   is
      function Internal (Group : in System.Address;
                         Label : in String) return System.Address;
      pragma Import (C, Internal,
                     "gtk_radio_button_new_with_label_from_widget");
   begin
      Set_Object (Button, Internal (Get_Object (Group),
                                    Label & ASCII.NUL));
   end Gtk_New;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group (Button : in Gtk_Radio_Button;
                        Group  : in Widget_SList.GSlist)
   is
      procedure Internal (Button : in System.Address;
                          Group  : in System.Address);
      pragma Import (C, Internal, "gtk_radio_button_set_group");
   begin
      Internal (Get_Object (Button), Get_Object (Group));
   end Set_Group;


end Gtk.Radio_Button;
