-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                  Copyright (C) 2000-2001                          --
--                            ACT-Europe                             --
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
with Glib; use Glib;
with Gtk; use Gtk;

package body Gnome.App is

   --------------------------
   -- Enable_Layout_Config --
   --------------------------

   procedure Enable_Layout_Config
     (App    : access Gnome_App_Record;
      Enable : Boolean)
   is
      procedure Internal
        (App    : System.Address;
         Enable : Gboolean);
      pragma Import (C, Internal, "gnome_app_enable_layout_config");

   begin
      Internal (Get_Object (App), Boolean'Pos (Enable));
   end Enable_Layout_Config;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (App     : out Gnome_App;
      Appname : String;
      Title   : String := "") is
   begin
      App := new Gnome_App_Record;
      Initialize (App, Appname, Title);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (App     : access Gnome_App_Record'Class;
      Appname : String;
      Title   : String := "")
   is
      function Internal
        (Appname : String; Title : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_app_new");

      The_Title : aliased constant String := Title & ASCII.NUL;

   begin
      if Title'Length = 0 then
         Set_Object (App, Internal (Appname & ASCII.NUL, System.Null_Address));
      else
         Set_Object (App, Internal (Appname & ASCII.NUL, The_Title'Address));
      end if;

      Initialize_User_Data (App);
   end Initialize;

   ------------------
   -- Set_Contents --
   ------------------

   procedure Set_Contents
     (App      : access Gnome_App_Record;
      Contents : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (App      : System.Address;
         Contents : System.Address);
      pragma Import (C, Internal, "gnome_app_set_contents");

   begin
      Internal (Get_Object (App), Get_Object (Contents));
   end Set_Contents;

   ---------------
   -- Set_Menus --
   ---------------

   procedure Set_Menus
     (App     : access Gnome_App_Record;
      Menubar : access Gtk_Menu_Bar_Record'Class)
   is
      procedure Internal
        (App     : System.Address;
         Menubar : System.Address);
      pragma Import (C, Internal, "gnome_app_set_menus");

   begin
      Internal (Get_Object (App), Get_Object (Menubar));
   end Set_Menus;

   -------------------
   -- Set_StatusBar --
   -------------------

   procedure Set_StatusBar
     (App       : access Gnome_App_Record;
      Statusbar : access Gtk_Status_Bar_Record'Class)
   is
      procedure Internal
        (App       : System.Address;
         Statusbar : System.Address);
      pragma Import (C, Internal, "gnome_app_set_statusbar");

   begin
      Internal (Get_Object (App), Get_Object (Statusbar));
   end Set_StatusBar;

   --------------------------
   -- Set_Statusbar_Custom --
   --------------------------

   procedure Set_Statusbar_Custom
     (App       : access Gnome_App_Record;
      Container : access Gtk_Container_Record'Class;
      Statusbar : access Gtk_Status_Bar_Record'Class)
   is
      procedure Internal
        (App       : System.Address;
         Container : System.Address;
         Statusbar : System.Address);
      pragma Import (C, Internal, "gnome_app_set_statusbar_custom");

   begin
      Internal
        (Get_Object (App), Get_Object (Container), Get_Object (Statusbar));
   end Set_Statusbar_Custom;

   -----------------
   -- Set_Toolbar --
   -----------------

   procedure Set_Toolbar
     (App     : access Gnome_App_Record;
      Toolbar : access Gtk_Toolbar_Record'Class)
   is
      procedure Internal
        (App     : System.Address;
         Toolbar : System.Address);
      pragma Import (C, Internal, "gnome_app_set_toolbar");

   begin
      Internal (Get_Object (App), Get_Object (Toolbar));
   end Set_Toolbar;

end Gnome.App;
