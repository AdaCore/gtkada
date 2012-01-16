------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Option_Menu is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Option_Menu_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------
   -- Get_Menu --
   --------------

   function Get_Menu
     (Option_Menu : access Gtk_Option_Menu_Record) return Gtk.Menu.Gtk_Menu
   is
      function Internal (Option_Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_option_menu_get_menu");

      Stub : Gtk.Menu.Gtk_Menu_Record;

   begin
      return Gtk.Menu.Gtk_Menu
        (Get_User_Data (Internal (Get_Object (Option_Menu)), Stub));
   end Get_Menu;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Option_Menu : access Gtk_Option_Menu_Record) return Gint
   is
      function Internal (Option_Menu : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_option_menu_get_history");

   begin
      return Internal (Get_Object (Option_Menu));
   end Get_History;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Option_Menu : out Gtk_Option_Menu) is
   begin
      Option_Menu := new Gtk_Option_Menu_Record;
      Gtk.Option_Menu.Initialize (Option_Menu);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Option_Menu : access Gtk_Option_Menu_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_option_menu_new");

   begin
      Set_Object (Option_Menu, Internal);
   end Initialize;

   -----------------
   -- Remove_Menu --
   -----------------

   procedure Remove_Menu
     (Option_Menu : access Gtk_Option_Menu_Record;
      Menu        : access Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Option_Menu, Menu : System.Address);
      pragma Import (C, Internal, "gtk_option_menu_remove_menu");

   begin
      Internal (Get_Object (Option_Menu), Get_Object (Menu));
   end Remove_Menu;

   -----------------
   -- Set_History --
   -----------------

   procedure Set_History
     (Option_Menu : access Gtk_Option_Menu_Record; Index : Gint)
   is
      procedure Internal (Option_Menu : System.Address; Index : Gint);
      pragma Import (C, Internal, "gtk_option_menu_set_history");

   begin
      Internal (Get_Object (Option_Menu), Index);
   end Set_History;

   --------------
   -- Set_Menu --
   --------------

   procedure Set_Menu
     (Option_Menu : access Gtk_Option_Menu_Record;
      Menu        : access Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Option_Menu, Menu : System.Address);
      pragma Import (C, Internal, "gtk_option_menu_set_menu");

   begin
      Internal (Get_Object (Option_Menu), Get_Object (Menu));
   end Set_Menu;

end Gtk.Option_Menu;
