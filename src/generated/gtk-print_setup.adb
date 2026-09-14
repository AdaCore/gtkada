------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Print_Setup is

   function From_Object_Free
     (B : access Gtk_Print_Setup'Class) return Gtk_Print_Setup
   is
      Result : constant Gtk_Print_Setup := Gtk_Print_Setup (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Print_Setup is
      S : Gtk_Print_Setup;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   --------------------
   -- Get_Page_Setup --
   --------------------

   function Get_Page_Setup
      (Self : Gtk_Print_Setup) return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_setup_get_page_setup");
      Stub_Gtk_Page_Setup : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Page_Setup));
   end Get_Page_Setup;

   ------------------------
   -- Get_Print_Settings --
   ------------------------

   function Get_Print_Settings
      (Self : Gtk_Print_Setup) return Gtk.Print_Settings.Gtk_Print_Settings
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_setup_get_print_settings");
      Stub_Gtk_Print_Settings : Gtk.Print_Settings.Gtk_Print_Settings_Record;
   begin
      return Gtk.Print_Settings.Gtk_Print_Settings (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Print_Settings));
   end Get_Print_Settings;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Print_Setup) return Gtk_Print_Setup is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_setup_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Print_Setup) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_print_setup_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Print_Setup;
