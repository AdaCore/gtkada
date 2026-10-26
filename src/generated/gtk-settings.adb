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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Settings is

   package Type_Conversion_Gtk_Settings is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Settings_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Settings);

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self : not null access Gtk_Settings_Record;
       Name : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_settings_reset_property");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Reset_Property;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Settings is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_settings_get_default");
      Stub_Gtk_Settings : Gtk_Settings_Record;
   begin
      return Gtk.Settings.Gtk_Settings (Get_User_Data (Internal, Stub_Gtk_Settings));
   end Get_Default;

   ---------------------
   -- Get_For_Display --
   ---------------------

   function Get_For_Display
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gtk_Settings
   is
      function Internal (Display : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_settings_get_for_display");
      Stub_Gtk_Settings : Gtk_Settings_Record;
   begin
      return Gtk.Settings.Gtk_Settings (Get_User_Data (Internal (Get_Object (Display)), Stub_Gtk_Settings));
   end Get_For_Display;

end Gtk.Settings;
