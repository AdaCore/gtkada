------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

package body Gdk.Device_Manager is

   function Get_Device_Manager
     (Self : not null access Gdk.Display.Gdk_Display_Record'Class)
   return Gdk.Device_Manager.Gdk_Device_Manager
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_get_device_manager");
      Stub_Gdk_Device_Manager : Gdk.Device_Manager.Gdk_Device_Manager_Record;
   begin
      return Gdk.Device_Manager.Gdk_Device_Manager (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device_Manager));
   end Get_Device_Manager;

   package Type_Conversion_Gdk_Device_Manager is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Device_Manager_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Device_Manager);

   ------------------------
   -- Get_Client_Pointer --
   ------------------------

   function Get_Client_Pointer
      (Self : not null access Gdk_Device_Manager_Record)
       return Gdk.Device.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_device_manager_get_client_pointer");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Client_Pointer;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Device_Manager_Record)
       return Gdk.Display.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_device_manager_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ------------------
   -- List_Devices --
   ------------------

   function List_Devices
      (Self     : not null access Gdk_Device_Manager_Record;
       The_Type : Gdk.Device.Gdk_Device_Type)
       return Gdk.Device.Device_List.Glist
   is
      function Internal
         (Self     : System.Address;
          The_Type : Gdk.Device.Gdk_Device_Type) return System.Address;
      pragma Import (C, Internal, "gdk_device_manager_list_devices");
      Tmp_Return : Gdk.Device.Device_List.Glist;
   begin
      Gdk.Device.Device_List.Set_Object (Tmp_Return, Internal (Get_Object (Self), The_Type));
      return Tmp_Return;
   end List_Devices;

end Gdk.Device_Manager;
