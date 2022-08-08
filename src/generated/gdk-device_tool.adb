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

package body Gdk.Device_Tool is

   package Type_Conversion_Gdk_Device_Tool is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Device_Tool_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Device_Tool);

   ---------------------
   -- Get_Hardware_Id --
   ---------------------

   function Get_Hardware_Id
      (Self : not null access Gdk_Device_Tool_Record) return Guint64
   is
      function Internal (Self : System.Address) return Guint64;
      pragma Import (C, Internal, "gdk_device_tool_get_hardware_id");
   begin
      return Internal (Get_Object (Self));
   end Get_Hardware_Id;

   ----------------
   -- Get_Serial --
   ----------------

   function Get_Serial
      (Self : not null access Gdk_Device_Tool_Record) return Guint64
   is
      function Internal (Self : System.Address) return Guint64;
      pragma Import (C, Internal, "gdk_device_tool_get_serial");
   begin
      return Internal (Get_Object (Self));
   end Get_Serial;

   -------------------
   -- Get_Tool_Type --
   -------------------

   function Get_Tool_Type
      (Self : not null access Gdk_Device_Tool_Record)
       return Gdk_Device_Tool_Type
   is
      function Internal (Self : System.Address) return Gdk_Device_Tool_Type;
      pragma Import (C, Internal, "gdk_device_tool_get_tool_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Tool_Type;

end Gdk.Device_Tool;
