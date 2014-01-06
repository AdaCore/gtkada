------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

package body Gdk.Drag_Contexts is

   package Type_Conversion_Drag_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Drag_Context_Record);
   pragma Unreferenced (Type_Conversion_Drag_Context);

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action
   is
      function Internal (Self : System.Address) return Gdk_Drag_Action;
      pragma Import (C, Internal, "gdk_drag_context_get_actions");
   begin
      return Internal (Get_Object (Self));
   end Get_Actions;

   ---------------------
   -- Get_Dest_Window --
   ---------------------

   function Get_Dest_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_drag_context_get_dest_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Dest_Window;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device
      (Self : not null access Drag_Context_Record)
       return Gdk.Device.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drag_context_get_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Device;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Protocol
   is
      function Internal (Self : System.Address) return Gdk_Drag_Protocol;
      pragma Import (C, Internal, "gdk_drag_context_get_protocol");
   begin
      return Internal (Get_Object (Self));
   end Get_Protocol;

   -------------------------
   -- Get_Selected_Action --
   -------------------------

   function Get_Selected_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action
   is
      function Internal (Self : System.Address) return Gdk_Drag_Action;
      pragma Import (C, Internal, "gdk_drag_context_get_selected_action");
   begin
      return Internal (Get_Object (Self));
   end Get_Selected_Action;

   -----------------------
   -- Get_Source_Window --
   -----------------------

   function Get_Source_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_drag_context_get_source_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Source_Window;

   --------------------------
   -- Get_Suggested_Action --
   --------------------------

   function Get_Suggested_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action
   is
      function Internal (Self : System.Address) return Gdk_Drag_Action;
      pragma Import (C, Internal, "gdk_drag_context_get_suggested_action");
   begin
      return Internal (Get_Object (Self));
   end Get_Suggested_Action;

   ----------------
   -- Set_Device --
   ----------------

   procedure Set_Device
      (Self   : not null access Drag_Context_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
   is
      procedure Internal (Self : System.Address; Device : System.Address);
      pragma Import (C, Internal, "gdk_drag_context_set_device");
   begin
      Internal (Get_Object (Self), Get_Object (Device));
   end Set_Device;

end Gdk.Drag_Contexts;
