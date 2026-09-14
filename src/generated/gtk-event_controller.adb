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
with Gdk.Device;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Event_Controller is

   package Type_Conversion_Gtk_Event_Controller is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Event_Controller_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Event_Controller);

   -----------------------
   -- Get_Current_Event --
   -----------------------

   function Get_Current_Event
      (Self : not null access Gtk_Event_Controller_Record)
       return Gdk.Event.Gdk_Event
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_get_current_event");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Current_Event;

   ------------------------------
   -- Get_Current_Event_Device --
   ------------------------------

   function Get_Current_Event_Device
      (Self : not null access Gtk_Event_Controller_Record)
       return Gdk.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_get_current_event_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Current_Event_Device;

   -----------------------------
   -- Get_Current_Event_State --
   -----------------------------

   function Get_Current_Event_State
      (Self : not null access Gtk_Event_Controller_Record)
       return Gdk.Enums.Gdk_Modifier_Type
   is
      function Internal
         (Self : System.Address) return Gdk.Enums.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_event_controller_get_current_event_state");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Event_State;

   ----------------------------
   -- Get_Current_Event_Time --
   ----------------------------

   function Get_Current_Event_Time
      (Self : not null access Gtk_Event_Controller_Record) return Guint32
   is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "gtk_event_controller_get_current_event_time");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Event_Time;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gtk_Event_Controller_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_event_controller_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ---------------------------
   -- Get_Propagation_Limit --
   ---------------------------

   function Get_Propagation_Limit
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Enums.Gtk_Propagation_Limit
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Propagation_Limit;
      pragma Import (C, Internal, "gtk_event_controller_get_propagation_limit");
   begin
      return Internal (Get_Object (Self));
   end Get_Propagation_Limit;

   ---------------------------
   -- Get_Propagation_Phase --
   ---------------------------

   function Get_Propagation_Phase
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Enums.Gtk_Propagation_Phase
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Propagation_Phase;
      pragma Import (C, Internal, "gtk_event_controller_get_propagation_phase");
   begin
      return Internal (Get_Object (Self));
   end Get_Propagation_Phase;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_get_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Widget;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : not null access Gtk_Event_Controller_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_event_controller_reset");
   begin
      Internal (Get_Object (Self));
   end Reset;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Self : not null access Gtk_Event_Controller_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_event_controller_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   ---------------------------
   -- Set_Propagation_Limit --
   ---------------------------

   procedure Set_Propagation_Limit
      (Self  : not null access Gtk_Event_Controller_Record;
       Limit : Gtk.Enums.Gtk_Propagation_Limit)
   is
      procedure Internal
         (Self  : System.Address;
          Limit : Gtk.Enums.Gtk_Propagation_Limit);
      pragma Import (C, Internal, "gtk_event_controller_set_propagation_limit");
   begin
      Internal (Get_Object (Self), Limit);
   end Set_Propagation_Limit;

   ---------------------------
   -- Set_Propagation_Phase --
   ---------------------------

   procedure Set_Propagation_Phase
      (Self  : not null access Gtk_Event_Controller_Record;
       Phase : Gtk.Enums.Gtk_Propagation_Phase)
   is
      procedure Internal
         (Self  : System.Address;
          Phase : Gtk.Enums.Gtk_Propagation_Phase);
      pragma Import (C, Internal, "gtk_event_controller_set_propagation_phase");
   begin
      Internal (Get_Object (Self), Phase);
   end Set_Propagation_Phase;

   ---------------------
   -- Set_Static_Name --
   ---------------------

   procedure Set_Static_Name
      (Self : not null access Gtk_Event_Controller_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_event_controller_set_static_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Static_Name;

end Gtk.Event_Controller;
