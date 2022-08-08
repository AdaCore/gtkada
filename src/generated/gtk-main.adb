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
with Ada.Unchecked_Conversion;
with Gtkada.Bindings;          use Gtkada.Bindings;
with Gtkada.Types;             use Gtkada.Types;
with Interfaces.C;             use Interfaces.C;

package body Gtk.Main is

   gnat_argc : Interfaces.C.int;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   procedure Init is
      procedure Internal (argc : System.Address; argv : System.Address);
      pragma Import (C, Internal, "gtk_init");

   begin
      Internal (gnat_argc'Address, gnat_argv'Address);
   end Init;

   function Init_Check return Boolean is
      function Internal
        (argc : System.Address; argv : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_init_check");
   begin
      return Boolean'Val (Internal (gnat_argc'Address, gnat_argv'Address));
   end Init_Check;

   function C_Gtk_Key_Snooper_Install
      (Snooper   : System.Address;
       Func_Data : System.Address) return Guint;
   pragma Import (C, C_Gtk_Key_Snooper_Install, "gtk_key_snooper_install");
   pragma Obsolescent (C_Gtk_Key_Snooper_Install);
   --  Installs a key snooper function, which will get called on all key
   --  events before delivering them normally.
   --  Deprecated since 3.4, 1
   --  "snooper": a Gtk_Key_Snoop_Func
   --  "func_data": data to pass to Snooper

   function To_Gtk_Key_Snoop_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Key_Snoop_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Key_Snoop_Func, System.Address);

   function Internal_Gtk_Key_Snoop_Func
      (Grab_Widget : System.Address;
       Event       : access Gdk.Event.Gdk_Event_Key;
       Func_Data   : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_Key_Snoop_Func);
   --  "grab_widget": the widget to which the event will be delivered
   --  "event": the key event
   --  "func_data": data supplied to Gtk.Main.Key_Snooper_Install

   ---------------------------------
   -- Internal_Gtk_Key_Snoop_Func --
   ---------------------------------

   function Internal_Gtk_Key_Snoop_Func
      (Grab_Widget : System.Address;
       Event       : access Gdk.Event.Gdk_Event_Key;
       Func_Data   : System.Address) return Glib.Gint
   is
      Func            : constant Gtk_Key_Snoop_Func := To_Gtk_Key_Snoop_Func (Func_Data);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Func (Gtk.Widget.Gtk_Widget (Get_User_Data (Grab_Widget, Stub_Gtk_Widget)), Event.all);
   end Internal_Gtk_Key_Snoop_Func;

   -------------------------
   -- Key_Snooper_Install --
   -------------------------

   function Key_Snooper_Install (Snooper : Gtk_Key_Snoop_Func) return Guint is
   begin
      if Snooper = null then
         return C_Gtk_Key_Snooper_Install (System.Null_Address, System.Null_Address);
      else
         return C_Gtk_Key_Snooper_Install (Internal_Gtk_Key_Snoop_Func'Address, To_Address (Snooper));
      end if;
   end Key_Snooper_Install;

   -------------------
   -- Check_Version --
   -------------------

   function Check_Version
      (Required_Major : Guint;
       Required_Minor : Guint;
       Required_Micro : Guint) return UTF8_String
   is
      function Internal
         (Required_Major : Guint;
          Required_Minor : Guint;
          Required_Micro : Guint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_check_version");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Required_Major, Required_Minor, Required_Micro));
   end Check_Version;

   ---------------------
   -- Device_Grab_Add --
   ---------------------

   procedure Device_Grab_Add
      (Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Device       : not null access Gdk.Device.Gdk_Device_Record'Class;
       Block_Others : Boolean)
   is
      procedure Internal
         (Widget       : System.Address;
          Device       : System.Address;
          Block_Others : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_device_grab_add");
   begin
      Internal (Get_Object (Widget), Get_Object (Device), Boolean'Pos (Block_Others));
   end Device_Grab_Add;

   ------------------------
   -- Device_Grab_Remove --
   ------------------------

   procedure Device_Grab_Remove
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
   is
      procedure Internal (Widget : System.Address; Device : System.Address);
      pragma Import (C, Internal, "gtk_device_grab_remove");
   begin
      Internal (Get_Object (Widget), Get_Object (Device));
   end Device_Grab_Remove;

   -----------------------
   -- Disable_Setlocale --
   -----------------------

   procedure Disable_Setlocale is
      procedure Internal;
      pragma Import (C, Internal, "gtk_disable_setlocale");
   begin
      Internal;
   end Disable_Setlocale;

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_events_pending");
   begin
      return Internal /= 0;
   end Events_Pending;

   -----------
   -- False --
   -----------

   function False return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_false");
   begin
      return Internal /= 0;
   end False;

   --------------------
   -- Get_Binary_Age --
   --------------------

   function Get_Binary_Age return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_binary_age");
   begin
      return Internal;
   end Get_Binary_Age;

   -----------------------
   -- Get_Current_Event --
   -----------------------

   function Get_Current_Event return Gdk.Event.Gdk_Event is
      function Internal return Gdk.Event.Gdk_Event;
      pragma Import (C, Internal, "gtk_get_current_event");
   begin
      return Internal;
   end Get_Current_Event;

   ------------------------------
   -- Get_Current_Event_Device --
   ------------------------------

   function Get_Current_Event_Device return Gdk.Device.Gdk_Device is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_get_current_event_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal, Stub_Gdk_Device));
   end Get_Current_Event_Device;

   -----------------------------
   -- Get_Current_Event_State --
   -----------------------------

   procedure Get_Current_Event_State
      (State             : out Gdk.Types.Gdk_Modifier_Type;
       Has_Current_Event : out Boolean)
   is
      function Internal
         (Acc_State : access Gdk.Types.Gdk_Modifier_Type)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_get_current_event_state");
      Acc_State  : aliased Gdk.Types.Gdk_Modifier_Type;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Acc_State'Access);
      State := Acc_State;
      Has_Current_Event := Tmp_Return /= 0;
   end Get_Current_Event_State;

   ----------------------------
   -- Get_Current_Event_Time --
   ----------------------------

   function Get_Current_Event_Time return Guint32 is
      function Internal return Guint32;
      pragma Import (C, Internal, "gtk_get_current_event_time");
   begin
      return Internal;
   end Get_Current_Event_Time;

   --------------------------
   -- Get_Default_Language --
   --------------------------

   function Get_Default_Language return Pango.Language.Pango_Language is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_get_default_language");
   begin
      return From_Object (Internal);
   end Get_Default_Language;

   ----------------------
   -- Get_Event_Widget --
   ----------------------

   function Get_Event_Widget
      (Event : Gdk.Event.Gdk_Event) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Event : Gdk.Event.Gdk_Event) return System.Address;
      pragma Import (C, Internal, "gtk_get_event_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Event), Stub_Gtk_Widget));
   end Get_Event_Widget;

   -----------------------
   -- Get_Interface_Age --
   -----------------------

   function Get_Interface_Age return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_interface_age");
   begin
      return Internal;
   end Get_Interface_Age;

   -----------------------
   -- Get_Major_Version --
   -----------------------

   function Get_Major_Version return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_major_version");
   begin
      return Internal;
   end Get_Major_Version;

   -----------------------
   -- Get_Micro_Version --
   -----------------------

   function Get_Micro_Version return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_micro_version");
   begin
      return Internal;
   end Get_Micro_Version;

   -----------------------
   -- Get_Minor_Version --
   -----------------------

   function Get_Minor_Version return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_minor_version");
   begin
      return Internal;
   end Get_Minor_Version;

   ----------------------
   -- Grab_Get_Current --
   ----------------------

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_grab_get_current");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal, Stub_Gtk_Widget));
   end Grab_Get_Current;

   ------------------------
   -- Key_Snooper_Remove --
   ------------------------

   procedure Key_Snooper_Remove (Snooper_Handler_Id : Guint) is
      procedure Internal (Snooper_Handler_Id : Guint);
      pragma Import (C, Internal, "gtk_key_snooper_remove");
   begin
      Internal (Snooper_Handler_Id);
   end Key_Snooper_Remove;

   ----------
   -- Main --
   ----------

   procedure Main is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main");
   begin
      Internal;
   end Main;

   -------------------
   -- Main_Do_Event --
   -------------------

   procedure Main_Do_Event (Event : Gdk.Event.Gdk_Event) is
      procedure Internal (Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_main_do_event");
   begin
      Internal (Event);
   end Main_Do_Event;

   --------------------
   -- Main_Iteration --
   --------------------

   function Main_Iteration return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_main_iteration");
   begin
      return Internal /= 0;
   end Main_Iteration;

   -----------------------
   -- Main_Iteration_Do --
   -----------------------

   function Main_Iteration_Do (Blocking : Boolean) return Boolean is
      function Internal (Blocking : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_main_iteration_do");
   begin
      return Internal (Boolean'Pos (Blocking)) /= 0;
   end Main_Iteration_Do;

   ----------------
   -- Main_Level --
   ----------------

   function Main_Level return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_main_level");
   begin
      return Internal;
   end Main_Level;

   ---------------
   -- Main_Quit --
   ---------------

   procedure Main_Quit is
      procedure Internal;
      pragma Import (C, Internal, "gtk_main_quit");
   begin
      Internal;
   end Main_Quit;

   ---------------------
   -- Propagate_Event --
   ---------------------

   procedure Propagate_Event
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Event  : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Widget : System.Address;
          Event  : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_propagate_event");
   begin
      Internal (Get_Object (Widget), Event);
   end Propagate_Event;

   ----------
   -- True --
   ----------

   function True return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_true");
   begin
      return Internal /= 0;
   end True;

end Gtk.Main;
