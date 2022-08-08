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

package body Gdk.Event is

   function From_Object_Free (B : access Gdk_Event_Sequence) return Gdk_Event_Sequence is
      Result : constant Gdk_Event_Sequence := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Any) return Gdk_Event_Any is
      Result : constant Gdk_Event_Any := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Button) return Gdk_Event_Button is
      Result : constant Gdk_Event_Button := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Expose) return Gdk_Event_Expose is
      Result : constant Gdk_Event_Expose := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Visibility) return Gdk_Event_Visibility is
      Result : constant Gdk_Event_Visibility := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Motion) return Gdk_Event_Motion is
      Result : constant Gdk_Event_Motion := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Scroll) return Gdk_Event_Scroll is
      Result : constant Gdk_Event_Scroll := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Key) return Gdk_Event_Key is
      Result : constant Gdk_Event_Key := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Crossing) return Gdk_Event_Crossing is
      Result : constant Gdk_Event_Crossing := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Focus) return Gdk_Event_Focus is
      Result : constant Gdk_Event_Focus := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Configure) return Gdk_Event_Configure is
      Result : constant Gdk_Event_Configure := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Property) return Gdk_Event_Property is
      Result : constant Gdk_Event_Property := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Selection) return Gdk_Event_Selection is
      Result : constant Gdk_Event_Selection := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Owner_Change) return Gdk_Event_Owner_Change is
      Result : constant Gdk_Event_Owner_Change := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Proximity) return Gdk_Event_Proximity is
      Result : constant Gdk_Event_Proximity := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_DND) return Gdk_Event_DND is
      Result : constant Gdk_Event_DND := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Window_State) return Gdk_Event_Window_State is
      Result : constant Gdk_Event_Window_State := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Setting) return Gdk_Event_Setting is
      Result : constant Gdk_Event_Setting := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Touch) return Gdk_Event_Touch is
      Result : constant Gdk_Event_Touch := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Grab_Broken) return Gdk_Event_Grab_Broken is
      Result : constant Gdk_Event_Grab_Broken := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Touchpad_Swipe) return Gdk_Event_Touchpad_Swipe is
      Result : constant Gdk_Event_Touchpad_Swipe := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Touchpad_Pinch) return Gdk_Event_Touchpad_Pinch is
      Result : constant Gdk_Event_Touchpad_Pinch := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Pad_Button) return Gdk_Event_Pad_Button is
      Result : constant Gdk_Event_Pad_Button := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Pad_Axis) return Gdk_Event_Pad_Axis is
      Result : constant Gdk_Event_Pad_Axis := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Pad_Group_Mode) return Gdk_Event_Pad_Group_Mode is
      Result : constant Gdk_Event_Pad_Group_Mode := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gdk_Event_Record) return Gdk_Event_Record is
      Result : constant Gdk_Event_Record := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Address (C : System.Address) return Gdk_Event is
      function Convert is new Ada.Unchecked_Conversion
        (Glib.C_Proxy, Gdk_Event);
   begin
      return Convert (Glib.C_Proxy'(Glib.To_Proxy (C)));
   end From_Address;

   function To_Address (C : Gdk_Event) return System.Address is
      function Convert is new Ada.Unchecked_Conversion
        (Gdk_Event, Glib.C_Proxy);
   begin
      return Glib.To_Address (Convert (C));
   end To_Address;

   function Get_Event (Value : Glib.Values.GValue) return Gdk_Event is
      function Convert is new Ada.Unchecked_Conversion
        (Glib.C_Proxy, Gdk_Event);
   begin
      return Convert (Glib.Values.Get_Proxy (Value));
   end Get_Event;

   function To_Event (Event : access Gdk_Event_Button) return Gdk_Event is
      type Gdk_Event_Button_Access is access all Gdk_Event_Button;
      function Convert is new Ada.Unchecked_Conversion
        (Gdk_Event_Button_Access, Gdk_Event);
   begin
      return Convert (Gdk_Event_Button_Access (Event));
   end To_Event;

   function To_Event (Event : access Gdk_Event_Key) return Gdk_Event is
      type Gdk_Event_Key_Access is access all Gdk_Event_Key;
      function Convert is new Ada.Unchecked_Conversion
        (Gdk_Event_Key_Access, Gdk_Event);
   begin
      return Convert (Gdk_Event_Key_Access (Event));
   end To_Event;

   procedure C_Gdk_Event_Handler_Set
      (Func   : System.Address;
       Data   : System.Address;
       Notify : System.Address);
   pragma Import (C, C_Gdk_Event_Handler_Set, "gdk_event_handler_set");
   --  Sets the function to call to handle all events from GDK.
   --  Note that GTK+ uses this to install its own event handler, so it is
   --  usually not useful for GTK+ applications. (Although an application can
   --  call this function then call Gtk.Main.Main_Do_Event to pass events to
   --  GTK+.)
   --  "func": the function to call to handle events from GDK.
   --  "data": user data to pass to the function.
   --  "notify": the function to call when the handler function is removed,
   --  i.e. when Gdk.Event.Handler_Set is called with another event handler.

   function To_Gdk_Event_Func is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Event_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gdk_Event_Func, System.Address);

   procedure Internal_Gdk_Event_Func
      (Event : Gdk.Event.Gdk_Event;
       Data  : System.Address);
   pragma Convention (C, Internal_Gdk_Event_Func);
   --  "event": the Gdk.Event.Gdk_Event to process.
   --  "data": user data set when the event handler was installed with
   --  Gdk.Event.Handler_Set.

   -----------------------------
   -- Internal_Gdk_Event_Func --
   -----------------------------

   procedure Internal_Gdk_Event_Func
      (Event : Gdk.Event.Gdk_Event;
       Data  : System.Address)
   is
      Func : constant Gdk_Event_Func := To_Gdk_Event_Func (Data);
   begin
      Func (Event);
   end Internal_Gdk_Event_Func;

   -------------------
   -- Gdk_Event_New --
   -------------------

   function Gdk_Event_New (The_Type : Gdk_Event_Type) return Gdk_Event is
      function Internal (The_Type : Gdk_Event_Type) return Gdk_Event;
      pragma Import (C, Internal, "gdk_event_new");
      Event : Gdk_Event;
   begin
      Event := Internal (The_Type);
      return Event;
   end Gdk_Event_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Event : out Gdk_Event; The_Type : Gdk_Event_Type) is
      function Internal (The_Type : Gdk_Event_Type) return Gdk_Event;
      pragma Import (C, Internal, "gdk_event_new");
   begin
      Event := Internal (The_Type);
   end Gdk_New;

   ---------------
   -- Get_Angle --
   ---------------

   function Get_Angle
      (Event  : Gdk_Event;
       Event2 : Gdk_Event;
       Angle  : access Gdouble) return Boolean
   is
      function Internal
         (Event     : Gdk_Event;
          Event2    : Gdk_Event;
          Acc_Angle : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_get_angle");
      Acc_Angle  : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Event, Event2, Acc_Angle'Access);
      Angle.all := Acc_Angle;
      return Tmp_Return /= 0;
   end Get_Angle;

   ----------------
   -- Get_Center --
   ----------------

   function Get_Center
      (Event  : Gdk_Event;
       Event2 : Gdk_Event;
       X      : access Gdouble;
       Y      : access Gdouble) return Boolean
   is
      function Internal
         (Event  : Gdk_Event;
          Event2 : Gdk_Event;
          Acc_X  : access Gdouble;
          Acc_Y  : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_get_center");
      Acc_X      : aliased Gdouble;
      Acc_Y      : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Event, Event2, Acc_X'Access, Acc_Y'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      return Tmp_Return /= 0;
   end Get_Center;

   ---------------------
   -- Get_Device_Tool --
   ---------------------

   function Get_Device_Tool
      (Event : Gdk_Event) return Gdk.Device_Tool.Gdk_Device_Tool
   is
      function Internal (Event : Gdk_Event) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_device_tool");
      Stub_Gdk_Device_Tool : Gdk.Device_Tool.Gdk_Device_Tool_Record;
   begin
      return Gdk.Device_Tool.Gdk_Device_Tool (Get_User_Data (Internal (Event), Stub_Gdk_Device_Tool));
   end Get_Device_Tool;

   ------------------
   -- Get_Distance --
   ------------------

   function Get_Distance
      (Event    : Gdk_Event;
       Event2   : Gdk_Event;
       Distance : access Gdouble) return Boolean
   is
      function Internal
         (Event        : Gdk_Event;
          Event2       : Gdk_Event;
          Acc_Distance : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_get_distance");
      Acc_Distance : aliased Gdouble;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Event, Event2, Acc_Distance'Access);
      Distance.all := Acc_Distance;
      return Tmp_Return /= 0;
   end Get_Distance;

   --------------------------
   -- Get_Pointer_Emulated --
   --------------------------

   function Get_Pointer_Emulated (Event : Gdk_Event) return Boolean is
      function Internal (Event : Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_get_pointer_emulated");
   begin
      return Internal (Event) /= 0;
   end Get_Pointer_Emulated;

   --------------
   -- Get_Seat --
   --------------

   function Get_Seat (Event : Gdk_Event) return Glib.Object.GObject is
      function Internal (Event : Gdk_Event) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_seat");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Event), Stub_GObject);
   end Get_Seat;

   -----------------
   -- Handler_Set --
   -----------------

   procedure Handler_Set (Func : Gdk_Event_Func) is
   begin
      if Func = null then
         C_Gdk_Event_Handler_Set (System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gdk_Event_Handler_Set (Internal_Gdk_Event_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Handler_Set;

   package body Handler_Set_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gdk_Event_Func is new Ada.Unchecked_Conversion
        (System.Address, Gdk_Event_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gdk_Event_Func, System.Address);

      procedure Internal_Cb
         (Event : Gdk.Event.Gdk_Event;
          Data  : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Specifies the type of function passed to Gdk.Event.Handler_Set to
      --  handle all GDK events.
      --  "event": the Gdk.Event.Gdk_Event to process.
      --  "data": user data set when the event handler was installed with
      --  Gdk.Event.Handler_Set.

      -----------------
      -- Handler_Set --
      -----------------

      procedure Handler_Set (Func : Gdk_Event_Func; Data : User_Data_Type) is
         D : System.Address;
      begin
         if Func = null then
            C_Gdk_Event_Handler_Set (System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gdk_Event_Handler_Set (Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Handler_Set;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Event : Gdk.Event.Gdk_Event;
          Data  : System.Address)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gdk_Event_Func (D.Func) (Event, D.Data.all);
      end Internal_Cb;

   end Handler_Set_User_Data;

   --------------------------
   -- Is_Scroll_Stop_Event --
   --------------------------

   function Is_Scroll_Stop_Event (Event : Gdk_Event) return Boolean is
      function Internal (Event : Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_is_scroll_stop_event");
   begin
      return Internal (Event) /= 0;
   end Is_Scroll_Stop_Event;

   ---------------------
   -- Set_Device_Tool --
   ---------------------

   procedure Set_Device_Tool
      (Event : Gdk_Event;
       Tool  : access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class)
   is
      procedure Internal (Event : Gdk_Event; Tool : System.Address);
      pragma Import (C, Internal, "gdk_event_set_device_tool");
   begin
      Internal (Event, Get_Object_Or_Null (GObject (Tool)));
   end Set_Device_Tool;

   ---------------------------
   -- Triggers_Context_Menu --
   ---------------------------

   function Triggers_Context_Menu (Event : Gdk_Event) return Boolean is
      function Internal (Event : Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_event_triggers_context_menu");
   begin
      return Internal (Event) /= 0;
   end Triggers_Context_Menu;

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_events_pending");
   begin
      return Internal /= 0;
   end Events_Pending;

   ---------------------
   -- Get_Show_Events --
   ---------------------

   function Get_Show_Events return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_get_show_events");
   begin
      return Internal /= 0;
   end Get_Show_Events;

   ---------------------
   -- Set_Show_Events --
   ---------------------

   procedure Set_Show_Events (Show_Events : Boolean) is
      procedure Internal (Show_Events : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_set_show_events");
   begin
      Internal (Boolean'Pos (Show_Events));
   end Set_Show_Events;

end Gdk.Event;
