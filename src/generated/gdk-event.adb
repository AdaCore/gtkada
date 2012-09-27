------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Glib.Object;

package body Gdk.Event is

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

   function To_Gdk_Event_Func is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Event_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gdk_Event_Func, System.Address);

   procedure C_Gdk_Event_Handler_Set
      (Func   : System.Address;
       Data   : System.Address;
       Notify : Glib.G_Destroy_Notify_Address);
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

   procedure Internal_Gdk_Event_Func
      (Event : Gdk_Event;
       Data  : System.Address);
   pragma Convention (C, Internal_Gdk_Event_Func);
   --  "event": the Gdk.Event.Gdk_Event to process.
   --  "data": user data set when the event handler was installed with
   --  Gdk.Event.Handler_Set.

   -----------------------------
   -- Internal_Gdk_Event_Func --
   -----------------------------

   procedure Internal_Gdk_Event_Func
      (Event : Gdk_Event;
       Data  : System.Address)
   is
      Func : constant Gdk_Event_Func := To_Gdk_Event_Func (Data);
   begin
      Func (Event);
   end Internal_Gdk_Event_Func;

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
          Acc_Angle : access Gdouble) return Integer;
      pragma Import (C, Internal, "gdk_events_get_angle");
      Acc_Angle  : aliased Gdouble;
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Event, Event2, Acc_Angle'Access);
      Angle.all := Acc_Angle;
      return Boolean'Val (Tmp_Return);
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
          Acc_Y  : access Gdouble) return Integer;
      pragma Import (C, Internal, "gdk_events_get_center");
      Acc_X      : aliased Gdouble;
      Acc_Y      : aliased Gdouble;
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Event, Event2, Acc_X'Access, Acc_Y'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      return Boolean'Val (Tmp_Return);
   end Get_Center;

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
          Acc_Distance : access Gdouble) return Integer;
      pragma Import (C, Internal, "gdk_events_get_distance");
      Acc_Distance : aliased Gdouble;
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Event, Event2, Acc_Distance'Access);
      Distance.all := Acc_Distance;
      return Boolean'Val (Tmp_Return);
   end Get_Distance;

   -----------------
   -- Handler_Set --
   -----------------

   procedure Handler_Set
      (Func   : Gdk_Event_Func;
       Notify : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Func = null then
         C_Gdk_Event_Handler_Set (System.Null_Address, System.Null_Address, Notify);
      else
         C_Gdk_Event_Handler_Set (Internal_Gdk_Event_Func'Address, To_Address (Func), Notify);
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

      procedure Handler_Set
         (Func   : Gdk_Event_Func;
          Data   : User_Data_Type;
          Notify : Glib.G_Destroy_Notify_Address)
      is
      begin
         if Func = null then
            C_Gdk_Event_Handler_Set (System.Null_Address, System.Null_Address, Notify);
         else
            C_Gdk_Event_Handler_Set (Internal_Cb'Address, Users.Build (To_Address (Func), Data), Notify);
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

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Integer;
      pragma Import (C, Internal, "gdk_events_pending");
   begin
      return Boolean'Val (Internal);
   end Events_Pending;

   ---------------------
   -- Get_Show_Events --
   ---------------------

   function Get_Show_Events return Boolean is
      function Internal return Integer;
      pragma Import (C, Internal, "gdk_get_show_events");
   begin
      return Boolean'Val (Internal);
   end Get_Show_Events;

   ---------------------
   -- Set_Show_Events --
   ---------------------

   procedure Set_Show_Events (Show_Events : Boolean) is
      procedure Internal (Show_Events : Integer);
      pragma Import (C, Internal, "gdk_set_show_events");
   begin
      Internal (Boolean'Pos (Show_Events));
   end Set_Show_Events;

end Gdk.Event;
