-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package body Gdk.Event is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ----------------------------------------------------------------------
   --  Local Services declarations

   type Local_Short_Array is array (Natural range <>) of aliased IC.short;
   package Shorts_Ptr is new IC.Pointers (Index => Natural,
                                          Element => IC.short,
                                          Element_Array => Local_Short_Array,
                                          Default_Terminator => 0);

   type Local_Long_Array is array (Natural range <>) of aliased IC.long;
   package Longs_Ptr is new IC.Pointers (Index => Natural,
                                         Element => IC.long,
                                         Element_Array => Local_Long_Array,
                                         Default_Terminator => 0);

   function Get_Length  (Event : in Gdk_Event_Key) return Gint;


   ----------------------------------------------------------------------
   --  Gdk_Event

   ------------
   --  Copy  --
   ------------

   procedure Copy (Source : in Gdk_Event;
                   Destination : out Gdk_Event) is
      function Internal (Source : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_copy");
   begin
      Set_Object (Destination, Internal (Get_Object (Source)));
   end Copy;


   ----------------------
   --  Events_Pending  --
   ----------------------

   function Events_Pending return Boolean is
      function Internal return Gboolean;
      pragma Import (C, Internal, "gdk_events_pending");
   begin
      return To_Boolean (Internal);
   end Events_Pending;


   ------------
   --  Free  --
   ------------

   procedure Free (Event : in out Gdk_Event) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_free");
   begin
      Internal (Get_Object (Event));
      Set_Object (Event, System.Null_Address);
   end Free;


   -----------
   --  Get  --
   -----------

   procedure Get (Event : out Gdk_Event) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_event_get");
   begin
      Set_Object (Event, Internal);
   end Get;

   ----------------------
   --  Get_Event_Type  --
   ----------------------

   function Get_Event_Type (Event : in Gdk_Event)
                            return Types.Gdk_Event_Type is
      function Internal (Event : in System.Address)
                         return Types.Gdk_Event_Type;
      pragma Import (C, Internal, "ada_gdk_event_any_get_event_type");
   begin
      return Internal (Get_Object (Event));
   end Get_Event_Type;


   ----------------------
   --  Get_Send_Event  --
   ----------------------

   function Get_Send_Event (Event : in Gdk_Event) return Boolean is
      function Internal (Event : in System.Address) return Gint8;
      pragma Import (C, Internal, "ada_gdk_event_any_get_send_event");
   begin
      return To_Boolean (Internal (Get_Object (Event)));
   end Get_Send_Event;


   -----------------------
   --  Get_Show_Events  --
   -----------------------

   function Get_Show_Events return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_get_show_events");
   begin
      return To_Boolean (Internal);
   end Get_Show_Events;


   --------------
   -- Get_Time --
   --------------

   function Get_Time (Event  : in Gdk.Event.Gdk_Event) return Guint32 is
      function Internal (Event  : in System.Address) return Guint32;
      pragma Import (C, Internal, "gdk_event_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   ------------------
   --  Get_Window  --
   ------------------

   function Get_Window (Event  : in Gdk_Event) return Gdk.Window.Gdk_Window is
      function Internal (Event : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gdk_event_any_get_window");
      Win : Gdk.Window.Gdk_Window;
   begin
      Set_Object (Win, Internal (Get_Object (Event)));
      return Win;
   end Get_Window;


   ------------
   --  Peek  --
   ------------

   procedure Peek (Event : out Gdk_Event) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_event_peek");
   begin
      Set_Object (Event, Internal);
   end Peek;


   -----------
   --  Put  --
   -----------

   procedure Put (Event : in Gdk_Event) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_put");
   begin
      Internal (Get_Object (Event));
   end Put;


   -------------------------
   -- Send_Client_Message --
   -------------------------

   function Send_Client_Message (Event  : in Gdk_Event;
                                 Xid    : in Guint32) return Boolean is
      function Internal (Event  : in System.Address;
                         Xid    : in Guint32) return Gboolean;
      pragma Import (C, Internal, "gdk_event_send_client_message");
   begin
      return To_Boolean (Internal (Get_Object (Event), Xid));
   end Send_Client_Message;


   ----------------------------------
   --  Send_Client_Message_To_All  --
   ----------------------------------

   procedure Send_Client_Message_To_All (Event : in Gdk_Event) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_send_clientmessage_toall");
   begin
      Internal (Get_Object (Event));
   end Send_Client_Message_To_All;


   ----------------------
   --  Set_Event_Type  --
   ----------------------

   procedure Set_Event_Type (Event      : in out Gdk_Event;
                             Event_Type : in     Types.Gdk_Event_Type) is
      procedure Internal (Event : in System.Address;
                          Event_Type : in Types.Gdk_Event_Type);
      pragma Import (C, Internal, "ada_gdk_event_any_set_event_type");
   begin
      Internal (Get_Object (Event), Event_Type);
   end Set_Event_Type;


   ----------------------
   --  Set_Send_Event  --
   ----------------------

   procedure Set_Send_Event (Event      : in out Gdk_Event;
                             Send_Event : in     Boolean := True) is
      procedure Internal (Event : in System.Address;
                          Send_Event : in Gint8);
      pragma Import (C, Internal, "ada_gdk_event_any_set_send_event");
   begin
      Internal (Get_Object (Event), To_Gint (Send_Event));
   end Set_Send_Event;



   -----------------------
   --  Set_Show_Events  --
   -----------------------

   procedure Set_Show_Events (Show_Events : in Boolean := True) is
      procedure Internal (Show_Events : in Gint);
      pragma Import (C, Internal, "gdk_set_show_events");
   begin
      Internal (To_Gint (Show_Events));
   end Set_Show_Events;


   ------------------
   --  Set_Window  --
   ------------------

   procedure Set_Window (Event  : in out Gdk_Event;
                         Window : in     Gdk.Window.Gdk_Window'Class) is
      procedure Internal (Event, Window : in System.Address);
      pragma Import (C, Internal, "ada_gdk_event_any_set_window");
   begin
      Internal (Get_Object (Event), Get_Object (Window));
   end Set_Window;


   ----------------------------------------------------------------------
   --  Gdk_Event_Any

   --  No service defined for this type for the moment.


   ----------------------------------------------------------------------
   --  Gdk_Event_Button


   ------------------
   --  Get_Button  --
   ------------------

   function Get_Button (Event : in Gdk_Event_Button) return Guint is
      function Internal (Event : in System.Address) return Guint;
      pragma Import (C, Internal, "ada_gdk_event_button_get_button");
   begin
      return Internal (Get_Object (Event));
   end Get_Button;


   ---------------------
   --  Get_Device_Id  --
   ---------------------

   function Get_Device_Id (Event : in Gdk_Event_Button)
                           return Gdk.Types.Gdk_Device_Id is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Device_Id;
      pragma Import (C, Internal, "ada_gdk_event_button_get_device_id");
   begin
      return Internal (Get_Object (Event));
   end Get_Device_Id;


   --------------------
   --  Get_Pressure  --
   --------------------

   function Get_Pressure (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_pressure");
   begin
      return Internal (Get_Object (Event));
   end Get_Pressure;


   ------------------
   --  Get_Source  --
   ------------------

   function Get_Source (Event : in Gdk_Event_Button)
                        return Gdk.Types.Gdk_Input_Source is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Input_Source;
      pragma Import (C, Internal, "ada_gdk_event_button_get_source");
   begin
      return Internal (Get_Object (Event));
   end Get_Source;


   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Button)
                       return Gdk.Types.Gdk_Modifier_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "ada_gdk_event_button_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Button) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_button_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_x");
   begin
      return Internal (Get_Object (Event));
   end Get_X;


   ------------------
   --  Get_X_Root  --
   ------------------

   function Get_X_Root (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_x_root");
   begin
      return Internal (Get_Object (Event));
   end Get_X_Root;


   ------------------
   --  Get_X_Tilt  --
   ------------------

   function Get_Xtilt (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_xtilt");
   begin
      return Internal (Get_Object (Event));
   end Get_Xtilt;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_y");
   begin
      return Internal (Get_Object (Event));
   end Get_Y;


   ------------------
   --  Get_Y_Root  --
   ------------------

   function Get_Y_Root (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_y_root");
   begin
      return Internal (Get_Object (Event));
   end Get_Y_Root;


   ------------------
   --  Get_Y_Tilt  --
   ------------------

   function Get_Ytilt (Event : in Gdk_Event_Button) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_button_get_ytilt");
   begin
      return Internal (Get_Object (Event));
   end Get_Ytilt;


   ----------------------------------------------------------------------
   --  Gdk_Event_Client


   ----------------
   --  Get_Data  --
   ----------------

   function Get_Data (Event : in Gdk_Event_Client)
                      return Gdk_Event_Client_Data is

      function Format_Of (Event : in System.Address)
                          return Gdk_Event_Client_Data_Format;
      pragma Import (C, Format_Of, "ada_gdk_event_client_get_data_format");

      function B_Of (Event : in System.Address) return ICS.chars_ptr;
      pragma Import (C, B_Of, "ada_gdk_event_client_get_b");

      function S_Of (Event : in System.Address) return Shorts_Ptr.Pointer;
      pragma Import (C, S_Of, "ada_gdk_event_client_get_s");

      function L_Of (Event : in System.Address) return Longs_Ptr.Pointer;
      pragma Import (C, L_Of, "ada_gdk_event_client_get_l");

      Result : Gdk_Event_Client_Data
        (Format => Format_Of (Get_Object (Event)));

   begin

      case Result.Format is

         when Char_Array =>
            Result.B :=
              IC.To_Ada (ICS.Value (Item => B_Of (Get_Object (Event)),
                                    Length => Number_Of_Characters),
                         Trim_Nul => False);

         when Short_Array =>
            declare
               Tmp : constant Local_Short_Array :=
                 Shorts_Ptr.Value (Ref => S_Of (Get_Object (Event)),
                                   Length => Number_Of_Shorts);
            begin
               for Index in 1 .. Number_Of_Shorts loop
                  Result.S (Index) := Gshort (Tmp (Index));
               end loop;
            end;

         when Long_Array =>
            declare
               Tmp : constant Local_Long_Array :=
                 Longs_Ptr.Value (Ref => L_Of (Get_Object (Event)),
                                  Length => Number_Of_Longs);
            begin
               for Index in 1 .. Number_Of_Shorts loop
                  Result.L (Index) := Glong (Tmp (Index));
               end loop;
            end;

      end case;

      return Result;

   end Get_Data;


   ------------------------
   --  Get_Message_Type  --
   ------------------------

   function Get_Message_Type (Event : in Gdk_Event_Client)
                              return Gdk.Types.Gdk_Atom is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "ada_gdk_event_client_get_message_type");
   begin
      return Internal (Get_Object (Event));
   end Get_Message_Type;


   ----------------------------------------------------------------------
   --  Gdk_Event_Configure


   ------------------
   --  Get_Height  --
   ------------------

   function Get_Height (Event : in Gdk_Event_Configure) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_configure_get_height");
   begin
      return Internal (Get_Object (Event));
   end Get_Height;


   -----------------
   --  Get_Width  --
   -----------------

   function Get_Width (Event : in Gdk_Event_Configure) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_configure_get_width");
   begin
      return Internal (Get_Object (Event));
   end Get_Width;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Event : in Gdk_Event_Configure) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_configure_get_x");
   begin
      return Internal (Get_Object (Event));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Event : in Gdk_Event_Configure) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_configure_get_y");
   begin
      return Internal (Get_Object (Event));
   end Get_Y;


   ------------------
   --  Set_Height  --
   ------------------

   procedure Set_Height (Event : in out Gdk_Event_Configure;
                         Height : in Gint16) is
      procedure Internal (Event : in System.Address; Height : in Gint16);
      pragma Import (C, Internal, "ada_gdk_event_configure_set_height");
   begin
      Internal (Get_Object (Event), Height);
   end Set_Height;


   -----------------
   --  Set_Width  --
   -----------------

   procedure Set_Width (Event : in out Gdk_Event_Configure;
                        Width : in Gint16) is
      procedure Internal (Event : in System.Address; Width : in Gint16);
      pragma Import (C, Internal, "ada_gdk_event_configure_set_width");
   begin
      Internal (Get_Object (Event), Width);
   end Set_Width;


   -------------
   --  Set_X  --
   -------------

   procedure Set_X (Event : in out Gdk_Event_Configure; X : in Gint16) is
      procedure Internal (Event : in System.Address; X : in Gint16);
      pragma Import (C, Internal, "ada_gdk_event_configure_set_x");
   begin
      Internal (Get_Object (Event), X);
   end Set_X;


   -------------
   --  Set_Y  --
   -------------

   procedure Set_Y (Event : in out Gdk_Event_Configure; Y : in Gint16) is
      procedure Internal (Event : in System.Address; Y : in Gint16);
      pragma Import (C, Internal, "ada_gdk_event_configure_set_y");
   begin
      Internal (Get_Object (Event), Y);
   end Set_Y;


   ----------------------------------------------------------------------
   --  Gdk_Event_Crossing


   ------------------
   --  Get_Detail  --
   ------------------

   function Get_Detail (Event : in Gdk_Event_Crossing)
                        return Gdk.Types.Gdk_Notify_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Notify_Type;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_detail");
   begin
      return Internal (Get_Object (Event));
   end Get_Detail;


   -----------------
   --  Get_Focus  --
   -----------------

   function Get_Focus (Event : in Gdk_Event_Crossing) return Boolean is
      function Internal (Event : in System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_focus");
   begin
      return To_Boolean (Internal (Get_Object (Event)));
   end Get_Focus;


   ----------------
   --  Get_Mode  --
   ----------------

   function Get_Mode (Event : in Gdk_Event_Crossing)
                      return Gdk.Types.Gdk_Crossing_Mode is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Crossing_Mode;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_mode");
   begin
      return Internal (Get_Object (Event));
   end Get_Mode;


   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Crossing)
                       return Gdk.Types.Gdk_Modifier_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;


   ---------------------
   --  Get_Subwindow  --
   ---------------------

   function Get_Subwindow (Event : in Gdk_Event_Crossing)
                           return Gdk.Window.Gdk_Window is
      function Internal (Event : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_subwindow");
      Result : Gdk.Window.Gdk_Window;
   begin
      Set_Object (Result, Internal (Get_Object (Event)));
      return Result;
   end Get_Subwindow;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Crossing) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Event : in Gdk_Event_Crossing) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_x");
   begin
      return Internal (Get_Object (Event));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Event : in Gdk_Event_Crossing) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_y");
   begin
      return Internal (Get_Object (Event));
   end Get_Y;


   ------------------
   --  Get_X_Root  --
   ------------------

   function Get_X_Root (Event : in Gdk_Event_Crossing) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_x_root");
   begin
      return Internal (Get_Object (Event));
   end Get_X_Root;


   ------------------
   --  Get_Y_Root  --
   ------------------

   function Get_Y_Root (Event : in Gdk_Event_Crossing) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_crossing_get_y_root");
   begin
      return Internal (Get_Object (Event));
   end Get_Y_Root;



   ----------------------------------------------------------------------
   --  Gdk_Event_Expose


   ----------------
   --  Get_Area  --
   ----------------

   function Get_Area (Event : in     Gdk_Event_Expose)
                      return Rectangle.Gdk_Rectangle
   is
      function Internal (Event : in System.Address)
                         return Gdk.Rectangle.Gdk_Rectangle;
      pragma Import (C, Internal, "ada_gdk_event_expose_get_area");
   begin
      return Internal (Get_Object (Event));
   end Get_Area;


   -----------------
   --  Get_Count  --
   -----------------

   function Get_Count (Event : in Gdk_Event_Expose) return Gint is
      function Internal (Event : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_expose_get_count");
   begin
      return Internal (Get_Object (Event));
   end Get_Count;


   ---------------------------
   --  Get_Graphics_Expose  --
   ---------------------------

   procedure Get_Graphics_Expose (Event  : out Gdk_Event_Expose;
                                  Window : in Gdk.Window.Gdk_Window'Class)
   is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_graphics_expose");
   begin
      Set_Object (Event, Internal (Get_Object (Window)));
   end Get_Graphics_Expose;


   ----------------
   --  Set_Area  --
   ----------------

   procedure Set_Area (Event : in out Gdk_Event_Expose;
                       Area  : in     Rectangle.Gdk_Rectangle) is
      procedure Internal (Event, Area : in System.Address);
      pragma Import (C, Internal, "ada_gdk_event_expose_set_area");
   begin
      Internal (Get_Object (Event), Area'Address);
   end Set_Area;


   -----------------
   --  Set_Count  --
   -----------------

   procedure Set_Count (Event : in out Gdk_Event_Expose;
                        Count : in     Gint) is
      procedure Internal (Event : in System.Address; Count : in Gint);
      pragma Import (C, Internal, "ada_gdk_event_expose_set_count");
   begin
      Internal (Get_Object (Event), Count);
   end Set_Count;


   ----------------------------------------------------------------------
   --  Gdk_Event_Focus


   --------------
   --  Get_In  --
   --------------

   function Get_In (Event : in Gdk_Event_Focus) return Boolean is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_focus_get_in");
   begin
      return To_Boolean (Internal (Get_Object (Event)));
   end Get_In;


   ----------------------------------------------------------------------
   --  Gdk_Event_Key


   ------------------
   --  Get_Length  --
   ------------------

   function Get_Length (Event : in Gdk_Event_Key) return Gint is
      function Internal (Event : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_gdk_event_key_get_length");
   begin
      return Internal (Get_Object (Event));
   end Get_Length;


   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Key)
                       return Gdk.Types.Gdk_Modifier_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "ada_gdk_event_key_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;


   ------------------
   --  Get_String  --
   ------------------

   function Get_String (Event : in Gdk_Event_Key) return String is
      function Internal (Event : in System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_event_key_get_string");
   begin
      return IC.To_Ada (ICS.Value (Item => Internal (Get_Object (Event)),
                                   Length => IC.size_t (Get_Length (Event))),
                        Trim_Nul => False);
   end Get_String;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Key) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_key_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   -------------------
   --  Get_Key_Val  --
   -------------------

   function Get_Key_Val (Event : in Gdk_Event_Key)
                         return Gdk.Types.Gdk_Key_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Key_Type;
      pragma Import (C, Internal, "ada_gdk_event_key_get_key_val");
   begin
      return Internal (Get_Object (Event));
   end Get_Key_Val;


   ----------------------------------------------------------------------
   --  Gdk_Event_Motion


   ---------------------
   --  Get_Device_Id  --
   ---------------------

   function Get_Device_Id (Event : in Gdk_Event_Motion)
                           return Gdk.Types.Gdk_Device_Id is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Device_Id;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_deviceid");
   begin
      return Internal (Get_Object (Event));
   end Get_Device_Id;


   -------------------
   --  Get_Is_Hint  --
   -------------------

   function Get_Is_Hint (Event : in Gdk_Event_Motion) return Boolean is
      function Internal (Event : in System.Address) return Guint16;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_is_hint");
   begin
      return To_Boolean (Internal (Get_Object (Event)));
   end Get_Is_Hint;


   --------------------
   --  Get_Pressure  --
   --------------------

   function Get_Pressure (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_pressure");
   begin
      return Internal (Get_Object (Event));
   end Get_Pressure;


   ------------------
   --  Get_Source  --
   ------------------

   function Get_Source (Event : in Gdk_Event_Motion)
                        return Gdk.Types.Gdk_Input_Source is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Input_Source;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_source");
   begin
      return Internal (Get_Object (Event));
   end Get_Source;


   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Motion)
                       return Gdk.Types.Gdk_Modifier_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Motion) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_x");
   begin
      return Internal (Get_Object (Event));
   end Get_X;


   ------------------
   --  Get_X_Root  --
   ------------------

   function Get_X_Root (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_x_root");
   begin
      return Internal (Get_Object (Event));
   end Get_X_Root;


   -----------------
   --  Get_Xtilt  --
   -----------------

   function Get_Xtilt (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_xtilt");
   begin
      return Internal (Get_Object (Event));
   end Get_Xtilt;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_y");
   begin
      return Internal (Get_Object (Event));
   end Get_Y;


   ------------------
   --  Get_Y_Root  --
   ------------------

   function Get_Y_Root (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_y_root");
   begin
      return Internal (Get_Object (Event));
   end Get_Y_Root;


   -----------------
   --  Get_Ytilt  --
   -----------------

   function Get_Ytilt (Event : in Gdk_Event_Motion) return Gdouble is
      function Internal (Event : in System.Address) return Gdouble;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_ytilt");
   begin
      return Internal (Get_Object (Event));
   end Get_Ytilt;


   ----------------------------------------------------------------------
   --  Gdk_Event_No_Expose

   --  No service defined for this type for the moment.


   ----------------------------------------------------------------------
   --  Gdk_Event_Property


   ----------------
   --  Get_Atom  --
   ----------------

   function Get_Atom (Event : in Gdk_Event_Property) return Gdk.Types.Gdk_Atom
   is
      function Internal (Event : in System.Address) return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "ada_gdk_event_property_get_atom");
   begin
      return Internal (Get_Object (Event));
   end Get_Atom;


   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Property)
                       return Gdk.Types.Gdk_Modifier_Type is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "ada_gdk_event_property_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Property) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_property_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   ----------------------------------------------------------------------
   --  Gdk_Event_Proximity


   ---------------------
   --  Get_Device_Id  --
   ---------------------

   function Get_Device_Id (Event : in Gdk_Event_Proximity)
                           return Gdk.Types.Gdk_Device_Id is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Device_Id;
      pragma Import (C, Internal, "ada_gdk_event_proximity_get_deviceid");
   begin
      return Internal (Get_Object (Event));
   end Get_Device_Id;


   ------------------
   --  Get_Source  --
   ------------------

   function Get_Source (Event : in Gdk_Event_Proximity)
                        return Gdk.Types.Gdk_Input_Source is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Input_Source;
      pragma Import (C, Internal, "ada_gdk_event_proximity_get_source");
   begin
      return Internal (Get_Object (Event));
   end Get_Source;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Proximity) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_proximity_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;



   ----------------------------------------------------------------------
   --  Gdk_Event_Selection


   --------------------
   --  Get_Property  --
   --------------------

   function Get_Property (Event : in Gdk_Event_Selection)
                           return Gdk.Types.Gdk_Atom is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "ada_gdk_event_selection_get_property");
   begin
      return Internal (Get_Object (Event));
   end Get_Property;


   ---------------------
   --  Get_Requestor  --
   ---------------------

   function Get_Requestor (Event : in Gdk_Event_Selection) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_selection_get_requestor");
   begin
      return Internal (Get_Object (Event));
   end Get_Requestor;


   ---------------------
   --  Get_Selection  --
   ---------------------

   function Get_Selection (Event : in Gdk_Event_Selection)
                           return Gdk.Types.Gdk_Atom is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "ada_gdk_event_selection_get_selection");
   begin
      return Internal (Get_Object (Event));
   end Get_Selection;


   ------------------
   --  Get_Target  --
   ------------------

   function Get_Target (Event : in Gdk_Event_Selection)
                           return Gdk.Types.Gdk_Atom is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "ada_gdk_event_selection_get_target");
   begin
      return Internal (Get_Object (Event));
   end Get_Target;


   ----------------
   --  Get_Time  --
   ----------------

   function Get_Time (Event : in Gdk_Event_Selection) return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_selection_get_time");
   begin
      return Internal (Get_Object (Event));
   end Get_Time;


   ----------------------------------------------------------------------
   --  Gdk_Event_Visibility


   ----------------------------
   --  Get_Visibility_State  --
   ----------------------------

   function Get_Visibility_State (Event : in Gdk_Event_Visibility)
     return Gdk.Types.Gdk_Visibility_State
   is
      function Internal (Event : in System.Address)
                         return Gdk.Types.Gdk_Visibility_State;
      pragma Import (C, Internal,
                     "ada_gdk_event_visibility_get_visibility_state");
   begin
      return Internal (Get_Object (Event));
   end Get_Visibility_State;


end Gdk.Event;
