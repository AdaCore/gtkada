-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

package body Gdk.Event is


   ------------
   --  Copy  --
   ------------

   procedure Copy (Source : in Gdk_Event'Class;
                   Destination : out Gdk_Event'Class) is
      function Internal (Source : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_copy");
   begin
      Set_Object (Destination, Internal (Get_Object (Source)));
   end Copy;

   -----------
   -- Event --
   -----------

   procedure Event (Widget : Gtk.Widget.Gtk_Widget'Class;
                    Event  : Gdk_Event'Class)
   is
      procedure Internal (Widget : System.Address; Event : System.Address);
      pragma Import (C, Internal, "gtk_widget_event");
   begin
      Internal (Get_Object (Widget), Get_Object (Event));
   end Event;
 
   ------------
   --  Free  --
   ------------

   procedure Free (Event : in out Gdk_Event'Class) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_free");
   begin
      Internal (Get_Object (Event));
      Set_Object (Event, System.Null_Address);
   end Free;


   -----------
   --  Get  --
   -----------

   procedure Get (Event : out Gdk_Event'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_event_get");
   begin
      Set_Object (Event, Internal);
   end Get;


   ----------------
   --  Get_Area  --
   ----------------

   procedure Get_Area (Event : in     Gdk_Event_Expose;
                       Area  :    out Rectangle.Gdk_Rectangle'Class) is
      function Internal (Event : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gdk_event_expose_get_area");
   begin
      Set_Object (Area, Internal (Get_Object (Event)));
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


   ---------------------------
   --  Get_Graphics_Expose  --
   ---------------------------

   procedure Get_Graphics_Expose (Event  : out Gdk_Event_Expose'Class;
                                  Window : in Gdk_Window'Class)
   is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_event_get_graphics_expose");
   begin
      Set_Object (Event, Internal (Get_Object (Window)));
   end Get_Graphics_Expose;

   ------------------
   --  Get_Height  --
   ------------------

   function Get_Height (Event : in Gdk_Event_Configure) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_configure_get_height");
   begin
      return Internal (Get_Object (Event));
   end Get_Height;


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


   -----------------
   --  Get_Width  --
   -----------------

   function Get_Width (Event : in Gdk_Event_Configure) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_configure_get_width");
   begin
      return Internal (Get_Object (Event));
   end Get_Width;


   ------------------
   --  Get_Window  --
   ------------------

   procedure Get_Window (Event  : in     Gdk_Event;
                         Window :    out Gdk_Window'Class) is
      function Internal (Event : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gdk_event_any_get_window");
   begin
      Set_Object (Window, Internal (Get_Object (Event)));
   end Get_Window;


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

   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Button) return Gdk_Modifier_Mask is
      function Internal (Event : in System.Address) return Gdk_Modifier_Mask;
      pragma Import (C, Internal, "ada_gdk_event_button_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;

   ------------------
   --  Get_Button  --
   ------------------

   function Get_Button (Event : in Gdk_Event_Button)
                        return Guint32 is
      function Internal (Event : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_gdk_event_button_get_button");
   begin
      return Internal (Get_Object (Event));
   end Get_Button;
 
   -----------
   --  Put  --
   -----------

   procedure Put (Event : in Gdk_Event'Class) is
      procedure Internal (Event : in System.Address);
      pragma Import (C, Internal, "gdk_event_put");
   begin
      Internal (Get_Object (Event));
   end Put;


   ----------------
   --  Set_Area  --
   ----------------

   procedure Set_Area (Event : in out Gdk_Event_Expose;
                       Area  : in     Rectangle.Gdk_Rectangle'Class) is
      procedure Internal (Event, Area : in System.Address);
      pragma Import (C, Internal, "ada_gdk_event_expose_set_area");
   begin
      Internal (Get_Object (Event), Get_Object (Area));
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

   -----------------
   --  Get_State  --
   -----------------

   function Get_State (Event : in Gdk_Event_Motion) return Gdk_Modifier_Mask is
      function Internal (Event : in System.Address) return Gdk_Modifier_Mask;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_state");
   begin
      return Internal (Get_Object (Event));
   end Get_State;

   -------------
   --  Get_X  --
   -------------

   function Get_X (Event : in Gdk_Event_Button) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_button_get_x");
   begin
      return Internal (Get_Object (Event));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Event : in Gdk_Event_Button) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_button_get_y");
   begin
      return Internal (Get_Object (Event));
   end Get_Y;

   -------------
   --  Get_X  --
   -------------

   function Get_X (Event : in Gdk_Event_Motion) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_x");
   begin
      return Internal (Get_Object (Event));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Event : in Gdk_Event_Motion) return Gint16 is
      function Internal (Event : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_event_motion_get_y");
   begin
      return Internal (Get_Object (Event));
   end Get_Y;

end Gdk.Event;
