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

with Glib; use Glib;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;

package Gdk.Event is


   -----------------
   --  Gdk_Event  --
   -----------------

   type Gdk_Event is new Root_Type with private;

   --
   --  Should probably be abstract but it would oblige us to
   --  define some of the following services as abstract.

   function Events_Pending return Boolean;

   procedure Get (Event : out Gdk_Event);

   procedure Peek (Event : out Gdk_Event);

   procedure Put (Event : in Gdk_Event);

   procedure Copy (Source : in Gdk_Event;
                   Destination : out Gdk_Event);

   procedure Free (Event : in out Gdk_Event);

   function Get_Time (Event  : in Gdk.Event.Gdk_Event) return Guint32;

   procedure Set_Show_Events (Show_Events : in Boolean := True);

   function Get_Show_Events return Boolean;


   function Get_Event_Type (Event : in Gdk_Event)
                            return Types.Gdk_Event_Type;

   procedure Set_Event_Type (Event      : in out Gdk_Event;
                             Event_Type : in     Types.Gdk_Event_Type);

   function Get_Window (Event  : in     Gdk_Event)
                        return Gdk.Window.Gdk_Window;

   procedure Set_Window (Event  : in out Gdk_Event;
                         Window : in     Gdk.Window.Gdk_Window'Class);

   function Get_Send_Event (Event : in Gdk_Event) return Boolean;

   procedure Set_Send_Event (Event      : in out Gdk_Event;
                             Send_Event : in     Boolean := True);


   procedure Send_Client_Message_To_All (Event : in Gdk_Event);

   function Send_Client_Message (Event : in Gdk_Event;
                                 Xid   : in Guint32)
                                 return Boolean;

   ---------------------
   --  Gdk_Event_Any  --
   ---------------------

   type Gdk_Event_Any is new Gdk_Event with private;


   ------------------------
   --  Gdk_Event_Button  --
   ------------------------

   type Gdk_Event_Button is new Gdk_Event with private;

   function Get_Time (Event : in Gdk_Event_Button) return Guint32;

   function Get_X (Event : in Gdk_Event_Button) return Gdouble;

   function Get_Y (Event : in Gdk_Event_Button) return Gdouble;

   function Get_Pressure (Event : in Gdk_Event_Button) return Gdouble;

   function Get_Xtilt (Event : in Gdk_Event_Button) return Gdouble;

   function Get_Ytilt (Event : in Gdk_Event_Button) return Gdouble;

   function Get_State (Event : in Gdk_Event_Button)
                       return Gdk.Types.Gdk_Modifier_Type;

   function Get_Button (Event : in Gdk_Event_Button) return Guint;

   function Get_Source (Event : in Gdk_Event_Button)
                        return Gdk.Types.Gdk_Input_Source;

   function Get_Device_Id (Event : in Gdk_Event_Button)
                           return Gdk.Types.Gdk_Device_Id;

   function Get_X_Root (Event : in Gdk_Event_Button) return Gdouble;

   function Get_Y_Root (Event : in Gdk_Event_Button) return Gdouble;


   ------------------------
   --  Gdk_Event_Client  --
   ------------------------

   type Gdk_Event_Client_Data_Format is (Char_Array,
                                         Short_Array,
                                         Long_Array);
   for Gdk_Event_Client_Data_Format use (Char_Array  => 8,
                                         Short_Array => 16,
                                         Long_Array  => 32);
   --  Values extracted from the XClientMessageEvent man page.

   Number_Of_Characters : constant := 20;
   Number_Of_Shorts     : constant := 10;
   Number_Of_Longs      : constant := 5;

   type Gdk_Event_Client_Data (Format : Gdk_Event_Client_Data_Format) is
      record
         case Format is
            when Char_Array =>
               B : String (1 .. Number_Of_Characters);
            when Short_Array =>
               S : Gshort_Array (1 .. Number_Of_Shorts);
            when Long_Array =>
               L : Glong_Array (1 .. Number_Of_Longs);
         end case;
      end record;

   type Gdk_Event_Client is new Gdk_Event with private;

   function Get_Message_Type (Event : in Gdk_Event_Client)
                              return Gdk.Types.Gdk_Atom;

   function Get_Data (Event : in Gdk_Event_Client)
                      return Gdk_Event_Client_Data;

   ---------------------------
   --  Gdk_Event_Configure  --
   ---------------------------

   type Gdk_Event_Configure is new Gdk_Event with private;

   function Get_X (Event : in Gdk_Event_Configure) return Gint16;

   procedure Set_X (Event : in out Gdk_Event_Configure;
                    X     : in     Gint16);

   function Get_Y (Event : in Gdk_Event_Configure) return Gint16;

   procedure Set_Y (Event : in out Gdk_Event_Configure;
                    Y     : in     Gint16);

   function Get_Width (Event : in Gdk_Event_Configure) return Gint16;

   procedure Set_Width (Event : in out Gdk_Event_Configure;
                        Width : in     Gint16);

   function Get_Height (Event : in Gdk_Event_Configure) return Gint16;

   procedure Set_Height (Event  : in out Gdk_Event_Configure;
                         Height : in     Gint16);


   --------------------------
   --  Gdk_Event_Crossing  --
   --------------------------

   type Gdk_Event_Crossing is new Gdk_Event with private;

   function Get_Subwindow (Event : in Gdk_Event_Crossing)
                           return Gdk.Window.Gdk_Window;

   function Get_Time (Event : in Gdk_Event_Crossing) return Guint32;

   function Get_X (Event : in Gdk_Event_Crossing) return Gdouble;

   function Get_Y (Event : in Gdk_Event_Crossing) return Gdouble;

   function Get_X_Root (Event : in Gdk_Event_Crossing) return Gdouble;

   function Get_Y_Root (Event : in Gdk_Event_Crossing) return Gdouble;

   function Get_Mode (Event : in Gdk_Event_Crossing)
                      return Gdk.Types.Gdk_Crossing_Mode;

   function Get_Detail (Event : in Gdk_Event_Crossing)
                        return Gdk.Types.Gdk_Notify_Type;

   function Get_Focus (Event : in Gdk_Event_Crossing) return Boolean;

   function Get_State (Event : in Gdk_Event_Crossing)
                       return Gdk.Types.Gdk_Modifier_Type;


   ------------------------
   --  Gdk_Event_Expose  --
   ------------------------

   type Gdk_Event_Expose is new Gdk_Event with private;

   procedure Get_Graphics_Expose
     (Event  : out Gdk_Event_Expose;
      Window : in Gdk.Window.Gdk_Window'Class);

   function Get_Area (Event : in     Gdk_Event_Expose)
                      return Rectangle.Gdk_Rectangle;

   procedure Set_Area (Event : in out Gdk_Event_Expose;
                       Area  : in     Rectangle.Gdk_Rectangle);

   function Get_Count (Event : in Gdk_Event_Expose) return Gint;

   procedure Set_Count (Event : in out Gdk_Event_Expose;
                        Count : in     Gint);


   -----------------------
   --  Gdk_Event_Focus  --
   -----------------------

   type Gdk_Event_Focus is new Gdk_Event with private;

   function Get_In (Event : in Gdk_Event_Focus) return Boolean;


   ---------------------
   --  Gdk_Event_Key  --
   ---------------------

   type Gdk_Event_Key is new Gdk_Event with private;

   function Get_Time (Event : in Gdk_Event_Key) return Guint32;

   function Get_State (Event : in Gdk_Event_Key)
                       return Gdk.Types.Gdk_Modifier_Type;

   function Get_Key_Val (Event : in Gdk_Event_Key)
                         return Gdk.Types.Gdk_Key_Type;

   function Get_String  (Event : in Gdk_Event_Key) return String;


   ------------------------
   --  Gdk_Event_Motion  --
   ------------------------

   type Gdk_Event_Motion is new Gdk_Event with private;

   function Get_Time (Event : in Gdk_Event_Motion) return Guint32;

   function Get_X (Event : in Gdk_Event_Motion) return Gdouble;

   function Get_Y (Event : in Gdk_Event_Motion) return Gdouble;

   function Get_Pressure (Event : in Gdk_Event_Motion) return Gdouble;

   function Get_Xtilt (Event : in Gdk_Event_Motion) return Gdouble;

   function Get_Ytilt (Event : in Gdk_Event_Motion) return Gdouble;

   function Get_State (Event : in Gdk_Event_Motion)
                       return Gdk.Types.Gdk_Modifier_Type;

   function Get_Is_Hint (Event : in Gdk_Event_Motion) return Boolean;

   function Get_Source (Event : in Gdk_Event_Motion)
                        return Gdk.Types.Gdk_Input_Source;

   function Get_Device_Id (Event : in Gdk_Event_Motion)
                           return Gdk.Types.Gdk_Device_Id;

   function Get_X_Root (Event : in Gdk_Event_Motion) return Gdouble;

   function Get_Y_Root (Event : in Gdk_Event_Motion) return Gdouble;


   ---------------------------
   --  Gdk_Event_No_Expose  --
   ---------------------------

   type Gdk_Event_No_Expose is new Gdk_Event with private;


   --------------------------
   --  Gdk_Event_Property  --
   --------------------------

   type Gdk_Event_Property is new Gdk_Event with private;

   function Get_Atom (Event : in Gdk_Event_Property) return Gdk.Types.Gdk_Atom;

   function Get_Time (Event : in Gdk_Event_Property) return Guint32;

   function Get_State (Event : in Gdk_Event_Property)
                       return Gdk.Types.Gdk_Modifier_Type;


   ---------------------------
   --  Gdk_Event_Proximity  --
   ---------------------------

   type Gdk_Event_Proximity is new Gdk_Event with private;

   function Get_Time (Event : in Gdk_Event_Proximity) return Guint32;

   function Get_Source (Event : in Gdk_Event_Proximity)
                        return Gdk.Types.Gdk_Input_Source;

   function Get_Device_Id (Event : in Gdk_Event_Proximity)
                           return Gdk.Types.Gdk_Device_Id;


   ---------------------------
   --  Gdk_Event_Selection  --
   ---------------------------

   type Gdk_Event_Selection is new Gdk_Event with private;

   function Get_Selection (Event : in Gdk_Event_Selection)
                           return Gdk.Types.Gdk_Atom;

   function Get_Target (Event : in Gdk_Event_Selection)
                        return Gdk.Types.Gdk_Atom;

   function Get_Property (Event : in Gdk_Event_Selection)
                          return Gdk.Types.Gdk_Atom;

   function Get_Requestor (Event : in Gdk_Event_Selection) return Guint32;

   function Get_Time (Event : in Gdk_Event_Selection) return Guint32;


   ----------------------------
   --  Gdk_Event_Visibility  --
   ----------------------------

   type Gdk_Event_Visibility is new Gdk_Event with private;

   function Get_Visibility_State (Event : in Gdk_Event_Visibility)
     return Gdk.Types.Gdk_Visibility_State;


private

   type Gdk_Event is new Root_Type with null record;
   type Gdk_Event_Any is new Gdk_Event with null record;
   type Gdk_Event_Button is new Gdk_Event with null record;
   type Gdk_Event_Client is new Gdk_Event with null record;
   type Gdk_Event_Configure is new Gdk_Event with null record;
   type Gdk_Event_Crossing is new Gdk_Event with null record;
   type Gdk_Event_Expose is new Gdk_Event with null record;
   type Gdk_Event_Focus is new Gdk_Event with null record;
   type Gdk_Event_Key is new Gdk_Event with null record;
   type Gdk_Event_Motion is new Gdk_Event with null record;
   type Gdk_Event_No_Expose is new Gdk_Event with null record;
   type Gdk_Event_Property is new Gdk_Event with null record;
   type Gdk_Event_Proximity is new Gdk_Event with null record;
   type Gdk_Event_Selection is new Gdk_Event with null record;
   type Gdk_Event_Visibility is new Gdk_Event with null record;

end Gdk.Event;
