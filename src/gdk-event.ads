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


--  !! Warning !! This is one of the only package that requires manual
--  memory management in some cases. If you use the function Allocate,
--  you have to use the function Free too...

with Glib; use Glib;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;

package Gdk.Event is

   ------------------------------
   --  Definition of the types --
   ------------------------------

   type Gdk_Event is new Root_Type with private;
   subtype Gdk_Event_Any is Gdk_Event;
   --  Change from GtkAda1.2.3: There is no longer a tagged type
   --  hierarchy, only one type.
   --  However there are now a few runtime tests for each of the
   --  function, to check whether a given field is available or not.
   --
   --  Fields common to all events: Window, Send_Event, Event_Type

   subtype Gdk_Event_Button is Gdk_Event;
   --  A button was pressed or release. Relevant fields:
   --  Time, X, Y, Pressure, Xtilt, Ytilt, State, Button, Source,
   --  Device_Id, X_Root, Y_Root, Window
   --  Type: Button_Press, Gdk_2Button_Press, Gdk_3Button_Press, Button_Release

   subtype Gdk_Event_Configure is Gdk_Event;
   --  The window configuration has changed: either it was remapped,
   --  resized, moved, ...
   --  Relevant fields: X, Y, Width, Height
   --  Type: Configure

   subtype Gdk_Event_Crossing is Gdk_Event;
   --  The mouse has been moved in or out of the window
   --  Relevant fields: Time, SubWindow, X, Y, X_Root, Y_Root, Mode,
   --  Detail, Focus, State
   --  Type: Enter_Notify, Leave_Notify

   subtype Gdk_Event_Expose is Gdk_Event;
   --  The window needs to be redrawn. For efficiency, gtk gives you the
   --  smallest area that you need to redraw
   --  Relevant fields: Area, Count, Graphics_Expose
   --  Type: Expose

   subtype Gdk_Event_No_Expose is Gdk_Event;
   --  ???
   --  No Relevent fields except the common ones
   --  Type: No_Expose

   subtype Gdk_Event_Focus is Gdk_Event;
   --  The focus has changed for a window.
   --  Relevant fields: in
   --  Type: Focus_Change

   subtype Gdk_Event_Motion is Gdk_Event;
   --  The mouse has moved
   --  Relevant fields: Time, X, Y, Pressure, Xtilt, Ytilt, State,
   --  Is_Hint, Source, Device_Id, X_Root, Y_Root
   --  Type: Motion_Notify

   subtype Gdk_Event_Key is Gdk_Event;
   --  A keyboard key was pressed
   --  Relevant fields: Time, State, Key_Val, String
   --  Type: Key_Press, Key_Release

   subtype Gdk_Event_Property is Gdk_Event;
   --  Some property of the window was modified. GtkAda provides a higher
   --  level interface, and you almost never need to use this event.
   --  Relevent fields: Atom, Time, Property_State
   --  Type: Property_Notify

   subtype Gdk_Event_Proximity is Gdk_Event;
   --  from gtk+: "This event type will be used pretty rarely. It only is
   --  important for XInput aware programs that are drawing their own
   --  cursor". This is only used with non standard input devices, like
   --  graphic tablets.
   --  Relevant fields: Time, Source, Device_Id
   --  Type: Proximity_In, Proximity_Out

   subtype Gdk_Event_Visibility is Gdk_Event;
   --  The visibility state of the window (partially visibly, fully visible,
   --  hidden). This event almost never need to be used, since other events
   --  are generated at the same time, like expose_events
   --  Relevant fields: Visibility_State
   --  type: Visibility_Notify

   subtype Gdk_Event_Selection is Gdk_Event;
   --  This is how X11 implements a simple cut-and-paste mechanism. However,
   --  GtkAda provides a higher level interface to the selection mechanism,
   --  so this event will almost never be used.
   --  Relevant fields: Selection, Target, Property, Requestor, Time
   --  Type: Selection_Clear, Selection_Request, Selection_Notify

   subtype Gdk_Event_Client is Gdk_Event;
   --  This is an event used to send arbitrary data from one X application
   --  to another. This event too is almost never used, and is not documented
   --  here. Please consult an X11 documentation for more information.
   --  Relevant fields: Message_Type, Data
   --  Type: Client_Event

   --  The following event types do not seem to have any associated type:
   --  Delete, Destroy, Map, Unmap, Drag_Enter, Drag_Leave, Drag_Motion,
   --  Drag_Status, Drag_Start, Drag_Finished


   -----------------------------------------
   --  Specific definition for the fields --
   -----------------------------------------

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

   -------------------------------------
   --  Access to fields of the event  --
   -------------------------------------
   --  If a field does not exist for the event you gave, an exception
   --  Invalid_Field is raised

   Invalid_Field : exception;

   function Get_Event_Type (Event : in Gdk_Event) return Types.Gdk_Event_Type;
   --  The type of the event.

   function Get_Send_Event (Event : in Gdk_Event) return Boolean;
   --  Set to true if the event was generated by the application, False
   --  if generated by the X server/Win32.

   function Get_Window (Event  : in Gdk_Event) return Gdk.Window.Gdk_Window;
   --  The window the event occured on

   function Get_Time      (Event : in Gdk_Event) return Guint32;
   --  Time when the event occured

   function Get_X         (Event : in Gdk_Event) return Gdouble;
   function Get_Y         (Event : in Gdk_Event) return Gdouble;
   --  Coordinates of the mouse when the event occured. The coordinates
   --  are relative to the parent window.

   function Get_X_Root    (Event : in Gdk_Event) return Gdouble;
   function Get_Y_Root    (Event : in Gdk_Event) return Gdouble;
   --  Coordinates of the mouse when the event occured, relative to the
   --  root window.

   function Get_Button    (Event : in Gdk_Event) return Guint;
   --  Number of the button that was pressed.

   function Get_State     (Event : in Gdk_Event)
                          return Gdk.Types.Gdk_Modifier_Type;
   --  State of the mouse buttons and keyboard keys just prior to the
   --  event

   function Get_Subwindow (Event : in Gdk_Event) return Gdk.Window.Gdk_Window;
   --  Child window for the event. For an EnterNotifyEvent, this is set to
   --  the initial window for the pointer, for an LeaveNotifyEvent this is set
   --  to the window occupied by the pointer in its last position.

   function Get_Mode (Event : in Gdk_Event) return Gdk.Types.Gdk_Crossing_Mode;
   --  Set to indicate whether the events are normal events, pseudo-motion
   --  events when a grab activates or pseudo-motion events when a grab
   --  deativates

   function Get_Detail (Event : in Gdk_Event) return Gdk.Types.Gdk_Notify_Type;
   --  Set to indicate the notify details.

   function Get_Focus (Event : in Gdk_Event) return Boolean;
   --  Set to true if the window for the event is the focus window

   function Get_Pressure  (Event : in Gdk_Event) return Gdouble;
   function Get_Xtilt     (Event : in Gdk_Event) return Gdouble;
   function Get_Ytilt     (Event : in Gdk_Event) return Gdouble;
   --  These are currently set to constants in the gtk+ code itself, so they
   --  are most probably useless... Their respective values are 0.5, 0 and 0
   --  They are only used with some special input devices, like drawing
   --  tablets,...

   function Get_Width (Event : in Gdk_Event) return Gint16;
   function Get_Height (Event : in Gdk_Event) return Gint16;
   --  Get the size and width in a configure event

   function Get_Source (Event : in Gdk_Event)
                       return Gdk.Types.Gdk_Input_Source;
   --  Set to a constant for now in the gtk+ source... Probably useless.

   function Get_Device_Id (Event : in Gdk_Event)
                          return Gdk.Types.Gdk_Device_Id;
   --  Set to a constant for now in the gtk+ source... Probably useless.
   --  Since multiple input devices can be used at the same time, like a mouse
   --  and a graphic tablet, this indicated which one generated the event.

   function Get_Area (Event : in Gdk_Event) return Rectangle.Gdk_Rectangle;
   --  The minimal area on which the event applies (For Expose_Events, this is
   --  the minimal area to redraw).

   function Get_Count (Event : in Gdk_Event) return Gint;
   --  Number of Expose_Events that are to follow this one

   function Get_In (Event : in Gdk_Event) return Boolean;
   --  True if the window has gained the focus, False otherwise

   function Get_Is_Hint (Event : in Gdk_Event) return Boolean;
   --  ???

   function Get_Key_Val (Event : in Gdk_Event) return Gdk.Types.Gdk_Key_Type;
   --  Code of the key that was pressed (and that generated the event

   function Get_String  (Event : in Gdk_Event) return String;
   --  Symbol of the key that was pressed, as a string

   function Get_Atom (Event : in Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  Indicates which property has changed
   --  ??? Atom should not be a Guint

   function Get_Property_State (Event : in Gdk_Event) return Guint;
   --  ??? The return type should be changed

   function Get_Visibility_State (Event : in Gdk_Event)
                                 return Gdk.Types.Gdk_Visibility_State;
   --  Return the new visibility state for the window

   function Get_Selection (Event : in Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  What was selected in the window...

   function Get_Target (Event : in Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  ???

   function Get_Property (Event : in Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  ???

   function Get_Requestor (Event : in Gdk_Event) return Guint32;
   --  ???

   function Get_Message_Type (Event : in Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  ???

   function Get_Data (Event : in Gdk_Event) return Gdk_Event_Client_Data;
   --  ???

   ----------------------------------------
   --  Modifying the fields of an event  --
   ----------------------------------------

   procedure Set_Window (Event : in Gdk_Event; Win : Gdk.Window.Gdk_Window);
   procedure Set_X      (Event : in Gdk_Event; X : Gdouble);
   procedure Set_Y      (Event : in Gdk_Event; Y : Gdouble);
   procedure Set_Width  (Event : in Gdk_Event; Width : Gint16);
   procedure Set_Height (Event : in Gdk_Event; Height : Gint16);
   procedure Set_Button (Event : in Gdk_Event; Button : Guint);
   procedure Set_State  (Event : in Gdk_Event;
                         State : in Gdk.Types.Gdk_Modifier_Type);
   procedure Set_Subwindow (Event  : in Gdk_Event;
                            Window : in Gdk.Window.Gdk_Window);
   procedure Set_Mode   (Event : in Gdk_Event;
                         Mode  : in Gdk.Types.Gdk_Crossing_Mode);
   procedure Set_Detail (Event  : in Gdk_Event;
                         Detail : in Gdk.Types.Gdk_Notify_Type);
   procedure Set_Focus (Event : in Gdk_Event; Has_Focus : Boolean);
   procedure Set_Area  (Event : in Gdk_Event; Area : Rectangle.Gdk_Rectangle);
   procedure Set_In    (Event : in Gdk_Event; Focus_In : Boolean);
   procedure Set_Is_Hint (Event : in Gdk_Event; Is_Hint : Boolean);
   procedure Set_Key_Val (Event : in Gdk_Event; Key : Gdk.Types.Gdk_Key_Type);
   procedure Set_Atom (Event : in Gdk_Event; Atom : Gdk.Types.Gdk_Atom);
   procedure Set_Property_State (Event : in Gdk_Event; State : Guint);
   procedure Set_Visibility_State (Event : in Gdk_Event;
                                   State : Gdk.Types.Gdk_Visibility_State);
   procedure Set_Selection (Event : in Gdk_Event;
                            Selection : Gdk.Types.Gdk_Atom);
   procedure Set_Target (Event : in Gdk_Event; Target : Gdk.Types.Gdk_Atom);
   procedure Set_Property (Event : in Gdk_Event;
                           Property : Gdk.Types.Gdk_Atom);
   procedure Set_Requestor (Event : in Gdk_Event; Requestor : Guint32);
   procedure Set_Message_Type (Event : in Gdk_Event; Typ : Gdk.Types.Gdk_Atom);

   -------------------------
   --  General functions  --
   -------------------------

   procedure Deep_Copy (From : Gdk_Event; To : out Gdk_Event);
   --  Deep copy for an event. The C structure is itself duplicated.
   --  You need to deallocated it yourself with a call to Free below.

   procedure Get_Graphics_Expose
     (Event  : out Gdk_Event_Expose;
      Window : in Gdk.Window.Gdk_Window'Class);
   --  ???

   function Events_Pending return Boolean;
   --  Is there any event pending on the queue ?

   procedure Get (Event : out Gdk_Event);
   --  Get the next event on the queue

   procedure Peek (Event : out Gdk_Event);
   --  Look at the next event on the queue, but leave if there.

   procedure Put (Event : in Gdk_Event);
   --  Add an event on the queue - Better to use Gtk.Signal.Emit_By_Name

   procedure Set_Show_Events (Show_Events : in Boolean := True);
   function Get_Show_Events return Boolean;
   --  For debug purposes, you can choose whether you want to see the events
   --  gtk+ receives.

   procedure Send_Client_Message_To_All (Event : in Gdk_Event);
   --  ???

   function Send_Client_Message (Event : in Gdk_Event;
                                 Xid   : in Guint32)
                                return Boolean;
   --  ???

   procedure Allocate (Event      : out Gdk_Event;
                       Event_Type : in Types.Gdk_Event_Type;
                       Window     : in Gdk.Window.Gdk_Window);
   --  Create an event, whose fields are uninitialized. You need to use the
   --  function Set_* above to modify them, before you can send the event with
   --  Emit_By_Name
   --  !!Note!!: The event has to be freed if you have called this function.
   --  Use the function Free.

   procedure Free (Event : in out Gdk_Event);
   --  Free the memory (and C structure) associated with an event. You need to
   --  call this function only if the event was created through Allocate, not
   --  if it was created by GtkAda itself.

private

   type Gdk_Event is new Root_Type with
      record
         User_Created : Boolean := False;  --  True if allocated by the user
      end record;

   ---------------------
   --  Design issues  --
   ---------------------
   --  See gdk-event.adb for some of the design issues behing that package.

end Gdk.Event;
