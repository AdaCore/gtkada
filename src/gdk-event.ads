-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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
with Gdk.Window; use Gdk.Window;

package Gdk.Event is


   -----------------
   --  Gdk_Event  --
   -----------------

   type Gdk_Event is new Root_Type with private;
   --
   --  Should probably be abstract but it would oblige us to
   --  define some of the following services as abstract.

   function Events_Pending return Gint;
   --  mapping: Events_Pending gdk.h gdk_events_pending

   procedure Get (Event : out Gdk_Event'Class);
   --  mapping: Get gdk.h gdk_event_get

   procedure Get_Graphics_Expose (Event : out Gdk_Event'Class;
                                  Window : in Gdk_Window'Class);
   --  mapping: Get_Graphics_Expose gdk.h gdk_event_get_graphics_expose

   procedure Put (Event : in Gdk_Event'Class);
   --  mapping: Put gdk.h gdk_event_put

   procedure Copy (Source : in Gdk_Event'Class;
                   Destination : out Gdk_Event'Class);
   --  mapping: Copy gdk.h gdk_event_copy

   procedure Free (Event : in out Gdk_Event'Class);
   --  mapping: Free gdk.h gdk_event_free

   procedure Set_Show_Events (Show_Events : in Boolean := True);
   --  mapping: Set_Show_Events gdk.h gdk_set_show_events

   function Get_Show_Events return Boolean;
   --  mapping: Get_Show_Events gdk.h gdk_get_show_events


   function Get_Event_Type (Event : in Gdk_Event)
                            return Types.Gdk_Event_Type;

   procedure Set_Event_Type (Event      : in out Gdk_Event;
                             Event_Type : in     Types.Gdk_Event_Type);

   procedure Get_Window (Event  : in     Gdk_Event;
                         Window :    out Gdk_Window'Class);

   procedure Set_Window (Event  : in out Gdk_Event;
                         Window : in     Gdk.Window.Gdk_Window'Class);

   function Get_Send_Event (Event : in Gdk_Event) return Boolean;

   procedure Set_Send_Event (Event      : in out Gdk_Event;
                             Send_Event : in     Boolean := True);


   ---------------------
   --  Gdk_Event_Any  --
   ---------------------

   type Gdk_Event_Any is new Gdk_Event with private;


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


   ------------------------
   --  Gdk_Event_Expose  --
   ------------------------

   type Gdk_Event_Expose is new Gdk_Event with private;


   procedure Get_Area (Event : in     Gdk_Event_Expose;
                       Area  :    out Rectangle.Gdk_Rectangle'Class);

   procedure Set_Area (Event : in out Gdk_Event_Expose;
                       Area  : in     Rectangle.Gdk_Rectangle'Class);

   function Get_Count (Event : in Gdk_Event_Expose) return Gint;

   procedure Set_Count (Event : in out Gdk_Event_Expose;
                        Count : in     Gint);


private

   type Gdk_Event is new Root_Type with null record;
   type Gdk_Event_Any is new Gdk_Event with null record;
   type Gdk_Event_Configure is new Gdk_Event with null record;
   type Gdk_Event_Expose is new Gdk_Event with null record;

   pragma Import (C, Events_Pending, "gdk_events_pending");

end Gdk.Event;
