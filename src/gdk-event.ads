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
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
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

   procedure Get (Event : out Gdk_Event);

   procedure Put (Event : in Gdk_Event);

   procedure Copy (Source : in Gdk_Event;
                   Destination : out Gdk_Event);

   procedure Free (Event : in out Gdk_Event);

   procedure Set_Show_Events (Show_Events : in Boolean := True);

   function Get_Show_Events return Boolean;


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

   procedure Get_Graphics_Expose (Event  : out Gdk_Event_Expose;
                                  Window : in Gdk_Window'Class);

   function Get_Area (Event : in     Gdk_Event_Expose)
                      return Rectangle.Gdk_Rectangle;

   procedure Set_Area (Event : in out Gdk_Event_Expose;
                       Area  : in     Rectangle.Gdk_Rectangle);

   function Get_Count (Event : in Gdk_Event_Expose) return Gint;

   procedure Set_Count (Event : in out Gdk_Event_Expose;
                        Count : in     Gint);


   ------------------------
   --  Gdk_Event_Button  --
   ------------------------

   type Gdk_Event_Button is new Gdk_Event with private;

   function Get_X (Event : in Gdk_Event_Button) return Gint16;

   function Get_Y (Event : in Gdk_Event_Button) return Gint16;

   function Get_State (Event : in Gdk_Event_Button) return Gdk_Modifier_Type;
   function Get_Button (Event : in Gdk_Event_Button) return Guint32;


   ------------------------
   --  Gdk_Event_Motion  --
   ------------------------

   type Gdk_Event_Motion is new Gdk_Event with private;

   function Get_X (Event : in Gdk_Event_Motion) return Gint16;

   function Get_Y (Event : in Gdk_Event_Motion) return Gint16;

   function Get_State (Event : in Gdk_Event_Motion) return Gdk_Modifier_Type;

   ----------------
   -- Gtk_Widget --
   ----------------

   procedure Event (Widget : Gtk.Widget.Gtk_Widget'Class;
                    Event  : Gdk_Event);


private

   type Gdk_Event is new Root_Type with null record;
   type Gdk_Event_Any is new Gdk_Event with null record;
   type Gdk_Event_Configure is new Gdk_Event with null record;
   type Gdk_Event_Expose is new Gdk_Event with null record;
   type Gdk_Event_Button is new Gdk_Event with null record;
   type Gdk_Event_Motion is new Gdk_Event with null record;

   pragma Import (C, Events_Pending, "gdk_events_pending");

end Gdk.Event;
