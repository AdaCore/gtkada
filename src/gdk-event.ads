with Glib; use Glib;
with Gdk.Window; use Gdk.Window;

package Gdk.Event is

   type Gdk_Event is new Root_Type with private;


   function Events_Pending return Gint;
   --  mapping: Events_Pending gdk.h gdk_events_pending

   procedure Get (Event : out Gdk_Event'Class);
   --  mapping: Get gdk.h gdk_event_get

   procedure Get_Graphics_Expose (Event : out Gdk_Event'Class;
                                  Window : in Gdk_Window'Class);
   --  mapping: Get_Graphics_Expose gdk.h gdk_event_get_graphics_expose

   procedure Put (Event : in Gdk_Event'Class);
   --  mapping: Put gdk.h gdk_event_put

   procedure Copy (Source : in Gdk_Event;
                   Destination : out Gdk_Event);
   --  mapping: Copy gdk.h gdk_event_copy

   procedure Free (Event : in out Gdk_Event);
   --  mapping: Free gdk.h gdk_event_free

   procedure Set_Show_Events (Show_Events : in Boolean := True);
   --  mapping: Set_Show_Events gdk.h gdk_set_show_events

   function Get_Show_Events return Boolean;
   --  mapping: Get_Show_Events gdk.h gdk_get_show_events

private

   type Gdk_Event is new Root_Type with null record;


   pragma Import (C, Events_Pending, "gdk_events_pending");

end Gdk.Event;
