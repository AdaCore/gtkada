with Glib; use Glib;
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


   function Get_Event_Type (Event : in Gdk_Event'Class)
                            return Types.Gdk_Event_Type;

   procedure Set_Event_Type (Event      : in out Gdk_Event'Class;
                             Event_Type : in     Types.Gdk_Event_Type);

   procedure Get_Window (Event  : in     Gdk_Event'Class;
                         Window :    out Gdk_Window'Class);

   procedure Set_Window (Event  : in out Gdk_Event'Class;
                         Window : in     Gdk.Window.Gdk_Window'Class);

   function Get_Send_Event (Event : in Gdk_Event'Class) return Boolean;

   procedure Set_Send_Event (Event      : in out Gdk_Event'Class;
                             Send_Event : in     Boolean := True);


   ---------------------
   --  Gdk_Event_Any  --
   ---------------------

   type Gdk_Event_Any is new Gdk_Event with private;

private

   type Gdk_Event is new Root_Type with null record;
   type Gdk_Event_Any is new Gdk_Event with null record;

   pragma Import (C, Events_Pending, "gdk_events_pending");

end Gdk.Event;
