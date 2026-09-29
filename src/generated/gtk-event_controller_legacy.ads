------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Provides raw access to the event stream.
--
--  It should only be used as a last resort if none of the other event
--  controllers or gestures do the job.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;            use Gdk.Event;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Gtk.Event_Controller; use Gtk.Event_Controller;

package Gtk.Event_Controller_Legacy is

   type Gtk_Event_Controller_Legacy_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Event_Controller_Legacy is access all Gtk_Event_Controller_Legacy_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Event_Controller_Legacy);
   procedure Initialize
      (Self : not null access Gtk_Event_Controller_Legacy_Record'Class);
   --  Creates a new legacy event controller.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Event_Controller_Legacy_New return Gtk_Event_Controller_Legacy;
   --  Creates a new legacy event controller.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_controller_legacy_get_type");

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Event_Controller_Legacy_Gdk_Event_Boolean is not null access function
     (Self  : access Gtk_Event_Controller_Legacy_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;

   type Cb_GObject_Gdk_Event_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;

   Signal_Event : constant Glib.Signal_Name := "event";
   procedure On_Event
      (Self  : not null access Gtk_Event_Controller_Legacy_Record;
       Call  : Cb_Gtk_Event_Controller_Legacy_Gdk_Event_Boolean;
       After : Boolean := False);
   procedure On_Event
      (Self  : not null access Gtk_Event_Controller_Legacy_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted for each GDK event delivered to Controller.
   -- 
   --  Callback parameters:
   --    --  @param Event the `GdkEvent` which triggered this signal

end Gtk.Event_Controller_Legacy;
