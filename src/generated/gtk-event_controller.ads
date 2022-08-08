------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Gtk.Event_Controller.Gtk_Event_Controller is a base, low-level
--  implementation for event controllers. Those react to a series of
--  Gdk_Events, and possibly trigger actions as a consequence of those.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;       use Gdk.Event;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Event_Controller is

   type Gtk_Event_Controller_Record is new GObject_Record with null record;
   type Gtk_Event_Controller is access all Gtk_Event_Controller_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_controller_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Propagation_Phase
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Enums.Gtk_Propagation_Phase;
   --  Gets the propagation phase at which Controller handles events.
   --  Since: gtk+ 3.14

   procedure Set_Propagation_Phase
      (Self  : not null access Gtk_Event_Controller_Record;
       Phase : Gtk.Enums.Gtk_Propagation_Phase);
   --  Sets the propagation phase at which a controller handles events.
   --  If Phase is Gtk.Enums.Phase_None, no automatic event handling will be
   --  performed, but other additional gesture maintenance will. In that phase,
   --  the events can be managed by calling Gtk.Event_Controller.Handle_Event.
   --  Since: gtk+ 3.14
   --  "phase": a propagation phase

   function Get_Widget
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the Gtk.Widget.Gtk_Widget this controller relates to.
   --  Since: gtk+ 3.14

   function Handle_Event
      (Self  : not null access Gtk_Event_Controller_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Feeds an events into Controller, so it can be interpreted and the
   --  controller actions triggered.
   --  Since: gtk+ 3.14
   --  "event": a Gdk.Event.Gdk_Event

   procedure Reset (Self : not null access Gtk_Event_Controller_Record);
   --  Resets the Controller to a clean state. Every interaction the
   --  controller did through
   --  Gtk.Event_Controller.Gtk_Event_Controller::handle-event will be dropped
   --  at this point.
   --  Since: gtk+ 3.14

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Propagation_Phase_Property : constant Gtk.Enums.Property_Gtk_Propagation_Phase;
   --  The propagation phase at which this controller will handle events.

   Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget receiving the Gdk_Events that the controller will handle.

private
   Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("widget");
   Propagation_Phase_Property : constant Gtk.Enums.Property_Gtk_Propagation_Phase :=
     Gtk.Enums.Build ("propagation-phase");
end Gtk.Event_Controller;
