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

--  The base class for event controllers.
--
--  These are ancillary objects associated to widgets, which react to
--  `GdkEvents`, and possibly trigger actions as a consequence.
--
--  Event controllers are added to a widget with
--  [methodGtk.Widget.add_controller]. It is rarely necessary to explicitly
--  remove a controller with [methodGtk.Widget.remove_controller].
--
--  See the chapter on [input handling](input-handling.html) for an overview
--  of the basic concepts, such as the capture and bubble phases of event
--  propagation.

pragma Warnings (Off, "*is already use-visible*");
with Gdk;             use Gdk;
with Gdk.Enums;       use Gdk.Enums;
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

   function Get_Current_Event
      (Self : not null access Gtk_Event_Controller_Record)
       return Gdk.Event.Gdk_Event;
   --  Returns the event that is currently being handled by the controller.
   --  At other times, null is returned.
   --  @return the event that is currently handled by Controller. Has
   --  transfer-ownership='none'.

   function Get_Current_Event_Device
      (Self : not null access Gtk_Event_Controller_Record)
       return Gdk.Gdk_Device;
   --  Returns the device of the event that is currently being handled by the
   --  controller.
   --  At other times, null is returned.
   --  @return device of the event is currently handled by Controller. Has
   --  transfer-ownership='none'.

   function Get_Current_Event_State
      (Self : not null access Gtk_Event_Controller_Record)
       return Gdk.Enums.Gdk_Modifier_Type;
   --  Returns the modifier state of the event that is currently being handled
   --  by the controller.
   --  At other times, 0 is returned.
   --  @return modifier state of the event is currently handled by Controller

   function Get_Current_Event_Time
      (Self : not null access Gtk_Event_Controller_Record) return Guint32;
   --  Returns the timestamp of the event that is currently being handled by
   --  the controller.
   --  At other times, 0 is returned.
   --  @return timestamp of the event is currently handled by Controller

   function Get_Name
      (Self : not null access Gtk_Event_Controller_Record)
       return UTF8_String;
   --  Gets the name of Controller.
   --  @return The controller name

   procedure Set_Name
      (Self : not null access Gtk_Event_Controller_Record;
       Name : UTF8_String := "");
   --  Sets a name on the controller that can be used for debugging.
   --  @param Name a name for Controller

   function Get_Propagation_Limit
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Enums.Gtk_Propagation_Limit;
   --  Gets the propagation limit of the event controller.
   --  @return the propagation limit

   procedure Set_Propagation_Limit
      (Self  : not null access Gtk_Event_Controller_Record;
       Limit : Gtk.Enums.Gtk_Propagation_Limit);
   --  Sets the event propagation limit on the event controller.
   --  If the limit is set to Gtk.Enums.Limit_Same_Native, the controller
   --  won't handle events that are targeted at widgets on a different surface,
   --  such as popovers.
   --  @param Limit the propagation limit

   function Get_Propagation_Phase
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Enums.Gtk_Propagation_Phase;
   --  Gets the propagation phase at which Controller handles events.
   --  @return the propagation phase

   procedure Set_Propagation_Phase
      (Self  : not null access Gtk_Event_Controller_Record;
       Phase : Gtk.Enums.Gtk_Propagation_Phase);
   --  Sets the propagation phase at which a controller handles events.
   --  If Phase is Gtk.Enums.Phase_None, no automatic event handling will be
   --  performed, but other additional gesture maintenance will.
   --  @param Phase a propagation phase

   function Get_Widget
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the `GtkWidget` this controller relates to.
   --  @return a `GtkWidget`. Has transfer-ownership='none'.

   procedure Reset (Self : not null access Gtk_Event_Controller_Record);
   --  Resets the Controller to a clean state.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Name_Property : constant Glib.Properties.Property_String;
   --  The name for this controller, typically used for debugging purposes.

   Propagation_Limit_Property : constant Gtk.Enums.Property_Gtk_Propagation_Limit;
   --  The limit for which events this controller will handle.

   Propagation_Phase_Property : constant Gtk.Enums.Property_Gtk_Propagation_Phase;
   --  The propagation phase at which this controller will handle events.

   Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget receiving the `GdkEvents` that the controller will handle.

private
   Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("widget");
   Propagation_Phase_Property : constant Gtk.Enums.Property_Gtk_Propagation_Phase :=
     Gtk.Enums.Build ("propagation-phase");
   Propagation_Limit_Property : constant Gtk.Enums.Property_Gtk_Propagation_Limit :=
     Gtk.Enums.Build ("propagation-limit");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
end Gtk.Event_Controller;
