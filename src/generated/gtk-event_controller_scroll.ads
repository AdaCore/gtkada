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

--  Handles scroll events.
--
--  It is capable of handling both discrete and continuous scroll events from
--  mice or touchpads, abstracting them both with the
--  [signalGtk.EventControllerScroll::scroll] signal. Deltas in the discrete
--  case are multiples of 1.
--
--  In the case of continuous scroll events, `GtkEventControllerScroll`
--  encloses all [signalGtk.EventControllerScroll::scroll] emissions between
--  two [signalGtk.EventControllerScroll::scroll-begin] and
--  [signalGtk.EventControllerScroll::scroll-end] signals.
--
--  The behavior of the event controller can be modified by the flags given at
--  creation time, or modified at a later point through
--  [methodGtk.EventControllerScroll.set_flags] (e.g. because the scrolling
--  conditions of the widget changed).
--
--  The controller can be set up to emit motion for either/both vertical and
--  horizontal scroll events through
--  Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Vertical,
--  Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Horizontal and
--  Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Both_Axes. If any axis
--  is disabled, the respective [signalGtk.EventControllerScroll::scroll] delta
--  will be 0. Vertical scroll events will be translated to horizontal motion
--  for the devices incapable of horizontal scrolling.
--
--  The event controller can also be forced to emit discrete events on all
--  devices through
--  Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Discrete. This can be
--  used to implement discrete actions triggered through scroll events (e.g.
--  switching across combobox options).
--
--  The Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Kinetic flag
--  toggles the emission of the [signalGtk.EventControllerScroll::decelerate]
--  signal, emitted at the end of scrolling with two X/Y velocity arguments
--  that are consistent with the motion that was received.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event.Scroll_Event;  use Gdk.Event.Scroll_Event;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Gtk.Event_Controller;    use Gtk.Event_Controller;

package Gtk.Event_Controller_Scroll is

   type Gtk_Event_Controller_Scroll_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Event_Controller_Scroll is access all Gtk_Event_Controller_Scroll_Record'Class;

   type Gtk_Event_Controller_Scroll_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Event_Controller_Scroll_Flags);
   --  Describes the behavior of a `GtkEventControllerScroll`.

   Event_Controller_Scroll_None : constant Gtk_Event_Controller_Scroll_Flags := 0;
   Event_Controller_Scroll_Vertical : constant Gtk_Event_Controller_Scroll_Flags := 1;
   Event_Controller_Scroll_Horizontal : constant Gtk_Event_Controller_Scroll_Flags := 2;
   Event_Controller_Scroll_Discrete : constant Gtk_Event_Controller_Scroll_Flags := 4;
   Event_Controller_Scroll_Kinetic : constant Gtk_Event_Controller_Scroll_Flags := 8;
   Event_Controller_Scroll_Physical_Direction : constant Gtk_Event_Controller_Scroll_Flags := 16;
   Event_Controller_Scroll_Both_Axes : constant Gtk_Event_Controller_Scroll_Flags := 3;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Event_Controller_Scroll_Flags_Properties is
      new Generic_Internal_Flags_Property (Gtk_Event_Controller_Scroll_Flags);
   type Property_Gtk_Event_Controller_Scroll_Flags is new Gtk_Event_Controller_Scroll_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self  : out Gtk_Event_Controller_Scroll;
       Flags : Gtk_Event_Controller_Scroll_Flags);
   procedure Initialize
      (Self  : not null access Gtk_Event_Controller_Scroll_Record'Class;
       Flags : Gtk_Event_Controller_Scroll_Flags);
   --  Creates a new event controller that will handle scroll events.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Flags flags affecting the controller behavior

   function Gtk_Event_Controller_Scroll_New
      (Flags : Gtk_Event_Controller_Scroll_Flags)
       return Gtk_Event_Controller_Scroll;
   --  Creates a new event controller that will handle scroll events.
   --  @param Flags flags affecting the controller behavior

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_controller_scroll_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Flags
      (Self : not null access Gtk_Event_Controller_Scroll_Record)
       return Gtk_Event_Controller_Scroll_Flags;
   --  Gets the flags conditioning the scroll controller behavior.
   --  @return the controller flags.

   procedure Set_Flags
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Flags : Gtk_Event_Controller_Scroll_Flags);
   --  Sets the flags conditioning scroll controller behavior.
   --  @param Flags flags affecting the controller behavior

   function Get_Unit
      (Self : not null access Gtk_Event_Controller_Scroll_Record)
       return Gdk.Event.Scroll_Event.Gdk_Scroll_Unit;
   --  Gets the scroll unit of the last
   --  [signalGtk.EventControllerScroll::scroll] signal received.
   --  Always returns Gdk.Event.Scroll_Event.Wheel if the
   --  Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Discrete flag is
   --  set.
   --  Since: gtk+ 4.8
   --  @return the scroll unit.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Flags_Property : constant Gtk.Event_Controller_Scroll.Property_Gtk_Event_Controller_Scroll_Flags;
   --  Type: Gtk_Event_Controller_Scroll_Flags
   --  The flags affecting event controller behavior.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void is not null access procedure
     (Self  : access Gtk_Event_Controller_Scroll_Record'Class;
      Vel_X : Gdouble;
      Vel_Y : Gdouble);

   type Cb_GObject_Gdouble_Gdouble_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Vel_X : Gdouble;
      Vel_Y : Gdouble);

   Signal_Decelerate : constant Glib.Signal_Name := "decelerate";
   procedure On_Decelerate
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Decelerate
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted after scroll is finished if the
   --  Gtk.Event_Controller_Scroll.Event_Controller_Scroll_Kinetic flag is set.
   --
   --  Vel_X and Vel_Y express the initial velocity that was imprinted by the
   --  scroll events. Vel_X and Vel_Y are expressed in pixels/ms.
   -- 
   --  Callback parameters:
   --    --  @param Vel_X X velocity
   --    --  @param Vel_Y Y velocity

   type Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean is not null access function
     (Self : access Gtk_Event_Controller_Scroll_Record'Class;
      Dx   : Gdouble;
      Dy   : Gdouble) return Boolean;

   type Cb_GObject_Gdouble_Gdouble_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Dx   : Gdouble;
      Dy   : Gdouble) return Boolean;

   Signal_Scroll : constant Glib.Signal_Name := "scroll";
   procedure On_Scroll
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean;
       After : Boolean := False);
   procedure On_Scroll
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that the widget should scroll by the amount specified by Dx and
   --  Dy.
   --
   --  For the representation unit of the deltas, see
   --  [methodGtk.EventControllerScroll.get_unit].
   -- 
   --  Callback parameters:
   --    --  @param Dx X delta
   --    --  @param Dy Y delta

   type Cb_Gtk_Event_Controller_Scroll_Void is not null access procedure
     (Self : access Gtk_Event_Controller_Scroll_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Scroll_Begin : constant Glib.Signal_Name := "scroll-begin";
   procedure On_Scroll_Begin
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Void;
       After : Boolean := False);
   procedure On_Scroll_Begin
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that a new scrolling operation has begun.
   --
   --  It will only be emitted on devices capable of it.

   Signal_Scroll_End : constant Glib.Signal_Name := "scroll-end";
   procedure On_Scroll_End
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Void;
       After : Boolean := False);
   procedure On_Scroll_End
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that a scrolling operation has finished.
   --
   --  It will only be emitted on devices capable of it.

private
   Flags_Property : constant Gtk.Event_Controller_Scroll.Property_Gtk_Event_Controller_Scroll_Flags :=
     Gtk.Event_Controller_Scroll.Build ("flags");
end Gtk.Event_Controller_Scroll;
