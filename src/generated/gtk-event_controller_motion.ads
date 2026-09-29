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

--  Tracks the pointer position.
--
--  The event controller offers [signalGtk.EventControllerMotion::enter] and
--  [signalGtk.EventControllerMotion::leave] signals, as well as
--  [propertyGtk.EventControllerMotion:is-pointer] and
--  [propertyGtk.EventControllerMotion:contains-pointer] properties which are
--  updated to reflect changes in the pointer position as it moves over the
--  widget.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Gtk.Event_Controller; use Gtk.Event_Controller;

package Gtk.Event_Controller_Motion is

   type Gtk_Event_Controller_Motion_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Event_Controller_Motion is access all Gtk_Event_Controller_Motion_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Event_Controller_Motion);
   procedure Initialize
      (Self : not null access Gtk_Event_Controller_Motion_Record'Class);
   --  Creates a new event controller that will handle motion events.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Event_Controller_Motion_New return Gtk_Event_Controller_Motion;
   --  Creates a new event controller that will handle motion events.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_controller_motion_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Pointer
      (Self : not null access Gtk_Event_Controller_Motion_Record)
       return Boolean;
   --  Returns if a pointer is within Self or one of its children.
   --  @return True if a pointer is within Self or one of its children

   function Is_Pointer
      (Self : not null access Gtk_Event_Controller_Motion_Record)
       return Boolean;
   --  Returns if a pointer is within Self, but not one of its children.
   --  @return True if a pointer is within Self but not one of its children

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Contains_Pointer_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the pointer is in the controllers widget or a descendant.
   --
   --  See also [propertyGtk.EventControllerMotion:is-pointer].
   --
   --  When handling crossing events, this property is updated before
   --  [signalGtk.EventControllerMotion::enter], but after
   --  [signalGtk.EventControllerMotion::leave] is emitted.

   Is_Pointer_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the pointer is in the controllers widget itself, as opposed to
   --  in a descendent widget.
   --
   --  See also [propertyGtk.EventControllerMotion:contains-pointer].
   --
   --  When handling crossing events, this property is updated before
   --  [signalGtk.EventControllerMotion::enter], but after
   --  [signalGtk.EventControllerMotion::leave] is emitted.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Event_Controller_Motion_Gdouble_Gdouble_Void is not null access procedure
     (Self : access Gtk_Event_Controller_Motion_Record'Class;
      X    : Gdouble;
      Y    : Gdouble);

   type Cb_GObject_Gdouble_Gdouble_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      X    : Gdouble;
      Y    : Gdouble);

   Signal_Enter : constant Glib.Signal_Name := "enter";
   procedure On_Enter
      (Self  : not null access Gtk_Event_Controller_Motion_Record;
       Call  : Cb_Gtk_Event_Controller_Motion_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Enter
      (Self  : not null access Gtk_Event_Controller_Motion_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that the pointer has entered the widget.
   -- 
   --  Callback parameters:
   --    --  @param X coordinates of pointer location
   --    --  @param Y coordinates of pointer location

   type Cb_Gtk_Event_Controller_Motion_Void is not null access procedure
     (Self : access Gtk_Event_Controller_Motion_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Leave : constant Glib.Signal_Name := "leave";
   procedure On_Leave
      (Self  : not null access Gtk_Event_Controller_Motion_Record;
       Call  : Cb_Gtk_Event_Controller_Motion_Void;
       After : Boolean := False);
   procedure On_Leave
      (Self  : not null access Gtk_Event_Controller_Motion_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Signals that the pointer has left the widget.

   Signal_Motion : constant Glib.Signal_Name := "motion";
   procedure On_Motion
      (Self  : not null access Gtk_Event_Controller_Motion_Record;
       Call  : Cb_Gtk_Event_Controller_Motion_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Motion
      (Self  : not null access Gtk_Event_Controller_Motion_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the pointer moves inside the widget.
   -- 
   --  Callback parameters:
   --    --  @param X the x coordinate
   --    --  @param Y the y coordinate

private
   Is_Pointer_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-pointer");
   Contains_Pointer_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("contains-pointer");
end Gtk.Event_Controller_Motion;
