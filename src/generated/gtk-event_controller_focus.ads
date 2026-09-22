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

--  Tracks keyboard focus.
--
--  The event controller offers [signalGtk.EventControllerFocus::enter] and
--  [signalGtk.EventControllerFocus::leave] signals, as well as
--  [propertyGtk.EventControllerFocus:is-focus] and
--  [propertyGtk.EventControllerFocus:contains-focus] properties which are
--  updated to reflect focus changes inside the widget hierarchy that is rooted
--  at the controllers widget.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Gtk.Event_Controller; use Gtk.Event_Controller;

package Gtk.Event_Controller_Focus is

   type Gtk_Event_Controller_Focus_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Event_Controller_Focus is access all Gtk_Event_Controller_Focus_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Event_Controller_Focus);
   procedure Initialize
      (Self : not null access Gtk_Event_Controller_Focus_Record'Class);
   --  Creates a new event controller that will handle focus events.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Event_Controller_Focus_New return Gtk_Event_Controller_Focus;
   --  Creates a new event controller that will handle focus events.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_controller_focus_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Focus
      (Self : not null access Gtk_Event_Controller_Focus_Record)
       return Boolean;
   --  Returns True if focus is within Self or one of its children.
   --  @return True if focus is within Self or one of its children

   function Is_Focus
      (Self : not null access Gtk_Event_Controller_Focus_Record)
       return Boolean;
   --  Returns True if focus is within Self, but not one of its children.
   --  @return True if focus is within Self, but not one of its children

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Contains_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  True if focus is contained in the controllers widget.
   --
   --  See [propertyGtk.EventControllerFocus:is-focus] for whether the focus
   --  is in the widget itself or inside a descendent.
   --
   --  When handling focus events, this property is updated before
   --  [signalGtk.EventControllerFocus::enter] or
   --  [signalGtk.EventControllerFocus::leave] are emitted.

   Is_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  True if focus is in the controllers widget itself, as opposed to in a
   --  descendent widget.
   --
   --  See also [propertyGtk.EventControllerFocus:contains-focus].
   --
   --  When handling focus events, this property is updated before
   --  [signalGtk.EventControllerFocus::enter] or
   --  [signalGtk.EventControllerFocus::leave] are emitted.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Event_Controller_Focus_Void is not null access procedure
     (Self : access Gtk_Event_Controller_Focus_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Enter : constant Glib.Signal_Name := "enter";
   procedure On_Enter
      (Self  : not null access Gtk_Event_Controller_Focus_Record;
       Call  : Cb_Gtk_Event_Controller_Focus_Void;
       After : Boolean := False);
   procedure On_Enter
      (Self  : not null access Gtk_Event_Controller_Focus_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the focus enters into the widget or one of its
   --  descendents.
   --
   --  Note that this means you may not get an ::enter signal even though the
   --  widget becomes the focus location, in certain cases (such as when the
   --  focus moves from a descendent of the widget to the widget itself). If
   --  you are interested in these cases, you can monitor the
   --  [propertyGtk.EventControllerFocus:is-focus] property for changes.

   Signal_Leave : constant Glib.Signal_Name := "leave";
   procedure On_Leave
      (Self  : not null access Gtk_Event_Controller_Focus_Record;
       Call  : Cb_Gtk_Event_Controller_Focus_Void;
       After : Boolean := False);
   procedure On_Leave
      (Self  : not null access Gtk_Event_Controller_Focus_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the focus leaves the widget hierarchy that is rooted
   --  at the widget that the controller is attached to.
   --
   --  Note that this means you may not get a ::leave signal even though the
   --  focus moves away from the widget, in certain cases (such as when the
   --  focus moves from the widget to a descendent). If you are interested in
   --  these cases, you can monitor the
   --  [propertyGtk.EventControllerFocus:is-focus] property for changes.

private
   Is_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-focus");
   Contains_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("contains-focus");
end Gtk.Event_Controller_Focus;
