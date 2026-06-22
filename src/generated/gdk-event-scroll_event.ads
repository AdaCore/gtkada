
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

--  An event related to a scrolling motion.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gdk.Event.Scroll_Event is

   type Gdk_Scroll_Event is new Gdk_Event with null record;

   type Gdk_Scroll_Unit is (
      Wheel,
      Surface);
   pragma Convention (C, Gdk_Scroll_Unit);
   --  Specifies the unit of scroll deltas.
   --
   --  When you get Gdk.Event.Scroll_Event.Wheel, a delta of 1.0 means 1 wheel
   --  detent click in the south direction, 2.0 means 2 wheel detent clicks in
   --  the south direction... This is the same logic for negative values but in
   --  the north direction.
   --
   --  If you get Gdk.Event.Scroll_Event.Surface, are managing a scrollable
   --  view and get a value of 123, you have to scroll 123 surface logical
   --  pixels right if it's Delta_X or down if it's Delta_Y. This is the same
   --  logic for negative values but you have to scroll left instead of right
   --  if it's Delta_X and up instead of down if it's Delta_Y.
   --
   --  1 surface logical pixel is equal to 1 real screen pixel multiplied by
   --  the final scale factor of your graphical interface (the product of the
   --  desktop scale factor and eventually a custom scale factor in your app).

   type Gdk_Scroll_Relative_Direction is (
      Identical,
      Inverted,
      Unknown);
   pragma Convention (C, Gdk_Scroll_Relative_Direction);
   --  Used in scroll events, to announce the direction relative to physical
   --  motion.

   type Gdk_Scroll_Direction is (
      Up,
      Down,
      Left,
      Right,
      Smooth);
   pragma Convention (C, Gdk_Scroll_Direction);
   --  Specifies the direction for scroll events.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Scroll_Unit_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Scroll_Unit);
   type Property_Gdk_Scroll_Unit is new Gdk_Scroll_Unit_Properties.Property;

   package Gdk_Scroll_Relative_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Scroll_Relative_Direction);
   type Property_Gdk_Scroll_Relative_Direction is new Gdk_Scroll_Relative_Direction_Properties.Property;

   package Gdk_Scroll_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Scroll_Direction);
   type Property_Gdk_Scroll_Direction is new Gdk_Scroll_Direction_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_scroll_event_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Deltas
      (Self    : Gdk.Event.Gdk_Event;
       Delta_X : out Gdouble;
       Delta_Y : out Gdouble);
   --  Extracts the scroll deltas of a scroll event.
   --  The deltas will be zero unless the scroll direction is
   --  Gdk.Event.Scroll_Event.Smooth.
   --  For the representation unit of these deltas, see
   --  [methodGdk.ScrollEvent.get_unit].
   --  @param Delta_X return location for x scroll delta
   --  @param Delta_Y return location for y scroll delta

   function Get_Direction
      (Self : Gdk.Event.Gdk_Event) return Gdk_Scroll_Direction;
   --  Extracts the direction of a scroll event.
   --  @return the scroll direction of Event

   function Get_Relative_Direction
      (Self : Gdk.Event.Gdk_Event) return Gdk_Scroll_Relative_Direction;
   --  Extracts the scroll direction relative to the physical motion.
   --  Since: gtk+ 4.20
   --  @return the relative scroll direction.

   function Get_Unit (Self : Gdk.Event.Gdk_Event) return Gdk_Scroll_Unit;
   --  Extracts the scroll delta unit of a scroll event.
   --  The unit will always be Gdk.Event.Scroll_Event.Wheel if the scroll
   --  direction is not Gdk.Event.Scroll_Event.Smooth.
   --  Since: gtk+ 4.8
   --  @return the scroll unit.

   function Is_Stop (Self : Gdk.Event.Gdk_Event) return Boolean;
   --  Check whether a scroll event is a stop scroll event.
   --  Scroll sequences with smooth scroll information may provide a stop
   --  scroll event once the interaction with the device finishes, e.g. by
   --  lifting a finger. This stop scroll event is the signal that a widget may
   --  trigger kinetic scrolling based on the current velocity.
   --  Stop scroll events always have a delta of 0/0.
   --  @return True if the event is a scroll stop event

end Gdk.Event.Scroll_Event;
