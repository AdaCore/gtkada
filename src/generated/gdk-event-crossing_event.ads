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

--  An event caused by a pointing device moving between surfaces.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gdk.Event.Crossing_Event is

   type Gdk_Crossing_Event is new Gdk_Event with null record;

   type Gdk_Notify_Type is (
      Ancestor,
      Virtual,
      Inferior,
      Nonlinear,
      Nonlinear_Virtual,
      Unknown);
   pragma Convention (C, Gdk_Notify_Type);
   --  Specifies the kind of crossing for enter and leave events.
   --
   --  See the X11 protocol specification of LeaveNotify for full details of
   --  crossing event generation.

   type Gdk_Crossing_Mode is (
      Normal,
      Grab,
      Ungrab,
      Gtk_Grab,
      Gtk_Ungrab,
      State_Changed,
      Touch_Begin,
      Touch_End,
      Device_Switch);
   pragma Convention (C, Gdk_Crossing_Mode);
   --  Specifies the crossing mode for enter and leave events.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Notify_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Notify_Type);
   type Property_Gdk_Notify_Type is new Gdk_Notify_Type_Properties.Property;

   package Gdk_Crossing_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Crossing_Mode);
   type Property_Gdk_Crossing_Mode is new Gdk_Crossing_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_crossing_event_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Detail (Self : Gdk.Event.Gdk_Event) return Gdk_Notify_Type;
   --  Extracts the notify detail from a crossing event.
   --  @return the notify detail of Event

   function Get_Focus (Self : Gdk.Event.Gdk_Event) return Boolean;
   --  Checks if the Event surface is the focus surface.
   --  @return True if the surface is the focus surface

   function Get_Mode (Self : Gdk.Event.Gdk_Event) return Gdk_Crossing_Mode;
   --  Extracts the crossing mode from a crossing event.
   --  @return the mode of Event

end Gdk.Event.Crossing_Event;
