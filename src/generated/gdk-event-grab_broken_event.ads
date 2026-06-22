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

--  An event related to a broken windowing system grab.

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Gdk.Event.Grab_Broken_Event is

   type Gdk_Grab_Broken_Event is new Gdk_Event with null record;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_grab_broken_event_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Grab_Surface
      (Self : Gdk.Event.Gdk_Event) return Gdk.Gdk_Surface;
   --  Extracts the grab surface from a grab broken event.
   --  @return the grab surface of Event

   function Get_Implicit (Self : Gdk.Event.Gdk_Event) return Boolean;
   --  Checks whether the grab broken event is for an implicit grab.
   --  @return True if the an implicit grab was broken

end Gdk.Event.Grab_Broken_Event;
