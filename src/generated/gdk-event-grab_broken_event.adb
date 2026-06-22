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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Gdk.Surface;
with Glib.Object; use Glib.Object;

package body Gdk.Event.Grab_Broken_Event is

   ----------------------
   -- Get_Grab_Surface --
   ----------------------

   function Get_Grab_Surface
      (Self : Gdk.Event.Gdk_Event) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_grab_broken_event_get_grab_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Grab_Surface;

   ------------------
   -- Get_Implicit --
   ------------------

   function Get_Implicit (Self : Gdk.Event.Gdk_Event) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_grab_broken_event_get_implicit");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Implicit;

end Gdk.Event.Grab_Broken_Event;
