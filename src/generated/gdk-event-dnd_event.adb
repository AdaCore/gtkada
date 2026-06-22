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
with Glib.Object; use Glib.Object;

package body Gdk.Event.DND_Event is

   --------------
   -- Get_Drop --
   --------------

   function Get_Drop (Self : Gdk.Event.Gdk_Event) return Gdk.Drop.Gdk_Drop is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_dnd_event_get_drop");
      Stub_Gdk_Drop : Gdk.Drop.Gdk_Drop_Record;
   begin
      return Gdk.Drop.Gdk_Drop (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Drop));
   end Get_Drop;

end Gdk.Event.DND_Event;
