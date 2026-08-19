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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Never_Trigger is

   package Type_Conversion_Gtk_Never_Trigger is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Never_Trigger_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Never_Trigger);

   ---------
   -- Get --
   ---------

   function Get return Gtk_Never_Trigger is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_never_trigger_get");
      Stub_Gtk_Never_Trigger : Gtk_Never_Trigger_Record;
   begin
      return Gtk.Never_Trigger.Gtk_Never_Trigger (Get_User_Data (Internal, Stub_Gtk_Never_Trigger));
   end Get;

end Gtk.Never_Trigger;
