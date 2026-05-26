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

package body Gtk.Layout_Manager is

   package Type_Conversion_Gtk_Layout_Manager is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Layout_Manager_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Layout_Manager);

   package Type_Conversion_Gtk_Layout_Child is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Layout_Child_Get_Type'Access, Gtk_Layout_Child_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Layout_Child);

   ----------------------
   -- Get_Request_Mode --
   ----------------------

   function Get_Request_Mode
      (Manager : not null access Gtk_Layout_Manager_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode
   is
      function Internal
         (Manager : System.Address) return Gtk.Enums.Gtk_Size_Request_Mode;
      pragma Import (C, Internal, "gtk_layout_manager_get_request_mode");
   begin
      return Internal (Get_Object (Manager));
   end Get_Request_Mode;

   --------------------
   -- Layout_Changed --
   --------------------

   procedure Layout_Changed
      (Manager : not null access Gtk_Layout_Manager_Record)
   is
      procedure Internal (Manager : System.Address);
      pragma Import (C, Internal, "gtk_layout_manager_layout_changed");
   begin
      Internal (Get_Object (Manager));
   end Layout_Changed;

end Gtk.Layout_Manager;
