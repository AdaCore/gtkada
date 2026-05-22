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
with Gdk.Display;

package body Gtk.Root is

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display (Self : Gtk_Root) return Gdk.Gdk_Display is
      function Internal (Self : Gtk_Root) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Self), Stub_Gdk_Display));
   end Get_Display;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus (Self : Gtk_Root) return Gtk.Widget.Gtk_Widget is
      function Internal (Self : Gtk_Root) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_focus");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Self), Stub_Gtk_Widget));
   end Get_Focus;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
      (Self  : Gtk_Root;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : Gtk_Root; Focus : System.Address);
      pragma Import (C, Internal, "gtk_root_set_focus");
   begin
      Internal (Self, Get_Object_Or_Null (GObject (Focus)));
   end Set_Focus;

   function "+" (W : Gtk_Root) return Gtk_Root is
   begin
      return W;
   end "+";

end Gtk.Root;
