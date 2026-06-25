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

package body Gdk.Event.Key_Event is

   -----------------
   -- Get_Keycode --
   -----------------

   function Get_Keycode (Self : Gdk.Event.Gdk_Event) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_key_event_get_keycode");
   begin
      return Internal (Get_Object (Self));
   end Get_Keycode;

   ----------------
   -- Get_Keyval --
   ----------------

   function Get_Keyval (Self : Gdk.Event.Gdk_Event) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_key_event_get_keyval");
   begin
      return Internal (Get_Object (Self));
   end Get_Keyval;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (Self : Gdk.Event.Gdk_Event) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_key_event_get_layout");
   begin
      return Internal (Get_Object (Self));
   end Get_Layout;

   ---------------
   -- Get_Level --
   ---------------

   function Get_Level (Self : Gdk.Event.Gdk_Event) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_key_event_get_level");
   begin
      return Internal (Get_Object (Self));
   end Get_Level;

   -----------------
   -- Is_Modifier --
   -----------------

   function Is_Modifier (Self : Gdk.Event.Gdk_Event) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_key_event_is_modifier");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Modifier;

end Gdk.Event.Key_Event;
