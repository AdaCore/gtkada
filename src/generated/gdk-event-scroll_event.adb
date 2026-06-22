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

package body Gdk.Event.Scroll_Event is

   ----------------
   -- Get_Deltas --
   ----------------

   procedure Get_Deltas
      (Self    : Gdk.Event.Gdk_Event;
       Delta_X : out Gdouble;
       Delta_Y : out Gdouble)
   is
      procedure Internal
         (Self    : System.Address;
          Delta_X : out Gdouble;
          Delta_Y : out Gdouble);
      pragma Import (C, Internal, "gdk_scroll_event_get_deltas");
   begin
      Internal (Get_Object (Self), Delta_X, Delta_Y);
   end Get_Deltas;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Self : Gdk.Event.Gdk_Event) return Gdk_Scroll_Direction
   is
      function Internal (Self : System.Address) return Gdk_Scroll_Direction;
      pragma Import (C, Internal, "gdk_scroll_event_get_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Direction;

   ----------------------------
   -- Get_Relative_Direction --
   ----------------------------

   function Get_Relative_Direction
      (Self : Gdk.Event.Gdk_Event) return Gdk_Scroll_Relative_Direction
   is
      function Internal
         (Self : System.Address) return Gdk_Scroll_Relative_Direction;
      pragma Import (C, Internal, "gdk_scroll_event_get_relative_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Relative_Direction;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit (Self : Gdk.Event.Gdk_Event) return Gdk_Scroll_Unit is
      function Internal (Self : System.Address) return Gdk_Scroll_Unit;
      pragma Import (C, Internal, "gdk_scroll_event_get_unit");
   begin
      return Internal (Get_Object (Self));
   end Get_Unit;

   -------------
   -- Is_Stop --
   -------------

   function Is_Stop (Self : Gdk.Event.Gdk_Event) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_scroll_event_is_stop");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Stop;

end Gdk.Event.Scroll_Event;
