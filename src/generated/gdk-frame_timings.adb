------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

package body Gdk.Frame_Timings is

   function From_Object_Free
     (B : access Gdk_Frame_Timings'Class) return Gdk_Frame_Timings
   is
      Result : constant Gdk_Frame_Timings := Gdk_Frame_Timings (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdk_Frame_Timings is
      S : Gdk_Frame_Timings;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ------------------
   -- Get_Complete --
   ------------------

   function Get_Complete (Self : Gdk_Frame_Timings) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_frame_timings_get_complete");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Complete;

   -----------------------
   -- Get_Frame_Counter --
   -----------------------

   function Get_Frame_Counter (Self : Gdk_Frame_Timings) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_timings_get_frame_counter");
   begin
      return Internal (Get_Object (Self));
   end Get_Frame_Counter;

   --------------------
   -- Get_Frame_Time --
   --------------------

   function Get_Frame_Time (Self : Gdk_Frame_Timings) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_timings_get_frame_time");
   begin
      return Internal (Get_Object (Self));
   end Get_Frame_Time;

   -------------------------------------
   -- Get_Predicted_Presentation_Time --
   -------------------------------------

   function Get_Predicted_Presentation_Time
      (Self : Gdk_Frame_Timings) return Gint64
   is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_timings_get_predicted_presentation_time");
   begin
      return Internal (Get_Object (Self));
   end Get_Predicted_Presentation_Time;

   ---------------------------
   -- Get_Presentation_Time --
   ---------------------------

   function Get_Presentation_Time (Self : Gdk_Frame_Timings) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_timings_get_presentation_time");
   begin
      return Internal (Get_Object (Self));
   end Get_Presentation_Time;

   --------------------------
   -- Get_Refresh_Interval --
   --------------------------

   function Get_Refresh_Interval (Self : Gdk_Frame_Timings) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "gdk_frame_timings_get_refresh_interval");
   begin
      return Internal (Get_Object (Self));
   end Get_Refresh_Interval;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdk_Frame_Timings) return Gdk_Frame_Timings is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_frame_timings_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdk_Frame_Timings) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_frame_timings_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gdk.Frame_Timings;
