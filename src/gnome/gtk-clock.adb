-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk; use Gtk;
with System;

package body Gtk.Clock is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Clock; The_Type : Gtk_Clock_Type) is
   begin
      Widget := new Gtk_Clock_Record;
      Initialize (Widget, The_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget   : access Gtk_Clock_Record'Class;
                         The_Type : Gtk_Clock_Type)
   is
      function Internal (The_Type : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_clock_new");
   begin
      Set_Object (Widget, Internal (Gtk_Clock_Type'Pos (The_Type)));
      Initialize_User_Data (Widget);
   end Initialize;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format (Gclock : access Gtk_Clock_Record; Fmt : String) is
      procedure Internal (Gclock : System.Address; Fmt : String);
      pragma Import (C, Internal, "gtk_clock_set_format");
   begin
      Internal (Get_Object (Gclock), Fmt & ASCII.NUL);
   end Set_Format;

   -----------------
   -- Set_Seconds --
   -----------------

   procedure Set_Seconds
     (Gclock  : access Gtk_Clock_Record;
      Seconds : Time_T)
   is
      procedure Internal (Gclock : System.Address; Seconds : Gint);
      pragma Import (C, Internal, "gtk_clock_set_seconds");
   begin
      Internal (Get_Object (Gclock), Time_T'Pos (Seconds));
   end Set_Seconds;

   -------------------------
   -- Set_Update_Interval --
   -------------------------

   procedure Set_Update_Interval
     (Gclock  : access Gtk_Clock_Record;
      Seconds : Gint)
   is
      procedure Internal (Gclock : System.Address; Seconds : Gint);
      pragma Import (C, Internal, "gtk_clock_set_update_interval");
   begin
      Internal (Get_Object (Gclock), Seconds);
   end Set_Update_Interval;

   -----------
   -- Start --
   -----------

   procedure Start (Gclock : access Gtk_Clock_Record) is
      procedure Internal (Gclock : System.Address);
      pragma Import (C, Internal, "gtk_clock_start");
   begin
      Internal (Get_Object (Gclock));
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Gclock : access Gtk_Clock_Record) is
      procedure Internal (Gclock : System.Address);
      pragma Import (C, Internal, "gtk_clock_stop");
   begin
      Internal (Get_Object (Gclock));
   end Stop;

end Gtk.Clock;
