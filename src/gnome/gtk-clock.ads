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

with Gtk;
with Gtk.Label;

package Gtk.Clock is

   type Gtk_Clock_Record is new Gtk.Label.Gtk_Label_Record with private;
   type Gtk_Clock is access all Gtk_Clock_Record'Class;

   type Gtk_Clock_Type is (Increasing, Decreasing, Realtime);

   type Time_T is new Long_Integer;

   procedure Gtk_New (Widget : out Gtk_Clock; The_Type : Gtk_Clock_Type);

   procedure Initialize (Widget   : access Gtk_Clock_Record'Class;
                         The_Type : Gtk_Clock_Type);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Format (Gclock : access Gtk_Clock_Record; Fmt : String);

   procedure Set_Seconds (Gclock : access Gtk_Clock_Record; Seconds : Time_T);

   procedure Set_Update_Interval
     (Gclock  : access Gtk_Clock_Record;
      Seconds : Gint);

   procedure Start (Gclock : access Gtk_Clock_Record);

   procedure Stop (Gclock : access Gtk_Clock_Record);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Clock_Record is new Gtk.Label.Gtk_Label_Record with null record;

   pragma Import (C, Get_Type, "gtk_clock_get_type");
end Gtk.Clock;
