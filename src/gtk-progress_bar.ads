-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

--  <description>
--  The progress bar provides a convenient way of displayed a state of
--  completion for typically lenghty tasks.
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Enums; use Gtk.Enums;
with Gtk.Progress;

package Gtk.Progress_Bar is

   type Gtk_Progress_Bar_Record is new
     Gtk.Progress.Gtk_Progress_Record with private;
   type Gtk_Progress_Bar is access all Gtk_Progress_Bar_Record'Class;

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar);
   --  Create a new progress bar.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Progress_Bar.

   procedure Initialize (Progress_Bar : access Gtk_Progress_Bar_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Pulse (Progress_Bar : access Gtk_Progress_Bar_Record);
   --  Indicate that some progress is made, but you don't know how much.
   --  Causes the progress bar to enter "activity mode," where a block
   --  bounces back and forth. Each call to Pulse causes the block to move by
   --  a little bit (the amount of movement per pulse is determined by
   --  Set_Pulse_Step).

   procedure Set_Text
     (Progress_Bar : access Gtk_Progress_Bar_Record; Text : String);
   --  Causes the given Text to appear superimposed on the progress bar.
   --  Text: a UTF-8 string.

   procedure Set_Fraction
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Fraction     : Gdouble);
   --  Cause the progress bar to "fill in" the given fraction of the bar.
   --  The fraction should be between 0.0 and 1.0, inclusive.

   procedure Set_Pulse_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Step         : Gdouble);
   --  Set the fraction of total progress bar length to move the
   --  bouncing block for each call to Pulse.

   procedure Set_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Orientation  : Gtk_Progress_Bar_Orientation);
   --  Cause the progress bar to switch to a different orientation
   --  (left-to-right, right-to-left, top-to-bottom, or bottom-to-top).

   function Get_Text
     (Progress_Bar : access Gtk_Progress_Bar_Record) return String;
   --  Retrieve the text displayed superimposed on the progress bar.

   function Get_Fraction
     (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble;
   --  Return the current fraction of the task that's been completed.

   function Get_Pulse_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble;
   --  Return the pulse step set with Set_Pulse_Step.

   function Get_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record)
      return Gtk_Progress_Bar_Orientation;
   --  Return the current progress bar orientation.

private
   type Gtk_Progress_Bar_Record is new Gtk.Progress.Gtk_Progress_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_progress_bar_get_type");
end Gtk.Progress_Bar;
