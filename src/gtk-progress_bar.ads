-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Progress;

package Gtk.Progress_Bar is

   type Gtk_Progress_Bar_Record is new Gtk.Progress.Gtk_Progress_Record
     with private;
   type Gtk_Progress_Bar is access all Gtk_Progress_Bar_Record'Class;

   procedure Gtk_New
     (Progress_Bar : out Gtk_Progress_Bar;
      Adjustment   : in Gtk.Adjustment.Gtk_Adjustment :=
        Gtk.Adjustment.Null_Adjustment);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Progress_Bar.

   procedure Initialize
     (Progress_Bar : access Gtk_Progress_Bar_Record'Class;
      Adjustment   : in Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Activity_Blocks
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Blocks       : in Guint);

   procedure Set_Activity_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Step         : in Guint);

   procedure Set_Bar_Style
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Style        : in Gtk_Progress_Bar_Style);

   procedure Set_Discrete_Blocks
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Blocks       : in Guint);

   procedure Set_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Orientation  : in Gtk_Progress_Bar_Orientation);

   procedure Update
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Percentage   : in Gfloat);

private
   type Gtk_Progress_Bar_Record is new Gtk.Progress.Gtk_Progress_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_progress_bar_get_type");
end Gtk.Progress_Bar;
