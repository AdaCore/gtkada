-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

   type Gtk_Progress_Bar is new Gtk.Progress.Gtk_Progress with private;

   procedure Gtk_New (Widget     : out Gtk_Progress_Bar;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Gtk_New (Widget : out Gtk_Progress_Bar);
   procedure Set_Activity_Blocks
      (Pbar   : in Gtk_Progress_Bar;
       Blocks : in Guint);
   procedure Set_Activity_Step
      (Pbar : in Gtk_Progress_Bar;
       Step : in Guint);
   procedure Set_Bar_Style
      (Pbar  : in Gtk_Progress_Bar;
       Style : in Gtk_Progress_Bar_Style);
   procedure Set_Discrete_Blocks
      (Pbar   : in Gtk_Progress_Bar;
       Blocks : in Guint);
   procedure Set_Orientation
      (Pbar        : in Gtk_Progress_Bar;
       Orientation : in Gtk_Progress_Bar_Orientation);
   procedure Update
      (Pbar       : in Gtk_Progress_Bar;
       Percentage : in Gfloat);

private
   type Gtk_Progress_Bar is new Gtk.Progress.Gtk_Progress with null record;

end Gtk.Progress_Bar;
