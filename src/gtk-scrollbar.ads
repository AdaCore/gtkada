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

with Gtk.GRange;
with Gtk.Adjustment;

package Gtk.Scrollbar is

   type Gtk_Scrollbar_Record is new Gtk.GRange.Gtk_Range_Record with private;
   type Gtk_Scrollbar is access all Gtk_Scrollbar_Record'Class;

   procedure Gtk_New_Hscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Gtk_New_Vscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Hscrollbar
     (Widget     : access Gtk_Scrollbar_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Vscrollbar
     (Widget     : access Gtk_Scrollbar_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);

private
   type Gtk_Scrollbar_Record is new Gtk.GRange.Gtk_Range_Record
     with null record;

end Gtk.Scrollbar;
