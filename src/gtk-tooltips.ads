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

with Gtk.Data;
with Gtk.Widget;

package Gtk.Tooltips is

   type Gtk_Tooltips_Record is new Gtk.Data.Gtk_Data_Record with private;
   type Gtk_Tooltips is access all Gtk_Tooltips_Record'Class;

   procedure Enable (Tooltips : access Gtk_Tooltips_Record);
   procedure Disable (Tooltips : access Gtk_Tooltips_Record);
   procedure Gtk_New (Widget : out Gtk_Tooltips);
   procedure Initialize (Widget : access Gtk_Tooltips_Record);
   procedure Set_Delay
     (Tooltips : access Gtk_Tooltips_Record;
      Duration : in Guint);
   procedure Set_Tip
     (Tooltips    : access Gtk_Tooltips_Record;
      Widget      : in Gtk.Widget.Gtk_Widget;
      Tip_Text    : in String;
      Tip_Private : in String);

private
   type Gtk_Tooltips_Record is new Gtk.Data.Gtk_Data_Record with null record;

end Gtk.Tooltips;
