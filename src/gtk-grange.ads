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

--  <c_version>1.3.3</c_version>

with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.GRange is

   type Gtk_Range_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Range is access all Gtk_Range_Record'Class;
   subtype Gtk_GRange is Gtk_Range;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Range.

   function Get_Adjustment
     (The_Range : access Gtk_Range_Record)
      return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Update_Policy
     (The_Range : access Gtk_Range_Record;
      Policy    : Gtk_Update_Type);

   procedure Set_Adjustment
     (The_Range  : access Gtk_Range_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Inverted
     (The_Range : access Gtk_Range_Record;
      Setting   : Boolean := True);

   function Get_Inverted (The_Range : access Gtk_Range_Record) return Boolean;

private
   type Gtk_Range_Record is new Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gtk_range_get_type");
end Gtk.GRange;
