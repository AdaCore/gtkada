-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------


with Gtk.GRange;
with Gtk.Adjustment;

package Gtk.Scrollbar is

   type Gtk_Scrollbar is new Gtk.GRange.Gtk_Range with private;

   procedure Gtk_New_Hscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Gtk_New_Vscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_Scrollbar is new Gtk.GRange.Gtk_Range with null record;

   --  mapping: NOT_IMPLEMENTED gtkscrollbar.h gtk_scrollbar_get_type
   --  mapping: Gtk_New_Hscrollbar gtkhscrollbar.h gtk_hscrollbar_new
   --  mapping: Gtk_New_Vscrollbar gtkvscrollbar.h gtk_vscrollbar_new
end Gtk.Scrollbar;
