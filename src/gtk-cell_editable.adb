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

with Gdk; use Gdk;
with Gtk.Cell_Editable;
with Gdk.Event;
with Gtk.Enums; use Gtk.Enums;
with Gtk; use Gtk;
with System;

package body Gtk.Cell_Editable is

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
     (Cell_Editable : access Gtk.Cell_Editable.Gtk_Cell_Editable_Record'Class;
      Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal (Cell_Editable : System.Address;
                          Event         : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Event);
   end Start_Editing;

   ------------------
   -- Editing_Done --
   ------------------

   procedure Editing_Done
     (Cell_Editable : access Gtk.Cell_Editable.Gtk_Cell_Editable_Record'Class)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_editing_done");
   begin
      Internal (Get_Object (Cell_Editable));
   end Editing_Done;

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
     (Cell_Editable : access Gtk.Cell_Editable.Gtk_Cell_Editable_Record'Class)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_remove_widget");
   begin
      Internal (Get_Object (Cell_Editable));
   end Remove_Widget;

end Gtk.Cell_Editable;
