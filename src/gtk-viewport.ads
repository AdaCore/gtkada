-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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
with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Viewport is

   type Gtk_Viewport is new Gtk.Bin.Gtk_Bin with private;

   function Get_Hadjustment (Viewport : in Gtk_Viewport)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class;
   function Get_Vadjustment (Viewport : in Gtk_Viewport)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class;
   procedure Gtk_New
      (Widget      : out Gtk_Viewport;
       Hadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Hadjustment
      (Viewport   : in Gtk_Viewport;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Shadow_Type
      (Viewport : in Gtk_Viewport;
       The_Type : in Gtk_Shadow_Type);
   procedure Set_Vadjustment
      (Viewport   : in Gtk_Viewport;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_Viewport is new Gtk.Bin.Gtk_Bin with null record;

end Gtk.Viewport;
