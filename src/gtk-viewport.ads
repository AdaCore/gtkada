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
with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Viewport is

   type Gtk_Viewport_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Viewport is access all Gtk_Viewport_Record'Class;

   procedure Gtk_New
     (Viewport    : out Gtk_Viewport;
      Hadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);

   procedure Initialize
     (Viewport    : access Gtk_Viewport_Record'Class;
      Hadjustment : in Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : in Gtk.Adjustment.Gtk_Adjustment);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Viewport.

   function Get_Hadjustment
     (Viewport : access Gtk_Viewport_Record)
      return Gtk.Adjustment.Gtk_Adjustment;

   function Get_Vadjustment
     (Viewport : access Gtk_Viewport_Record)
      return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
     (Viewport   : access Gtk_Viewport_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Shadow_Type
     (Viewport : access Gtk_Viewport_Record;
      The_Type : in Gtk_Shadow_Type);

   procedure Set_Vadjustment
     (Viewport   : access Gtk_Viewport_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);

   ----------------------
   -- Support for Gate --
   ----------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

private
   type Gtk_Viewport_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

   pragma Import (C, Get_Type, "gtk_viewport_get_type");
end Gtk.Viewport;
