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

with System;
with Gdk; use Gdk;

package body Gtk.Progress_Bar is

   --------------------
   -- Get_Percentage --
   --------------------

   function Get_Percentage (Widget : in Gtk_Progress_Bar'Class)
                            return      Gfloat
   is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_progress_bar_get_percentage");
   begin
      return Internal (Get_Object (Widget));
   end Get_Percentage;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Progress_Bar)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_progress_bar_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------
   -- Update --
   ------------

   procedure Update
      (Pbar       : in Gtk_Progress_Bar'Class;
       Percentage : in Gfloat)
   is
      procedure Internal
         (Pbar       : in System.Address;
          Percentage : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_bar_update");
   begin
      Internal (Get_Object (Pbar),
                Percentage);
   end Update;

end Gtk.Progress_Bar;
