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

with System;
with Gdk; use Gdk;

package body Gtk.Arrow is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Arrow;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type)
   is
      function Internal
         (Arrow_Type  : in Gint;
          Shadow_Type : in Gint)
          return           System.Address;
      pragma Import (C, Internal, "gtk_arrow_new");
   begin
      Set_Object (Widget, Internal (Gtk_Arrow_Type'Pos (Arrow_Type),
                                    Gtk_Shadow_Type'Pos (Shadow_Type)));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Arrow       : in Gtk_Arrow;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Arrow       : in System.Address;
          Arrow_Type  : in Gint;
          Shadow_Type : in Gint);
      pragma Import (C, Internal, "gtk_arrow_set");
   begin
      Internal (Get_Object (Arrow),
                Gtk_Arrow_Type'Pos (Arrow_Type),
                Gtk_Shadow_Type'Pos (Shadow_Type));
   end Set;

end Gtk.Arrow;
