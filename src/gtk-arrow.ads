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


with Gtk.Enums; use Gtk.Enums;
with Gtk.Misc;

package Gtk.Arrow is

   type Gtk_Arrow is new Gtk.Misc.Gtk_Misc with private;

   procedure Gtk_New
      (Widget      : out Gtk_Arrow;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type);
   procedure Set
      (Arrow       : in Gtk_Arrow'Class;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type);

private
   type Gtk_Arrow is new Gtk.Misc.Gtk_Misc with null record;

   --  mapping: NOT_IMPLEMENTED gtkarrow.h gtk_arrow_get_type
   --  mapping: Gtk_New gtkarrow.h gtk_arrow_new
   --  mapping: Set gtkarrow.h gtk_arrow_set
end Gtk.Arrow;
