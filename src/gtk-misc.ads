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

with Gtk.Widget;

package Gtk.Misc is

   type Gtk_Misc is new Widget.Gtk_Widget with private;

   procedure Set_Alignment (Misc   : in out Gtk_Misc'Class;
                            Xalign : in     Gfloat;
                            Yalign : in     Gfloat);
   --  mapping: Set_Alignment gtkmisc.h gtk_misc_set_alignment

   procedure Set_Padding (Misc : in out Gtk_Misc'Class;
                          Xpad : in     Gint;
                          Ypad : in     Gint);
   --  mapping: Set_Padding gtkmisc.h gtk_misc_set_padding

private

   type Gtk_Misc is new Widget.Gtk_Widget with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkmisc.h gtk_misc_get_type
end Gtk.Misc;
