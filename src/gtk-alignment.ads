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
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------


with Gtk.Bin;

package Gtk.Alignment is

   type Gtk_Alignment is new Gtk.Bin.Gtk_Bin with private;

   function Get_Xalign (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   function Get_Xscale (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   function Get_Yalign (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   function Get_Yscale (Widget : in Gtk_Alignment'Class)
                        return      Gfloat;
   procedure Gtk_New
      (Widget : out Gtk_Alignment;
       Xalign : in Gfloat;
       Yalign : in Gfloat;
       Xscale : in Gfloat;
       Yscale : in Gfloat);
   procedure Set
      (Alignment : in Gtk_Alignment'Class;
       Xalign    : in Gfloat;
       Yalign    : in Gfloat;
       Xscale    : in Gfloat;
       Yscale    : in Gfloat);

private
   type Gtk_Alignment is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: NOT_IMPLEMENTED gtkalignment.h gtk_alignment_get_type
   --  mapping: Get_Xalign gtkalignment.h GtkAlignment->xalign
   --  mapping: Get_Xscale gtkalignment.h GtkAlignment->xscale
   --  mapping: Get_Yalign gtkalignment.h GtkAlignment->yalign
   --  mapping: Get_Yscale gtkalignment.h GtkAlignment->yscale
   --  mapping: Gtk_New gtkalignment.h gtk_alignment_new
   --  mapping: Set gtkalignment.h gtk_alignment_set
end Gtk.Alignment;
