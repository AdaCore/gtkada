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

--  <description>
--
--  A Gtk_Alignment controls the size and alignment of its single child inside
--  the area allocated to the alignment widget.
--
--  The scale/size settings indicate how much the child will expand to fill
--  the container. The values should be in the range 0.0 (no expansion) to 1.0
--  (full expansion). Note that the scale only indicates the minimal size for
--  the child, it does not force an absolute size.
--
--  The alignment settings indicate where in the alignment widget the child
--  should be located. The values are in the range 0.0 (top or left) to 1.0
--  (bottom or right). These settings are irrelevant if the child is fully
--  expanded.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Object;
with Gtk.Bin;

package Gtk.Alignment is

   type Gtk_Alignment_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Alignment is access all Gtk_Alignment_Record'Class;

   procedure Gtk_New (Alignment : out Gtk_Alignment;
                      Xalign    : in Gfloat;
                      Yalign    : in Gfloat;
                      Xscale    : in Gfloat;
                      Yscale    : in Gfloat);
   --  Create a new alignment widget, with initial values for the settings.
   --  See the description of the settings above.

   procedure Initialize (Alignment : access Gtk_Alignment_Record'Class;
                         Xalign    : in Gfloat;
                         Yalign    : in Gfloat;
                         Xscale    : in Gfloat;
                         Yscale    : in Gfloat);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Alignment.

   procedure Set (Alignment : access Gtk_Alignment_Record;
                  Xalign    : in Gfloat;
                  Yalign    : in Gfloat;
                  Xscale    : in Gfloat;
                  Yscale    : in Gfloat);
   --  Modify the settings for the alignment.
   --  See the description of the settings above.

   function Get_Xalign (Alignment : access Gtk_Alignment_Record) return Gfloat;
   --  Return the X alignment value.
   --  Its value is in the range 0.0 .. 1.0, from left to right.

   function Get_Yalign (Alignment : access Gtk_Alignment_Record) return Gfloat;
   --  Return the Y alignment value.
   --  Its value is in the range 0.0 .. 1.0, from top to bottom.

   function Get_Xscale (Alignment : access Gtk_Alignment_Record) return Gfloat;
   --  Return the X expansion value, in the range 0.0 .. 1.0.
   --  0.0 means no expansion while 1.0 means full expansion.

   function Get_Yscale (Alignment : access Gtk_Alignment_Record) return Gfloat;
   --  Return the Y expansion value, in the range 0.0 .. 1.0
   --  0.0 means no expansion while 1.0 means full expansion.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type);
   --  Gate internal function

   procedure Generate (Alignment : in out Object.Gtk_Object;
                       N         : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Alignment_Record is new Gtk.Bin.Gtk_Bin_Record with null record;
   pragma Import (C, Get_Type, "gtk_alignment_get_type");
end Gtk.Alignment;
