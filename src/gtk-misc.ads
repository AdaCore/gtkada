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
--  This widget is a base class for all the widgets that require an
--  alignment and padding.
--  This widget can not be instantiated directly.
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Widget;

package Gtk.Misc is

   type Gtk_Misc_Record is new Widget.Gtk_Widget_Record with private;
   type Gtk_Misc is access all Gtk_Misc_Record'Class;

   procedure Set_Alignment (Misc   : access Gtk_Misc_Record;
                            Xalign : in     Gfloat;
                            Yalign : in     Gfloat);
   --  Modify the alignment for the widget.
   --  Xalign and Yalign are both values between 0.0 and 1.0 that specify the
   --  alignment: if Xalign is 0.0, the widget will be left aligned; if it is
   --  0.5, the widget will be centered; if it is 1.0 the widget will be
   --  right aligned. Yalign is from top (0.0) to bottom (1.0).
   --  Both Xalign and Yalign will be constrained to the range 0.0 .. 1.0
   --  Note that if the widget fills its allocated area, these two parameters
   --  won't have any effect.

   procedure Set_Padding (Misc : access Gtk_Misc_Record;
                          Xpad : in     Gint;
                          Ypad : in     Gint);
   --  Set the padding (i.e. the extra spaces on the side of the widget).
   --  If Xpad or Ypad is negative, they will be changed to 0.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Misc.

   ----------------------
   -- Support for Gate --
   ----------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Misc_Record is new Widget.Gtk_Widget_Record with null record;
   pragma Import (C, Get_Type, "gtk_misc_get_type");
end Gtk.Misc;
