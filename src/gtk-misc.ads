-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  <c_version>1.3.6</c_version>

with Glib.Properties;
with Gtk.Widget;

package Gtk.Misc is

   type Gtk_Misc_Record is new Widget.Gtk_Widget_Record with private;
   type Gtk_Misc is access all Gtk_Misc_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Misc.

   procedure Set_Alignment
     (Misc : access Gtk_Misc_Record; Xalign : Gfloat; Yalign : Gfloat);
   --  Modify the alignment for the widget.
   --  Xalign and Yalign are both values between 0.0 and 1.0 that specify the
   --  alignment: if Xalign is 0.0, the widget will be left aligned; if it is
   --  0.5, the widget will be centered; if it is 1.0 the widget will be
   --  right aligned. Yalign is from top (0.0) to bottom (1.0).
   --  Both Xalign and Yalign will be constrained to the range 0.0 .. 1.0
   --  Note that if the widget fills its allocated area, these two parameters
   --  won't have any effect.

   procedure Set_Padding
     (Misc : access Gtk_Misc_Record; Xpad : Gint; Ypad : Gint);
   --  Set the padding (i.e. the extra spaces on the side of the widget).
   --  If Xpad or Ypad is negative, they will be changed to 0.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Xalign_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: The horizontal alignment, from 0 (left) to 1 (right)
   --    See also: Set_Alignment
   --
   --  - Name:  Yalign_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: The vertical alignment, from 0 (left) to 1 (right)
   --    See also: Set_Alignment
   --
   --  - Name:  Xpad_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The amount of space to add on the left and right of the widget,
   --           in pixels
   --    See also: Set_Padding
   --
   --  - Name:  Ypad_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The amount of space to add on the top and bottom of the widget,
   --           in pixels
   --    See also: Set_Padding
   --
   --  </properties>

   Xalign_Property : constant Glib.Properties.Property_Float;
   Yalign_Property : constant Glib.Properties.Property_Float;
   Xpad_Property   : constant Glib.Properties.Property_Int;
   Ypad_Property   : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Misc_Record is new Widget.Gtk_Widget_Record with null record;

   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xpad_Property   : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("xpad");
   Ypad_Property   : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("ypad");

   pragma Import (C, Get_Type, "gtk_misc_get_type");
end Gtk.Misc;
