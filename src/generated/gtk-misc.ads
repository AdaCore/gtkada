------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  The Gtk.Misc.Gtk_Misc widget is an abstract widget which is not useful
--  itself, but is used to derive subclasses which have alignment and padding
--  attributes.
--
--  The horizontal and vertical padding attributes allows extra space to be
--  added around the widget.
--
--  The horizontal and vertical alignment attributes enable the widget to be
--  positioned within its allocated area. Note that if the widget is added to a
--  container in such a way that it expands automatically to fill its allocated
--  area, the alignment settings will not alter the widget's position.
--
--  Note that the desired effect can in most cases be achieved by using the
--  Gtk.Widget.Gtk_Widget:halign, Gtk.Widget.Gtk_Widget:valign and
--  Gtk.Widget.Gtk_Widget:margin properties on the child widget, so GtkMisc
--  should not be used in new code. To reflect this fact, all Gtk.Misc.Gtk_Misc
--  API has been deprecated.
--
--  </description>
--  <group>Abstract base classes</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Misc is

   type Gtk_Misc_Record is new Gtk_Widget_Record with null record;
   type Gtk_Misc is access all Gtk_Misc_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_misc_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Alignment
      (Misc   : not null access Gtk_Misc_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat);
   pragma Obsolescent (Get_Alignment);
   --  Gets the X and Y alignment of the widget within its allocation. See
   --  Gtk.Misc.Set_Alignment.
   --  Deprecated since 3.14, 1
   --  "xalign": location to store X alignment of Misc, or null
   --  "yalign": location to store Y alignment of Misc, or null

   procedure Set_Alignment
      (Misc   : not null access Gtk_Misc_Record;
       Xalign : Gfloat;
       Yalign : Gfloat);
   pragma Obsolescent (Set_Alignment);
   --  Modify the alignment for the widget. Xalign and Yalign are both values
   --  between 0.0 and 1.0 that specify the alignment: if Xalign is 0.0, the
   --  widget will be left aligned; if it is 0.5, the widget will be centered;
   --  if it is 1.0 the widget will be right aligned. Yalign is from top (0.0)
   --  to bottom (1.0). Both Xalign and Yalign will be constrained to the range
   --  0.0 .. 1.0 Note that if the widget fills its allocated area, these two
   --  parameters won't have any effect.
   --  Deprecated since 3.14, 1
   --  "xalign": the horizontal alignment, from 0 (left) to 1 (right).
   --  "yalign": the vertical alignment, from 0 (top) to 1 (bottom).

   procedure Get_Padding
      (Misc : not null access Gtk_Misc_Record;
       Xpad : out Glib.Gint;
       Ypad : out Glib.Gint);
   pragma Obsolescent (Get_Padding);
   --  Gets the padding in the X and Y directions of the widget. See
   --  Gtk.Misc.Set_Padding.
   --  Deprecated since 3.14, 1
   --  "xpad": location to store padding in the X direction, or null
   --  "ypad": location to store padding in the Y direction, or null

   procedure Set_Padding
      (Misc : not null access Gtk_Misc_Record;
       Xpad : Glib.Gint;
       Ypad : Glib.Gint);
   pragma Obsolescent (Set_Padding);
   --  Sets the amount of space to add around the widget.
   --  Deprecated since 3.14, 1
   --  "xpad": the amount of space to add on the left and right of the widget,
   --  in pixels.
   --  "ypad": the amount of space to add on the top and bottom of the widget,
   --  in pixels.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  The horizontal alignment. A value of 0.0 means left alignment (or right
   --  on RTL locales); a value of 1.0 means right alignment (or left on RTL
   --  locales).

   Xpad_Property : constant Glib.Properties.Property_Int;
   --  The amount of space to add on the left and right of the widget, in
   --  pixels.

   Yalign_Property : constant Glib.Properties.Property_Float;
   --  The vertical alignment. A value of 0.0 means top alignment; a value of
   --  1.0 means bottom alignment.

   Ypad_Property : constant Glib.Properties.Property_Int;
   --  The amount of space to add on the top and bottom of the widget, in
   --  pixels.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Misc_Record, Gtk_Misc);
   function "+"
     (Widget : access Gtk_Misc_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Misc
   renames Implements_Gtk_Buildable.To_Object;

private
   Ypad_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("ypad");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xpad_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("xpad");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
end Gtk.Misc;
