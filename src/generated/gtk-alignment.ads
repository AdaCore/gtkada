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
--  The Gtk.Alignment.Gtk_Alignment widget controls the alignment and size of
--  its child widget. It has four settings: xscale, yscale, xalign, and yalign.
--
--  The scale settings are used to specify how much the child widget should
--  expand to fill the space allocated to the Gtk.Alignment.Gtk_Alignment. The
--  values can range from 0 (meaning the child doesn't expand at all) to 1
--  (meaning the child expands to fill all of the available space).
--
--  The align settings are used to place the child widget within the available
--  area. The values range from 0 (top or left) to 1 (bottom or right). Of
--  course, if the scale settings are both set to 1, the alignment settings
--  have no effect.
--
--  GtkAlignment has been deprecated in 3.14 and should not be used in
--  newly-written code. The desired effect can be achieved by using the
--  Gtk.Widget.Gtk_Widget:halign, Gtk.Widget.Gtk_Widget:valign and
--  Gtk.Widget.Gtk_Widget:margin properties on the child widget.
--
--  </description>
--  <screenshot>gtk-alignment</screenshot>
--  <testgtk>create_alignment.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;

package Gtk.Alignment is

   type Gtk_Alignment_Record is new Gtk_Bin_Record with null record;
   type Gtk_Alignment is access all Gtk_Alignment_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Alignment : out Gtk_Alignment;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat);
   procedure Initialize
      (Alignment : not null access Gtk_Alignment_Record'Class;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat);
   --  Creates a new Gtk.Alignment.Gtk_Alignment.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "xalign": the horizontal alignment of the child widget, from 0 (left)
   --  to 1 (right).
   --  "yalign": the vertical alignment of the child widget, from 0 (top) to 1
   --  (bottom).
   --  "xscale": the amount that the child widget expands horizontally to fill
   --  up unused space, from 0 to 1. A value of 0 indicates that the child
   --  widget should never expand. A value of 1 indicates that the child widget
   --  will expand to fill all of the space allocated for the
   --  Gtk.Alignment.Gtk_Alignment.
   --  "yscale": the amount that the child widget expands vertically to fill
   --  up unused space, from 0 to 1. The values are similar to Xscale.

   function Gtk_Alignment_New
      (Xalign : Gfloat;
       Yalign : Gfloat;
       Xscale : Gfloat;
       Yscale : Gfloat) return Gtk_Alignment;
   --  Creates a new Gtk.Alignment.Gtk_Alignment.
   --  "xalign": the horizontal alignment of the child widget, from 0 (left)
   --  to 1 (right).
   --  "yalign": the vertical alignment of the child widget, from 0 (top) to 1
   --  (bottom).
   --  "xscale": the amount that the child widget expands horizontally to fill
   --  up unused space, from 0 to 1. A value of 0 indicates that the child
   --  widget should never expand. A value of 1 indicates that the child widget
   --  will expand to fill all of the space allocated for the
   --  Gtk.Alignment.Gtk_Alignment.
   --  "yscale": the amount that the child widget expands vertically to fill
   --  up unused space, from 0 to 1. The values are similar to Xscale.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_alignment_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Padding
      (Alignment      : not null access Gtk_Alignment_Record;
       Padding_Top    : out Guint;
       Padding_Bottom : out Guint;
       Padding_Left   : out Guint;
       Padding_Right  : out Guint);
   pragma Obsolescent (Get_Padding);
   --  Gets the padding on the different sides of the widget. See
   --  gtk_alignment_set_padding ().
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1
   --  "padding_top": location to store the padding for the top of the widget,
   --  or null
   --  "padding_bottom": location to store the padding for the bottom of the
   --  widget, or null
   --  "padding_left": location to store the padding for the left of the
   --  widget, or null
   --  "padding_right": location to store the padding for the right of the
   --  widget, or null

   procedure Set_Padding
      (Alignment      : not null access Gtk_Alignment_Record;
       Padding_Top    : Guint;
       Padding_Bottom : Guint;
       Padding_Left   : Guint;
       Padding_Right  : Guint);
   pragma Obsolescent (Set_Padding);
   --  Sets the padding on the different sides of the widget. The padding adds
   --  blank space to the sides of the widget. For instance, this can be used
   --  to indent the child widget towards the right by adding padding on the
   --  left.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.14, 1
   --  "padding_top": the padding at the top of the widget
   --  "padding_bottom": the padding at the bottom of the widget
   --  "padding_left": the padding at the left of the widget
   --  "padding_right": the padding at the right of the widget.

   procedure Set
      (Alignment : not null access Gtk_Alignment_Record;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat);
   pragma Obsolescent (Set);
   --  Sets the Gtk.Alignment.Gtk_Alignment values.
   --  Deprecated since 3.14, 1
   --  "xalign": the horizontal alignment of the child widget, from 0 (left)
   --  to 1 (right).
   --  "yalign": the vertical alignment of the child widget, from 0 (top) to 1
   --  (bottom).
   --  "xscale": the amount that the child widget expands horizontally to fill
   --  up unused space, from 0 to 1. A value of 0 indicates that the child
   --  widget should never expand. A value of 1 indicates that the child widget
   --  will expand to fill all of the space allocated for the
   --  Gtk.Alignment.Gtk_Alignment.
   --  "yscale": the amount that the child widget expands vertically to fill
   --  up unused space, from 0 to 1. The values are similar to Xscale.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Bottom_Padding_Property : constant Glib.Properties.Property_Uint;
   --  The padding to insert at the bottom of the widget.

   Left_Padding_Property : constant Glib.Properties.Property_Uint;
   --  The padding to insert at the left of the widget.

   Right_Padding_Property : constant Glib.Properties.Property_Uint;
   --  The padding to insert at the right of the widget.

   Top_Padding_Property : constant Glib.Properties.Property_Uint;
   --  The padding to insert at the top of the widget.

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  Horizontal position of child in available space. A value of 0.0 will
   --  flush the child left (or right, in RTL locales); a value of 1.0 will
   --  flush the child right (or left, in RTL locales).

   Xscale_Property : constant Glib.Properties.Property_Float;
   --  If available horizontal space is bigger than needed, how much of it to
   --  use for the child. A value of 0.0 means none; a value of 1.0 means all.

   Yalign_Property : constant Glib.Properties.Property_Float;
   --  Vertical position of child in available space. A value of 0.0 will
   --  flush the child to the top; a value of 1.0 will flush the child to the
   --  bottom.

   Yscale_Property : constant Glib.Properties.Property_Float;
   --  If available vertical space is bigger than needed, how much of it to
   --  use for the child. A value of 0.0 means none; a value of 1.0 means all.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Alignment_Record, Gtk_Alignment);
   function "+"
     (Widget : access Gtk_Alignment_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Alignment
   renames Implements_Gtk_Buildable.To_Object;

private
   Yscale_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yscale");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xscale_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xscale");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Top_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("top-padding");
   Right_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("right-padding");
   Left_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("left-padding");
   Bottom_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("bottom-padding");
end Gtk.Alignment;
