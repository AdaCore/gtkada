------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  <screenshot>gtk-alignment</screenshot>
--  <testgtk>create_alignment.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

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
      (Alignment : access Gtk_Alignment_Record'Class;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat);
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
      (Alignment      : access Gtk_Alignment_Record;
       Padding_Top    : out Guint;
       Padding_Bottom : out Guint;
       Padding_Left   : out Guint;
       Padding_Right  : out Guint);
   procedure Set_Padding
      (Alignment      : access Gtk_Alignment_Record;
       Padding_Top    : Guint;
       Padding_Bottom : Guint;
       Padding_Left   : Guint;
       Padding_Right  : Guint);
   --  Sets the padding on the different sides of the widget. The padding adds
   --  blank space to the sides of the widget. For instance, this can be used
   --  to indent the child widget towards the right by adding padding on the
   --  left.
   --  Since: gtk+ 2.4
   --  "padding_top": the padding at the top of the widget
   --  "padding_bottom": the padding at the bottom of the widget
   --  "padding_left": the padding at the left of the widget
   --  "padding_right": the padding at the right of the widget.

   procedure Set
      (Alignment : access Gtk_Alignment_Record;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat);
   --  Sets the Gtk.Alignment.Gtk_Alignment values.
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
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Alignment_Record, Gtk_Alignment);
   function "+"
     (Widget : access Gtk_Alignment_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Alignment
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Bottom_Padding_Property
   --  Type: Guint
   --  Flags: read-write
   --  The padding to insert at the bottom of the widget.
   --
   --  Name: Left_Padding_Property
   --  Type: Guint
   --  Flags: read-write
   --  The padding to insert at the left of the widget.
   --
   --  Name: Right_Padding_Property
   --  Type: Guint
   --  Flags: read-write
   --  The padding to insert at the right of the widget.
   --
   --  Name: Top_Padding_Property
   --  Type: Guint
   --  Flags: read-write
   --  The padding to insert at the top of the widget.
   --
   --  Name: Xalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Xscale_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Yalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Yscale_Property
   --  Type: Gfloat
   --  Flags: read-write

   Bottom_Padding_Property : constant Glib.Properties.Property_Uint;
   Left_Padding_Property : constant Glib.Properties.Property_Uint;
   Right_Padding_Property : constant Glib.Properties.Property_Uint;
   Top_Padding_Property : constant Glib.Properties.Property_Uint;
   Xalign_Property : constant Glib.Properties.Property_Float;
   Xscale_Property : constant Glib.Properties.Property_Float;
   Yalign_Property : constant Glib.Properties.Property_Float;
   Yscale_Property : constant Glib.Properties.Property_Float;

private
   Bottom_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("bottom-padding");
   Left_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("left-padding");
   Right_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("right-padding");
   Top_Padding_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("top-padding");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Xscale_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xscale");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Yscale_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yscale");
end Gtk.Alignment;
