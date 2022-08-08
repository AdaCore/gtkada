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
--  The GtkBox widget arranges child widgets into a single row or column,
--  depending upon the value of its Gtk.Orientable.Gtk_Orientable:orientation
--  property. Within the other dimension, all children are allocated the same
--  size. Of course, the Gtk.Widget.Gtk_Widget:halign and
--  Gtk.Widget.Gtk_Widget:valign properties can be used on the children to
--  influence their allocation.
--
--  GtkBox uses a notion of packing. Packing refers to adding widgets with
--  reference to a particular position in a Gtk.Container.Gtk_Container. For a
--  GtkBox, there are two reference positions: the start and the end of the
--  box. For a vertical Gtk.Box.Gtk_Box, the start is defined as the top of the
--  box and the end is defined as the bottom. For a horizontal Gtk.Box.Gtk_Box
--  the start is defined as the left side and the end is defined as the right
--  side.
--
--  Use repeated calls to Gtk.Box.Pack_Start to pack widgets into a GtkBox
--  from start to end. Use Gtk.Box.Pack_End to add widgets from end to start.
--  You may intersperse these calls and add widgets from both ends of the same
--  GtkBox.
--
--  Because GtkBox is a Gtk.Container.Gtk_Container, you may also use
--  Gtk.Container.Add to insert widgets into the box, and they will be packed
--  with the default values for expand and fill child properties. Use
--  Gtk.Container.Remove to remove widgets from the GtkBox.
--
--  Use Gtk.Box.Set_Homogeneous to specify whether or not all children of the
--  GtkBox are forced to get the same amount of space.
--
--  Use Gtk.Box.Set_Spacing to determine how much space will be minimally
--  placed between all children in the GtkBox. Note that spacing is added
--  between the children, while padding added by Gtk.Box.Pack_Start or
--  Gtk.Box.Pack_End is added on either side of the widget it belongs to.
--
--  Use Gtk.Box.Reorder_Child to move a GtkBox child to a different place in
--  the box.
--
--  Use Gtk.Box.Set_Child_Packing to reset the expand, fill and padding child
--  properties. Use Gtk.Box.Query_Child_Packing to query these fields.
--
--  # CSS nodes
--
--  GtkBox uses a single CSS node with name box.
--
--  In horizontal orientation, the nodes of the children are always arranged
--  from left to right. So :first-child will always select the leftmost child,
--  regardless of text direction.
--
--  </description>
--  <description>
--  See the testgtk example in the GtkAda distribution to see concrete
--  examples on how all the parameters for the boxes work.
--
--  </description>
--  <screenshot>gtk-box</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_box.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Box is

   type Gtk_Box_Record is new Gtk_Container_Record with null record;
   type Gtk_Box is access all Gtk_Box_Record'Class;

   subtype Gtk_Hbox_Record is Gtk_Box_Record;
   subtype Gtk_Hbox is Gtk_Box;

   subtype Gtk_Vbox_Record is Gtk_Box_Record;
   subtype Gtk_Vbox is Gtk_Box;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Box         : out Gtk_Box;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Glib.Gint);
   procedure Initialize
      (Box         : not null access Gtk_Box_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Glib.Gint);
   --  Creates a new Gtk.Box.Gtk_Box.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "orientation": the box's orientation.
   --  "spacing": the number of pixels to place by default between children.

   function Gtk_Box_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Glib.Gint) return Gtk_Box;
   --  Creates a new Gtk.Box.Gtk_Box.
   --  Since: gtk+ 3.0
   --  "orientation": the box's orientation.
   --  "spacing": the number of pixels to place by default between children.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_box_get_type");
   --  Used with Glib.Object.G_New, this creates a horizontal box

   procedure Gtk_New_Hbox
      (Box         : out Gtk_Hbox;
       Homogeneous : Boolean := False;
       Spacing     : Glib.Gint := 0);
   procedure Initialize_Hbox
      (Box         : not null access Gtk_Hbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Glib.Gint := 0);
   --  Creates a new Gtk.Box.Gtk_Hbox.
   --  Initialize_Hbox does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "homogeneous": True if all children are to be given equal space
   --  allotments.
   --  "spacing": the number of pixels to place by default between children.

   function Gtk_Hbox_New
      (Homogeneous : Boolean := False;
       Spacing     : Glib.Gint := 0) return Gtk_Hbox;
   --  Creates a new Gtk.Box.Gtk_Hbox.
   --  "homogeneous": True if all children are to be given equal space
   --  allotments.
   --  "spacing": the number of pixels to place by default between children.

   function Get_Hbox_Type return Glib.GType;
   pragma Import (C, Get_Hbox_Type, "gtk_hbox_get_type");

   procedure Gtk_New_Vbox
      (Box         : out Gtk_Vbox;
       Homogeneous : Boolean := False;
       Spacing     : Glib.Gint := 0);
   procedure Initialize_Vbox
      (Box         : not null access Gtk_Vbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Glib.Gint := 0);
   --  Creates a new Gtk.Box.Gtk_Vbox.
   --  Initialize_Vbox does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  "homogeneous": True if all children are to be given equal space
   --  allotments.
   --  "spacing": the number of pixels to place by default between children.

   function Gtk_Vbox_New
      (Homogeneous : Boolean := False;
       Spacing     : Glib.Gint := 0) return Gtk_Vbox;
   --  Creates a new Gtk.Box.Gtk_Vbox.
   --  "homogeneous": True if all children are to be given equal space
   --  allotments.
   --  "spacing": the number of pixels to place by default between children.

   function Get_Vbox_Type return Glib.GType;
   pragma Import (C, Get_Vbox_Type, "gtk_vbox_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Baseline_Position
      (Box : not null access Gtk_Box_Record)
       return Gtk.Enums.Gtk_Baseline_Position;
   --  Gets the value set by Gtk.Box.Set_Baseline_Position.
   --  Since: gtk+ 3.10

   procedure Set_Baseline_Position
      (Box      : not null access Gtk_Box_Record;
       Position : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets the baseline position of a box. This affects only horizontal boxes
   --  with at least one baseline aligned child. If there is more vertical
   --  space available than requested, and the baseline is not allocated by the
   --  parent then Position is used to allocate the baseline wrt the extra
   --  space available.
   --  Since: gtk+ 3.10
   --  "position": a Gtk.Enums.Gtk_Baseline_Position

   function Get_Center_Widget
      (Box : not null access Gtk_Box_Record) return Gtk.Widget.Gtk_Widget;
   --  Retrieves the center widget of the box.
   --  Since: gtk+ 3.12

   procedure Set_Center_Widget
      (Box    : not null access Gtk_Box_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets a center widget; that is a child widget that will be centered with
   --  respect to the full width of the box, even if the children at either
   --  side take up different amounts of space.
   --  Since: gtk+ 3.12
   --  "widget": the widget to center

   function Get_Homogeneous
      (Box : not null access Gtk_Box_Record) return Boolean;
   --  Returns whether the box is homogeneous (all children are the same
   --  size). See Gtk.Box.Set_Homogeneous.

   procedure Set_Homogeneous
      (Box         : not null access Gtk_Box_Record;
       Homogeneous : Boolean);
   --  Sets the Gtk.Box.Gtk_Box:homogeneous property of Box, controlling
   --  whether or not all children of Box are given equal space in the box.
   --  "homogeneous": a boolean value, True to create equal allotments, False
   --  for variable allotments

   function Get_Spacing
      (Box : not null access Gtk_Box_Record) return Glib.Gint;
   --  Gets the value set by Gtk.Box.Set_Spacing.

   procedure Set_Spacing
      (Box     : not null access Gtk_Box_Record;
       Spacing : Glib.Gint);
   --  Sets the Gtk.Box.Gtk_Box:spacing property of Box, which is the number
   --  of pixels to place between children of Box.
   --  "spacing": the number of pixels to put between children

   procedure Pack_End
      (In_Box  : not null access Gtk_Box_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand  : Boolean := True;
       Fill    : Boolean := True;
       Padding : Guint := 0);
   --  Adds Child to Box, packed with reference to the end of Box. The Child
   --  is packed after (away from end of) any other child packed with reference
   --  to the end of Box.
   --  "child": the Gtk.Widget.Gtk_Widget to be added to Box
   --  "expand": True if the new child is to be given extra space allocated to
   --  Box. The extra space will be divided evenly between all children of Box
   --  that use this option
   --  "fill": True if space given to Child by the Expand option is actually
   --  allocated to Child, rather than just padding it. This parameter has no
   --  effect if Expand is set to False. A child is always allocated the full
   --  height of a horizontal Gtk.Box.Gtk_Box and the full width of a vertical
   --  Gtk.Box.Gtk_Box. This option affects the other dimension
   --  "padding": extra space in pixels to put between this child and its
   --  neighbors, over and above the global amount specified by
   --  Gtk.Box.Gtk_Box:spacing property. If Child is a widget at one of the
   --  reference ends of Box, then Padding pixels are also put between Child
   --  and the reference edge of Box

   procedure Pack_Start
      (In_Box  : not null access Gtk_Box_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand  : Boolean := True;
       Fill    : Boolean := True;
       Padding : Guint := 0);
   --  Adds Child to Box, packed with reference to the start of Box. The Child
   --  is packed after any other child packed with reference to the start of
   --  Box.
   --  "child": the Gtk.Widget.Gtk_Widget to be added to Box
   --  "expand": True if the new child is to be given extra space allocated to
   --  Box. The extra space will be divided evenly between all children that
   --  use this option
   --  "fill": True if space given to Child by the Expand option is actually
   --  allocated to Child, rather than just padding it. This parameter has no
   --  effect if Expand is set to False. A child is always allocated the full
   --  height of a horizontal Gtk.Box.Gtk_Box and the full width of a vertical
   --  Gtk.Box.Gtk_Box. This option affects the other dimension
   --  "padding": extra space in pixels to put between this child and its
   --  neighbors, over and above the global amount specified by
   --  Gtk.Box.Gtk_Box:spacing property. If Child is a widget at one of the
   --  reference ends of Box, then Padding pixels are also put between Child
   --  and the reference edge of Box

   procedure Query_Child_Packing
      (Box       : not null access Gtk_Box_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : out Boolean;
       Fill      : out Boolean;
       Padding   : out Guint;
       Pack_Type : out Gtk.Enums.Gtk_Pack_Type);
   --  Obtains information about how Child is packed into Box.
   --  "child": the Gtk.Widget.Gtk_Widget of the child to query
   --  "expand": pointer to return location for expand child property
   --  "fill": pointer to return location for fill child property
   --  "padding": pointer to return location for padding child property
   --  "pack_type": pointer to return location for pack-type child property

   procedure Set_Child_Packing
      (Box       : not null access Gtk_Box_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : Boolean;
       Fill      : Boolean;
       Padding   : Guint;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type);
   --  Sets the way Child is packed into Box.
   --  "child": the Gtk.Widget.Gtk_Widget of the child to set
   --  "expand": the new value of the expand child property
   --  "fill": the new value of the fill child property
   --  "padding": the new value of the padding child property
   --  "pack_type": the new value of the pack-type child property

   procedure Reorder_Child
      (Box      : not null access Gtk_Box_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint);
   --  Moves Child to a new Position in the list of Box children. The list
   --  contains widgets packed GTK_PACK_START as well as widgets packed
   --  GTK_PACK_END, in the order that these widgets were added to Box.
   --  A widget's position in the Box children list determines where the
   --  widget is packed into Box. A child widget at some position in the list
   --  will be packed just after all other widgets of the same packing type
   --  that appear earlier in the list.
   --  "child": the Gtk.Widget.Gtk_Widget to move
   --  "position": the new position for Child in the list of children of Box,
   --  starting from 0. If negative, indicates the end of the list

   function Get_Child
      (Box : not null access Gtk_Box_Record;
       Num : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the Num-th child of the box, or null if there is no such child
   --  Since: gtk+ GtkAda 1.0

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Box_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position;

   Homogeneous_Property : constant Glib.Properties.Property_Boolean;

   Spacing_Property : constant Glib.Properties.Property_Int;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Box
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Box
   renames Implements_Gtk_Orientable.To_Object;

private
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position :=
     Gtk.Enums.Build ("baseline-position");
end Gtk.Box;
