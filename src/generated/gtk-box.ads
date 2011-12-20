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
--  A box is a container that can have multiple children, organized either
--  horizontally or vertically. Two subtypes are provided, Gtk_Hbox and
--  Gtk_Vbox, to conform to the C API. In Ada, you do not need to distinguish
--  between the two, but note that the Gtk_Box type is conceptually an abstract
--  type: there is no way to create a "Gtk_Box", only ways to create either an
--  horizontal box, or a vertical box.
--
--  Children can be added to one of two positions in the box, either at the
--  beginning (ie left or top) or at the end (ie right or bottom). Each of
--  these positions can contain multiple widgets.
--
--  Every time a child is added to the start, it is placed to the right (resp.
--  the bottom) of the previous widget added to the start.
--
--  Every time a child is added to the end, it is placed to the left (resp.
--  the top) of the previous widget added to the end.
--
--  There are a number of parameters to specify the behavior of the box when
--  it is resized, and how the children should be reorganized and/or resized.
--
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
       Spacing     : Gint);
   procedure Initialize
      (Box         : access Gtk_Box_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Gint);
   --  Creates a new Gtk.Box.Gtk_Box.
   --  Since: gtk+ 3.0
   --  "orientation": the box's orientation.
   --  "spacing": the number of pixels to place by default between children.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_box_get_type");

   procedure Gtk_New_Hbox
      (Box         : out Gtk_Hbox;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0);
   procedure Initialize_Hbox
      (Box         : access Gtk_Hbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0);
   --  Creates a new Gtk.Box.Gtk_Hbox. wich is a very quick and easy change.
   --  But the recommendation is to switch to Gtk.Grid.Gtk_Grid, since
   --  Gtk.Box.Gtk_Box is going to go away eventually. See <xref
   --  linkend="gtk-migrating-GtkGrid"/>.
   --  "homogeneous": True if all children are to be given equal space
   --  allotments.
   --  "spacing": the number of pixels to place by default between children.

   function Get_Hbox_Type return Glib.GType;
   pragma Import (C, Get_Hbox_Type, "gtk_hbox_get_type");

   procedure Gtk_New_Vbox
      (Box         : out Gtk_Vbox;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0);
   procedure Initialize_Vbox
      (Box         : access Gtk_Vbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0);
   --  Creates a new Gtk.Box.Gtk_Vbox. wich is a very quick and easy change.
   --  But the recommendation is to switch to Gtk.Grid.Gtk_Grid, since
   --  Gtk.Box.Gtk_Box is going to go away eventually. See <xref
   --  linkend="gtk-migrating-GtkGrid"/>.
   --  "homogeneous": True if all children are to be given equal space
   --  allotments.
   --  "spacing": the number of pixels to place by default between children.

   function Get_Vbox_Type return Glib.GType;
   pragma Import (C, Get_Vbox_Type, "gtk_vbox_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Homogeneous (Box : access Gtk_Box_Record) return Boolean;
   procedure Set_Homogeneous
      (Box         : access Gtk_Box_Record;
       Homogeneous : Boolean);
   --  Sets the Gtk.Box.Gtk_Box:homogeneous property of Box, controlling
   --  whether or not all children of Box are given equal space in the box.
   --  "homogeneous": a boolean value, True to create equal allotments, False
   --  for variable allotments

   function Get_Spacing (Box : access Gtk_Box_Record) return Gint;
   procedure Set_Spacing (Box : access Gtk_Box_Record; Spacing : Gint);
   --  Sets the Gtk.Box.Gtk_Box:spacing property of Box, which is the number
   --  of pixels to place between children of Box.
   --  "spacing": the number of pixels to put between children

   procedure Pack_End
      (In_Box  : access Gtk_Box_Record;
       Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
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
   --  reference ends of Box, then Padding pixels are also put between

   procedure Pack_Start
      (In_Box  : access Gtk_Box_Record;
       Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
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
   --  reference ends of Box, then Padding pixels are also put between

   procedure Query_Child_Packing
      (Box       : access Gtk_Box_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : out Boolean;
       Fill      : out Boolean;
       Padding   : out Guint;
       Pack_Type : out Gtk.Enums.Gtk_Pack_Type);
   procedure Set_Child_Packing
      (Box       : access Gtk_Box_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : Boolean;
       Fill      : Boolean;
       Padding   : Guint;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type);
   --  Sets the way Child is packed into Box.
   --  "child": the Gtk.Widget.Gtk_Widget of the child to set
   --  "expand": the new value of the Gtk.Box.Gtk_Box:expand child property
   --  "fill": the new value of the Gtk.Box.Gtk_Box:fill child property
   --  "padding": the new value of the Gtk.Box.Gtk_Box:padding child property
   --  "pack_type": the new value of the Gtk.Box.Gtk_Box:pack-type child
   --  property

   procedure Reorder_Child
      (Box      : access Gtk_Box_Record;
       Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Gint);
   --  Moves Child to a new Position in the list of Box children. The list is
   --  the <structfield>children</structfield> field of Gtk.Box.Gtk_Box-struct,
   --  and contains both widgets packed GTK_PACK_START as well as widgets
   --  packed GTK_PACK_END, in the order that these widgets were added to Box.
   --  A widget's position in the Box children list determines where the widget
   --  is packed into Box. A child widget at some position in the list will be
   --  packed just after all other widgets of the same packing type that appear
   --  earlier in the list.
   --  "child": the Gtk.Widget.Gtk_Widget to move
   --  "position": the new position for Child in the list of children of Box,
   --  starting from 0. If negative, indicates the end of the list

   function Get_Child
      (Box : access Gtk_Box_Record;
       Num : Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the Num-th child of the box, or null if there is no such child
   --  Since: gtk+ GtkAda 1.0

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Box_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Box
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Box_Record, Gtk_Box);
   function "+"
     (Widget : access Gtk_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Box
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Homogeneous_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Spacing_Property
   --  Type: Gint
   --  Flags: read-write
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   Spacing_Property : constant Glib.Properties.Property_Int;

private
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
end Gtk.Box;
