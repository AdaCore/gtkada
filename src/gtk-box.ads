-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  A box is a container that can have multiple children, organized either
--  horizontally or vertically. Two subtypes are provided, Gtk_Hbox and
--  Gtk_Vbox, to conform to the C API. In Ada, you do not need to
--  distinguish between the two, but note that the Gtk_Box type is conceptually
--  an abstract type: there is no way to create a "Gtk_Box", only ways to
--  create either an horizontal box, or a vertical box.
--
--  Children can be added to one of two positions in the box, either at the
--  beginning (ie left or top) or at the end (ie right or bottom). Each of
--  these positions can contain multiple widgets.
--
--  Every time a child is added to the start, it is placed to the right
--  (resp. the bottom) of the previous widget added to the start.
--
--  Every time a child is added to the end, it is placed to the left (resp.
--  the top) of the previous widget added to the end.
--
--  There are a number of parameters to specify the behavior of the box when
--  it is resized, and how the children should be reorganized and/or resized.
--
--  See the testgtk example in the GtkAda distribution to see concrete examples
--  on how all the parameters for the boxes work.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Glib.Properties;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Box is

   type Gtk_Box_Record is new Gtk.Container.Gtk_Container_Record with private;

   --  <doc_ignore>
   subtype Gtk_Hbox_Record is Gtk_Box_Record;
   subtype Gtk_Vbox_Record is Gtk_Box_Record;

   type Gtk_Box is access all Gtk_Box_Record'Class;
   subtype Gtk_Hbox is Gtk_Box;
   subtype Gtk_Vbox is Gtk_Box;
   --  </doc_ignore>

   procedure Gtk_New_Vbox
     (Box         : out Gtk_Box;
      Homogeneous : Boolean := False;
      Spacing     : Gint := 0);
   --  Create a new vertical box.
   --  Its children will be placed one above the other.
   --  If Homogeneous is True, all the children will be allocated exactly the
   --  same screen real-estate, whereas if it is False, each child can have
   --  its own size.
   --  Spacing is the space left between two adjacent children.

   procedure Gtk_New_Hbox
     (Box         : out Gtk_Box;
      Homogeneous : Boolean := False;
      Spacing     : Gint := 0);
   --  Create a new horizontal box.
   --  Its children will be placed one besides the other.
   --  If Homogeneous is True, all the children will be allocated exactly the
   --  same screen real-estate, whereas if it is False, each child can have
   --  its own size.
   --  Spacing is the space left between two adjacent children.

   procedure Initialize_Vbox
     (Box         : access Gtk_Box_Record'Class;
      Homogeneous : Boolean := False;
      Spacing     : Gint := 0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_Hbox
     (Box         : access Gtk_Box_Record'Class;
      Homogeneous : Boolean := False;
      Spacing     : Gint := 0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Box.

   function Get_Hbox_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_HBox.

   function Get_Vbox_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_VBox.

   procedure Pack_Start
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand  : Boolean := True;
      Fill    : Boolean := True;
      Padding : Gint    := 0);
   --  Add a new child to the beginning of the box (ie left or top part).
   --  It is added to the right (resp. the bottom) of the previous child added
   --  to the beginning of the box. Note that a child added to the beginning of
   --  the box will always remain on the left (resp. top) of all the children
   --  added to the end of the box.
   --
   --  If Expand is False, the size allocated for each size will be the one
   --  requested by the widget (or the largest child if Homogeneous was set to
   --  true when the box was created). Otherwise, the total size of the box is
   --  divided between all the children. Note that this does not mean that the
   --  children have to occupy all the space given to them...
   --
   --  If Fill is True, then the widget will be resized so as to occupy all the
   --  space allocated to them. This is only relevant if Expand is True, since
   --  otherwise the space allocated is the same one as the child's size.
   --
   --  Padding is the amount of space left around the widget when it is drawn.

   procedure Pack_End
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand  : Boolean := True;
      Fill    : Boolean := True;
      Padding : Gint    := 0);
   --  Add a new child to the end of the box (ie right or bottom part).
   --  It is added to the left (resp. top) of the previous child added to the
   --  end of the box. Note that a child added to the end of the box will
   --  always remain on the right (resp. bottom) of all the children added to
   --  the beginning of the box.
   --
   --  See Pack_Start for an explanation of all the parameters.

   procedure Pack_Start_Defaults
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  This is the same as Pack_Start if you use the default parameter values.
   --  It is provided for backward compatibility only.

   procedure Pack_End_Defaults
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  This is the same as Pack_End if you use the default parameter values.
   --  It is provided for backward compatibility only.

   procedure Set_Homogeneous
     (In_Box      : access Gtk_Box_Record;
      Homogeneous : Boolean);
   --  Modify the homogeneous parameter for the box.
   --  If the box is homogeneous, then all its children will be allocated the
   --  same amount of space, even if they are not resized to occupy it
   --  (depending on the parameters given to Pack_Start and Pack_End).

   function Get_Homogeneous
     (In_Box : access Gtk_Box_Record) return Boolean;
   --  Return the homogeneous parameter of the box.

   procedure Set_Spacing
     (In_Box  : access Gtk_Box_Record;
      Spacing : Gint);
   --  Modify the spacing for the box.
   --  I.e. the amount of space left between two adjacent children.

   function Get_Spacing (In_Box : access Gtk_Box_Record) return Gint;
   --  Return the spacing of the box.

   procedure Reorder_Child
     (In_Box : access Gtk_Box_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Pos    : Guint);
   --  Move the Child to a new position.
   --  Nothing is done if Child is not in the box.
   --  Pos starts at 0, and indicates the position of the child relative to all
   --  other children, no matter where they were packed  (the beginning or the
   --  end of the box).

   procedure Query_Child_Packing
     (In_Box   : access Gtk_Box_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Gtk.Enums.Gtk_Pack_Type);
   --  Get information on how the child was packed in the box.
   --  The results are undefined if Child is not in the box.

   procedure Set_Child_Packing
     (In_Box    : access Gtk_Box_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand    : Boolean;
      Fill      : Boolean;
      Padding   : Gint;
      Pack_Type : Gtk.Enums.Gtk_Pack_Type);
   --  Modify the packing for a child.

   function Get_Child
     (Box : access Gtk_Box_Record; Num : Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the Num-th child of the box, or null if there is no such child.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Spacing_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The amount of space between children.
   --    See also: Set_Spacing and Get_Spacing
   --
   --  - Name:  Homogeneous_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the childrenshould all be the same size.
   --    See also: Set_Homogeneous
   --
   --  </properties>

   Spacing_Property : constant Glib.Properties.Property_Int;
   Homogeneous_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Box_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");

   pragma Import (C, Get_Type,      "gtk_box_get_type");
   pragma Import (C, Get_Hbox_Type, "gtk_hbox_get_type");
   pragma Import (C, Get_Vbox_Type, "gtk_vbox_get_type");
end Gtk.Box;
