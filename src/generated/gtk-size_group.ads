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
--  Gtk.Size_Group.Gtk_Size_Group provides a mechanism for grouping a number
--  of widgets together so they all request the same amount of space. This is
--  typically useful when you want a column of widgets to have the same size,
--  but you can't use a Gtk.Grid.Gtk_Grid widget.
--
--  In detail, the size requested for each widget in a
--  Gtk.Size_Group.Gtk_Size_Group is the maximum of the sizes that would have
--  been requested for each widget in the size group if they were not in the
--  size group. The mode of the size group (see Gtk.Size_Group.Set_Mode)
--  determines whether this applies to the horizontal size, the vertical size,
--  or both sizes.
--
--  Note that size groups only affect the amount of space requested, not the
--  size that the widgets finally receive. If you want the widgets in a
--  Gtk.Size_Group.Gtk_Size_Group to actually be the same size, you need to
--  pack them in such a way that they get the size they request and not more.
--  For example, if you are packing your widgets into a table, you would not
--  include the Gtk.Enums.Fill flag.
--
--  Gtk.Size_Group.Gtk_Size_Group objects are referenced by each widget in the
--  size group, so once you have added all widgets to a
--  Gtk.Size_Group.Gtk_Size_Group, you can drop the initial reference to the
--  size group with g_object_unref. If the widgets in the size group are
--  subsequently destroyed, then they will be removed from the size group and
--  drop their references on the size group; when all widgets have been
--  removed, the size group will be freed.
--
--  Widgets can be part of multiple size groups; GTK+ will compute the
--  horizontal size of a widget from the horizontal requisition of all widgets
--  that can be reached from the widget by a chain of size groups of type
--  Gtk.Size_Group.Horizontal or Gtk.Size_Group.Both, and the vertical size
--  from the vertical requisition of all widgets that can be reached from the
--  widget by a chain of size groups of type Gtk.Size_Group.Vertical or
--  Gtk.Size_Group.Both.
--
--  Note that only non-contextual sizes of every widget are ever consulted by
--  size groups (since size groups have no knowledge of what size a widget will
--  be allocated in one dimension, it cannot derive how much height a widget
--  will receive for a given width). When grouping widgets that trade height
--  for width in mode Gtk.Size_Group.Vertical or Gtk.Size_Group.Both: the
--  height for the minimum width will be the requested height for all widgets
--  in the group. The same is of course true when horizontally grouping width
--  for height widgets.
--
--  Widgets that trade height-for-width should set a reasonably large minimum
--  width by way of Gtk.Label.Gtk_Label:width-chars for instance. Widgets with
--  static sizes as well as widgets that grow (such as ellipsizing text) need
--  no such considerations.
--
--  # GtkSizeGroup as GtkBuildable
--
--  Size groups can be specified in a UI definition by placing an <object>
--  element with `class="GtkSizeGroup"` somewhere in the UI definition. The
--  widgets that belong to the size group are specified by a <widgets> element
--  that may contain multiple <widget> elements, one for each member of the
--  size group. The "name" attribute gives the id of the widget.
--
--  An example of a UI definition fragment with GtkSizeGroup: |[ <object
--  class="GtkSizeGroup"> <property
--  name="mode">GTK_SIZE_GROUP_HORIZONTAL</property> <widgets> <widget
--  name="radio1"/> <widget name="radio2"/> </widgets> </object> ]|
--
--  </description>
--  <testgtk>create_size_groups.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Size_Group is

   type Gtk_Size_Group_Record is new GObject_Record with null record;
   type Gtk_Size_Group is access all Gtk_Size_Group_Record'Class;

   type Size_Group_Mode is (
      None,
      Horizontal,
      Vertical,
      Both);
   pragma Convention (C, Size_Group_Mode);
   --  The mode of the size group determines the directions in which the size
   --  group affects the requested sizes of its component widgets.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Size_Group_Mode_Properties is
      new Generic_Internal_Discrete_Property (Size_Group_Mode);
   type Property_Size_Group_Mode is new Size_Group_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Size_Group : out Gtk_Size_Group;
       Mode       : Size_Group_Mode := Both);
   procedure Initialize
      (Size_Group : not null access Gtk_Size_Group_Record'Class;
       Mode       : Size_Group_Mode := Both);
   --  Create a new Gtk.Size_Group.Gtk_Size_Group.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "mode": the mode for the new size group.

   function Gtk_Size_Group_New
      (Mode : Size_Group_Mode := Both) return Gtk_Size_Group;
   --  Create a new Gtk.Size_Group.Gtk_Size_Group.
   --  "mode": the mode for the new size group.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_size_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Widget
      (Size_Group : not null access Gtk_Size_Group_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a widget to a Gtk.Size_Group.Gtk_Size_Group. In the future, the
   --  requisition of the widget will be determined as the maximum of its
   --  requisition and the requisition of the other widgets in the size group.
   --  Whether this applies horizontally, vertically, or in both directions
   --  depends on the mode of the size group. See Gtk.Size_Group.Set_Mode.
   --  When the widget is destroyed or no longer referenced elsewhere, it will
   --  be removed from the size group.
   --  "widget": the Gtk.Widget.Gtk_Widget to add

   function Get_Ignore_Hidden
      (Size_Group : not null access Gtk_Size_Group_Record) return Boolean;
   pragma Obsolescent (Get_Ignore_Hidden);
   --  Returns if invisible widgets are ignored when calculating the size.
   --  Since: gtk+ 2.8
   --  Deprecated since 3.22, 1

   procedure Set_Ignore_Hidden
      (Size_Group    : not null access Gtk_Size_Group_Record;
       Ignore_Hidden : Boolean);
   pragma Obsolescent (Set_Ignore_Hidden);
   --  Sets whether unmapped widgets should be ignored when calculating the
   --  size.
   --  Since: gtk+ 2.8
   --  Deprecated since 3.22, 1
   --  "ignore_hidden": whether unmapped widgets should be ignored when
   --  calculating the size

   function Get_Mode
      (Size_Group : not null access Gtk_Size_Group_Record)
       return Size_Group_Mode;
   --  Gets the current mode of the size group. See Gtk.Size_Group.Set_Mode.

   procedure Set_Mode
      (Size_Group : not null access Gtk_Size_Group_Record;
       Mode       : Size_Group_Mode);
   --  Sets the Gtk.Size_Group.Size_Group_Mode of the size group. The mode of
   --  the size group determines whether the widgets in the size group should
   --  all have the same horizontal requisition (Gtk.Size_Group.Horizontal) all
   --  have the same vertical requisition (Gtk.Size_Group.Vertical), or should
   --  all have the same requisition in both directions (Gtk.Size_Group.Both).
   --  "mode": the mode to set for the size group.

   function Get_Widgets
      (Size_Group : not null access Gtk_Size_Group_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the list of widgets associated with Size_Group.
   --  Since: gtk+ 2.10

   procedure Remove_Widget
      (Size_Group : not null access Gtk_Size_Group_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a widget from a Gtk.Size_Group.Gtk_Size_Group.
   --  "widget": the Gtk.Widget.Gtk_Widget to remove

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Ignore_Hidden_Property : constant Glib.Properties.Property_Boolean;
   --  If True, unmapped widgets are ignored when determining the size of the
   --  group.

   Mode_Property : constant Gtk.Size_Group.Property_Size_Group_Mode;
   --  Type: Size_Group_Mode

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Size_Group_Record, Gtk_Size_Group);
   function "+"
     (Widget : access Gtk_Size_Group_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Size_Group
   renames Implements_Gtk_Buildable.To_Object;

private
   Mode_Property : constant Gtk.Size_Group.Property_Size_Group_Mode :=
     Gtk.Size_Group.Build ("mode");
   Ignore_Hidden_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("ignore-hidden");
end Gtk.Size_Group;
