------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Groups widgets together so they all request the same size.
--
--  This is typically useful when you want a column of widgets to have the
--  same size, but you can't use a [classGtk.Grid] or [classGtk.Box].
--
--  In detail, the size requested for each widget in a `GtkSizeGroup` is the
--  maximum of the sizes that would have been requested for each widget in the
--  size group if they were not in the size group. The
--  [mode][methodGtk.SizeGroup.set_mode] of the size group determines whether
--  this applies to the horizontal size, the vertical size, or both sizes.
--
--  Note that size groups only affect the amount of space requested, not the
--  size that the widgets finally receive. If you want the widgets in a
--  `GtkSizeGroup` to actually be the same size, you need to pack them in such
--  a way that they get the size they request and not more. In particular it
--  doesn't make a lot of sense to set [the expand
--  flags][methodGtk.Widget.set_hexpand] on the widgets that are members of a
--  size group.
--
--  `GtkSizeGroup` objects are referenced by each widget in the size group, so
--  once you have added all widgets to a `GtkSizeGroup`, you can drop the
--  initial reference to the size group with [methodGobject.Object.unref]. If
--  the widgets in the size group are subsequently destroyed, then they will be
--  removed from the size group and drop their references on the size group;
--  when all widgets have been removed, the size group will be freed.
--
--  Widgets can be part of multiple size groups; GTK will compute the
--  horizontal size of a widget from the horizontal requisition of all widgets
--  that can be reached from the widget by a chain of size groups with mode
--  [enumGtk.SizeGroupMode.HORIZONTAL] or [enumGtk.SizeGroupMode.BOTH], and the
--  vertical size from the vertical requisition of all widgets that can be
--  reached from the widget by a chain of size groups with mode
--  [enumGtk.SizeGroupMode.VERTICAL] or [enumGtk.SizeGroupMode.BOTH].
--
--  # Size groups and trading height-for-width
--
--  ::: warning Generally, size groups don't interact well with widgets that
--  trade height for width (or width for height), such as wrappable labels.
--  Avoid using size groups with such widgets.
--
--  A size group with mode [enumGtk.SizeGroupMode.HORIZONTAL] or
--  [enumGtk.SizeGroupMode.VERTICAL] only consults non-contextual sizes of
--  widgets other than the one being measured, since it has no knowledge of
--  what size a widget will get allocated in the other orientation. This can
--  lead to widgets in a group actually requesting different contextual sizes,
--  contrary to the purpose of `GtkSizeGroup`.
--
--  In contrast, a size group with mode [enumGtk.SizeGroupMode.BOTH] can
--  properly propagate the available size in the opposite orientation when
--  measuring widgets in the group, which results in consistent and accurate
--  measurements.
--
--  In case some mechanism other than a size group is already used to ensure
--  that widgets in a group all get the same size in one orientation (for
--  example, some common ancestor is known to allocate the same width to all
--  its children), and the size group is only really needed to also make the
--  widgets request the same size in the other orientation, it is beneficial to
--  still set the group's mode to [enumGtk.SizeGroupMode.BOTH]. This lets the
--  group assume and count on sizes of the widgets in the former orientation
--  being the same, which enables it to propagate the available size as
--  described above.
--
--  # Alternatives to size groups
--
--  Size groups have many limitations, such as only influencing size requests
--  but not allocations, and poor height-for-width support. When possible,
--  prefer using dedicated mechanisms that can properly ensure that the widgets
--  get the same size.
--
--  Various container widgets and layout managers support a homogeneous layout
--  mode, where they will explicitly give the same size to their children (see
--  [propertyGtk.Box:homogeneous]). Using homogeneous mode can also have large
--  performance benefits compared to either the same container in
--  non-homogeneous mode, or to size groups.
--
--  [classGtk.Grid] can be used to position widgets into rows and columns.
--  Members of each column will have the same width among them; likewise,
--  members of each row will have the same height. On top of that, the heights
--  can be made equal between all rows with [propertyGtk.Grid:row-homogeneous],
--  and the widths can be made equal between all columns with
--  [propertyGtk.Grid:column-homogeneous].
--
--  # GtkSizeGroup as GtkBuildable
--
--  Size groups can be specified in a UI definition by placing an `<object>`
--  element with `class="GtkSizeGroup"` somewhere in the UI definition. The
--  widgets that belong to the size group are specified by a `<widgets>`
--  element that may contain multiple `<widget>` elements, one for each member
--  of the size group. The "name" attribute gives the id of the widget.
--
--  An example of a UI definition fragment with `GtkSizeGroup`: ```xml <object
--  class="GtkSizeGroup"> <property name="mode">horizontal</property> <widgets>
--  <widget name="radio1"/> <widget name="radio2"/> </widgets> </object> ```
--
--  <gtkada_demo>create_size_groups.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Size_Group is

   type Gtk_Size_Group_Record is new GObject_Record with null record;
   type Gtk_Size_Group is access all Gtk_Size_Group_Record'Class;

   type Gtk_Size_Group_Mode is (
      None,
      Horizontal,
      Vertical,
      Both);
   pragma Convention (C, Gtk_Size_Group_Mode);
   --  The mode of the size group determines the directions in which the size
   --  group affects the requested sizes of its component widgets.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Size_Group_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Size_Group_Mode);
   type Property_Gtk_Size_Group_Mode is new Gtk_Size_Group_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Size_Group : out Gtk_Size_Group;
       Mode       : Gtk_Size_Group_Mode);
   procedure Initialize
      (Size_Group : not null access Gtk_Size_Group_Record'Class;
       Mode       : Gtk_Size_Group_Mode);
   --  Create a new `GtkSizeGroup`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Mode the mode for the new size group.

   function Gtk_Size_Group_New
      (Mode : Gtk_Size_Group_Mode) return Gtk_Size_Group;
   --  Create a new `GtkSizeGroup`.
   --  @param Mode the mode for the new size group.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_size_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Widget
      (Size_Group : not null access Gtk_Size_Group_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a widget to a `GtkSizeGroup`.
   --  In the future, the requisition of the widget will be determined as the
   --  maximum of its requisition and the requisition of the other widgets in
   --  the size group. Whether this applies horizontally, vertically, or in
   --  both directions depends on the mode of the size group. See
   --  [methodGtk.SizeGroup.set_mode].
   --  When the widget is destroyed or no longer referenced elsewhere, it will
   --  be removed from the size group.
   --  @param Widget the `GtkWidget` to add

   function Get_Mode
      (Size_Group : not null access Gtk_Size_Group_Record)
       return Gtk_Size_Group_Mode;
   --  Gets the current mode of the size group.
   --  @return the current mode of the size group.

   procedure Set_Mode
      (Size_Group : not null access Gtk_Size_Group_Record;
       Mode       : Gtk_Size_Group_Mode);
   --  Sets the `GtkSizeGroupMode` of the size group.
   --  The mode of the size group determines whether the widgets in the size
   --  group should all have the same horizontal requisition
   --  (Gtk.Size_Group.Horizontal) all have the same vertical requisition
   --  (Gtk.Size_Group.Vertical), or should all have the same requisition in
   --  both directions (Gtk.Size_Group.Both).
   --  @param Mode the mode to set for the size group.

   function Get_Widgets
      (Size_Group : not null access Gtk_Size_Group_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the list of widgets associated with Size_Group.
   --  @return a `GSList` of widgets. The list is owned by GTK and should not
   --  be modified.

   procedure Remove_Widget
      (Size_Group : not null access Gtk_Size_Group_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a widget from a `GtkSizeGroup`.
   --  @param Widget the `GtkWidget` to remove

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Mode_Property : constant Gtk.Size_Group.Property_Gtk_Size_Group_Mode;
   --  Type: Gtk_Size_Group_Mode
   --  The direction in which the size group affects requested sizes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"

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
   Mode_Property : constant Gtk.Size_Group.Property_Gtk_Size_Group_Mode :=
     Gtk.Size_Group.Build ("mode");
end Gtk.Size_Group;
