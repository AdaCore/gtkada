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

--  Arranges children in a single row or column.
--
--  Whether it is a row or column depends on the value of its
--  [propertyGtk.Orientable:orientation] property. Within the other dimension
--  all children all allocated the same size. The `GtkBoxLayout` will respect
--  the [propertyGtk.Widget:halign] and [propertyGtk.Widget:valign] properties
--  of each child widget.
--
--  If you want all children to be assigned the same size, you can use the
--  [propertyGtk.BoxLayout:homogeneous] property.
--
--  If you want to specify the amount of space placed between each child, you
--  can use the [propertyGtk.BoxLayout:spacing] property.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Properties;    use Glib.Properties;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;

package Gtk.Box_Layout is

   pragma Elaborate_Body;

   type Gtk_Box_Layout_Record is new Gtk_Layout_Manager_Record with null record;
   type Gtk_Box_Layout is access all Gtk_Box_Layout_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Box_Layout  : out Gtk_Box_Layout;
       Orientation : Gtk.Enums.Gtk_Orientation);
   procedure Initialize
      (Box_Layout  : not null access Gtk_Box_Layout_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Creates a new `GtkBoxLayout`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Orientation the orientation for the new layout

   function Gtk_Box_Layout_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Box_Layout;
   --  Creates a new `GtkBoxLayout`.
   --  @param Orientation the orientation for the new layout

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_box_layout_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Baseline_Child
      (Box_Layout : not null access Gtk_Box_Layout_Record) return Glib.Gint;
   --  Gets the value set by Gtk.Box_Layout.Set_Baseline_Child.
   --  Since: gtk+ 4.12
   --  @return the index of the child that determines the baseline in vertical
   --  layout, or -1

   procedure Set_Baseline_Child
      (Box_Layout : not null access Gtk_Box_Layout_Record;
       Child      : Glib.Gint);
   --  Sets the index of the child that determines the baseline in vertical
   --  layout.
   --  Since: gtk+ 4.12
   --  @param Child the child position, or -1

   function Get_Baseline_Position
      (Box_Layout : not null access Gtk_Box_Layout_Record)
       return Gtk.Enums.Gtk_Baseline_Position;
   --  Gets the value set by Gtk.Box_Layout.Set_Baseline_Position.
   --  @return the baseline position

   procedure Set_Baseline_Position
      (Box_Layout : not null access Gtk_Box_Layout_Record;
       Position   : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets the baseline position of a box layout.
   --  The baseline position affects only horizontal boxes with at least one
   --  baseline aligned child. If there is more vertical space available than
   --  requested, and the baseline is not allocated by the parent then the
   --  given Position is used to allocate the baseline within the extra space
   --  available.
   --  @param Position a `GtkBaselinePosition`

   function Get_Homogeneous
      (Box_Layout : not null access Gtk_Box_Layout_Record) return Boolean;
   --  Returns whether the layout is set to be homogeneous.
   --  @return True if the layout is homogeneous

   procedure Set_Homogeneous
      (Box_Layout  : not null access Gtk_Box_Layout_Record;
       Homogeneous : Boolean);
   --  Sets whether the box layout will allocate the same size to all
   --  children.
   --  @param Homogeneous True to set the box layout as homogeneous

   function Get_Spacing
      (Box_Layout : not null access Gtk_Box_Layout_Record) return Guint;
   --  Returns the space that Box_Layout puts between children.
   --  @return the spacing of the layout

   procedure Set_Spacing
      (Box_Layout : not null access Gtk_Box_Layout_Record;
       Spacing    : Guint);
   --  Sets how much spacing to put between children.
   --  @param Spacing the spacing to apply between children

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Baseline_Child_Property : constant Glib.Properties.Property_Int;
   --  The child that determines the baseline of the box in vertical layout.
   --
   --  If the child does baseline positioning, then its baseline is lined up
   --  with the baseline of the box. If it doesn't, then the bottom edge of the
   --  child is used.

   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position;
   --  The position of the allocated baseline within the extra space allocated
   --  to each child.
   --
   --  This property is only relevant for horizontal layouts containing at
   --  least one child with a baseline alignment.

   Homogeneous_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the box layout should distribute the available space equally
   --  among the children.

   Spacing_Property : constant Glib.Properties.Property_Int;
   --  The space to put between the children.

private
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Homogeneous_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");
   Baseline_Position_Property : constant Gtk.Enums.Property_Gtk_Baseline_Position :=
     Gtk.Enums.Build ("baseline-position");
   Baseline_Child_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("baseline-child");
end Gtk.Box_Layout;
