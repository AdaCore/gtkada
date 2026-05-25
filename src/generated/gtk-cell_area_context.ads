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

--  Stores geometrical information for a series of rows in a GtkCellArea
--
--  The `GtkCellAreaContext` object is created by a given `GtkCellArea`
--  implementation via its `GtkCellAreaClass.create_context` virtual method and
--  is used to store cell sizes and alignments for a series of `GtkTreeModel`
--  rows that are requested and rendered in the same context.
--
--  `GtkCellLayout` widgets can create any number of contexts in which to
--  request and render groups of data rows. However, it's important that the
--  same context which was used to request sizes for a given `GtkTreeModel` row
--  also be used for the same row when calling other `GtkCellArea` APIs such as
--  gtk_cell_area_render and gtk_cell_area_event.
--
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Cell_Area_Context is

   pragma Obsolescent;

   type Gtk_Cell_Area_Context_Record is new GObject_Record with null record;
   type Gtk_Cell_Area_Context is access all Gtk_Cell_Area_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_area_context_get_type");

   -------------
   -- Methods --
   -------------

   procedure Allocate
      (Self   : not null access Gtk_Cell_Area_Context_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   pragma Obsolescent (Allocate);
   --  Allocates a width and/or a height for all rows which are to be rendered
   --  with Context.
   --  Usually allocation is performed only horizontally or sometimes
   --  vertically since a group of rows are usually rendered side by side
   --  vertically or horizontally and share either the same width or the same
   --  height. Sometimes they are allocated in both horizontal and vertical
   --  orientations producing a homogeneous effect of the rows. This is
   --  generally the case for `GtkTreeView` when
   --  `GtkTreeView:fixed-height-mode` is enabled.
   --  Deprecated since 4.10, 1
   --  @param Width the allocated width for all `GtkTreeModel` rows rendered
   --  with Context, or -1
   --  @param Height the allocated height for all `GtkTreeModel` rows rendered
   --  with Context, or -1

   procedure Get_Allocation
      (Self   : not null access Gtk_Cell_Area_Context_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   pragma Obsolescent (Get_Allocation);
   --  Fetches the current allocation size for Context.
   --  If the context was not allocated in width or height, or if the context
   --  was recently reset with Gtk.Cell_Area_Context.Reset, the returned value
   --  will be -1.
   --  Deprecated since 4.10, 1
   --  @param Width location to store the allocated width
   --  @param Height location to store the allocated height

   procedure Get_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Height);
   --  Gets the accumulative preferred height for all rows which have been
   --  requested with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a `GtkCellArea`, the returned values are 0.
   --  Deprecated since 4.10, 1
   --  @param Minimum_Height location to store the minimum height
   --  @param Natural_Height location to store the natural height

   procedure Get_Preferred_Height_For_Width
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Height_For_Width);
   --  Gets the accumulative preferred height for Width for all rows which
   --  have been requested for the same said Width with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a `GtkCellArea`, the returned values are -1.
   --  Deprecated since 4.10, 1
   --  @param Width a proposed width for allocation
   --  @param Minimum_Height location to store the minimum height
   --  @param Natural_Height location to store the natural height

   procedure Get_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Width);
   --  Gets the accumulative preferred width for all rows which have been
   --  requested with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a `GtkCellArea`, the returned values are 0.
   --  Deprecated since 4.10, 1
   --  @param Minimum_Width location to store the minimum width
   --  @param Natural_Width location to store the natural width

   procedure Get_Preferred_Width_For_Height
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Width_For_Height);
   --  Gets the accumulative preferred width for Height for all rows which
   --  have been requested for the same said Height with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a `GtkCellArea`, the returned values are -1.
   --  Deprecated since 4.10, 1
   --  @param Height a proposed height for allocation
   --  @param Minimum_Width location to store the minimum width
   --  @param Natural_Width location to store the natural width

   procedure Push_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Height : Glib.Gint;
       Natural_Height : Glib.Gint);
   pragma Obsolescent (Push_Preferred_Height);
   --  Causes the minimum and/or natural height to grow if the new proposed
   --  sizes exceed the current minimum and natural height.
   --  This is used by `GtkCellAreaContext` implementations during the request
   --  process over a series of `GtkTreeModel` rows to progressively push the
   --  requested height over a series of gtk_cell_area_get_preferred_height
   --  requests.
   --  Deprecated since 4.10, 1
   --  @param Minimum_Height the proposed new minimum height for Context
   --  @param Natural_Height the proposed new natural height for Context

   procedure Push_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Width : Glib.Gint;
       Natural_Width : Glib.Gint);
   pragma Obsolescent (Push_Preferred_Width);
   --  Causes the minimum and/or natural width to grow if the new proposed
   --  sizes exceed the current minimum and natural width.
   --  This is used by `GtkCellAreaContext` implementations during the request
   --  process over a series of `GtkTreeModel` rows to progressively push the
   --  requested width over a series of gtk_cell_area_get_preferred_width
   --  requests.
   --  Deprecated since 4.10, 1
   --  @param Minimum_Width the proposed new minimum width for Context
   --  @param Natural_Width the proposed new natural width for Context

   procedure Reset (Self : not null access Gtk_Cell_Area_Context_Record);
   pragma Obsolescent (Reset);
   --  Resets any previously cached request and allocation data.
   --  When underlying `GtkTreeModel` data changes its important to reset the
   --  context if the content size is allowed to shrink. If the content size is
   --  only allowed to grow (this is usually an option for views rendering
   --  large data stores as a measure of optimization), then only the row that
   --  changed or was inserted needs to be (re)requested with
   --  gtk_cell_area_get_preferred_width.
   --  When the new overall size of the context requires that the allocated
   --  size changes (or whenever this allocation changes at all), the variable
   --  row sizes need to be re-requested for every row.
   --  For instance, if the rows are displayed all with the same width from
   --  top to bottom then a change in the allocated width necessitates a
   --  recalculation of all the displayed row heights using
   --  gtk_cell_area_get_preferred_height_for_width.
   --  Deprecated since 4.10, 1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The `GtkCellArea` this context was created by

   Minimum_Height_Property : constant Glib.Properties.Property_Int;
   --  The minimum height for the `GtkCellArea` in this context for all
   --  `GtkTreeModel` rows that this context was requested for using
   --  gtk_cell_area_get_preferred_height.

   Minimum_Width_Property : constant Glib.Properties.Property_Int;
   --  The minimum width for the `GtkCellArea` in this context for all
   --  `GtkTreeModel` rows that this context was requested for using
   --  gtk_cell_area_get_preferred_width.

   Natural_Height_Property : constant Glib.Properties.Property_Int;
   --  The natural height for the `GtkCellArea` in this context for all
   --  `GtkTreeModel` rows that this context was requested for using
   --  gtk_cell_area_get_preferred_height.

   Natural_Width_Property : constant Glib.Properties.Property_Int;
   --  The natural width for the `GtkCellArea` in this context for all
   --  `GtkTreeModel` rows that this context was requested for using
   --  gtk_cell_area_get_preferred_width.

private
   Natural_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("natural-width");
   Natural_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("natural-height");
   Minimum_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("minimum-width");
   Minimum_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("minimum-height");
   Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("area");
end Gtk.Cell_Area_Context;
