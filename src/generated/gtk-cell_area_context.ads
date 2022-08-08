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
--  The Gtk.Cell_Area_Context.Gtk_Cell_Area_Context object is created by a
--  given Gtk.Cell_Area.Gtk_Cell_Area implementation via its
--  Gtk.Cell_Area_Class.Gtk_Cell_Area_Class.create_context virtual method and
--  is used to store cell sizes and alignments for a series of
--  Gtk.Tree_Model.Gtk_Tree_Model rows that are requested and rendered in the
--  same context.
--
--  Gtk.Cell_Layout.Gtk_Cell_Layout widgets can create any number of contexts
--  in which to request and render groups of data rows. However, it's important
--  that the same context which was used to request sizes for a given
--  Gtk.Tree_Model.Gtk_Tree_Model row also be used for the same row when
--  calling other Gtk.Cell_Area.Gtk_Cell_Area APIs such as Gtk.Cell_Area.Render
--  and Gtk.Cell_Area.Event.
--
--  </description>
--  <group>Layout Containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Cell_Area_Context is

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
   --  Allocates a width and/or a height for all rows which are to be rendered
   --  with Context.
   --  Usually allocation is performed only horizontally or sometimes
   --  vertically since a group of rows are usually rendered side by side
   --  vertically or horizontally and share either the same width or the same
   --  height. Sometimes they are allocated in both horizontal and vertical
   --  orientations producing a homogeneous effect of the rows. This is
   --  generally the case for Gtk.Tree_View.Gtk_Tree_View when
   --  Gtk.Tree_View.Gtk_Tree_View:fixed-height-mode is enabled.
   --  Since 3.0
   --  "width": the allocated width for all Gtk.Tree_Model.Gtk_Tree_Model rows
   --  rendered with Context, or -1.
   --  "height": the allocated height for all Gtk.Tree_Model.Gtk_Tree_Model
   --  rows rendered with Context, or -1.

   procedure Get_Allocation
      (Self   : not null access Gtk_Cell_Area_Context_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Fetches the current allocation size for Context.
   --  If the context was not allocated in width or height, or if the context
   --  was recently reset with Gtk.Cell_Area_Context.Reset, the returned value
   --  will be -1.
   --  Since: gtk+ 3.0
   --  "width": location to store the allocated width, or null
   --  "height": location to store the allocated height, or null

   procedure Get_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Gets the accumulative preferred height for all rows which have been
   --  requested with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a Gtk.Cell_Area.Gtk_Cell_Area, the returned
   --  values are 0.
   --  Since: gtk+ 3.0
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Height_For_Width
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Gets the accumulative preferred height for Width for all rows which
   --  have been requested for the same said Width with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a Gtk.Cell_Area.Gtk_Cell_Area, the returned
   --  values are -1.
   --  Since: gtk+ 3.0
   --  "width": a proposed width for allocation
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Gets the accumulative preferred width for all rows which have been
   --  requested with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a Gtk.Cell_Area.Gtk_Cell_Area, the returned
   --  values are 0.
   --  Since: gtk+ 3.0
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   procedure Get_Preferred_Width_For_Height
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Gets the accumulative preferred width for Height for all rows which
   --  have been requested for the same said Height with this context.
   --  After Gtk.Cell_Area_Context.Reset is called and/or before ever
   --  requesting the size of a Gtk.Cell_Area.Gtk_Cell_Area, the returned
   --  values are -1.
   --  Since: gtk+ 3.0
   --  "height": a proposed height for allocation
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   procedure Push_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Height : Glib.Gint;
       Natural_Height : Glib.Gint);
   --  Causes the minimum and/or natural height to grow if the new proposed
   --  sizes exceed the current minimum and natural height.
   --  This is used by Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   --  implementations during the request process over a series of
   --  Gtk.Tree_Model.Gtk_Tree_Model rows to progressively push the requested
   --  height over a series of Gtk.Cell_Area.Get_Preferred_Height requests.
   --  Since: gtk+ 3.0
   --  "minimum_height": the proposed new minimum height for Context
   --  "natural_height": the proposed new natural height for Context

   procedure Push_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Width : Glib.Gint;
       Natural_Width : Glib.Gint);
   --  Causes the minimum and/or natural width to grow if the new proposed
   --  sizes exceed the current minimum and natural width.
   --  This is used by Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   --  implementations during the request process over a series of
   --  Gtk.Tree_Model.Gtk_Tree_Model rows to progressively push the requested
   --  width over a series of Gtk.Cell_Area.Get_Preferred_Width requests.
   --  Since: gtk+ 3.0
   --  "minimum_width": the proposed new minimum width for Context
   --  "natural_width": the proposed new natural width for Context

   procedure Reset (Self : not null access Gtk_Cell_Area_Context_Record);
   --  Resets any previously cached request and allocation data.
   --  When underlying Gtk.Tree_Model.Gtk_Tree_Model data changes its
   --  important to reset the context if the content size is allowed to shrink.
   --  If the content size is only allowed to grow (this is usually an option
   --  for views rendering large data stores as a measure of optimization),
   --  then only the row that changed or was inserted needs to be (re)requested
   --  with Gtk.Cell_Area.Get_Preferred_Width.
   --  When the new overall size of the context requires that the allocated
   --  size changes (or whenever this allocation changes at all), the variable
   --  row sizes need to be re-requested for every row.
   --  For instance, if the rows are displayed all with the same width from
   --  top to bottom then a change in the allocated width necessitates a
   --  recalculation of all the displayed row heights using
   --  Gtk.Cell_Area.Get_Preferred_Height_For_Width.
   --  Since 3.0

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The Gtk.Cell_Area.Gtk_Cell_Area this context was created by

   Minimum_Height_Property : constant Glib.Properties.Property_Int;
   --  The minimum height for the Gtk.Cell_Area.Gtk_Cell_Area in this context
   --  for all Gtk.Tree_Model.Gtk_Tree_Model rows that this context was
   --  requested for using Gtk.Cell_Area.Get_Preferred_Height.

   Minimum_Width_Property : constant Glib.Properties.Property_Int;
   --  The minimum width for the Gtk.Cell_Area.Gtk_Cell_Area in this context
   --  for all Gtk.Tree_Model.Gtk_Tree_Model rows that this context was
   --  requested for using Gtk.Cell_Area.Get_Preferred_Width.

   Natural_Height_Property : constant Glib.Properties.Property_Int;
   --  The natural height for the Gtk.Cell_Area.Gtk_Cell_Area in this context
   --  for all Gtk.Tree_Model.Gtk_Tree_Model rows that this context was
   --  requested for using Gtk.Cell_Area.Get_Preferred_Height.

   Natural_Width_Property : constant Glib.Properties.Property_Int;
   --  The natural width for the Gtk.Cell_Area.Gtk_Cell_Area in this context
   --  for all Gtk.Tree_Model.Gtk_Tree_Model rows that this context was
   --  requested for using Gtk.Cell_Area.Get_Preferred_Width.

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
