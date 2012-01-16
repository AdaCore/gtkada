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

pragma Ada_05;
--  <description>
-- 
--
--  </description>
--  <group>Layout Containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                 use Cairo;
with Gdk.Event;             use Gdk.Event;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Glib.Values;           use Glib.Values;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Cell_Area_Context; use Gtk.Cell_Area_Context;
with Gtk.Cell_Editable;     use Gtk.Cell_Editable;
with Gtk.Cell_Layout;       use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Cell_Area is

   type Gtk_Cell_Area_Record is new GObject_Record with null record;
   type Gtk_Cell_Area is access all Gtk_Cell_Area_Record'Class;

   type GtkCellCallback is access function
     (Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   return Boolean;
   --  The type of the callback functions used for iterating over the cell
   --  renderers of a Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach.
   --  "renderer": the cell renderer to operate on

   type GtkCellAllocCallback is access function
     (Renderer        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  The type of the callback functions used for iterating over the cell
   --  renderers and their allocated areas inside a
   --  Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach_Alloc.
   --  "renderer": the cell renderer to operate on
   --  "cell_area": the area allocated to Renderer inside the rectangle
   --  provided to Gtk.Cell_Area.Foreach_Alloc.
   --  "cell_background": the background area for Renderer inside the
   --  background area provided to Gtk.Cell_Area.Foreach_Alloc.

   type Cell_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a GtkTreeIter indicating the row to set the value for

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_area_get_type");

   -------------
   -- Methods --
   -------------

   function Activate
      (Self      : access Gtk_Cell_Area_Record;
       Context   : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Edit_Only : Boolean) return Boolean;
   --  Activates Area, usually by activating the currently focused cell,
   --  however some subclasses which embed widgets in the area can also
   --  activate a widget if it currently has the focus.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context in context
   --  with the current row data
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering on
   --  "cell_area": the size and location of Area relative to Widget's
   --  allocation
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State flags for Area
   --  for this row of data.
   --  "edit_only": if True then only cell renderers that are
   --  %GTK_CELL_RENDERER_MODE_EDITABLE will be activated.

   function Activate_Cell
      (Self      : access Gtk_Cell_Area_Record;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State) return Boolean;
   --  This is used by Gtk.Cell_Area.Gtk_Cell_Area subclasses when handling
   --  events to activate cells, the base Gtk.Cell_Area.Gtk_Cell_Area class
   --  activates cells for keyboard events for free in its own
   --  GtkCellArea->activate implementation.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering onto
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area to activate
   --  "event": the GdkEvent for which cell activation should occur
   --  "cell_area": the GdkRectangle in Widget relative coordinates of
   --  Renderer for the current row.
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State for Renderer

   procedure Add
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Adds Renderer to Area with the default child cell properties.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to add to Area

   procedure Add_Focus_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Adds Sibling to Renderer's focusable area, focus will be drawn around
   --  Renderer and all of its siblings if Renderer can focus for a given row.
   --  Events handled by focus siblings can also activate the given focusable
   --  Renderer.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus
   --  "sibling": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to add to Renderer's
   --  focus area

   procedure Apply_Attributes
      (Self        : access Gtk_Cell_Area_Record;
       Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean);
   --  Applies any connected attributes to the renderers in Area by pulling
   --  the values from Tree_Model.
   --  Since: gtk+ 3.0
   --  "tree_model": the Gtk.Tree_Model.Gtk_Tree_Model to pull values from
   --  "iter": the GtkTreeIter in Tree_Model to apply values for
   --  "is_expander": whether Iter has children
   --  "is_expanded": whether Iter is expanded in the view and children are
   --  visible

   procedure Attribute_Connect
      (Self      : access Gtk_Cell_Area_Record;
       Renderer  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Gint);
   --  Connects an Attribute to apply values from Column for the
   --  Gtk.Tree_Model.Gtk_Tree_Model in use.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to connect an
   --  attribute for
   --  "attribute": the attribute name
   --  "column": the Gtk.Tree_Model.Gtk_Tree_Model column to fetch attribute
   --  values from

   procedure Attribute_Disconnect
      (Self      : access Gtk_Cell_Area_Record;
       Renderer  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String);
   --  Disconnects Attribute for the Renderer in Area so that attribute will
   --  no longer be updated with values from the model.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to disconnect an
   --  attribute for
   --  "attribute": the attribute name

   procedure Cell_Get_Property
      (Self          : access Gtk_Cell_Area_Record;
       Renderer      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   procedure Cell_Set_Property
      (Self          : access Gtk_Cell_Area_Record;
       Renderer      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Sets a cell property for Renderer in Area.
   --  Since: gtk+ 3.0
   --  "renderer": a Gtk.Cell_Renderer.Gtk_Cell_Renderer inside Area
   --  "property_name": the name of the cell property to set
   --  "value": the value to set the cell property to

   function Copy_Context
      (Self    : access Gtk_Cell_Area_Record;
       Context : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context;
   --  This is sometimes needed for cases where rows need to share alignments
   --  in one orientation but may be separately grouped in the opposing
   --  orientation.
   --  For instance, Gtk.Iconview.Gtk_Iconview creates all icons (rows) to have
   --  the same width and the cells theirin to have the same horizontal
   --  alignments. However each row of icons may have a separate collective
   --  height. Gtk.Iconview.Gtk_Iconview uses this to request the heights of
   --  each row based on a context which was already used to request all the
   --  row widths that are to be displayed.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to copy

   function Create_Context
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context;
   --  Creates a Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to be used with
   --  Area for all purposes. Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   --  stores geometry information for rows for which it was operated on, it is
   --  important to use the same context for the same row of data at all times
   --  (i.e. one should render and handle events with the same
   --  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which was used to request
   --  the size of those rows of data).
   --  Since: gtk+ 3.0

   function Event
      (Self      : access Gtk_Cell_Area_Record;
       Context   : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State) return Gint;
   --  Delegates event handling to a Gtk.Cell_Area.Gtk_Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "event": the GdkEvent to handle
   --  "cell_area": the Widget relative coordinates for Area
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State for Area in this
   --  row.

   function Focus
      (Self      : access Gtk_Cell_Area_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;
   --  This should be called by the Area's owning layout widget when focus is
   --  to be passed to Area, or moved within Area for a given Direction and row
   --  data.
   --  Implementing Gtk.Cell_Area.Gtk_Cell_Area classes should implement this
   --  method to receive and navigate focus in its own way particular to how it
   --  lays out cells.
   --  Since: gtk+ 3.0
   --  "direction": the Gtk.Enums.Gtk_Direction_Type

   procedure Foreach
      (Self     : access Gtk_Cell_Area_Record;
       Callback : GtkCellCallback);
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area.
   --  Since: gtk+ 3.0
   --  "callback": the GtkCellCallback to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type GtkCellCallback is access function
        (Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Data     : User_Data_Type) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers of a Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach.
      --  "renderer": the cell renderer to operate on
      --  "data": user-supplied data

      procedure Foreach
         (Self          : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Callback      : GtkCellCallback;
          Callback_Data : User_Data_Type);
      --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in
      --  Area.
      --  Since: gtk+ 3.0
      --  "callback": the GtkCellCallback to call
      --  "callback_data": user provided data pointer

   end Foreach_User_Data;

   procedure Foreach_Alloc
      (Self            : access Gtk_Cell_Area_Record;
       Context         : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Callback        : GtkCellAllocCallback);
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area
   --  with the allocated rectangle inside Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "cell_area": the Widget relative coordinates and size for Area
   --  "background_area": the Widget relative coordinates of the background
   --  area
   --  "callback": the GtkCellAllocCallback to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_Alloc_User_Data is

      type GtkCellAllocCallback is access function
        (Renderer        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
         Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
         Data            : User_Data_Type) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers and their allocated areas inside a
      --  Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach_Alloc.
      --  "renderer": the cell renderer to operate on
      --  "cell_area": the area allocated to Renderer inside the rectangle
      --  provided to Gtk.Cell_Area.Foreach_Alloc.
      --  "cell_background": the background area for Renderer inside the
      --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
      --  "data": user-supplied data

      procedure Foreach_Alloc
         (Self            : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Context         : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
          Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Callback        : GtkCellAllocCallback;
          Callback_Data   : User_Data_Type);
      --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area
      --  with the allocated rectangle inside Cell_Area.
      --  Since: gtk+ 3.0
      --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this
      --  row of data.
      --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
      --  "cell_area": the Widget relative coordinates and size for Area
      --  "background_area": the Widget relative coordinates of the background
      --  area
      --  "callback": the GtkCellAllocCallback to call
      --  "callback_data": user provided data pointer

   end Foreach_Alloc_User_Data;

   procedure Get_Cell_Allocation
      (Self       : access Gtk_Cell_Area_Record;
       Context    : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer   : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Cell_Area  : access Gdk.Rectangle.Gdk_Rectangle;
       Allocation : access Gdk.Rectangle.Gdk_Rectangle);
   --  Derives the allocation of Renderer inside Area if Area were to be
   --  renderered in Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context used to hold
   --  sizes for Area.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering on
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to get the
   --  allocation for
   --  "cell_area": the whole allocated area for Area in Widget for this row
   --  "allocation": where to store the allocation for Renderer

   function Get_Cell_At_Position
      (Self       : access Gtk_Cell_Area_Record;
       Context    : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
       X          : Gint;
       Y          : Gint;
       Alloc_Area : access Gdk.Rectangle.Gdk_Rectangle)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --  Gets the Gtk.Cell_Renderer.Gtk_Cell_Renderer at X and Y coordinates
   --  inside Area and optionally returns the full cell allocation for it
   --  inside Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context used to hold
   --  sizes for Area.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering on
   --  "cell_area": the whole allocated area for Area in Widget for this row
   --  "x": the x position
   --  "y": the y position
   --  "alloc_area": where to store the inner allocated area of the returned
   --  cell renderer, or null.

   function Get_Current_Path_String
      (Self : access Gtk_Cell_Area_Record) return UTF8_String;
   --  Gets the current GtkTreePath string for the currently applied
   --  GtkTreeIter, this is implicitly updated when
   --  Gtk.Cell_Area.Apply_Attributes is called and can be used to interact
   --  with renderers from Gtk.Cell_Area.Gtk_Cell_Area subclasses.
   --  attributes applied to Area. This string belongs to the area and should
   --  not be freed.
   --  Since: gtk+ 3.0

   function Get_Edit_Widget
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Editable.Gtk_Cell_Editable;
   --  Gets the Gtk.Cell_Editable.Gtk_Cell_Editable widget currently used to
   --  edit the currently edited cell.
   --  Since: gtk+ 3.0

   function Get_Edited_Cell
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --  Gets the Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area that is currently
   --  being edited.
   --  Since: gtk+ 3.0

   function Get_Focus_Cell
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   procedure Set_Focus_Cell
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Explicitly sets the currently focused cell to Renderer.
   --  This is generally called by implementations of GtkCellAreaClass.focus or
   --  GtkCellAreaClass.event, however it can also be used to implement
   --  functions such as gtk_tree_view_set_cursor_on_cell.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to give focus to

   function Get_Focus_From_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --  Gets the Gtk.Cell_Renderer.Gtk_Cell_Renderer which is expected to be
   --  focusable for which Renderer is, or may be a sibling.
   --  This is handy for Gtk.Cell_Area.Gtk_Cell_Area subclasses when handling
   --  events, after determining the renderer at the event location it can then
   --  chose to activate the focus cell for which the event cell may have been
   --  a sibling.
   --  is a sibling, or null.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer

   function Get_Focus_Siblings
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Glib.Object.Object_Simple_List.GList;
   --  Gets the focus sibling cell renderers for Renderer.
   --  The returned list is internal and should not be freed.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus

   procedure Get_Preferred_Height
      (Self           : access Gtk_Cell_Area_Record;
       Context        : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Height : out Gint;
       Natural_Height : out Gint);
   --  Retrieves a cell area's initial minimum and natural height.
   --  Area will store some geometrical information in Context along the way,
   --  when requesting sizes over an arbitrary number of rows, its not
   --  important to check the Minimum_Height and Natural_Height of this call
   --  but rather to consult Gtk.Cell_Area_Context.Get_Preferred_Height after a
   --  series of requests.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to perform
   --  this request with
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Height_For_Width
      (Self           : access Gtk_Cell_Area_Record;
       Context        : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Gint;
       Minimum_Height : out Gint;
       Natural_Height : out Gint);
   --  Retrieves a cell area's minimum and natural height if it would be given
   --  the specified Width.
   --  Area stores some geometrical information in Context along the way while
   --  calling Gtk.Cell_Area.Get_Preferred_Width. It's important to perform a
   --  series of Gtk.Cell_Area.Get_Preferred_Width requests with Context first
   --  and then call Gtk.Cell_Area.Get_Preferred_Height_For_Width on each cell
   --  area individually to get the height for width of each fully requested
   --  row.
   --  If at some point, the width of a single row changes, it should be
   --  requested with Gtk.Cell_Area.Get_Preferred_Width again and then the full
   --  width of the requested rows checked again with
   --  Gtk.Cell_Area_Context.Get_Preferred_Width.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which has
   --  already been requested for widths.
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "width": the width for which to check the height of this area
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Width
      (Self          : access Gtk_Cell_Area_Record;
       Context       : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Width : out Gint;
       Natural_Width : out Gint);
   --  Retrieves a cell area's initial minimum and natural width.
   --  Area will store some geometrical information in Context along the way,
   --  when requesting sizes over an arbitrary number of rows, its not
   --  important to check the Minimum_Width and Natural_Width of this call but
   --  rather to consult Gtk.Cell_Area_Context.Get_Preferred_Width after a
   --  series of requests.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to perform
   --  this request with
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   procedure Get_Preferred_Width_For_Height
      (Self          : access Gtk_Cell_Area_Record;
       Context       : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Gint;
       Minimum_Width : out Gint;
       Natural_Width : out Gint);
   --  Retrieves a cell area's minimum and natural width if it would be given
   --  the specified Height.
   --  Area stores some geometrical information in Context along the way while
   --  calling Gtk.Cell_Area.Get_Preferred_Height. It's important to perform a
   --  series of Gtk.Cell_Area.Get_Preferred_Height requests with Context first
   --  and then call Gtk.Cell_Area.Get_Preferred_Width_For_Height on each cell
   --  area individually to get the height for width of each fully requested
   --  row.
   --  If at some point, the height of a single row changes, it should be
   --  requested with Gtk.Cell_Area.Get_Preferred_Height again and then the
   --  full height of the requested rows checked again with
   --  Gtk.Cell_Area_Context.Get_Preferred_Height.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which has
   --  already been requested for widths.
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "height": the height for which to check the width of this area
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   function Get_Request_Mode
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   --  Gets whether the area prefers a height-for-width layout or a
   --  width-for-height layout.
   --  Since: gtk+ 3.0

   function Has_Renderer
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean;
   --  Checks if Area contains Renderer.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to check

   procedure Inner_Cell_Area
      (Self       : access Gtk_Cell_Area_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
       Inner_Area : out Gdk.Rectangle.Gdk_Rectangle);
   --  This is a convenience function for Gtk.Cell_Area.Gtk_Cell_Area
   --  implementations to get the inner area where a given
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer will be rendered. It removes any
   --  padding previously added by Gtk.Cell_Area.Request_Renderer.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering onto
   --  "cell_area": the Widget relative coordinates where one of Area's cells
   --  is to be placed
   --  "inner_area": the return location for the inner cell area

   function Is_Activatable
      (Self : access Gtk_Cell_Area_Record) return Boolean;
   --  Returns whether the area can do anything when activated, after applying
   --  new attributes to Area.
   --  Since: gtk+ 3.0

   function Is_Focus_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean;
   --  Returns whether Sibling is one of Renderer's focus siblings (see
   --  Gtk.Cell_Area.Add_Focus_Sibling).
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus
   --  "sibling": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to check against
   --  Renderer's sibling list

   procedure Remove
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Removes Renderer from Area.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to remove from Area

   procedure Remove_Focus_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Removes Sibling from Renderer's focus sibling list (see
   --  Gtk.Cell_Area.Add_Focus_Sibling).
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus
   --  "sibling": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to remove from
   --  Renderer's focus area

   procedure Render
      (Self            : access Gtk_Cell_Area_Record;
       Context         : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cr              : in out Cairo.Cairo_Context;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Paint_Focus     : Boolean);
   --  Renders Area's cells according to Area's layout onto Widget at the
   --  given coordinates.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "cr": the Cairo.Cairo_Context to render with
   --  "background_area": the Widget relative coordinates for Area's
   --  background
   --  "cell_area": the Widget relative coordinates for Area
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State for Area in this
   --  row.
   --  "paint_focus": whether Area should paint focus on focused cells for
   --  focused rows or not.

   procedure Request_Renderer
      (Self         : access Gtk_Cell_Area_Record;
       Renderer     : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Orientation  : Gtk.Enums.Gtk_Orientation;
       Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
       For_Size     : Gint;
       Minimum_Size : out Gint;
       Natural_Size : out Gint);
   --  This is a convenience function for Gtk.Cell_Area.Gtk_Cell_Area
   --  implementations to request size for cell renderers. It's important to
   --  use this function to request size and then use
   --  Gtk.Cell_Area.Inner_Cell_Area at render and event time since this
   --  function will add padding around the cell for focus painting.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to request size for
   --  "orientation": the Gtk.Enums.Gtk_Orientation in which to request size
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering onto
   --  "for_size": the allocation contextual size to request for, or -1 if the
   --  base request for the orientation is to be returned.
   --  "minimum_size": location to store the minimum size, or null
   --  "natural_size": location to store the natural size, or null

   procedure Stop_Editing
      (Self     : access Gtk_Cell_Area_Record;
       Canceled : Boolean);
   --  Explicitly stops the editing of the currently edited cell.
   --  If Canceled is True, the currently edited cell renderer will emit the
   --  ::editing-canceled signal, otherwise the the ::editing-done signal will
   --  be emitted on the current edit widget.
   --  See Gtk.Cell_Area.Get_Edited_Cell and Gtk.Cell_Area.Get_Edit_Widget.
   --  Since: gtk+ 3.0
   --  "canceled": whether editing was canceled.

   procedure Set_Cell_Data_Func
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk.Cell_Layout.Cell_Data_Func);
   --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk.Cell_Layout.Cell_Data_Func to use, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Cell_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function which should set the value of Cell_Layout's cell renderer(s)
      --  as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a GtkTreeIter indicating the row to set the value for
      --  "data": user data passed to Gtk.Cell_Area.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Cell_Layout : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Cell_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Since: gtk+ 2.4
      --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
      --  "func": the Gtk.Cell_Layout.Cell_Data_Func to use, or null
      --  "func_data": user data for Func

   end Set_Cell_Data_Func_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Area
     (Context : access Gtk_Cell_Area_Context_Record)
   return Gtk.Cell_Area.Gtk_Cell_Area;
   --  Fetches the Gtk.Cell_Area.Gtk_Cell_Area this Context was created by.
   --  This is generally unneeded by layouting widgets; however it is important
   --  for the context implementation itself to fetch information about the
   --  area it is being used for.
   --  For instance at GtkCellAreaContextClass.allocate time its important to
   --  know details about any cell spacing that the Gtk.Cell_Area.Gtk_Cell_Area
   --  is configured with in order to compute a proper allocation.
   --  Since: gtk+ 3.0

   function Get_Area
     (Cell_Layout : Gtk_Cell_Layout) return Gtk.Cell_Area.Gtk_Cell_Area;
   --  Returns the underlying Gtk.Cell_Area.Gtk_Cell_Area which might be
   --  Cell_Layout if called on a Gtk.Cell_Area.Gtk_Cell_Area or might be null
   --  if no Gtk.Cell_Area.Gtk_Cell_Area is used by Cell_Layout.
   --  Since: gtk+ 3.0

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   procedure Add_Attribute
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Gint);

   procedure Clear (Cell_Layout : access Gtk_Cell_Area_Record);

   procedure Clear_Attributes
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      ;

   function Get_Cells
      (Cell_Layout : access Gtk_Cell_Area_Record)
       return Glib.Object.Object_Simple_List.GList;

   procedure Pack_End
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Gint);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Cell_Area_Record, Gtk_Cell_Area);
   function "+"
     (Widget : access Gtk_Cell_Area_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Cell_Area
   renames Implements_Buildable.To_Object;

   package Implements_CellLayout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Cell_Area_Record, Gtk_Cell_Area);
   function "+"
     (Widget : access Gtk_Cell_Area_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_CellLayout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Cell_Area
   renames Implements_CellLayout.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Edited_Cell_Property
   --  Type: Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  Flags: read-write
   --  The cell in the area that is currently edited
   --  This property is read-only and only changes as a result of a call
   --  Gtk.Cell_Area.Activate_Cell.
   --
   --  Name: Focus_Cell_Property
   --  Type: Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  Flags: read-write
   --  The cell in the area that currently has focus

   Edited_Cell_Property : constant Glib.Properties.Property_Object;
   Focus_Cell_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "add-editable"
   --     procedure Handler
   --       (Self      : access Gtk_Cell_Area_Record'Class;
   --        Renderer  : Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --        Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
   --        Cell_Area : cairo.RectangleInt;
   --        Path      : UTF8_String);
   --    --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer that started the
   --    --  edited
   --    --  "editable": the Gtk.Cell_Editable.Gtk_Cell_Editable widget to add
   --    --  "cell_area": the Gtk.Widget.Gtk_Widget relative GdkRectangle
   --    --  coordinates where Editable should be added
   --    --  "path": the GtkTreePath string this edit was initiated for
   --  Indicates that editing has started on Renderer and that Editable should
   --  be added to the owning cell-layouting widget at Cell_Area.
   --
   --  "apply-attributes"
   --     procedure Handler
   --       (Self        : access Gtk_Cell_Area_Record'Class;
   --        Model       : Gtk.Tree_Model.Gtk_Tree_Model;
   --        Iter        : TreeIter;
   --        Is_Expander : Boolean;
   --        Is_Expanded : Boolean);
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model to apply the attributes from
   --    --  "iter": the GtkTreeIter indicating which row to apply the attributes of
   --    --  "is_expander": whether the view shows children for this row
   --    --  "is_expanded": whether the view is currently showing the children of
   --    --  this row
   --  This signal is emitted whenever applying attributes to Area from Model
   --
   --  "focus-changed"
   --     procedure Handler
   --       (Self     : access Gtk_Cell_Area_Record'Class;
   --        Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --        Path     : UTF8_String);
   --    --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer that has focus
   --    --  "path": the current GtkTreePath string set for Area
   --  Indicates that focus changed on this Area. This signal is emitted
   --  either as a result of focus handling or event handling.
   --  It's possible that the signal is emitted even if the currently focused
   --  renderer did not change, this is because focus may change to the same
   --  renderer in the same cell area for a different row of data.
   --
   --  "remove-editable"
   --     procedure Handler
   --       (Self     : access Gtk_Cell_Area_Record'Class;
   --        Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --        Editable : Gtk.Cell_Editable.Gtk_Cell_Editable);
   --    --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer that finished
   --    --  editeding
   --    --  "editable": the Gtk.Cell_Editable.Gtk_Cell_Editable widget to remove
   --  Indicates that editing finished on Renderer and that Editable should be
   --  removed from the owning cell-layouting widget.

   Signal_Add_Editable : constant Glib.Signal_Name := "add-editable";
   Signal_Apply_Attributes : constant Glib.Signal_Name := "apply-attributes";
   Signal_Focus_Changed : constant Glib.Signal_Name := "focus-changed";
   Signal_Remove_Editable : constant Glib.Signal_Name := "remove-editable";

private
   Edited_Cell_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("edited-cell");
   Focus_Cell_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("focus-cell");
end Gtk.Cell_Area;
