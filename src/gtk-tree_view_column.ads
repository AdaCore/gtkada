-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gdk.Rectangle;
with Gdk.Window;
with Gtk;
with Gtk.Cell_Renderer;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object;
with Gtk.Tree_Model;
with Gtk.Widget;

with Glib.Glist;
with Unchecked_Conversion;

package Gtk.Tree_View_Column is

   type Gtk_Tree_View_Column_Record is
     new Gtk.Object.Gtk_Object_Record with private;
   type Gtk_Tree_View_Column is access all Gtk_Tree_View_Column_Record'Class;

   function Convert is new Unchecked_Conversion
     (Gtk_Tree_View_Column, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Column);
   package Column_List is
      new Glib.Glist.Generic_List (Gtk_Tree_View_Column);

   type Gtk_Tree_View_Column_Sizing is
     (Tree_View_Column_Grow_Only,
      Tree_View_Column_Resizable,
      Tree_View_Column_Autosize,
      Tree_View_Column_Fixed);

   procedure Gtk_New (Widget : out Gtk_Tree_View_Column);
   --  Creates a new Gtk_Tree_View_Column.

   procedure Initialize (Widget : access Gtk_Tree_View_Column_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Pack_Start
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean);

   procedure Pack_End
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean);

   procedure Clear (Tree_Column : access Gtk_Tree_View_Column_Record);

   function Get_Cell_Renderers
     (Tree_Column : access Gtk_Tree_View_Column_Record)
     return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Add_Attribute
     (Tree_Column   : access Gtk_Tree_View_Column_Record;
      Cell_Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Attribute     : String;
      Column        : Gint);
   --  Adds an attribute mapping to the list in Tree_Column.  The Column is
   --  the column of the model to get a value from, and the Attribute is the
   --  parameter on Cell_Renderer to be set from the value. So for example if
   --  column 2 of the model contains strings, you could have the "text"
   --  attribute of a Gtk_Cell_Renderer_Text get its values from column 2.
   --
   --  For a list of properties available for each Cell_Renderer, please
   --  refer to the corresponding packages specifications.

--    procedure Set_Cell_Data_Func
--      (Tree_Column   : access Gtk_Tree_View_Column_Record;
--       Cell_Renderer :
--         access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
--       Func          : Gtk_Tree_Cell_Data_Func;
--       Func_Data     : gpointer;
--       Destroy       : Gtk_Destroy_Notify);

   procedure Clear_Attributes
     (Tree_Column   : access Gtk_Tree_View_Column_Record;
      Cell_Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Clears all existing attributes previously set with
   --  Gtk.Tree_View_Column.Set_Attributes.

   procedure Set_Spacing
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Spacing     : Gint);
   --  Sets the spacing field of Tree_Column, which is the number of pixels to
   --  place between cell renderers packed into it.

   function Get_Spacing (Tree_Column : access Gtk_Tree_View_Column_Record)
                         return Gint;
   --  Returns the spacing of Tree_Column.

   ------------------------------------------
   -- Options for manipulating the columns --
   ------------------------------------------

   procedure Set_Visible
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Visible     : Boolean);
   --  Sets the visibility of Tree_Column.

   function Get_Visible (Tree_Column : access Gtk_Tree_View_Column_Record)
                         return Boolean;
   --  Returns True if Tree_Column is visible.

   procedure Set_Sizing
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      The_Type    : Gtk_Tree_View_Column_Sizing);
   --  Sets the growth behavior of Tree_Column to The_Type.

   function Get_Sizing (Tree_Column : access Gtk_Tree_View_Column_Record)
                        return Gtk_Tree_View_Column_Sizing;
   --  Returns the current type of Tree_Column.

   function Get_Width (Tree_Column : access Gtk_Tree_View_Column_Record)
                       return Gint;
   --  Returns the current size of the Tree_Column in pixels.

   function Get_Fixed_Width (Tree_Column : access Gtk_Tree_View_Column_Record)
                             return Gint;
   --  Gets the fixed width of the column.  This value is only meaning may not
   --  be the actual width of the column on the screen, just what is requested.

   procedure Set_Fixed_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Fixed_Width : Gint);
   --  Sets the size of the column in pixels.  This is meaningful only if the
   --  sizing type is Gtk_Tree_View_Column_Fixed.  In this case, the value is
   --  discarded as the size of the column is based on the calculated width of
   --  the column. The width is clamped to the min/max width for the column.

   procedure Set_Min_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Min_Width   : Gint);
   --  Sets the minimum width of the Tree_Column.  If Min_Width is -1, then the
   --  minimum width is unset.

   function Get_Min_Width (Tree_Column : access Gtk_Tree_View_Column_Record)
                           return Gint;
   --  Returns the minimum width in pixels of the Tree_Column, or -1 if no
   --  minimum width is set.

   procedure Set_Max_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Max_Width   : Gint);
   --  Sets the maximum width of the Tree_Column.  If Max_Width is -1, then
   --  the maximum width is unset.  Note, the column can actually be wider
   --  than max width if it's the last column in a view.  In this case, the
   --  column expands to fill the view.

   function Get_Max_Width (Tree_Column : access Gtk_Tree_View_Column_Record)
                           return Gint;
   --  Returns the maximum width in pixels of the Tree_Column, or -1 if no
   --  maximum width is set.

   procedure Clicked (Tree_Column : access Gtk_Tree_View_Column_Record);
   --  Emits the "clicked" signal on the column.  This function will only work
   --  if the user could have conceivably clicked on the button.

   procedure Set_Title
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Title       : String);
   --  Sets the title of the Tree_Column.  If a custom widget has been set,
   --  then this value is ignored.

   procedure Set_Clickable
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Clickable   : Boolean);
   --  Sets the header to be active if Active is True.  When the header is
   --  active, then it can take keyboard focus, and can be clicked.

   function Get_Clickable (Tree_Column : access Gtk_Tree_View_Column_Record)
                           return Boolean;
   --  Returns True if the user can click on the header for the column.

   procedure Set_Widget
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Widget      : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Widget (Tree_Column : access Gtk_Tree_View_Column_Record)
                        return Gtk.Widget.Gtk_Widget;
   --  Returns the Gtk_Widget in the button in the column header.  If a custom
   --  widget has not been set, then this will be a Gtk_Alignment with a
   --  Gtk_Label in it.

   procedure Set_Alignment
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Xalign      : Gfloat);
   --  Sets the alignment of the title or custom widget inside the column
   --  header. The alignment determines its location inside the button
   --  -- 0.0 for left, 0.5 for center, 1.0 for right.

   function Get_Alignment (Tree_Column : access Gtk_Tree_View_Column_Record)
                           return Gfloat;
   --  Returns the current x alignment of Tree_Column.  This value can range
   --  between 0.0 and 1.0.

   procedure Set_Reorderable
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Reorderable : Boolean);

   function Get_Reorderable (Tree_Column : access Gtk_Tree_View_Column_Record)
                             return Boolean;

   procedure Set_Sort_Column_Id
     (Tree_Column    : access Gtk_Tree_View_Column_Record;
      Sort_Column_Id : Gint);
   --  Sets the logical sort_column_id that this column sorts on when this
   --  column is selected for sorting.  Doing so makes the column header
   --  clickable.

   function Get_Sort_Column_Id
     (Tree_Column : access Gtk_Tree_View_Column_Record)
     return Gint;
   --  Gets the logical sort_column_id that the model sorts on when this
   --  column is selected for sorting.
   --  See Gtk.Tree_View_Column.Set_Sort_Column_Id.
   --  Return value: the current sort_column_id for this column, or -1 if
   --  this column can't be used for sorting.

   procedure Set_Sort_Indicator
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Setting     : Boolean);
   --  Call this function with a Setting of True to display an arrow in
   --  the header button indicating the column is sorted. Call
   --  gtk_tree_view_column_set_sort_order() to change the direction of
   --  the arrow.

   function Get_Sort_Indicator
     (Tree_Column : access Gtk_Tree_View_Column_Record)
     return Boolean;
   --  Gets the value set by gtk_tree_view_column_set_sort_indicator().

   procedure Set_Sort_Order
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Order       : Gtk_Sort_Type);
   --  Changes the appearance of the sort indicator. (This <emphasis>does
   --  not</emphasis> actually sort the model.  Use
   --  Gtk.Tree_View_Column.Set_Sort_Column_Id if you want automatic sorting
   --  support.  This function is primarily for custom sorting behavior, and
   --  should be used in conjunction with Gtk.Tree_Sortable.Set_Sort_Column
   --  to do that. For custom models, the mechanism will vary. The sort
   --  indicator changes direction to indicate normal sort or reverse sort.
   --  Note that you must have the sort indicator enabled to see anything
   --  when calling this function; see Gtk.Tree_View_Column.Set_Sort_Indicator.

   function Get_Sort_Order (Tree_Column : access Gtk_Tree_View_Column_Record)
                           return Gtk_Sort_Type;
   --  Gets the value set by Gtk.Tree_View_Column.Set_Sort_Order.

   procedure Cell_Set_Cell_Data
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean);
   --  Sets the cell renderer based on the Tree_Model and Tree_Node.
   --  That is, for every attribute mapping in Tree_Column, it will get a
   --  value from the set column on the Tree_Node, and use that value to
   --  set the attribute on the cell renderer.  This is used primarily by
   --  the GtkTreeView.

   procedure Cell_Get_Size
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell_Area   : Gdk.Rectangle.Gdk_Rectangle;
      X_Offset    : out Gint;
      Y_Offset    : out Gint;
      Width       : out Gint;
      Height      : out Gint);
   --  Obtains the width and height needed to render the column.  This is used
   --  primarily by the Gtk_Tree_View.

   procedure Cell_Render
     (Tree_Column     : access Gtk_Tree_View_Column_Record;
      Window          : Gdk.Window.Gdk_Window;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Expose_Area     : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Guint);
   --  Renders the cell contained by Tree_Column. This is used primarily by the
   --  Gtk_Tree_View.

   function Cell_Focus
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Direction   : Gint)
      return Boolean;

   procedure Cell_Draw_Focus
     (Tree_Column     : access Gtk_Tree_View_Column_Record;
      Window          : Gdk.Window.Gdk_Window;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Expose_Area     : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Guint);

   function Cell_Is_Visible (Tree_Column : access Gtk_Tree_View_Column_Record)
                             return Boolean;

   procedure Cell_Set_Dirty (Tree_Column : access Gtk_Tree_View_Column_Record);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler (Widget : access Gtk_Tree_View_Column_Record'Class);
   --
   --  </signals>

private
   type Gtk_Tree_View_Column_Record is
     new Gtk.Object.Gtk_Object_Record with null record;

   pragma Import (C, Get_Type, "gtk_tree_view_column_get_type");
end Gtk.Tree_View_Column;

--  ??? missing :
--
--    function Cell_Event
--      (Tree_Column     : access Gtk_Tree_View_Column_Record;
--       Event           : Gdk.Event.Gdk_Event;
--       Path_String     : String;
--       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
--       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
--       Flags           : Guint)
--       return Boolean;
