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
--  Gtk.Icon_View.Gtk_Icon_View provides an alternative view on a
--  Gtk.Tree_Model.Gtk_Tree_Model. It displays the model as a grid of icons
--  with labels. Like Gtk.Tree_View.Gtk_Tree_View, it allows to select one or
--  multiple items (depending on the selection mode, see
--  Gtk.Icon_View.Set_Selection_Mode). In addition to selection with the arrow
--  keys, Gtk.Icon_View.Gtk_Icon_View supports rubberband selection, which is
--  controlled by dragging the pointer.
--
--  Note that if the tree model is backed by an actual tree store (as opposed
--  to a flat list where the mapping to icons is obvious),
--  Gtk.Icon_View.Gtk_Icon_View will only display the first level of the tree
--  and ignore the tree's branches.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> iconview.view ╰── [rubberband] ]|
--
--  GtkIconView has a single CSS node with name iconview and style class
--  .view. For rubberband selection, a subnode with name rubberband is used.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Gdk.Dnd;                 use Gdk.Dnd;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Cell_Area;           use Gtk.Cell_Area;
with Gtk.Cell_Layout;         use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;       use Gtk.Cell_Renderer;
with Gtk.Container;           use Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Scrollable;          use Gtk.Scrollable;
with Gtk.Style;               use Gtk.Style;
with Gtk.Target_List;         use Gtk.Target_List;
with Gtk.Tooltip;             use Gtk.Tooltip;
with Gtk.Tree_Model;          use Gtk.Tree_Model;

package Gtk.Icon_View is

   type Gtk_Icon_View_Record is new Gtk_Container_Record with null record;
   type Gtk_Icon_View is access all Gtk_Icon_View_Record'Class;

   type Gtk_Icon_View_Drop_Position is (
      No_Drop,
      Drop_Into,
      Drop_Left,
      Drop_Right,
      Drop_Above,
      Drop_Below);
   pragma Convention (C, Gtk_Icon_View_Drop_Position);
   --  An enum for determining where a dropped item goes.

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Icon_View_Foreach_Func is access procedure
     (Icon_View : not null access Gtk_Icon_View_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  A function used by Gtk.Icon_View.Selected_Foreach to map all selected
   --  rows. It will be called on every selected row in the view.
   --  "icon_view": a Gtk.Icon_View.Gtk_Icon_View
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row

   type Gtk_Cell_Layout_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
   --  value for

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Icon_View_Drop_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Icon_View_Drop_Position);
   type Property_Gtk_Icon_View_Drop_Position is new Gtk_Icon_View_Drop_Position_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Icon_View : out Gtk_Icon_View);
   procedure Initialize
      (Icon_View : not null access Gtk_Icon_View_Record'Class);
   --  Creates a new Gtk.Icon_View.Gtk_Icon_View widget
   --  Since: gtk+ 2.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Icon_View_New return Gtk_Icon_View;
   --  Creates a new Gtk.Icon_View.Gtk_Icon_View widget
   --  Since: gtk+ 2.6

   procedure Gtk_New_With_Area
      (Icon_View : out Gtk_Icon_View;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area
      (Icon_View : not null access Gtk_Icon_View_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new Gtk.Icon_View.Gtk_Icon_View widget using the specified
   --  Area to layout cells inside the icons.
   --  Since: gtk+ 3.0
   --  Initialize_With_Area does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to use to layout cells

   function Gtk_Icon_View_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Icon_View;
   --  Creates a new Gtk.Icon_View.Gtk_Icon_View widget using the specified
   --  Area to layout cells inside the icons.
   --  Since: gtk+ 3.0
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to use to layout cells

   procedure Gtk_New_With_Model
      (Icon_View : out Gtk_Icon_View;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   procedure Initialize_With_Model
      (Icon_View : not null access Gtk_Icon_View_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Creates a new Gtk.Icon_View.Gtk_Icon_View widget with the model Model.
   --  Since: gtk+ 2.6
   --  Initialize_With_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "model": The model.

   function Gtk_Icon_View_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Icon_View;
   --  Creates a new Gtk.Icon_View.Gtk_Icon_View widget with the model Model.
   --  Since: gtk+ 2.6
   --  "model": The model.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_view_get_type");

   -------------
   -- Methods --
   -------------

   procedure Convert_Widget_To_Bin_Window_Coords
      (Icon_View : not null access Gtk_Icon_View_Record;
       Wx        : Glib.Gint;
       Wy        : Glib.Gint;
       Bx        : out Glib.Gint;
       By        : out Glib.Gint);
   --  Converts widget coordinates to coordinates for the bin_window, as
   --  expected by e.g. Gtk.Icon_View.Get_Path_At_Pos.
   --  Since: gtk+ 2.12
   --  "wx": X coordinate relative to the widget
   --  "wy": Y coordinate relative to the widget
   --  "bx": return location for bin_window X coordinate
   --  "by": return location for bin_window Y coordinate

   function Create_Drag_Icon
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Cairo.Cairo_Surface;
   --  Creates a cairo_surface_t representation of the item at Path. This
   --  image is used for a drag icon.
   --  Since: gtk+ 2.8
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path in Icon_View

   function Get_Activate_On_Single_Click
      (Icon_View : not null access Gtk_Icon_View_Record) return Boolean;
   --  Gets the setting set by Gtk.Icon_View.Set_Activate_On_Single_Click.
   --  Since: gtk+ 3.8

   procedure Set_Activate_On_Single_Click
      (Icon_View : not null access Gtk_Icon_View_Record;
       Single    : Boolean);
   --  Causes the Gtk.Icon_View.Gtk_Icon_View::item-activated signal to be
   --  emitted on a single click instead of a double click.
   --  Since: gtk+ 3.8
   --  "single": True to emit item-activated on a single click

   function Get_Cell_Rect
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Rect      : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  Fills the bounding rectangle in widget coordinates for the cell
   --  specified by Path and Cell. If Cell is null the main cell area is used.
   --  This function is only valid if Icon_View is realized.
   --  Since: gtk+ 3.6
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer or null
   --  "rect": rectangle to fill with cell rect

   function Get_Column_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::column-spacing property.
   --  Since: gtk+ 2.6

   procedure Set_Column_Spacing
      (Icon_View      : not null access Gtk_Icon_View_Record;
       Column_Spacing : Glib.Gint);
   --  Sets the ::column-spacing property which specifies the space which is
   --  inserted between the columns of the icon view.
   --  Since: gtk+ 2.6
   --  "column_spacing": the column spacing

   function Get_Columns
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::columns property.
   --  Since: gtk+ 2.6

   procedure Set_Columns
      (Icon_View : not null access Gtk_Icon_View_Record;
       Columns   : Glib.Gint);
   --  Sets the ::columns property which determines in how many columns the
   --  icons are arranged. If Columns is -1, the number of columns will be
   --  chosen automatically to fill the available area.
   --  Since: gtk+ 2.6
   --  "columns": the number of columns

   procedure Get_Cursor
      (Icon_View     : not null access Gtk_Icon_View_Record;
       Path          : out Gtk.Tree_Model.Gtk_Tree_Path;
       Cell          : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
       Cursor_Is_Set : out Boolean);
   --  Fills in Path and Cell with the current cursor path and cell. If the
   --  cursor isn't currently set, then *Path will be null. If no cell
   --  currently has focus, then *Cell will be null.
   --  The returned Gtk.Tree_Model.Gtk_Tree_Path must be freed with
   --  Gtk.Tree_Model.Path_Free.
   --  Since: gtk+ 2.8
   --  "path": Return location for the current cursor path, or null
   --  "cell": Return location the current focus cell, or null

   procedure Set_Cursor
      (Icon_View     : not null access Gtk_Icon_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Cell          : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Start_Editing : Boolean);
   --  Sets the current keyboard focus to be at Path, and selects it. This is
   --  useful when you want to focus the user's attention on a particular item.
   --  If Cell is not null, then focus is given to the cell specified by it.
   --  Additionally, if Start_Editing is True, then editing should be started
   --  in the specified cell.
   --  This function is often followed by `gtk_widget_grab_focus (icon_view)`
   --  in order to give keyboard focus to the widget. Please note that editing
   --  can only happen when the widget is realized.
   --  Since: gtk+ 2.8
   --  "path": A Gtk.Tree_Model.Gtk_Tree_Path
   --  "cell": One of the cell renderers of Icon_View, or null
   --  "start_editing": True if the specified cell should start being edited.

   procedure Get_Dest_Item_At_Pos
      (Icon_View : not null access Gtk_Icon_View_Record;
       Drag_X    : Glib.Gint;
       Drag_Y    : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : out Gtk_Icon_View_Drop_Position;
       Has_Item  : out Boolean);
   --  Determines the destination item for a given position.
   --  Since: gtk+ 2.8
   --  "drag_x": the position to determine the destination item for
   --  "drag_y": the position to determine the destination item for
   --  "path": Return location for the path of the item, or null.
   --  "pos": Return location for the drop position, or null

   procedure Get_Drag_Dest_Item
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : out Gtk_Icon_View_Drop_Position);
   --  Gets information about the item that is highlighted for feedback.
   --  Since: gtk+ 2.8
   --  "path": Return location for the path of the highlighted item, or null.
   --  "pos": Return location for the drop position, or null

   procedure Set_Drag_Dest_Item
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : Gtk_Icon_View_Drop_Position);
   --  Sets the item that is highlighted for feedback.
   --  Since: gtk+ 2.8
   --  "path": The path of the item to highlight, or null.
   --  "pos": Specifies where to drop, relative to the item

   procedure Get_Item_At_Pos
      (Icon_View : not null access Gtk_Icon_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Cell      : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
       Has_Item  : out Boolean);
   --  Finds the path at the point (X, Y), relative to bin_window coordinates.
   --  In contrast to Gtk.Icon_View.Get_Path_At_Pos, this function also obtains
   --  the cell at the specified position. The returned path should be freed
   --  with Gtk.Tree_Model.Path_Free. See
   --  Gtk.Icon_View.Convert_Widget_To_Bin_Window_Coords for converting widget
   --  coordinates to bin_window coordinates.
   --  Since: gtk+ 2.8
   --  "x": The x position to be identified
   --  "y": The y position to be identified
   --  "path": Return location for the path, or null
   --  "cell": Return location for the renderer responsible for the cell at
   --  (X, Y), or null

   function Get_Item_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Glib.Gint;
   --  Gets the column in which the item Path is currently displayed. Column
   --  numbers start at 0.
   --  Since: gtk+ 2.22
   --  "path": the Gtk.Tree_Model.Gtk_Tree_Path of the item

   function Get_Item_Orientation
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Orientation;
   --  Returns the value of the ::item-orientation property which determines
   --  whether the labels are drawn beside the icons instead of below.
   --  Since: gtk+ 2.6

   procedure Set_Item_Orientation
      (Icon_View   : not null access Gtk_Icon_View_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Sets the ::item-orientation property which determines whether the
   --  labels are drawn beside the icons instead of below.
   --  Since: gtk+ 2.6
   --  "orientation": the relative position of texts and icons

   function Get_Item_Padding
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::item-padding property.
   --  Since: gtk+ 2.18

   procedure Set_Item_Padding
      (Icon_View    : not null access Gtk_Icon_View_Record;
       Item_Padding : Glib.Gint);
   --  Sets the Gtk.Icon_View.Gtk_Icon_View:item-padding property which
   --  specifies the padding around each of the icon view's items.
   --  Since: gtk+ 2.18
   --  "item_padding": the item padding

   function Get_Item_Row
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Glib.Gint;
   --  Gets the row in which the item Path is currently displayed. Row numbers
   --  start at 0.
   --  Since: gtk+ 2.22
   --  "path": the Gtk.Tree_Model.Gtk_Tree_Path of the item

   function Get_Item_Width
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::item-width property.
   --  Since: gtk+ 2.6

   procedure Set_Item_Width
      (Icon_View  : not null access Gtk_Icon_View_Record;
       Item_Width : Glib.Gint);
   --  Sets the ::item-width property which specifies the width to use for
   --  each item. If it is set to -1, the icon view will automatically
   --  determine a suitable item size.
   --  Since: gtk+ 2.6
   --  "item_width": the width for each item

   function Get_Margin
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::margin property.
   --  Since: gtk+ 2.6

   procedure Set_Margin
      (Icon_View : not null access Gtk_Icon_View_Record;
       Margin    : Glib.Gint);
   --  Sets the ::margin property which specifies the space which is inserted
   --  at the top, bottom, left and right of the icon view.
   --  Since: gtk+ 2.6
   --  "margin": the margin

   function Get_Markup_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the column with markup text for Icon_View.
   --  Since: gtk+ 2.6

   procedure Set_Markup_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint);
   --  Sets the column with markup information for Icon_View to be Column. The
   --  markup column must be of type G_TYPE_STRING. If the markup column is set
   --  to something, it overrides the text column set by
   --  Gtk.Icon_View.Set_Text_Column.
   --  Since: gtk+ 2.6
   --  "column": A column in the currently used model, or -1 to display no
   --  text

   function Get_Model
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model the Gtk.Icon_View.Gtk_Icon_View is based on. Returns
   --  null if the model is unset.
   --  Since: gtk+ 2.6

   procedure Set_Model
      (Icon_View : not null access Gtk_Icon_View_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model for a Gtk.Icon_View.Gtk_Icon_View. If the Icon_View
   --  already has a model set, it will remove it before setting the new model.
   --  If Model is null, then it will unset the old model.
   --  Since: gtk+ 2.6
   --  "model": The model.

   function Get_Path_At_Pos
      (Icon_View : not null access Gtk_Icon_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Finds the path at the point (X, Y), relative to bin_window coordinates.
   --  See Gtk.Icon_View.Get_Item_At_Pos, if you are also interested in the
   --  cell at the specified position. See
   --  Gtk.Icon_View.Convert_Widget_To_Bin_Window_Coords for converting widget
   --  coordinates to bin_window coordinates.
   --  Since: gtk+ 2.6
   --  "x": The x position to be identified
   --  "y": The y position to be identified

   function Get_Pixbuf_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the column with pixbufs for Icon_View.
   --  Since: gtk+ 2.6

   procedure Set_Pixbuf_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint);
   --  Sets the column with pixbufs for Icon_View to be Column. The pixbuf
   --  column must be of type GDK_TYPE_PIXBUF
   --  Since: gtk+ 2.6
   --  "column": A column in the currently used model, or -1 to disable

   function Get_Reorderable
      (Icon_View : not null access Gtk_Icon_View_Record) return Boolean;
   --  Retrieves whether the user can reorder the list via drag-and-drop. See
   --  Gtk.Icon_View.Set_Reorderable.
   --  Since: gtk+ 2.8

   procedure Set_Reorderable
      (Icon_View   : not null access Gtk_Icon_View_Record;
       Reorderable : Boolean);
   --  This function is a convenience function to allow you to reorder models
   --  that support the Gtk_Tree_Drag_Source_Iface and the
   --  Gtk_Tree_Drag_Dest_Iface. Both Gtk.Tree_Store.Gtk_Tree_Store and
   --  Gtk.List_Store.Gtk_List_Store support these. If Reorderable is True,
   --  then the user can reorder the model by dragging and dropping rows. The
   --  developer can listen to these changes by connecting to the model's
   --  row_inserted and row_deleted signals. The reordering is implemented by
   --  setting up the icon view as a drag source and destination. Therefore,
   --  drag and drop can not be used in a reorderable view for any other
   --  purpose.
   --  This function does not give you any degree of control over the order --
   --  any reordering is allowed. If more control is needed, you should
   --  probably handle drag and drop manually.
   --  Since: gtk+ 2.8
   --  "reorderable": True, if the list of items can be reordered.

   function Get_Row_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::row-spacing property.
   --  Since: gtk+ 2.6

   procedure Set_Row_Spacing
      (Icon_View   : not null access Gtk_Icon_View_Record;
       Row_Spacing : Glib.Gint);
   --  Sets the ::row-spacing property which specifies the space which is
   --  inserted between the rows of the icon view.
   --  Since: gtk+ 2.6
   --  "row_spacing": the row spacing

   function Get_Selected_Items
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
   --  Creates a list of paths of all selected items. Additionally, if you are
   --  planning on modifying the model after calling this function, you may
   --  want to convert the returned list into a list of
   --  Gtk_Tree_Row_References. To do this, you can use
   --  gtk_tree_row_reference_new.
   --  To free the return value, use: |[<!-- language="C" --> g_list_free_full
   --  (list, (GDestroyNotify) gtk_tree_path_free); ]|
   --  Since: gtk+ 2.6

   function Get_Selection_Mode
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   --  Gets the selection mode of the Icon_View.
   --  Since: gtk+ 2.6

   procedure Set_Selection_Mode
      (Icon_View : not null access Gtk_Icon_View_Record;
       Mode      : Gtk.Enums.Gtk_Selection_Mode);
   --  Sets the selection mode of the Icon_View.
   --  Since: gtk+ 2.6
   --  "mode": The selection mode

   function Get_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the value of the ::spacing property.
   --  Since: gtk+ 2.6

   procedure Set_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record;
       Spacing   : Glib.Gint);
   --  Sets the ::spacing property which specifies the space which is inserted
   --  between the cells (i.e. the icon and the text) of an item.
   --  Since: gtk+ 2.6
   --  "spacing": the spacing

   function Get_Text_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the column with text for Icon_View.
   --  Since: gtk+ 2.6

   procedure Set_Text_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint);
   --  Sets the column with text for Icon_View to be Column. The text column
   --  must be of type G_TYPE_STRING.
   --  Since: gtk+ 2.6
   --  "column": A column in the currently used model, or -1 to display no
   --  text

   function Get_Tooltip_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint;
   --  Returns the column of Icon_View's model which is being used for
   --  displaying tooltips on Icon_View's rows.
   --  Since: gtk+ 2.12

   procedure Set_Tooltip_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint);
   --  If you only plan to have simple (text-only) tooltips on full items, you
   --  can use this function to have Gtk.Icon_View.Gtk_Icon_View handle these
   --  automatically for you. Column should be set to the column in Icon_View's
   --  model containing the tooltip texts, or -1 to disable this feature.
   --  When enabled, Gtk.Widget.Gtk_Widget:has-tooltip will be set to True and
   --  Icon_View will connect a Gtk.Widget.Gtk_Widget::query-tooltip signal
   --  handler.
   --  Note that the signal handler sets the text with Gtk.Tooltip.Set_Markup,
   --  so &, <, etc have to be escaped in the text.
   --  Since: gtk+ 2.12
   --  "column": an integer, which is a valid column number for Icon_View's
   --  model

   procedure Get_Tooltip_Context
      (Icon_View    : not null access Gtk_Icon_View_Record;
       X            : in out Glib.Gint;
       Y            : in out Glib.Gint;
       Keyboard_Tip : Boolean;
       Model        : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Iter         : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Success      : out Boolean);
   --  This function is supposed to be used in a
   --  Gtk.Widget.Gtk_Widget::query-tooltip signal handler for
   --  Gtk.Icon_View.Gtk_Icon_View. The X, Y and Keyboard_Tip values which are
   --  received in the signal handler, should be passed to this function
   --  without modification.
   --  The return value indicates whether there is an icon view item at the
   --  given coordinates (True) or not (False) for mouse tooltips. For keyboard
   --  tooltips the item returned will be the cursor item. When True, then any
   --  of Model, Path and Iter which have been provided will be set to point to
   --  that row and the corresponding model. X and Y will always be converted
   --  to be relative to Icon_View's bin_window if Keyboard_Tooltip is False.
   --  Since: gtk+ 2.12
   --  "x": the x coordinate (relative to widget coordinates)
   --  "y": the y coordinate (relative to widget coordinates)
   --  "keyboard_tip": whether this is a keyboard tooltip or not
   --  "model": a pointer to receive a Gtk.Tree_Model.Gtk_Tree_Model or null
   --  "path": a pointer to receive a Gtk.Tree_Model.Gtk_Tree_Path or null
   --  "iter": a pointer to receive a Gtk.Tree_Model.Gtk_Tree_Iter or null

   procedure Get_Visible_Range
      (Icon_View  : not null access Gtk_Icon_View_Record;
       Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets Start_Path and End_Path to be the first and last visible path.
   --  Note that there may be invisible paths in between.
   --  Both paths should be freed with Gtk.Tree_Model.Path_Free after use.
   --  Since: gtk+ 2.8
   --  "start_path": Return location for start of region, or null
   --  "end_path": Return location for end of region, or null

   procedure Item_Activated
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Activates the item determined by Path.
   --  Since: gtk+ 2.6
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path to be activated

   function Path_Is_Selected
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Returns True if the icon pointed to by Path is currently selected. If
   --  Path does not point to a valid location, False is returned.
   --  Since: gtk+ 2.6
   --  "path": A Gtk.Tree_Model.Gtk_Tree_Path to check selection on.

   procedure Scroll_To_Path
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Use_Align : Boolean;
       Row_Align : Gfloat;
       Col_Align : Gfloat);
   --  Moves the alignments of Icon_View to the position specified by Path.
   --  Row_Align determines where the row is placed, and Col_Align determines
   --  where Column is placed. Both are expected to be between 0.0 and 1.0. 0.0
   --  means left/top alignment, 1.0 means right/bottom alignment, 0.5 means
   --  center.
   --  If Use_Align is False, then the alignment arguments are ignored, and
   --  the tree does the minimum amount of work to scroll the item onto the
   --  screen. This means that the item will be scrolled to the edge closest to
   --  its current position. If the item is currently visible on the screen,
   --  nothing is done.
   --  This function only works if the model is set, and Path is a valid row
   --  on the model. If the model changes before the Icon_View is realized, the
   --  centered path will be modified to reflect this change.
   --  Since: gtk+ 2.8
   --  "path": The path of the item to move to.
   --  "use_align": whether to use alignment arguments, or False.
   --  "row_align": The vertical alignment of the item specified by Path.
   --  "col_align": The horizontal alignment of the item specified by Path.

   procedure Select_All (Icon_View : not null access Gtk_Icon_View_Record);
   --  Selects all the icons. Icon_View must has its selection mode set to
   --  GTK_SELECTION_MULTIPLE.
   --  Since: gtk+ 2.6

   procedure Select_Path
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Selects the row at Path.
   --  Since: gtk+ 2.6
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path to be selected.

   procedure Selected_Foreach
      (Icon_View : not null access Gtk_Icon_View_Record;
       Func      : Gtk_Icon_View_Foreach_Func);
   --  Calls a function for each selected icon. Note that the model or
   --  selection cannot be modified from within this function.
   --  Since: gtk+ 2.6
   --  "func": The function to call for each selected icon.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Selected_Foreach_User_Data is

      type Gtk_Icon_View_Foreach_Func is access procedure
        (Icon_View : not null access Gtk.Icon_View.Gtk_Icon_View_Record'Class;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Data      : User_Data_Type);
      --  A function used by Gtk.Icon_View.Selected_Foreach to map all selected
      --  rows. It will be called on every selected row in the view.
      --  "icon_view": a Gtk.Icon_View.Gtk_Icon_View
      --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
      --  "data": user data

      procedure Selected_Foreach
         (Icon_View : not null access Gtk.Icon_View.Gtk_Icon_View_Record'Class;
          Func      : Gtk_Icon_View_Foreach_Func;
          Data      : User_Data_Type);
      --  Calls a function for each selected icon. Note that the model or
      --  selection cannot be modified from within this function.
      --  Since: gtk+ 2.6
      --  "func": The function to call for each selected icon.
      --  "data": User data to pass to the function.

   end Selected_Foreach_User_Data;

   procedure Set_Tooltip_Cell
      (Icon_View : not null access Gtk_Icon_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Sets the tip area of Tooltip to the area which Cell occupies in the
   --  item pointed to by Path. See also Gtk.Tooltip.Set_Tip_Area.
   --  See also Gtk.Icon_View.Set_Tooltip_Column for a simpler alternative.
   --  Since: gtk+ 2.12
   --  "tooltip": a Gtk.Tooltip.Gtk_Tooltip
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer or null

   procedure Set_Tooltip_Item
      (Icon_View : not null access Gtk_Icon_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets the tip area of Tooltip to be the area covered by the item at
   --  Path. See also Gtk.Icon_View.Set_Tooltip_Column for a simpler
   --  alternative. See also Gtk.Tooltip.Set_Tip_Area.
   --  Since: gtk+ 2.12
   --  "tooltip": a Gtk.Tooltip.Gtk_Tooltip
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path

   procedure Unselect_All (Icon_View : not null access Gtk_Icon_View_Record);
   --  Unselects all the icons.
   --  Since: gtk+ 2.6

   procedure Unselect_Path
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Unselects the row at Path.
   --  Since: gtk+ 2.6
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path to be unselected.

   procedure Unset_Model_Drag_Dest
      (Icon_View : not null access Gtk_Icon_View_Record);
   --  Undoes the effect of gtk_icon_view_enable_model_drag_dest. Calling this
   --  method sets Gtk.Icon_View.Gtk_Icon_View:reorderable to False.
   --  Since: gtk+ 2.8

   procedure Unset_Model_Drag_Source
      (Icon_View : not null access Gtk_Icon_View_Record);
   --  Undoes the effect of gtk_icon_view_enable_model_drag_source. Calling
   --  this method sets Gtk.Icon_View.Gtk_Icon_View:reorderable to False.
   --  Since: gtk+ 2.8

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk_Cell_Layout_Data_Func);
   --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk_Cell_Layout_Data_Func to use, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Gtk_Cell_Layout_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function which should set the value of Cell_Layout's cell renderer(s)
      --  as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Cell_Layout : not null access Gtk.Icon_View.Gtk_Icon_View_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Gtk_Cell_Layout_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Since: gtk+ 2.4
      --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
      --  "func": the Gtk_Cell_Layout_Data_Func to use, or null
      --  "func_data": user data for Func

   end Set_Cell_Data_Func_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Enable_Model_Drag_Dest
     (Icon_View : access Gtk_Icon_View_Record;
      Targets   : Gtk.Target_List.Target_Entry_Array;
      Actions   : Gdk.Dnd.Drag_Action);
   --  Turns Icon_view into a drop destination for automatic DND.
   --  Targets is the list of targets that the drag will support.

   procedure Enable_Model_Drag_Source
     (Icon_View         : access Gtk_Icon_View_Record;
      Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
      Targets           : Gtk.Target_List.Target_Entry_Array;
      Actions           : Gdk.Dnd.Drag_Action);
   --  Turns Icon_view into a drag source for automatic DND.
   --  Start_Button_Mask is the allowed buttons to start drag.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear (Cell_Layout : not null access Gtk_Icon_View_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Icon_View_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   function Get_Border
      (Self   : not null access Gtk_Icon_View_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean;

   function Get_Hadjustment
      (Self : not null access Gtk_Icon_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Icon_View_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Icon_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   function Get_Vadjustment
      (Self : not null access Gtk_Icon_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Icon_View_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Icon_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean;
   --  The activate-on-single-click property specifies whether the
   --  "item-activated" signal will be emitted after a single click.

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The Gtk.Cell_Area.Gtk_Cell_Area used to layout cell renderers for this
   --  view.
   --
   --  If no area is specified when creating the icon view with
   --  Gtk.Icon_View.Gtk_New_With_Area a Gtk.Cell_Area_Box.Gtk_Cell_Area_Box
   --  will be used.

   Column_Spacing_Property : constant Glib.Properties.Property_Int;
   --  The column-spacing property specifies the space which is inserted
   --  between the columns of the icon view.

   Columns_Property : constant Glib.Properties.Property_Int;
   --  The columns property contains the number of the columns in which the
   --  items should be displayed. If it is -1, the number of columns will be
   --  chosen automatically to fill the available area.

   Item_Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation;
   --  The item-orientation property specifies how the cells (i.e. the icon
   --  and the text) of the item are positioned relative to each other.

   Item_Padding_Property : constant Glib.Properties.Property_Int;
   --  The item-padding property specifies the padding around each of the icon
   --  view's item.

   Item_Width_Property : constant Glib.Properties.Property_Int;
   --  The item-width property specifies the width to use for each item. If it
   --  is set to -1, the icon view will automatically determine a suitable item
   --  size.

   Margin_Property : constant Glib.Properties.Property_Int;
   --  The margin property specifies the space which is inserted at the edges
   --  of the icon view.

   Markup_Column_Property : constant Glib.Properties.Property_Int;
   --  The ::markup-column property contains the number of the model column
   --  containing markup information to be displayed. The markup column must be
   --  of type G_TYPE_STRING. If this property and the :text-column property
   --  are both set to column numbers, it overrides the text column. If both
   --  are set to -1, no texts are displayed.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model

   Pixbuf_Column_Property : constant Glib.Properties.Property_Int;
   --  The ::pixbuf-column property contains the number of the model column
   --  containing the pixbufs which are displayed. The pixbuf column must be of
   --  type GDK_TYPE_PIXBUF. Setting this property to -1 turns off the display
   --  of pixbufs.

   Reorderable_Property : constant Glib.Properties.Property_Boolean;
   --  The reorderable property specifies if the items can be reordered by
   --  DND.

   Row_Spacing_Property : constant Glib.Properties.Property_Int;
   --  The row-spacing property specifies the space which is inserted between
   --  the rows of the icon view.

   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;
   --  The ::selection-mode property specifies the selection mode of icon
   --  view. If the mode is GTK_SELECTION_MULTIPLE, rubberband selection is
   --  enabled, for the other modes, only keyboard selection is possible.

   Spacing_Property : constant Glib.Properties.Property_Int;
   --  The spacing property specifies the space which is inserted between the
   --  cells (i.e. the icon and the text) of an item.

   Text_Column_Property : constant Glib.Properties.Property_Int;
   --  The ::text-column property contains the number of the model column
   --  containing the texts which are displayed. The text column must be of
   --  type G_TYPE_STRING. If this property and the :markup-column property are
   --  both set to -1, no texts are displayed.

   Tooltip_Column_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Icon_View_Boolean is not null access function
     (Self : access Gtk_Icon_View_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Activate_Cursor_Item : constant Glib.Signal_Name := "activate-cursor-item";
   procedure On_Activate_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Boolean;
       After : Boolean := False);
   procedure On_Activate_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A [keybinding signal][GtkBindingSignal] which gets emitted when the
   --  user activates the currently focused item.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control activation
   --  programmatically.
   --
   --  The default bindings for this signal are Space, Return and Enter.

   type Cb_Gtk_Icon_View_Gtk_Tree_Path_Void is not null access procedure
     (Self : access Gtk_Icon_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   type Cb_GObject_Gtk_Tree_Path_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   Signal_Item_Activated : constant Glib.Signal_Name := "item-activated";
   procedure On_Item_Activated
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Gtk_Tree_Path_Void;
       After : Boolean := False);
   procedure On_Item_Activated
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::item-activated signal is emitted when the method
   --  Gtk.Icon_View.Item_Activated is called, when the user double clicks an
   --  item with the "activate-on-single-click" property set to False, or when
   --  the user single clicks an item when the "activate-on-single-click"
   --  property set to True. It is also emitted when a non-editable item is
   --  selected and one of the keys: Space, Return or Enter is pressed.

   type Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean is not null access function
     (Self  : access Gtk_Icon_View_Record'Class;
      Step  : Gtk.Enums.Gtk_Movement_Step;
      Count : Glib.Gint) return Boolean;

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Step  : Gtk.Enums.Gtk_Movement_Step;
      Count : Glib.Gint) return Boolean;

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::move-cursor signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates a cursor movement.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal include - Arrow keys which move by
   --  individual steps - Home/End keys which move to the first/last item -
   --  PageUp/PageDown which move by "pages" All of these will extend the
   --  selection when combined with the Shift modifier.
   -- 
   --  Callback parameters:
   --    --  "step": the granularity of the move, as a Gtk.Enums.Gtk_Movement_Step
   --    --  "count": the number of Step units to move

   type Cb_Gtk_Icon_View_Void is not null access procedure (Self : access Gtk_Icon_View_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Select_All : constant Glib.Signal_Name := "select-all";
   procedure On_Select_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False);
   procedure On_Select_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A [keybinding signal][GtkBindingSignal] which gets emitted when the
   --  user selects all items.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control selection
   --  programmatically.
   --
   --  The default binding for this signal is Ctrl-a.

   Signal_Select_Cursor_Item : constant Glib.Signal_Name := "select-cursor-item";
   procedure On_Select_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False);
   procedure On_Select_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A [keybinding signal][GtkBindingSignal] which gets emitted when the
   --  user selects the item that is currently focused.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control selection
   --  programmatically.
   --
   --  There is no default binding for this signal.

   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";
   procedure On_Selection_Changed
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False);
   procedure On_Selection_Changed
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::selection-changed signal is emitted when the selection (i.e. the
   --  set of selected items) changes.

   Signal_Toggle_Cursor_Item : constant Glib.Signal_Name := "toggle-cursor-item";
   procedure On_Toggle_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False);
   procedure On_Toggle_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A [keybinding signal][GtkBindingSignal] which gets emitted when the
   --  user toggles whether the currently focused item is selected or not. The
   --  exact effect of this depend on the selection mode.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control selection
   --  programmatically.
   --
   --  There is no default binding for this signal is Ctrl-Space.

   Signal_Unselect_All : constant Glib.Signal_Name := "unselect-all";
   procedure On_Unselect_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False);
   procedure On_Unselect_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A [keybinding signal][GtkBindingSignal] which gets emitted when the
   --  user unselects all items.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control selection
   --  programmatically.
   --
   --  The default binding for this signal is Ctrl-Shift-a.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"
   --
   --  - "Scrollable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Icon_View_Record, Gtk_Icon_View);
   function "+"
     (Widget : access Gtk_Icon_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Icon_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Icon_View_Record, Gtk_Icon_View);
   function "+"
     (Widget : access Gtk_Icon_View_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Icon_View
   renames Implements_Gtk_Cell_Layout.To_Object;

   package Implements_Gtk_Scrollable is new Glib.Types.Implements
     (Gtk.Scrollable.Gtk_Scrollable, Gtk_Icon_View_Record, Gtk_Icon_View);
   function "+"
     (Widget : access Gtk_Icon_View_Record'Class)
   return Gtk.Scrollable.Gtk_Scrollable
   renames Implements_Gtk_Scrollable.To_Interface;
   function "-"
     (Interf : Gtk.Scrollable.Gtk_Scrollable)
   return Gtk_Icon_View
   renames Implements_Gtk_Scrollable.To_Object;

private
   Tooltip_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("tooltip-column");
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode :=
     Gtk.Enums.Build ("selection-mode");
   Row_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-spacing");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Pixbuf_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixbuf-column");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Markup_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("markup-column");
   Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin");
   Item_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("item-width");
   Item_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("item-padding");
   Item_Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Build ("item-orientation");
   Columns_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("columns");
   Column_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-spacing");
   Cell_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area");
   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activate-on-single-click");
end Gtk.Icon_View;
