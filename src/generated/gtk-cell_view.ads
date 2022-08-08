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
--  A Gtk.Cell_View.Gtk_Cell_View displays a single row of a
--  Gtk.Tree_Model.Gtk_Tree_Model using a Gtk.Cell_Area.Gtk_Cell_Area and
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context. A
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context can be provided to the
--  Gtk.Cell_View.Gtk_Cell_View at construction time in order to keep the
--  cellview in context of a group of cell views, this ensures that the
--  renderers displayed will be properly aligned with eachother (like the
--  aligned cells in the menus of Gtk.Combo_Box.Gtk_Combo_Box).
--
--  Gtk.Cell_View.Gtk_Cell_View is Gtk.Orientable.Gtk_Orientable in order to
--  decide in which orientation the underlying
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context should be allocated. Taking the
--  Gtk.Combo_Box.Gtk_Combo_Box menu as an example, cellviews should be
--  oriented horizontally if the menus are listed top-to-bottom and thus all
--  share the same width but may have separate individual heights
--  (left-to-right menus should be allocated vertically since they all share
--  the same height but may have variable widths).
--
--  # CSS nodes
--
--  GtkCellView has a single CSS node with name cellview.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;             use Gdk.Color;
with Gdk.Pixbuf;            use Gdk.Pixbuf;
with Gdk.RGBA;              use Gdk.RGBA;
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Cell_Area;         use Gtk.Cell_Area;
with Gtk.Cell_Area_Context; use Gtk.Cell_Area_Context;
with Gtk.Cell_Layout;       use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Cell_View is

   type Gtk_Cell_View_Record is new Gtk_Widget_Record with null record;
   type Gtk_Cell_View is access all Gtk_Cell_View_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

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

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Cell_View : out Gtk_Cell_View);
   procedure Initialize
      (Cell_View : not null access Gtk_Cell_View_Record'Class);
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget.
   --  Since: gtk+ 2.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_View_New return Gtk_Cell_View;
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget.
   --  Since: gtk+ 2.6

   procedure Gtk_New_With_Context
      (Cell_View : out Gtk_Cell_View;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class);
   procedure Initialize_With_Context
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class);
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget with a specific
   --  Gtk.Cell_Area.Gtk_Cell_Area to layout cells and a specific
   --  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context.
   --  Specifying the same context for a handfull of cells lets the underlying
   --  area synchronize the geometry for those cells, in this way alignments
   --  with cellviews for other rows are possible.
   --  Since: gtk+ 2.6
   --  Initialize_With_Context does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to layout cells
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context in which to
   --  calculate cell geometry

   function Gtk_Cell_View_New_With_Context
      (Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk_Cell_View;
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget with a specific
   --  Gtk.Cell_Area.Gtk_Cell_Area to layout cells and a specific
   --  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context.
   --  Specifying the same context for a handfull of cells lets the underlying
   --  area synchronize the geometry for those cells, in this way alignments
   --  with cellviews for other rows are possible.
   --  Since: gtk+ 2.6
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to layout cells
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context in which to
   --  calculate cell geometry

   procedure Gtk_New_With_Markup
      (Cell_View : out Gtk_Cell_View;
       Markup    : UTF8_String);
   procedure Initialize_With_Markup
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Markup    : UTF8_String);
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget, adds a
   --  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text to it, and makes it show
   --  Markup. The text can be marked up with the [Pango text markup
   --  language][PangoMarkupFormat].
   --  Since: gtk+ 2.6
   --  Initialize_With_Markup does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "markup": the text to display in the cell view

   function Gtk_Cell_View_New_With_Markup
      (Markup : UTF8_String) return Gtk_Cell_View;
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget, adds a
   --  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text to it, and makes it show
   --  Markup. The text can be marked up with the [Pango text markup
   --  language][PangoMarkupFormat].
   --  Since: gtk+ 2.6
   --  "markup": the text to display in the cell view

   procedure Gtk_New_With_Pixbuf
      (Cell_View : out Gtk_Cell_View;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize_With_Pixbuf
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget, adds a
   --  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf to it, and makes it
   --  show Pixbuf.
   --  Since: gtk+ 2.6
   --  Initialize_With_Pixbuf does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "pixbuf": the image to display in the cell view

   function Gtk_Cell_View_New_With_Pixbuf
      (Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Cell_View;
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget, adds a
   --  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf to it, and makes it
   --  show Pixbuf.
   --  Since: gtk+ 2.6
   --  "pixbuf": the image to display in the cell view

   procedure Gtk_New_With_Text
      (Cell_View : out Gtk_Cell_View;
       Text      : UTF8_String);
   procedure Initialize_With_Text
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Text      : UTF8_String);
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget, adds a
   --  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text to it, and makes it show
   --  Text.
   --  Since: gtk+ 2.6
   --  Initialize_With_Text does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "text": the text to display in the cell view

   function Gtk_Cell_View_New_With_Text
      (Text : UTF8_String) return Gtk_Cell_View;
   --  Creates a new Gtk.Cell_View.Gtk_Cell_View widget, adds a
   --  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text to it, and makes it show
   --  Text.
   --  Since: gtk+ 2.6
   --  "text": the text to display in the cell view

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_view_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Displayed_Row
      (Cell_View : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Returns a Gtk.Tree_Model.Gtk_Tree_Path referring to the currently
   --  displayed row. If no row is currently displayed, null is returned.
   --  Since: gtk+ 2.6

   procedure Set_Displayed_Row
      (Cell_View : not null access Gtk_Cell_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets the row of the model that is currently displayed by the
   --  Gtk.Cell_View.Gtk_Cell_View. If the path is unset, then the contents of
   --  the cellview "stick" at their last value; this is not normally a desired
   --  result, but may be a needed intermediate state if say, the model for the
   --  Gtk.Cell_View.Gtk_Cell_View becomes temporarily empty.
   --  Since: gtk+ 2.6
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path or null to unset.

   function Get_Draw_Sensitive
      (Cell_View : not null access Gtk_Cell_View_Record) return Boolean;
   --  Gets whether Cell_View is configured to draw all of its cells in a
   --  sensitive state.
   --  Since: gtk+ 3.0

   procedure Set_Draw_Sensitive
      (Cell_View      : not null access Gtk_Cell_View_Record;
       Draw_Sensitive : Boolean);
   --  Sets whether Cell_View should draw all of its cells in a sensitive
   --  state, this is used by Gtk.Combo_Box.Gtk_Combo_Box menus to ensure that
   --  rows with insensitive cells that contain children appear sensitive in
   --  the parent menu item.
   --  Since: gtk+ 3.0
   --  "draw_sensitive": whether to draw all cells in a sensitive state.

   function Get_Fit_Model
      (Cell_View : not null access Gtk_Cell_View_Record) return Boolean;
   --  Gets whether Cell_View is configured to request space to fit the entire
   --  Gtk.Tree_Model.Gtk_Tree_Model.
   --  Since: gtk+ 3.0

   procedure Set_Fit_Model
      (Cell_View : not null access Gtk_Cell_View_Record;
       Fit_Model : Boolean);
   --  Sets whether Cell_View should request space to fit the entire
   --  Gtk.Tree_Model.Gtk_Tree_Model.
   --  This is used by Gtk.Combo_Box.Gtk_Combo_Box to ensure that the cell
   --  view displayed on the combo box's button always gets enough space and
   --  does not resize when selection changes.
   --  Since: gtk+ 3.0
   --  "fit_model": whether Cell_View should request space for the whole
   --  model.

   function Get_Model
      (Cell_View : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model for Cell_View. If no model is used null is returned.
   --  Since: gtk+ 2.16

   procedure Set_Model
      (Cell_View : not null access Gtk_Cell_View_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model for Cell_View. If Cell_View already has a model set, it
   --  will remove it before setting the new model. If Model is null, then it
   --  will unset the old model.
   --  Since: gtk+ 2.6
   --  "model": a Gtk.Tree_Model.Gtk_Tree_Model

   function Get_Size_Of_Row
      (Cell_View   : not null access Gtk_Cell_View_Record;
       Path        : Gtk.Tree_Model.Gtk_Tree_Path;
       Requisition : access Gtk.Widget.Gtk_Requisition) return Boolean;
   pragma Obsolescent (Get_Size_Of_Row);
   --  Sets Requisition to the size needed by Cell_View to display the model
   --  row pointed to by Path.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.0, 1
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path
   --  "requisition": return location for the size

   procedure Set_Background_Color
      (Cell_View : not null access Gtk_Cell_View_Record;
       Color     : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Set_Background_Color);
   --  Sets the background color of View.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.4, 1
   --  "color": the new background color

   procedure Set_Background_Rgba
      (Cell_View : not null access Gtk_Cell_View_Record;
       Rgba      : Gdk.RGBA.Gdk_RGBA);
   --  Sets the background color of Cell_View.
   --  Since: gtk+ 3.0
   --  "rgba": the new background color

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Cell_View_Record;
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
         (Cell_Layout : not null access Gtk.Cell_View.Gtk_Cell_View_Record'Class;
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

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear (Cell_Layout : not null access Gtk_Cell_View_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Cell_View_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   function Get_Orientation
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Cell_View_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  The background color as a Gdk.Color.Gdk_Color

   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The background color as a Gdk.RGBA.Gdk_RGBA

   Background_Set_Property : constant Glib.Properties.Property_Boolean;

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The Gtk.Cell_Area.Gtk_Cell_Area rendering cells
   --
   --  If no area is specified when creating the cell view with
   --  Gtk.Cell_View.Gtk_New_With_Context a horizontally oriented
   --  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box will be used.
   --
   --  since 3.0

   Cell_Area_Context_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   --  The Gtk.Cell_Area_Context.Gtk_Cell_Area_Context used to compute the
   --  geometry of the cell view.
   --
   --  A group of cell views can be assigned the same context in order to
   --  ensure the sizes and cell alignments match across all the views with the
   --  same context.
   --
   --  Gtk.Combo_Box.Gtk_Combo_Box menus uses this to assign the same context
   --  to all cell views in the menu items for a single menu (each submenu
   --  creates its own context since the size of each submenu does not depend
   --  on parent or sibling menus).
   --
   --  since 3.0

   Draw_Sensitive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether all cells should be draw as sensitive for this view regardless
   --  of the actual cell properties (used to make menus with submenus appear
   --  sensitive when the items in submenus might be insensitive).
   --
   --  since 3.0

   Fit_Model_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the view should request enough space to always fit the size of
   --  every row in the model (used by the combo box to ensure the combo box
   --  size doesnt change when different items are selected).
   --
   --  since 3.0

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  The model for cell view
   --
   --  since 2.10

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Cell_View_Record, Gtk_Cell_View);
   function "+"
     (Widget : access Gtk_Cell_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Cell_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Cell_View_Record, Gtk_Cell_View);
   function "+"
     (Widget : access Gtk_Cell_View_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Cell_View
   renames Implements_Gtk_Cell_Layout.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Cell_View_Record, Gtk_Cell_View);
   function "+"
     (Widget : access Gtk_Cell_View_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Cell_View
   renames Implements_Gtk_Orientable.To_Object;

private
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Fit_Model_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fit-model");
   Draw_Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-sensitive");
   Cell_Area_Context_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area-context");
   Cell_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area");
   Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-set");
   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("background-rgba");
   Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("background-gdk");
   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
end Gtk.Cell_View;
