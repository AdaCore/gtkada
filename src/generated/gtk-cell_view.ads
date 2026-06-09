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

--  A widget displaying a single row of a GtkTreeModel
--
--  A `GtkCellView` displays a single row of a `GtkTreeModel` using a
--  `GtkCellArea` and `GtkCellAreaContext`. A `GtkCellAreaContext` can be
--  provided to the `GtkCellView` at construction time in order to keep the
--  cellview in context of a group of cell views, this ensures that the
--  renderers displayed will be properly aligned with each other (like the
--  aligned cells in the menus of `GtkComboBox`).
--
--  `GtkCellView` is `GtkOrientable` in order to decide in which orientation
--  the underlying `GtkCellAreaContext` should be allocated. Taking the
--  `GtkComboBox` menu as an example, cellviews should be oriented horizontally
--  if the menus are listed top-to-bottom and thus all share the same width but
--  may have separate individual heights (left-to-right menus should be
--  allocated vertically since they all share the same height but may have
--  variable widths).
--
--  ## CSS nodes
--
--  GtkCellView has a single CSS node with name cellview.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Texture;           use Gdk.Texture;
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Cell_Area;         use Gtk.Cell_Area;
with Gtk.Cell_Area_Context; use Gtk.Cell_Area_Context;
with Gtk.Cell_Layout;       use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Cell_View is

   pragma Obsolescent;
   --  List views use widgets to display their contents. You can use [class@Gtk.Box] instead

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
   --  @param Cell_Layout a `GtkCellLayout`
   --  @param Cell the cell renderer whose value is to be set
   --  @param Tree_Model the model
   --  @param Iter a `GtkTreeIter` indicating the row to set the value for

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_View);
   procedure Initialize (Self : not null access Gtk_Cell_View_Record'Class);
   --  Creates a new `GtkCellView` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_View_New return Gtk_Cell_View;
   --  Creates a new `GtkCellView` widget.

   procedure Gtk_New_With_Context
      (Self    : out Gtk_Cell_View;
       Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class);
   procedure Initialize_With_Context
      (Self    : not null access Gtk_Cell_View_Record'Class;
       Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class);
   --  Creates a new `GtkCellView` widget with a specific `GtkCellArea` to
   --  layout cells and a specific `GtkCellAreaContext`.
   --  Specifying the same context for a handful of cells lets the underlying
   --  area synchronize the geometry for those cells, in this way alignments
   --  with cellviews for other rows are possible.
   --  Initialize_With_Context does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Area the `GtkCellArea` to layout cells
   --  @param Context the `GtkCellAreaContext` in which to calculate cell
   --  geometry

   function Gtk_Cell_View_New_With_Context
      (Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk_Cell_View;
   --  Creates a new `GtkCellView` widget with a specific `GtkCellArea` to
   --  layout cells and a specific `GtkCellAreaContext`.
   --  Specifying the same context for a handful of cells lets the underlying
   --  area synchronize the geometry for those cells, in this way alignments
   --  with cellviews for other rows are possible.
   --  @param Area the `GtkCellArea` to layout cells
   --  @param Context the `GtkCellAreaContext` in which to calculate cell
   --  geometry

   procedure Gtk_New_With_Markup
      (Self   : out Gtk_Cell_View;
       Markup : UTF8_String);
   procedure Initialize_With_Markup
      (Self   : not null access Gtk_Cell_View_Record'Class;
       Markup : UTF8_String);
   --  Creates a new `GtkCellView` widget, adds a `GtkCellRendererText` to it,
   --  and makes it show Markup. The text can be marked up with the [Pango text
   --  markup language](https://docs.gtk.org/Pango/pango_markup.html).
   --  Initialize_With_Markup does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Markup the text to display in the cell view

   function Gtk_Cell_View_New_With_Markup
      (Markup : UTF8_String) return Gtk_Cell_View;
   --  Creates a new `GtkCellView` widget, adds a `GtkCellRendererText` to it,
   --  and makes it show Markup. The text can be marked up with the [Pango text
   --  markup language](https://docs.gtk.org/Pango/pango_markup.html).
   --  @param Markup the text to display in the cell view

   procedure Gtk_New_With_Text
      (Self : out Gtk_Cell_View;
       Text : UTF8_String);
   procedure Initialize_With_Text
      (Self : not null access Gtk_Cell_View_Record'Class;
       Text : UTF8_String);
   --  Creates a new `GtkCellView` widget, adds a `GtkCellRendererText` to it,
   --  and makes it show Text.
   --  Initialize_With_Text does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Text the text to display in the cell view

   function Gtk_Cell_View_New_With_Text
      (Text : UTF8_String) return Gtk_Cell_View;
   --  Creates a new `GtkCellView` widget, adds a `GtkCellRendererText` to it,
   --  and makes it show Text.
   --  @param Text the text to display in the cell view

   procedure Gtk_New_With_Texture
      (Self    : out Gtk_Cell_View;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class);
   procedure Initialize_With_Texture
      (Self    : not null access Gtk_Cell_View_Record'Class;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class);
   --  Creates a new `GtkCellView` widget, adds a `GtkCellRendererPixbuf` to
   --  it, and makes it show Texture.
   --  Initialize_With_Texture does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Texture the image to display in the cell view

   function Gtk_Cell_View_New_With_Texture
      (Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class)
       return Gtk_Cell_View;
   --  Creates a new `GtkCellView` widget, adds a `GtkCellRendererPixbuf` to
   --  it, and makes it show Texture.
   --  @param Texture the image to display in the cell view

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_view_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Displayed_Row
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Obsolescent (Get_Displayed_Row);
   --  Returns a `GtkTreePath` referring to the currently displayed row. If no
   --  row is currently displayed, null is returned.
   --  Deprecated since 4.10, 1
   --  @return the currently displayed row

   procedure Set_Displayed_Row
      (Self : not null access Gtk_Cell_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Set_Displayed_Row);
   --  Sets the row of the model that is currently displayed by the
   --  `GtkCellView`. If the path is unset, then the contents of the cellview
   --  "stick" at their last value; this is not normally a desired result, but
   --  may be a needed intermediate state if say, the model for the
   --  `GtkCellView` becomes temporarily empty.
   --  Deprecated since 4.10, 1
   --  @param Path a `GtkTreePath` or null to unset.

   function Get_Draw_Sensitive
      (Self : not null access Gtk_Cell_View_Record) return Boolean;
   pragma Obsolescent (Get_Draw_Sensitive);
   --  Gets whether Cell_View is configured to draw all of its cells in a
   --  sensitive state.
   --  Deprecated since 4.10, 1
   --  @return whether Cell_View draws all of its cells in a sensitive state

   procedure Set_Draw_Sensitive
      (Self           : not null access Gtk_Cell_View_Record;
       Draw_Sensitive : Boolean);
   pragma Obsolescent (Set_Draw_Sensitive);
   --  Sets whether Cell_View should draw all of its cells in a sensitive
   --  state, this is used by `GtkComboBox` menus to ensure that rows with
   --  insensitive cells that contain children appear sensitive in the parent
   --  menu item.
   --  Deprecated since 4.10, 1
   --  @param Draw_Sensitive whether to draw all cells in a sensitive state.

   function Get_Fit_Model
      (Self : not null access Gtk_Cell_View_Record) return Boolean;
   pragma Obsolescent (Get_Fit_Model);
   --  Gets whether Cell_View is configured to request space to fit the entire
   --  `GtkTreeModel`.
   --  Deprecated since 4.10, 1
   --  @return whether Cell_View requests space to fit the entire
   --  `GtkTreeModel`.

   procedure Set_Fit_Model
      (Self      : not null access Gtk_Cell_View_Record;
       Fit_Model : Boolean);
   pragma Obsolescent (Set_Fit_Model);
   --  Sets whether Cell_View should request space to fit the entire
   --  `GtkTreeModel`.
   --  This is used by `GtkComboBox` to ensure that the cell view displayed on
   --  the combo box's button always gets enough space and does not resize when
   --  selection changes.
   --  Deprecated since 4.10, 1
   --  @param Fit_Model whether Cell_View should request space for the whole
   --  model.

   function Get_Model
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   pragma Obsolescent (Get_Model);
   --  Returns the model for Cell_View. If no model is used null is returned.
   --  Deprecated since 4.10, 1
   --  @return a `GtkTreeModel` used

   procedure Set_Model
      (Self  : not null access Gtk_Cell_View_Record;
       Model : Gtk.Tree_Model.Gtk_Tree_Model);
   pragma Obsolescent (Set_Model);
   --  Sets the model for Cell_View. If Cell_View already has a model set, it
   --  will remove it before setting the new model. If Model is null, then it
   --  will unset the old model.
   --  Deprecated since 4.10, 1
   --  @param Model a `GtkTreeModel`

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Cell_View_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func : Gtk_Cell_Layout_Data_Func);
   pragma Obsolescent (Set_Cell_Data_Func);
   --  Sets the `GtkCellLayout`DataFunc to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Func the `GtkCellLayout`DataFunc to use

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
      --  @param Cell_Layout a `GtkCellLayout`
      --  @param Cell the cell renderer whose value is to be set
      --  @param Tree_Model the model
      --  @param Iter a `GtkTreeIter` indicating the row to set the value for
      --  @param Data user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Self      : not null access Gtk.Cell_View.Gtk_Cell_View_Record'Class;
          Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func      : Gtk_Cell_Layout_Data_Func;
          Func_Data : User_Data_Type);
      pragma Obsolescent (Set_Cell_Data_Func);
      --  Sets the `GtkCellLayout`DataFunc to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Deprecated since 4.10, 1
      --  @param Cell a `GtkCellRenderer`
      --  @param Func the `GtkCellLayout`DataFunc to use
      --  @param Func_Data user data for Func

   end Set_Cell_Data_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Cell_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Cell_View_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Cell_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Cell_View_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Cell_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Cell_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Cell_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Cell_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Cell_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Cell_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Add_Attribute
      (Self      : not null access Gtk_Cell_View_Record;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   pragma Obsolescent (Add_Attribute);

   procedure Clear (Self : not null access Gtk_Cell_View_Record);
   pragma Obsolescent (Clear);

   procedure Clear_Attributes
      (Self : not null access Gtk_Cell_View_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Clear_Attributes);

   function Get_Cells
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   pragma Obsolescent (Get_Cells);

   procedure Pack_End
      (Self   : not null access Gtk_Cell_View_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_End);

   procedure Pack_Start
      (Self   : not null access Gtk_Cell_View_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_Start);

   procedure Reorder
      (Self     : not null access Gtk_Cell_View_Record;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint);
   pragma Obsolescent (Reorder);

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

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The `GtkCellArea` rendering cells
   --
   --  If no area is specified when creating the cell view with
   --  Gtk.Cell_View.Gtk_New_With_Context a horizontally oriented
   --  `GtkCellArea`Box will be used.
   --
   --  since 3.0

   Cell_Area_Context_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   --  The `GtkCellAreaContext` used to compute the geometry of the cell view.
   --
   --  A group of cell views can be assigned the same context in order to
   --  ensure the sizes and cell alignments match across all the views with the
   --  same context.
   --
   --  `GtkComboBox` menus uses this to assign the same context to all cell
   --  views in the menu items for a single menu (each submenu creates its own
   --  context since the size of each submenu does not depend on parent or
   --  sibling menus).
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
   --  size doesn't change when different items are selected).
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
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.CellLayout"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Cell_View_Record, Gtk_Cell_View);
   function "+"
     (Widget : access Gtk_Cell_View_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Cell_View
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Cell_View_Record, Gtk_Cell_View);
   function "+"
     (Widget : access Gtk_Cell_View_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Cell_View
   renames Implements_Gtk_Constraint_Target.To_Object;

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
end Gtk.Cell_View;
