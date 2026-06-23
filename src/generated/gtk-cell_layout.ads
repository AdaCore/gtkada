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

--  An interface for packing cells
--
--  `GtkCellLayout` is an interface to be implemented by all objects which
--  want to provide a `GtkTreeViewColumn` like API for packing cells, setting
--  attributes and data funcs.
--
--  One of the notable features provided by implementations of `GtkCellLayout`
--  are attributes. Attributes let you set the properties in flexible ways.
--  They can just be set to constant values like regular properties. But they
--  can also be mapped to a column of the underlying tree model with
--  gtk_cell_layout_set_attributes, which means that the value of the attribute
--  can change from cell to cell as they are rendered by the cell renderer.
--  Finally, it is possible to specify a function with
--  Gtk.Cell_Layout.Set_Cell_Data_Func that is called to determine the value of
--  the attribute for each cell that is rendered.
--
--  ## GtkCellLayouts as GtkBuildable
--
--  Implementations of GtkCellLayout which also implement the GtkBuildable
--  interface (`GtkCellView`, `GtkIconView`, `GtkComboBox`,
--  `GtkEntryCompletion`, `GtkTreeViewColumn`) accept `GtkCellRenderer` objects
--  as `<child>` elements in UI definitions. They support a custom
--  `<attributes>` element for their children, which can contain multiple
--  `<attribute>` elements. Each `<attribute>` element has a name attribute
--  which specifies a property of the cell renderer; the content of the element
--  is the attribute value.
--
--  This is an example of a UI definition fragment specifying attributes:
--
--  ```xml <object class="GtkCellView"> <child> <object
--  class="GtkCellRendererText"/> <attributes> <attribute
--  name="text">0</attribute> </attributes> </child> </object> ```
--
--  Furthermore for implementations of `GtkCellLayout` that use a
--  `GtkCellArea` to lay out cells (all `GtkCellLayout`s in GTK use a
--  `GtkCellArea`) [cell properties](class.CellArea.htmlcell-properties) can
--  also be defined in the format by specifying the custom `<cell-packing>`
--  attribute which can contain multiple `<property>` elements.
--
--  Here is a UI definition fragment specifying cell properties:
--
--  ```xml <object class="GtkTreeViewColumn"> <child> <object
--  class="GtkCellRendererText"/> <cell-packing> <property
--  name="align">True</property> <property name="expand">False</property>
--  </cell-packing> </child> </object> ```
--
--  ## Subclassing GtkCellLayout implementations
--
--  When subclassing a widget that implements `GtkCellLayout` like
--  `GtkIconView` or `GtkComboBox`, there are some considerations related to
--  the fact that these widgets internally use a `GtkCellArea`. The cell area
--  is exposed as a construct-only property by these widgets. This means that
--  it is possible to e.g. do
--
--  ```c GtkWIdget *combo = g_object_new (GTK_TYPE_COMBO_BOX, "cell-area",
--  my_cell_area, NULL); ```
--
--  to use a custom cell area with a combo box. But construct properties are
--  only initialized after instance `init` functions have run, which means that
--  using functions which rely on the existence of the cell area in your
--  subclass `init` function will cause the default cell area to be
--  instantiated. In this case, a provided construct property value will be
--  ignored (with a warning, to alert you to the problem).
--
--  ```c static void my_combo_box_init (MyComboBox *b) { GtkCellRenderer
--  *cell;
--
--  cell = gtk_cell_renderer_pixbuf_new ();
--
--  // The following call causes the default cell area for combo boxes, // a
--  GtkCellAreaBox, to be instantiated gtk_cell_layout_pack_start
--  (GTK_CELL_LAYOUT (b), cell, FALSE); ... }
--
--  GtkWidget * my_combo_box_new (GtkCellArea *area) { // This call is going
--  to cause a warning about area being ignored return g_object_new
--  (MY_TYPE_COMBO_BOX, "cell-area", area, NULL); } ```
--
--  If supporting alternative cell areas with your derived widget is not
--  important, then this does not have to concern you. If you want to support
--  alternative cell areas, you can do so by moving the problematic calls out
--  of `init` and into a `constructor` for your class.
--
--  <group>Trees and Lists</group>
--  <gtkada_demo>create_cell_view.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Types;        use Glib.Types;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Tree_Model;    use Gtk.Tree_Model;

package Gtk.Cell_Layout is

   pragma Obsolescent;
   --  List views use widgets to display their contents. See [class@Gtk.LayoutManager] for layout manager delegate objects

   type Gtk_Cell_Layout is new Glib.Types.GType_Interface;
   Null_Gtk_Cell_Layout : constant Gtk_Cell_Layout;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Cell_Layout_Data_Func is access procedure
     (Cell_Layout : Gtk_Cell_Layout;
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

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_layout_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Attribute
      (Self      : Gtk_Cell_Layout;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   pragma Obsolescent (Add_Attribute);
   --  Adds an attribute mapping to the list in Cell_Layout.
   --  The Column is the column of the model to get a value from, and the
   --  Attribute is the property on Cell to be set from that value. So for
   --  example if column 2 of the model contains strings, you could have the
   --  "text" attribute of a `GtkCellRendererText` get its values from column
   --  2. In this context "attribute" and "property" are used interchangeably.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Attribute a property on the renderer
   --  @param Column the column position on the model to get the attribute
   --  from

   procedure Clear (Self : Gtk_Cell_Layout);
   pragma Import (C, Clear, "gtk_cell_layout_clear");
   pragma Obsolescent (Clear);
   --  Unsets all the mappings on all renderers on Cell_Layout and removes all
   --  renderers from Cell_Layout.
   --  Deprecated since 4.10, 1

   procedure Clear_Attributes
      (Self : Gtk_Cell_Layout;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Clear_Attributes);
   --  Clears all existing attributes previously set with
   --  gtk_cell_layout_set_attributes.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer` to clear the attribute mapping on

   function Get_Cells
      (Self : Gtk_Cell_Layout)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   pragma Obsolescent (Get_Cells);
   --  Returns the cell renderers which have been added to Cell_Layout.
   --  Deprecated since 4.10, 1

   procedure Pack_End
      (Self   : Gtk_Cell_Layout;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_End);
   --  Adds the Cell to the end of Cell_Layout. If Expand is False, then the
   --  Cell is allocated no more space than it needs. Any unused space is
   --  divided evenly between cells for which Expand is True.
   --  Note that reusing the same cell renderer is not supported.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Expand True if Cell is to be given extra space allocated to
   --  Cell_Layout

   procedure Pack_Start
      (Self   : Gtk_Cell_Layout;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_Start);
   --  Packs the Cell into the beginning of Cell_Layout. If Expand is False,
   --  then the Cell is allocated no more space than it needs. Any unused space
   --  is divided evenly between cells for which Expand is True.
   --  Note that reusing the same cell renderer is not supported.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Expand True if Cell is to be given extra space allocated to
   --  Cell_Layout

   procedure Reorder
      (Self     : Gtk_Cell_Layout;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint);
   pragma Obsolescent (Reorder);
   --  Re-inserts Cell at Position.
   --  Note that Cell has already to be packed into Cell_Layout for this to
   --  function properly.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer` to reorder
   --  @param Position new position to insert Cell at

   procedure Set_Cell_Data_Func
      (Self : Gtk_Cell_Layout;
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
         (Self      : Gtk.Cell_Layout.Gtk_Cell_Layout;
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

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Cell_Layout"

   function "+" (W : Gtk_Cell_Layout) return Gtk_Cell_Layout;
   pragma Inline ("+");

private

   Null_Gtk_Cell_Layout : constant Gtk_Cell_Layout :=
      Gtk_Cell_Layout (Glib.Types.Null_Interface);
end Gtk.Cell_Layout;
