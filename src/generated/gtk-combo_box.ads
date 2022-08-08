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
--  A GtkComboBox is a widget that allows the user to choose from a list of
--  valid choices. The GtkComboBox displays the selected choice. When
--  activated, the GtkComboBox displays a popup which allows the user to make a
--  new choice. The style in which the selected value is displayed, and the
--  style of the popup is determined by the current theme. It may be similar to
--  a Windows-style combo box.
--
--  The GtkComboBox uses the model-view pattern; the list of valid choices is
--  specified in the form of a tree model, and the display of the choices can
--  be adapted to the data in the model by using cell renderers, as you would
--  in a tree view. This is possible since GtkComboBox implements the
--  Gtk.Cell_Layout.Gtk_Cell_Layout interface. The tree model holding the valid
--  choices is not restricted to a flat list, it can be a real tree, and the
--  popup will reflect the tree structure.
--
--  To allow the user to enter values not in the model, the "has-entry"
--  property allows the GtkComboBox to contain a Gtk.GEntry.Gtk_Entry. This
--  entry can be accessed by calling Gtk.Bin.Get_Child on the combo box.
--
--  For a simple list of textual choices, the model-view API of GtkComboBox
--  can be a bit overwhelming. In this case,
--  Gtk.Combo_Box_Text.Gtk_Combo_Box_Text offers a simple alternative. Both
--  GtkComboBox and Gtk.Combo_Box_Text.Gtk_Combo_Box_Text can contain an entry.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> combobox ├── box.linked │ ╰── button.combo │
--  ╰── box │ ├── cellview │ ╰── arrow ╰── window.popup ]|
--
--  A normal combobox contains a box with the .linked class, a button with the
--  .combo class and inside those buttons, there are a cellview and an arrow.
--
--  |[<!-- language="plain" --> combobox ├── box.linked │ ├── entry.combo │
--  ╰── button.combo │ ╰── box │ ╰── arrow ╰── window.popup ]|
--
--  A GtkComboBox with an entry has a single CSS node with name combobox. It
--  contains a box with the .linked class. That box contains an entry and a
--  button, both with the .combo class added. The button also contains another
--  node with name arrow.
--
--  </description>
--  <group>Numeric/Text Data Entry</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Bin;           use Gtk.Bin;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Cell_Area;     use Gtk.Cell_Area;
with Gtk.Cell_Editable; use Gtk.Cell_Editable;
with Gtk.Cell_Layout;   use Gtk.Cell_Layout;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Gtk.Tree_View;     use Gtk.Tree_View;

package Gtk.Combo_Box is

   type Gtk_Combo_Box_Record is new Gtk_Bin_Record with null record;
   type Gtk_Combo_Box is access all Gtk_Combo_Box_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_View_Row_Separator_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Function type for determining whether the row pointed to by Iter should
   --  be rendered as a separator. A common way to implement this is to have a
   --  boolean column in the model, whose values the
   --  Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func returns.
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model

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

   procedure Gtk_New (Combo_Box : out Gtk_Combo_Box);
   procedure Initialize
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class);
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Combo_Box_New return Gtk_Combo_Box;
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box.
   --  Since: gtk+ 2.4

   procedure Gtk_New_With_Area
      (Combo_Box : out Gtk_Combo_Box;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box using Area to layout
   --  cells.
   --  Initialize_With_Area does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to use to layout cell renderers

   function Gtk_Combo_Box_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Combo_Box;
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box using Area to layout
   --  cells.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to use to layout cell renderers

   procedure Gtk_New_With_Area_And_Entry
      (Combo_Box : out Gtk_Combo_Box;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area_And_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box with an entry.
   --  The new combo box will use Area to layout cells.
   --  Initialize_With_Area_And_Entry does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to use to layout cell renderers

   function Gtk_Combo_Box_New_With_Area_And_Entry
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Combo_Box;
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box with an entry.
   --  The new combo box will use Area to layout cells.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area to use to layout cell renderers

   procedure Gtk_New_With_Entry (Combo_Box : out Gtk_Combo_Box);
   procedure Initialize_With_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class);
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box with an entry.
   --  Since: gtk+ 2.24
   --  Initialize_With_Entry does nothing if the object was already created
   --  with another call to Initialize* or G_New.

   function Gtk_Combo_Box_New_With_Entry return Gtk_Combo_Box;
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box with an entry.
   --  Since: gtk+ 2.24

   procedure Gtk_New_With_Model
      (Combo_Box : out Gtk_Combo_Box;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   procedure Initialize_With_Model
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Creates a new Gtk.Combo_Box.Gtk_Combo_Box with the model initialized to
   --  Model.
   --  Since: gtk+ 2.4
   --  Initialize_With_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model.

   function Gtk_Combo_Box_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Combo_Box;
   --  Creates a new Gtk.Combo_Box.Gtk_Combo_Box with the model initialized to
   --  Model.
   --  Since: gtk+ 2.4
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model.

   procedure Gtk_New_With_Model_And_Entry
      (Combo_Box : out Gtk_Combo_Box;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   procedure Initialize_With_Model_And_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box with an entry and with
   --  the model initialized to Model.
   --  Since: gtk+ 2.24
   --  Initialize_With_Model_And_Entry does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model

   function Gtk_Combo_Box_New_With_Model_And_Entry
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Combo_Box;
   --  Creates a new empty Gtk.Combo_Box.Gtk_Combo_Box with an entry and with
   --  the model initialized to Model.
   --  Since: gtk+ 2.24
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_combo_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint;
   --  Returns the index of the currently active item, or -1 if there's no
   --  active item. If the model is a non-flat treemodel, and the active item
   --  is not an immediate child of the root of the tree, this function returns
   --  `gtk_tree_path_get_indices (path)[0]`, where `path` is the
   --  Gtk.Tree_Model.Gtk_Tree_Path of the active item.
   --  Since: gtk+ 2.4

   procedure Set_Active
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Index     : Glib.Gint);
   --  Sets the active item of Combo_Box to be the item at Index.
   --  Since: gtk+ 2.4
   --  "index_": An index in the model passed during construction, or -1 to
   --  have no active item

   function Get_Active_Id
      (Combo_Box : not null access Gtk_Combo_Box_Record) return UTF8_String;
   --  Returns the ID of the active row of Combo_Box. This value is taken from
   --  the active row and the column specified by the
   --  Gtk.Combo_Box.Gtk_Combo_Box:id-column property of Combo_Box (see
   --  Gtk.Combo_Box.Set_Id_Column).
   --  The returned value is an interned string which means that you can
   --  compare the pointer by value to other interned strings and that you must
   --  not free it.
   --  If the Gtk.Combo_Box.Gtk_Combo_Box:id-column property of Combo_Box is
   --  not set, or if no row is active, or if the active row has a null ID
   --  value, then null is returned.
   --  Since: gtk+ 3.0

   function Set_Active_Id
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Active_Id : UTF8_String := "") return Boolean;
   --  Changes the active row of Combo_Box to the one that has an ID equal to
   --  Active_Id, or unsets the active row if Active_Id is null. Rows having a
   --  null ID string cannot be made active by this function.
   --  If the Gtk.Combo_Box.Gtk_Combo_Box:id-column property of Combo_Box is
   --  unset or if no row has the given ID then the function does nothing and
   --  returns False.
   --  Since: gtk+ 3.0
   --  "active_id": the ID of the row to select, or null

   function Get_Add_Tearoffs
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean;
   pragma Obsolescent (Get_Add_Tearoffs);
   --  Gets the current value of the :add-tearoffs property.
   --  Deprecated since 3.10, 1

   procedure Set_Add_Tearoffs
      (Combo_Box    : not null access Gtk_Combo_Box_Record;
       Add_Tearoffs : Boolean);
   pragma Obsolescent (Set_Add_Tearoffs);
   --  Sets whether the popup menu should have a tearoff menu item.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.10, 1
   --  "add_tearoffs": True to add tearoff menu items

   function Get_Button_Sensitivity
      (Combo_Box : not null access Gtk_Combo_Box_Record)
       return Gtk.Enums.Gtk_Sensitivity_Type;
   --  Returns whether the combo box sets the dropdown button sensitive or not
   --  when there are no items in the model.
   --  Since: gtk+ 2.14

   procedure Set_Button_Sensitivity
      (Combo_Box   : not null access Gtk_Combo_Box_Record;
       Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
   --  Sets whether the dropdown button of the combo box should be always
   --  sensitive (Gtk.Enums.Sensitivity_On), never sensitive
   --  (Gtk.Enums.Sensitivity_Off) or only if there is at least one item to
   --  display (Gtk.Enums.Sensitivity_Auto).
   --  Since: gtk+ 2.14
   --  "sensitivity": specify the sensitivity of the dropdown button

   function Get_Column_Span_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint;
   --  Returns the column with column span information for Combo_Box.
   --  Since: gtk+ 2.6

   procedure Set_Column_Span_Column
      (Combo_Box   : not null access Gtk_Combo_Box_Record;
       Column_Span : Glib.Gint);
   --  Sets the column with column span information for Combo_Box to be
   --  Column_Span. The column span column contains integers which indicate how
   --  many columns an item should span.
   --  Since: gtk+ 2.4
   --  "column_span": A column in the model passed during construction

   function Get_Entry_Text_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint;
   --  Returns the column which Combo_Box is using to get the strings from to
   --  display in the internal entry.
   --  Since: gtk+ 2.24

   procedure Set_Entry_Text_Column
      (Combo_Box   : not null access Gtk_Combo_Box_Record;
       Text_Column : Glib.Gint);
   --  Sets the model column which Combo_Box should use to get strings from to
   --  be Text_Column. The column Text_Column in the model of Combo_Box must be
   --  of type G_TYPE_STRING.
   --  This is only relevant if Combo_Box has been created with
   --  Gtk.Combo_Box.Gtk_Combo_Box:has-entry as True.
   --  Since: gtk+ 2.24
   --  "text_column": A column in Model to get the strings from for the
   --  internal entry

   function Get_Focus_On_Click
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean;
   pragma Obsolescent (Get_Focus_On_Click);
   --  Returns whether the combo box grabs focus when it is clicked with the
   --  mouse. See Gtk.Combo_Box.Set_Focus_On_Click.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.20, 1

   procedure Set_Focus_On_Click
      (Combo_Box      : not null access Gtk_Combo_Box_Record;
       Focus_On_Click : Boolean);
   pragma Obsolescent (Set_Focus_On_Click);
   --  Sets whether the combo box will grab focus when it is clicked with the
   --  mouse. Making mouse clicks not grab focus is useful in places like
   --  toolbars where you don't want the keyboard focus removed from the main
   --  area of the application.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.20, 1
   --  "focus_on_click": whether the combo box grabs focus when clicked with
   --  the mouse

   function Get_Has_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean;
   --  Returns whether the combo box has an entry.
   --  Since: gtk+ 2.24

   function Get_Id_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint;
   --  Returns the column which Combo_Box is using to get string IDs for
   --  values from.
   --  Since: gtk+ 3.0

   procedure Set_Id_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Id_Column : Glib.Gint);
   --  Sets the model column which Combo_Box should use to get string IDs for
   --  values from. The column Id_Column in the model of Combo_Box must be of
   --  type G_TYPE_STRING.
   --  Since: gtk+ 3.0
   --  "id_column": A column in Model to get string IDs for values from

   function Get_Model
      (Combo_Box : not null access Gtk_Combo_Box_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the Gtk.Tree_Model.Gtk_Tree_Model which is acting as data
   --  source for Combo_Box.
   --  Since: gtk+ 2.4

   procedure Set_Model
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model used by Combo_Box to be Model. Will unset a previously
   --  set model (if applicable). If model is null, then it will unset the
   --  model.
   --  Note that this function does not clear the cell renderers, you have to
   --  call Gtk.Cell_Layout.Clear yourself if you need to set up different cell
   --  renderers for the new model.
   --  Since: gtk+ 2.4
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model

   function Get_Popup_Fixed_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean;
   --  Gets whether the popup uses a fixed width matching the allocated width
   --  of the combo box.
   --  Since: gtk+ 3.0

   procedure Set_Popup_Fixed_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Fixed     : Boolean);
   --  Specifies whether the popup's width should be a fixed width matching
   --  the allocated width of the combo box.
   --  Since: gtk+ 3.0
   --  "fixed": whether to use a fixed popup width

   procedure Get_Row_Separator_Func
      (Combo_Box : not null access Gtk_Combo_Box_Record);
   --  Returns the current row separator function.
   --  Since: gtk+ 2.6

   procedure Set_Row_Separator_Func
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Func      : Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func);
   --  Sets the row separator function, which is used to determine whether a
   --  row should be drawn as a separator. If the row separator function is
   --  null, no separators are drawn. This is the default value.
   --  Since: gtk+ 2.6
   --  "func": a Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func

   function Get_Row_Span_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint;
   --  Returns the column with row span information for Combo_Box.
   --  Since: gtk+ 2.6

   procedure Set_Row_Span_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Row_Span  : Glib.Gint);
   --  Sets the column with row span information for Combo_Box to be Row_Span.
   --  The row span column contains integers which indicate how many rows an
   --  item should span.
   --  Since: gtk+ 2.4
   --  "row_span": A column in the model passed during construction.

   function Get_Title
      (Combo_Box : not null access Gtk_Combo_Box_Record) return UTF8_String;
   pragma Obsolescent (Get_Title);
   --  Gets the current title of the menu in tearoff mode. See
   --  Gtk.Combo_Box.Set_Add_Tearoffs.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.10, 1

   procedure Set_Title
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Title     : UTF8_String);
   pragma Obsolescent (Set_Title);
   --  Sets the menu's title in tearoff mode.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.10, 1
   --  "title": a title for the menu in tearoff mode

   function Get_Wrap_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint;
   --  Returns the wrap width which is used to determine the number of columns
   --  for the popup menu. If the wrap width is larger than 1, the combo box is
   --  in table mode.
   --  Since: gtk+ 2.6

   procedure Set_Wrap_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Width     : Glib.Gint);
   --  Sets the wrap width of Combo_Box to be Width. The wrap width is
   --  basically the preferred number of columns when you want the popup to be
   --  layed out in a table.
   --  Since: gtk+ 2.4
   --  "width": Preferred number of columns

   procedure Popdown (Combo_Box : not null access Gtk_Combo_Box_Record);
   --  Hides the menu or dropdown list of Combo_Box.
   --  This function is mostly intended for use by accessibility technologies;
   --  applications should have little use for it.
   --  Since: gtk+ 2.4

   procedure Popup (Combo_Box : not null access Gtk_Combo_Box_Record);
   --  Pops up the menu or dropdown list of Combo_Box.
   --  This function is mostly intended for use by accessibility technologies;
   --  applications should have little use for it.
   --  Before calling this, Combo_Box must be mapped, or nothing will happen.
   --  Since: gtk+ 2.4

   procedure Set_Active_Iter
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Sets the current active item to be the one referenced by Iter, or
   --  unsets the active item if Iter is null.
   --  Since: gtk+ 2.4
   --  "iter": The Gtk.Tree_Model.Gtk_Tree_Iter, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Row_Separator_Func_User_Data is

      type Gtk_Tree_View_Row_Separator_Func is access function
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : User_Data_Type) return Boolean;
      --  Function type for determining whether the row pointed to by Iter should
      --  be rendered as a separator. A common way to implement this is to have a
      --  boolean column in the model, whose values the
      --  Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func returns.
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model
      --  "data": user data

      procedure Set_Row_Separator_Func
         (Combo_Box : not null access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
          Func      : Gtk_Tree_View_Row_Separator_Func;
          Data      : User_Data_Type);
      --  Sets the row separator function, which is used to determine whether
      --  a row should be drawn as a separator. If the row separator function
      --  is null, no separators are drawn. This is the default value.
      --  Since: gtk+ 2.6
      --  "func": a Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func
      --  "data": user data to pass to Func, or null

   end Set_Row_Separator_Func_User_Data;

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
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
         (Cell_Layout : not null access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
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

   function Get_Active_Iter
     (Combo_Box : not null access Gtk_Combo_Box_Record)
   return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Return the currently active iter

   function Get_Active_Text
     (Combo_Box : not null access Gtk_Combo_Box_Record)
   return UTF8_String;
   --  Return the text present in the entry if it has one, or the empty string

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Combo_Box_Record);

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Combo_Box_Record);

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Combo_Box_Record;
       Event         : Gdk.Event.Gdk_Event);

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear (Cell_Layout : not null access Gtk_Combo_Box_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Combo_Box_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Active_Property : constant Glib.Properties.Property_Int;
   --  The item which is currently active. If the model is a non-flat
   --  treemodel, and the active item is not an immediate child of the root of
   --  the tree, this property has the value `gtk_tree_path_get_indices
   --  (path)[0]`, where `path` is the Gtk.Tree_Model.Gtk_Tree_Path of the
   --  active item.

   Active_Id_Property : constant Glib.Properties.Property_String;
   --  The value of the ID column of the active row.

   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean;
   --  The add-tearoffs property controls whether generated menus have tearoff
   --  menu items.
   --
   --  Note that this only affects menu style combo boxes.

   Button_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type;
   --  Whether the dropdown button is sensitive when the model is empty.

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The Gtk.Cell_Area.Gtk_Cell_Area used to layout cell renderers for this
   --  combo box.
   --
   --  If no area is specified when creating the combo box with
   --  Gtk.Combo_Box.Gtk_New_With_Area a horizontally oriented
   --  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box will be used.

   Column_Span_Column_Property : constant Glib.Properties.Property_Int;
   --  If this is set to a non-negative value, it must be the index of a
   --  column of type G_TYPE_INT in the model. The value in that column for
   --  each item will determine how many columns that item will span in the
   --  popup. Therefore, values in this column must be greater than zero, and
   --  the sum of an item's column position + span should not exceed
   --  Gtk.Combo_Box.Gtk_Combo_Box:wrap-width.

   Entry_Text_Column_Property : constant Glib.Properties.Property_Int;
   --  The column in the combo box's model to associate with strings from the
   --  entry if the combo was created with
   --  Gtk.Combo_Box.Gtk_Combo_Box:has-entry = True.

   Has_Entry_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the combo box has an entry.

   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   --  The has-frame property controls whether a frame is drawn around the
   --  entry.

   Id_Column_Property : constant Glib.Properties.Property_Int;
   --  The column in the combo box's model that provides string IDs for the
   --  values in the model, if != -1.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  The model from which the combo box takes the values shown in the list.

   Popup_Fixed_Width_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the popup's width should be a fixed width matching the
   --  allocated width of the combo box.

   Popup_Shown_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the combo boxes dropdown is popped up. Note that this property
   --  is mainly useful, because it allows you to connect to
   --  notify::popup-shown.

   Row_Span_Column_Property : constant Glib.Properties.Property_Int;
   --  If this is set to a non-negative value, it must be the index of a
   --  column of type G_TYPE_INT in the model. The value in that column for
   --  each item will determine how many rows that item will span in the popup.
   --  Therefore, values in this column must be greater than zero.

   Tearoff_Title_Property : constant Glib.Properties.Property_String;
   --  A title that may be displayed by the window manager when the popup is
   --  torn-off.

   Wrap_Width_Property : constant Glib.Properties.Property_Int;
   --  If wrap-width is set to a positive value, items in the popup will be
   --  laid out along multiple columns, starting a new row on reaching the wrap
   --  width.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Combo_Box_Void is not null access procedure (Self : access Gtk_Combo_Box_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The changed signal is emitted when the active item is changed. The can
   --  be due to the user selecting a different item from the list, or due to a
   --  call to Gtk.Combo_Box.Set_Active_Iter. It will also be emitted while
   --  typing into the entry of a combo box with an entry.

   type Cb_Gtk_Combo_Box_UTF8_String_UTF8_String is not null access function
     (Self : access Gtk_Combo_Box_Record'Class;
      Path : UTF8_String) return UTF8_String;

   type Cb_GObject_UTF8_String_UTF8_String is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String) return UTF8_String;

   Signal_Format_Entry_Text : constant Glib.Signal_Name := "format-entry-text";
   procedure On_Format_Entry_Text
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_UTF8_String_UTF8_String;
       After : Boolean := False);
   procedure On_Format_Entry_Text
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_UTF8_String_UTF8_String;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  For combo boxes that are created with an entry (See
   --  GtkComboBox:has-entry).
   --
   --  A signal which allows you to change how the text displayed in a combo
   --  box's entry is displayed.
   --
   --  Connect a signal handler which returns an allocated string representing
   --  Path. That string will then be used to set the text in the combo box's
   --  entry. The default signal handler uses the text from the
   --  GtkComboBox::entry-text-column model column.
   --
   --  Here's an example signal handler which fetches data from the model and
   --  displays it in the entry. |[<!-- language="C" --> static gchar*
   --  format_entry_text_callback (GtkComboBox *combo, const gchar *path,
   --  gpointer user_data) { GtkTreeIter iter; GtkTreeModel model; gdouble
   --  value;
   --
   --  model = gtk_combo_box_get_model (combo);
   --
   --  gtk_tree_model_get_iter_from_string (model, &iter, path);
   --  gtk_tree_model_get (model, &iter, THE_DOUBLE_VALUE_COLUMN, &value, -1);
   --
   --  return g_strdup_printf ("%g", value); } ]|
   -- 
   --  Callback parameters:
   --    --  "path": the GtkTreePath string from the combo box's current model to
   --    --  format text for
   --    --  Returns a newly allocated string representing Path
   -- for the current GtkComboBox model.

   type Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void is not null access procedure
     (Self        : access Gtk_Combo_Box_Record'Class;
      Scroll_Type : Gtk.Enums.Gtk_Scroll_Type);

   type Cb_GObject_Gtk_Scroll_Type_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Scroll_Type : Gtk.Enums.Gtk_Scroll_Type);

   Signal_Move_Active : constant Glib.Signal_Name := "move-active";
   procedure On_Move_Active
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void;
       After : Boolean := False);
   procedure On_Move_Active
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::move-active signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to move the active selection.

   type Cb_Gtk_Combo_Box_Boolean is not null access function
     (Self : access Gtk_Combo_Box_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Popdown : constant Glib.Signal_Name := "popdown";
   procedure On_Popdown
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Boolean;
       After : Boolean := False);
   procedure On_Popdown
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::popdown signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted to popdown the combo box list.
   --
   --  The default bindings for this signal are Alt+Up and Escape.

   Signal_Popup : constant Glib.Signal_Name := "popup";
   procedure On_Popup
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Void;
       After : Boolean := False);
   procedure On_Popup
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::popup signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted to popup the combo box list.
   --
   --  The default binding for this signal is Alt+Down.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellEditable"
   --
   --  - "CellLayout"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Combo_Box_Record, Gtk_Combo_Box);
   function "+"
     (Widget : access Gtk_Combo_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Combo_Box
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable, Gtk_Combo_Box_Record, Gtk_Combo_Box);
   function "+"
     (Widget : access Gtk_Combo_Box_Record'Class)
   return Gtk.Cell_Editable.Gtk_Cell_Editable
   renames Implements_Gtk_Cell_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Editable.Gtk_Cell_Editable)
   return Gtk_Combo_Box
   renames Implements_Gtk_Cell_Editable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Combo_Box_Record, Gtk_Combo_Box);
   function "+"
     (Widget : access Gtk_Combo_Box_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Combo_Box
   renames Implements_Gtk_Cell_Layout.To_Object;

private
   Wrap_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("wrap-width");
   Tearoff_Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tearoff-title");
   Row_Span_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-span-column");
   Popup_Shown_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-shown");
   Popup_Fixed_Width_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-fixed-width");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Id_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("id-column");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Has_Entry_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-entry");
   Entry_Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("entry-text-column");
   Column_Span_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-span-column");
   Cell_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area");
   Button_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type :=
     Gtk.Enums.Build ("button-sensitivity");
   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("add-tearoffs");
   Active_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("active-id");
   Active_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("active");
end Gtk.Combo_Box;
