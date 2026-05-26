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

--  The selection object for GtkTreeView
--
--  The `GtkTreeSelection` object is a helper object to manage the selection
--  for a `GtkTreeView` widget. The `GtkTreeSelection` object is automatically
--  created when a new `GtkTreeView` widget is created, and cannot exist
--  independently of this widget. The primary reason the `GtkTreeSelection`
--  objects exists is for cleanliness of code and API. That is, there is no
--  conceptual reason all these functions could not be methods on the
--  `GtkTreeView` widget instead of a separate function.
--
--  The `GtkTreeSelection` object is gotten from a `GtkTreeView` by calling
--  Gtk.Tree_View.Get_Selection. It can be manipulated to check the selection
--  status of the tree, as well as select and deselect individual rows.
--  Selection is done completely view side. As a result, multiple views of the
--  same model can have completely different selections. Additionally, you
--  cannot change the selection of a row on the model that is not currently
--  displayed by the view without expanding its parents first.
--
--  One of the important things to remember when monitoring the selection of a
--  view is that the `GtkTreeSelection`::changed signal is mostly a hint. That
--  is, it may only emit one signal when a range of rows is selected.
--  Additionally, it may on occasion emit a `GtkTreeSelection`::changed signal
--  when nothing has happened (mostly as a result of programmers calling
--  select_row on an already selected row).

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Tree_Selection is

   pragma Obsolescent;

   type Gtk_Tree_Selection_Record is new GObject_Record with null record;
   type Gtk_Tree_Selection is access all Gtk_Tree_Selection_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_Selection_Foreach_Func is access procedure
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function used by Gtk.Tree_Selection.Selected_Foreach to map all
   --  selected rows. It will be called on every selected row in the view.
   --  @param Model The `GtkTreeModel` being viewed
   --  @param Path The `GtkTreePath` of a selected row
   --  @param Iter A `GtkTreeIter` pointing to a selected row

   type Gtk_Tree_Selection_Func is access function
     (Selection               : not null access Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
      Path                    : Gtk.Tree_Model.Gtk_Tree_Path;
      Path_Currently_Selected : Boolean) return Boolean;
   --  A function used by Gtk.Tree_Selection.Set_Select_Function to filter
   --  whether or not a row may be selected. It is called whenever a row's
   --  state might change.
   --  A return value of True indicates to Selection that it is okay to change
   --  the selection.
   --  @param Selection A `GtkTreeSelection`
   --  @param Model A `GtkTreeModel` being viewed
   --  @param Path The `GtkTreePath` of the row in question
   --  @param Path_Currently_Selected True, if the path is currently selected
   --  @return True, if the selection state of the row can be toggled

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_selection_get_type");

   -------------
   -- Methods --
   -------------

   function Count_Selected_Rows
      (Self : not null access Gtk_Tree_Selection_Record) return Glib.Gint;
   pragma Obsolescent (Count_Selected_Rows);
   --  Returns the number of rows that have been selected in Tree.
   --  Deprecated since 4.10, 1
   --  @return The number of rows selected.

   function Get_Mode
      (Self : not null access Gtk_Tree_Selection_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   pragma Obsolescent (Get_Mode);
   --  Gets the selection mode for Selection. See Gtk.Tree_Selection.Set_Mode.
   --  Deprecated since 4.10, 1
   --  @return the current selection mode

   procedure Set_Mode
      (Self     : not null access Gtk_Tree_Selection_Record;
       The_Type : Gtk.Enums.Gtk_Selection_Mode);
   pragma Obsolescent (Set_Mode);
   --  Sets the selection mode of the Selection. If the previous type was
   --  Gtk.Enums.Selection_Multiple, then the anchor is kept selected, if it
   --  was previously selected.
   --  Deprecated since 4.10, 1
   --  @param The_Type The selection mode

   procedure Get_Select_Function
      (Self : not null access Gtk_Tree_Selection_Record);
   pragma Obsolescent (Get_Select_Function);
   --  Returns the current selection function.
   --  Deprecated since 4.10, 1
   --  @return The function.

   procedure Set_Select_Function
      (Self : not null access Gtk_Tree_Selection_Record;
       Func : Gtk_Tree_Selection_Func);
   pragma Obsolescent (Set_Select_Function);
   --  Sets the selection function.
   --  If set, this function is called before any node is selected or
   --  unselected, giving some control over which nodes are selected. The
   --  select function should return True if the state of the node may be
   --  toggled, and False if the state of the node should be left unchanged.
   --  Deprecated since 4.10, 1
   --  @param Func The selection function. May be null

   procedure Get_Selected
      (Self  : not null access Gtk_Tree_Selection_Record;
       Model : out Gtk.Tree_Model.Gtk_Tree_Model;
       Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Get_Selected);
   --  Sets Iter to the currently selected node if Selection is set to
   --  Gtk.Enums.Selection_Single or Gtk.Enums.Selection_Browse. Iter may be
   --  NULL if you just want to test if Selection has any selected nodes. Model
   --  is filled with the current model as a convenience. This function will
   --  not work if you use Selection is Gtk.Enums.Selection_Multiple.
   --  Deprecated since 4.10, 1
   --  @param Model A pointer to set to the `GtkTreeModel`
   --  @param Iter The `GtkTreeIter`

   function Get_Tree_View
      (Self : not null access Gtk_Tree_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Tree_View);
   --  Returns the tree view associated with Selection.
   --  Deprecated since 4.10, 1

   function Get_User_Data
      (Self : not null access Gtk_Tree_Selection_Record)
       return System.Address;
   pragma Obsolescent (Get_User_Data);
   --  Returns the user data for the selection function.
   --  Deprecated since 4.10, 1
   --  @return The user data.

   function Iter_Is_Selected
      (Self : not null access Gtk_Tree_Selection_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   pragma Obsolescent (Iter_Is_Selected);
   --  Returns True if the row at Iter is currently selected.
   --  Deprecated since 4.10, 1
   --  @param Iter A valid `GtkTreeIter`
   --  @return True, if Iter is selected

   function Path_Is_Selected
      (Self : not null access Gtk_Tree_Selection_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Path_Is_Selected);
   --  Returns True if the row pointed to by Path is currently selected. If
   --  Path does not point to a valid location, False is returned
   --  Deprecated since 4.10, 1
   --  @param Path A `GtkTreePath` to check selection on.
   --  @return True if Path is selected.

   procedure Select_All (Self : not null access Gtk_Tree_Selection_Record);
   pragma Obsolescent (Select_All);
   --  Selects all the nodes. Selection must be set to
   --  Gtk.Enums.Selection_Multiple mode.
   --  Deprecated since 4.10, 1

   procedure Select_Iter
      (Self : not null access Gtk_Tree_Selection_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Select_Iter);
   --  Selects the specified iterator.
   --  Deprecated since 4.10, 1
   --  @param Iter The `GtkTreeIter` to be selected.

   procedure Select_Path
      (Self : not null access Gtk_Tree_Selection_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Select_Path);
   --  Select the row at Path.
   --  Deprecated since 4.10, 1
   --  @param Path The `GtkTreePath` to be selected.

   procedure Select_Range
      (Self       : not null access Gtk_Tree_Selection_Record;
       Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Select_Range);
   --  Selects a range of nodes, determined by Start_Path and End_Path
   --  inclusive. Selection must be set to Gtk.Enums.Selection_Multiple mode.
   --  Deprecated since 4.10, 1
   --  @param Start_Path The initial node of the range.
   --  @param End_Path The final node of the range.

   procedure Selected_Foreach
      (Self : not null access Gtk_Tree_Selection_Record;
       Func : Gtk_Tree_Selection_Foreach_Func);
   pragma Obsolescent (Selected_Foreach);
   --  Calls a function for each selected node. Note that you cannot modify
   --  the tree or selection from within this function. As a result,
   --  gtk_tree_selection_get_selected_rows might be more useful.
   --  Deprecated since 4.10, 1
   --  @param Func The function to call for each selected node.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Selected_Foreach_User_Data is

      type Gtk_Tree_Selection_Foreach_Func is access procedure
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : User_Data_Type);
      --  A function used by Gtk.Tree_Selection.Selected_Foreach to map all
      --  selected rows. It will be called on every selected row in the view.
      --  @param Model The `GtkTreeModel` being viewed
      --  @param Path The `GtkTreePath` of a selected row
      --  @param Iter A `GtkTreeIter` pointing to a selected row
      --  @param Data user data

      procedure Selected_Foreach
         (Self : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
          Func : Gtk_Tree_Selection_Foreach_Func;
          Data : User_Data_Type);
      pragma Obsolescent (Selected_Foreach);
      --  Calls a function for each selected node. Note that you cannot modify
      --  the tree or selection from within this function. As a result,
      --  gtk_tree_selection_get_selected_rows might be more useful.
      --  Deprecated since 4.10, 1
      --  @param Func The function to call for each selected node.
      --  @param Data user data to pass to the function.

   end Selected_Foreach_User_Data;

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Select_Function_User_Data is

      type Gtk_Tree_Selection_Func is access function
        (Selection               : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
         Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
         Path                    : Gtk.Tree_Model.Gtk_Tree_Path;
         Path_Currently_Selected : Boolean;
         Data                    : User_Data_Type) return Boolean;
      --  A function used by Gtk.Tree_Selection.Set_Select_Function to filter
      --  whether or not a row may be selected. It is called whenever a row's
      --  state might change.
      --  A return value of True indicates to Selection that it is okay to change
      --  the selection.
      --  @param Selection A `GtkTreeSelection`
      --  @param Model A `GtkTreeModel` being viewed
      --  @param Path The `GtkTreePath` of the row in question
      --  @param Path_Currently_Selected True, if the path is currently selected
      --  @param Data user data
      --  @return True, if the selection state of the row can be toggled

      procedure Set_Select_Function
         (Self : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
          Func : Gtk_Tree_Selection_Func;
          Data : User_Data_Type);
      pragma Obsolescent (Set_Select_Function);
      --  Sets the selection function.
      --  If set, this function is called before any node is selected or
      --  unselected, giving some control over which nodes are selected. The
      --  select function should return True if the state of the node may be
      --  toggled, and False if the state of the node should be left unchanged.
      --  Deprecated since 4.10, 1
      --  @param Func The selection function. May be null
      --  @param Data The selection function's data. May be null

   end Set_Select_Function_User_Data;

   procedure Unselect_All (Self : not null access Gtk_Tree_Selection_Record);
   pragma Obsolescent (Unselect_All);
   --  Unselects all the nodes.
   --  Deprecated since 4.10, 1

   procedure Unselect_Iter
      (Self : not null access Gtk_Tree_Selection_Record;
       Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Obsolescent (Unselect_Iter);
   --  Unselects the specified iterator.
   --  Deprecated since 4.10, 1
   --  @param Iter The `GtkTreeIter` to be unselected.

   procedure Unselect_Path
      (Self : not null access Gtk_Tree_Selection_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Unselect_Path);
   --  Unselects the row at Path.
   --  Deprecated since 4.10, 1
   --  @param Path The `GtkTreePath` to be unselected.

   procedure Unselect_Range
      (Self       : not null access Gtk_Tree_Selection_Record;
       Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Unselect_Range);
   --  Unselects a range of nodes, determined by Start_Path and End_Path
   --  inclusive.
   --  Deprecated since 4.10, 1
   --  @param Start_Path The initial node of the range.
   --  @param End_Path The initial node of the range.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Get_Selected_Rows
     (Self      : access Gtk_Tree_Selection_Record;
      Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path_List : out Gtk.Tree_Model.Gtk_Tree_Path_List.Glist);
   --  Creates a list of path of all selected rows. Additionally, if you are
   --  planning on modifying the model after calling this function, you may
   --  want to convert the returned list into a list of Gtk_Tree_Row_Reference.
   --
   --  You must free the resulting list by calling Path_Free on each item, and
   --  then freeing the list itself.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;
   --  Selection mode. See Gtk.Tree_Selection.Set_Mode for more information on
   --  this property.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tree_Selection_Void is not null access procedure
     (Self : access Gtk_Tree_Selection_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Tree_Selection_Record;
       Call  : Cb_Gtk_Tree_Selection_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Tree_Selection_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the selection has (possibly) changed. Please note that
   --  this signal is mostly a hint. It may only be emitted once when a range
   --  of rows are selected, and it may occasionally be emitted when nothing
   --  has happened.

private
   Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode :=
     Gtk.Enums.Build ("mode");
end Gtk.Tree_Selection;
