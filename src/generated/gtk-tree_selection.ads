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
--  The Gtk.Tree_Selection.Gtk_Tree_Selection object is a helper object to
--  manage the selection for a Gtk.Tree_View.Gtk_Tree_View widget. The
--  Gtk.Tree_Selection.Gtk_Tree_Selection object is automatically created when
--  a new Gtk.Tree_View.Gtk_Tree_View widget is created, and cannot exist
--  independently of this widget. The primary reason the
--  Gtk.Tree_Selection.Gtk_Tree_Selection objects exists is for cleanliness of
--  code and API. That is, there is no conceptual reason all these functions
--  could not be methods on the Gtk.Tree_View.Gtk_Tree_View widget instead of a
--  separate function.
--
--  The Gtk.Tree_Selection.Gtk_Tree_Selection object is gotten from a
--  Gtk.Tree_View.Gtk_Tree_View by calling Gtk.Tree_View.Get_Selection. It can
--  be manipulated to check the selection status of the tree, as well as select
--  and deselect individual rows. Selection is done completely view side. As a
--  result, multiple views of the same model can have completely different
--  selections. Additionally, you cannot change the selection of a row on the
--  model that is not currently displayed by the view without expanding its
--  parents first.
--
--  One of the important things to remember when monitoring the selection of a
--  view is that the Gtk.Tree_Selection.Gtk_Tree_Selection::changed signal is
--  mostly a hint. That is, it may only emit one signal when a range of rows is
--  selected. Additionally, it may on occasion emit a
--  Gtk.Tree_Selection.Gtk_Tree_Selection::changed signal when nothing has
--  happened (mostly as a result of programmers calling select_row on an
--  already selected row).
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Tree_Selection is

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
   --  "model": The Gtk.Tree_Model.Gtk_Tree_Model being viewed
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter pointing to a selected row

   type Gtk_Tree_Selection_Func is access function
     (Selection               : not null access Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
      Path                    : Gtk.Tree_Model.Gtk_Tree_Path;
      Path_Currently_Selected : Boolean) return Boolean;
   --  A function used by Gtk.Tree_Selection.Set_Select_Function to filter
   --  whether or not a row may be selected. It is called whenever a row's
   --  state might change. A return value of True indicates to Selection that
   --  it is okay to change the selection.
   --  "selection": A Gtk.Tree_Selection.Gtk_Tree_Selection
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model being viewed
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of the row in question
   --  "path_currently_selected": True, if the path is currently selected

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_selection_get_type");

   -------------
   -- Methods --
   -------------

   function Count_Selected_Rows
      (Selection : not null access Gtk_Tree_Selection_Record)
       return Glib.Gint;
   --  Returns the number of rows that have been selected in Tree.
   --  Since: gtk+ 2.2

   function Get_Mode
      (Selection : not null access Gtk_Tree_Selection_Record)
       return Gtk.Enums.Gtk_Selection_Mode;
   --  Gets the selection mode for Selection. See Gtk.Tree_Selection.Set_Mode.

   procedure Set_Mode
      (Selection : not null access Gtk_Tree_Selection_Record;
       The_Type  : Gtk.Enums.Gtk_Selection_Mode);
   --  Sets the selection mode of the Selection. If the previous type was
   --  GTK_SELECTION_MULTIPLE, then the anchor is kept selected, if it was
   --  previously selected.
   --  "type": The selection mode

   procedure Get_Select_Function
      (Selection : not null access Gtk_Tree_Selection_Record);
   --  Returns the current selection function.
   --  Since: gtk+ 2.14

   procedure Set_Select_Function
      (Selection : not null access Gtk_Tree_Selection_Record;
       Func      : Gtk_Tree_Selection_Func);
   --  Sets the selection function.
   --  If set, this function is called before any node is selected or
   --  unselected, giving some control over which nodes are selected. The
   --  select function should return True if the state of the node may be
   --  toggled, and False if the state of the node should be left unchanged.
   --  "func": The selection function. May be null

   procedure Get_Selected
      (Selection : not null access Gtk_Tree_Selection_Record;
       Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
       Iter      : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Sets Iter to the currently selected node if Selection is set to
   --  GTK_SELECTION_SINGLE or GTK_SELECTION_BROWSE. Iter may be NULL if you
   --  just want to test if Selection has any selected nodes. Model is filled
   --  with the current model as a convenience. This function will not work if
   --  you use Selection is GTK_SELECTION_MULTIPLE.
   --  "model": A pointer to set to the Gtk.Tree_Model.Gtk_Tree_Model, or
   --  NULL.
   --  "iter": The Gtk.Tree_Model.Gtk_Tree_Iter, or NULL.

   function Get_Tree_View
      (Selection : not null access Gtk_Tree_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the tree view associated with Selection.

   function Get_User_Data
      (Selection : not null access Gtk_Tree_Selection_Record)
       return System.Address;
   --  Returns the user data for the selection function.

   function Iter_Is_Selected
      (Selection : not null access Gtk_Tree_Selection_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Returns True if the row at Iter is currently selected.
   --  "iter": A valid Gtk.Tree_Model.Gtk_Tree_Iter

   function Path_Is_Selected
      (Selection : not null access Gtk_Tree_Selection_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Returns True if the row pointed to by Path is currently selected. If
   --  Path does not point to a valid location, False is returned
   --  "path": A Gtk.Tree_Model.Gtk_Tree_Path to check selection on.

   procedure Select_All
      (Selection : not null access Gtk_Tree_Selection_Record);
   --  Selects all the nodes. Selection must be set to GTK_SELECTION_MULTIPLE
   --  mode.

   procedure Select_Iter
      (Selection : not null access Gtk_Tree_Selection_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Selects the specified iterator.
   --  "iter": The Gtk.Tree_Model.Gtk_Tree_Iter to be selected.

   procedure Select_Path
      (Selection : not null access Gtk_Tree_Selection_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Select the row at Path.
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path to be selected.

   procedure Select_Range
      (Selection  : not null access Gtk_Tree_Selection_Record;
       Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Selects a range of nodes, determined by Start_Path and End_Path
   --  inclusive. Selection must be set to GTK_SELECTION_MULTIPLE mode.
   --  "start_path": The initial node of the range.
   --  "end_path": The final node of the range.

   procedure Selected_Foreach
      (Selection : not null access Gtk_Tree_Selection_Record;
       Func      : Gtk_Tree_Selection_Foreach_Func);
   --  Calls a function for each selected node. Note that you cannot modify
   --  the tree or selection from within this function. As a result,
   --  gtk_tree_selection_get_selected_rows might be more useful.
   --  "func": The function to call for each selected node.

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
      --  "model": The Gtk.Tree_Model.Gtk_Tree_Model being viewed
      --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
      --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter pointing to a selected row
      --  "data": user data

      procedure Selected_Foreach
         (Selection : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
          Func      : Gtk_Tree_Selection_Foreach_Func;
          Data      : User_Data_Type);
      --  Calls a function for each selected node. Note that you cannot modify
      --  the tree or selection from within this function. As a result,
      --  gtk_tree_selection_get_selected_rows might be more useful.
      --  "func": The function to call for each selected node.
      --  "data": user data to pass to the function.

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
      --  state might change. A return value of True indicates to Selection that
      --  it is okay to change the selection.
      --  "selection": A Gtk.Tree_Selection.Gtk_Tree_Selection
      --  "model": A Gtk.Tree_Model.Gtk_Tree_Model being viewed
      --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of the row in question
      --  "path_currently_selected": True, if the path is currently selected
      --  "data": user data

      procedure Set_Select_Function
         (Selection : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
          Func      : Gtk_Tree_Selection_Func;
          Data      : User_Data_Type);
      --  Sets the selection function.
      --  If set, this function is called before any node is selected or
      --  unselected, giving some control over which nodes are selected. The
      --  select function should return True if the state of the node may be
      --  toggled, and False if the state of the node should be left unchanged.
      --  "func": The selection function. May be null
      --  "data": The selection function's data. May be null

   end Set_Select_Function_User_Data;

   procedure Unselect_All
      (Selection : not null access Gtk_Tree_Selection_Record);
   --  Unselects all the nodes.

   procedure Unselect_Iter
      (Selection : not null access Gtk_Tree_Selection_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Unselects the specified iterator.
   --  "iter": The Gtk.Tree_Model.Gtk_Tree_Iter to be unselected.

   procedure Unselect_Path
      (Selection : not null access Gtk_Tree_Selection_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Unselects the row at Path.
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path to be unselected.

   procedure Unselect_Range
      (Selection  : not null access Gtk_Tree_Selection_Record;
       Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Unselects a range of nodes, determined by Start_Path and End_Path
   --  inclusive.
   --  Since: gtk+ 2.2
   --  "start_path": The initial node of the range.
   --  "end_path": The initial node of the range.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Get_Selected_Rows
     (Selection : access Gtk_Tree_Selection_Record;
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
