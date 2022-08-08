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
--  Gtk.Entry_Completion.Gtk_Entry_Completion is an auxiliary object to be
--  used in conjunction with Gtk.GEntry.Gtk_Entry to provide the completion
--  functionality. It implements the Gtk.Cell_Layout.Gtk_Cell_Layout interface,
--  to allow the user to add extra cells to the Gtk.Tree_View.Gtk_Tree_View
--  with completion matches.
--
--  "Completion functionality" means that when the user modifies the text in
--  the entry, Gtk.Entry_Completion.Gtk_Entry_Completion checks which rows in
--  the model match the current content of the entry, and displays a list of
--  matches. By default, the matching is done by comparing the entry text
--  case-insensitively against the text column of the model (see
--  Gtk.Entry_Completion.Set_Text_Column), but this can be overridden with a
--  custom match function (see Gtk.Entry_Completion.Set_Match_Func).
--
--  When the user selects a completion, the content of the entry is updated.
--  By default, the content of the entry is replaced by the text column of the
--  model, but this can be overridden by connecting to the
--  Gtk.Entry_Completion.Gtk_Entry_Completion::match-selected signal and
--  updating the entry in the signal handler. Note that you should return True
--  from the signal handler to suppress the default behaviour.
--
--  To add completion functionality to an entry, use
--  Gtk.GEntry.Set_Completion.
--
--  In addition to regular completion matches, which will be inserted into the
--  entry when they are selected, Gtk.Entry_Completion.Gtk_Entry_Completion
--  also allows to display "actions" in the popup window. Their appearance is
--  similar to menuitems, to differentiate them clearly from completion
--  strings. When an action is selected, the
--  Gtk.Entry_Completion.Gtk_Entry_Completion::action-activated signal is
--  emitted.
--
--  GtkEntryCompletion uses a Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter
--  model to represent the subset of the entire model that is currently
--  matching. While the GtkEntryCompletion signals
--  Gtk.Entry_Completion.Gtk_Entry_Completion::match-selected and
--  Gtk.Entry_Completion.Gtk_Entry_Completion::cursor-on-match take the
--  original model and an iter pointing to that model as arguments, other
--  callbacks and signals (such as Gtk_Cell_Layout_Data_Funcs or
--  Gtk.Cell_Area.Gtk_Cell_Area::apply-attributes) will generally take the
--  filter model as argument. As long as you are only calling
--  gtk_tree_model_get, this will make no difference to you. If for some
--  reason, you need the original model, use Gtk.Tree_Model_Filter.Get_Model.
--  Don't forget to use Gtk.Tree_Model_Filter.Convert_Iter_To_Child_Iter to
--  obtain a matching iter.
--
--  </description>
--  <group>Numeric/Text Data Entry</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Cell_Area;     use Gtk.Cell_Area;
with Gtk.Cell_Layout;   use Gtk.Cell_Layout;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Gtk.Widget;        use Gtk.Widget;

package Gtk.Entry_Completion is

   type Gtk_Entry_Completion_Record is new GObject_Record with null record;
   type Gtk_Entry_Completion is access all Gtk_Entry_Completion_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Entry_Completion_Match_Func is access function
     (Completion : not null access Gtk_Entry_Completion_Record'Class;
      Key        : UTF8_String;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  A function which decides whether the row indicated by Iter matches a
   --  given Key, and should be displayed as a possible completion for Key.
   --  Note that Key is normalized and case-folded (see g_utf8_normalize and
   --  g_utf8_casefold). If this is not appropriate, match functions have
   --  access to the unmodified key via `gtk_entry_get_text (GTK_ENTRY
   --  (gtk_entry_completion_get_entry ()))`.
   --  "completion": the Gtk.Entry_Completion.Gtk_Entry_Completion
   --  "key": the string to match, normalized and case-folded
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to match

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

   procedure Gtk_New (Completion : out Gtk_Entry_Completion);
   procedure Initialize
      (Completion : not null access Gtk_Entry_Completion_Record'Class);
   --  Creates a new Gtk.Entry_Completion.Gtk_Entry_Completion object.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Entry_Completion_New return Gtk_Entry_Completion;
   --  Creates a new Gtk.Entry_Completion.Gtk_Entry_Completion object.
   --  Since: gtk+ 2.4

   procedure Gtk_New_With_Area
      (Completion : out Gtk_Entry_Completion;
       Area       : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area
      (Completion : not null access Gtk_Entry_Completion_Record'Class;
       Area       : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new Gtk.Entry_Completion.Gtk_Entry_Completion object using
   --  the specified Area to layout cells in the underlying
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column for the drop-down menu.
   --  Since: gtk+ 3.0
   --  Initialize_With_Area does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area used to layout cells

   function Gtk_Entry_Completion_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Entry_Completion;
   --  Creates a new Gtk.Entry_Completion.Gtk_Entry_Completion object using
   --  the specified Area to layout cells in the underlying
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column for the drop-down menu.
   --  Since: gtk+ 3.0
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area used to layout cells

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_entry_completion_get_type");

   -------------
   -- Methods --
   -------------

   procedure Complete
      (Completion : not null access Gtk_Entry_Completion_Record);
   --  Requests a completion operation, or in other words a refiltering of the
   --  current list with completions, using the current key. The completion
   --  list view will be updated accordingly.
   --  Since: gtk+ 2.4

   function Compute_Prefix
      (Completion : not null access Gtk_Entry_Completion_Record;
       Key        : UTF8_String) return UTF8_String;
   --  Computes the common prefix that is shared by all rows in Completion
   --  that start with Key. If no row matches Key, null will be returned. Note
   --  that a text column must have been set for this function to work, see
   --  Gtk.Entry_Completion.Set_Text_Column for details.
   --  Since: gtk+ 3.4
   --  "key": The text to complete for

   procedure Delete_Action
      (Completion : not null access Gtk_Entry_Completion_Record;
       Index      : Glib.Gint);
   --  Deletes the action at Index_ from Completion's action list.
   --  Note that Index_ is a relative position and the position of an action
   --  may have changed since it was inserted.
   --  Since: gtk+ 2.4
   --  "index_": the index of the item to delete

   function Get_Completion_Prefix
      (Completion : not null access Gtk_Entry_Completion_Record)
       return UTF8_String;
   --  Get the original text entered by the user that triggered the completion
   --  or null if there's no completion ongoing.
   --  Since: gtk+ 2.12

   function Get_Entry
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the entry Completion has been attached to.
   --  Since: gtk+ 2.4

   function Get_Inline_Completion
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
   --  Returns whether the common prefix of the possible completions should be
   --  automatically inserted in the entry.
   --  Since: gtk+ 2.6

   procedure Set_Inline_Completion
      (Completion        : not null access Gtk_Entry_Completion_Record;
       Inline_Completion : Boolean);
   --  Sets whether the common prefix of the possible completions should be
   --  automatically inserted in the entry.
   --  Since: gtk+ 2.6
   --  "inline_completion": True to do inline completion

   function Get_Inline_Selection
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
   --  Returns True if inline-selection mode is turned on.
   --  Since: gtk+ 2.12

   procedure Set_Inline_Selection
      (Completion       : not null access Gtk_Entry_Completion_Record;
       Inline_Selection : Boolean);
   --  Sets whether it is possible to cycle through the possible completions
   --  inside the entry.
   --  Since: gtk+ 2.12
   --  "inline_selection": True to do inline selection

   function Get_Minimum_Key_Length
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Glib.Gint;
   --  Returns the minimum key length as set for Completion.
   --  Since: gtk+ 2.4

   procedure Set_Minimum_Key_Length
      (Completion : not null access Gtk_Entry_Completion_Record;
       Length     : Glib.Gint);
   --  Requires the length of the search key for Completion to be at least
   --  Length. This is useful for long lists, where completing using a small
   --  key takes a lot of time and will come up with meaningless results anyway
   --  (ie, a too large dataset).
   --  Since: gtk+ 2.4
   --  "length": the minimum length of the key in order to start completing

   function Get_Model
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model the Gtk.Entry_Completion.Gtk_Entry_Completion is
   --  using as data source. Returns null if the model is unset.
   --  Since: gtk+ 2.4

   procedure Set_Model
      (Completion : not null access Gtk_Entry_Completion_Record;
       Model      : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model for a Gtk.Entry_Completion.Gtk_Entry_Completion. If
   --  Completion already has a model set, it will remove it before setting the
   --  new model. If model is null, then it will unset the model.
   --  Since: gtk+ 2.4
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model

   function Get_Popup_Completion
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
   --  Returns whether the completions should be presented in a popup window.
   --  Since: gtk+ 2.6

   procedure Set_Popup_Completion
      (Completion       : not null access Gtk_Entry_Completion_Record;
       Popup_Completion : Boolean);
   --  Sets whether the completions should be presented in a popup window.
   --  Since: gtk+ 2.6
   --  "popup_completion": True to do popup completion

   function Get_Popup_Set_Width
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
   --  Returns whether the completion popup window will be resized to the
   --  width of the entry.
   --  Since: gtk+ 2.8

   procedure Set_Popup_Set_Width
      (Completion      : not null access Gtk_Entry_Completion_Record;
       Popup_Set_Width : Boolean);
   --  Sets whether the completion popup window will be resized to be the same
   --  width as the entry.
   --  Since: gtk+ 2.8
   --  "popup_set_width": True to make the width of the popup the same as the
   --  entry

   function Get_Popup_Single_Match
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
   --  Returns whether the completion popup window will appear even if there
   --  is only a single match.
   --  Since: gtk+ 2.8

   procedure Set_Popup_Single_Match
      (Completion         : not null access Gtk_Entry_Completion_Record;
       Popup_Single_Match : Boolean);
   --  Sets whether the completion popup window will appear even if there is
   --  only a single match. You may want to set this to False if you are using
   --  [inline completion][GtkEntryCompletion--inline-completion].
   --  Since: gtk+ 2.8
   --  "popup_single_match": True if the popup should appear even for a single
   --  match

   function Get_Text_Column
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Glib.Gint;
   --  Returns the column in the model of Completion to get strings from.
   --  Since: gtk+ 2.6

   procedure Set_Text_Column
      (Completion : not null access Gtk_Entry_Completion_Record;
       Column     : Glib.Gint);
   --  Convenience function for setting up the most used case of this code: a
   --  completion list with just strings. This function will set up Completion
   --  to have a list displaying all (and just) strings in the completion list,
   --  and to get those strings from Column in the model of Completion.
   --  This functions creates and adds a
   --  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text for the selected column.
   --  If you need to set the text column, but don't want the cell renderer,
   --  use g_object_set to set the
   --  Gtk.Entry_Completion.Gtk_Entry_Completion:text-column property directly.
   --  Since: gtk+ 2.4
   --  "column": the column in the model of Completion to get strings from

   procedure Insert_Action_Markup
      (Completion : not null access Gtk_Entry_Completion_Record;
       Index      : Glib.Gint;
       Markup     : UTF8_String);
   --  Inserts an action in Completion's action item list at position Index_
   --  with markup Markup.
   --  Since: gtk+ 2.4
   --  "index_": the index of the item to insert
   --  "markup": markup of the item to insert

   procedure Insert_Action_Text
      (Completion : not null access Gtk_Entry_Completion_Record;
       Index      : Glib.Gint;
       Text       : UTF8_String);
   --  Inserts an action in Completion's action item list at position Index_
   --  with text Text. If you want the action item to have markup, use
   --  Gtk.Entry_Completion.Insert_Action_Markup.
   --  Note that Index_ is a relative position in the list of actions and the
   --  position of an action can change when deleting a different action.
   --  Since: gtk+ 2.4
   --  "index_": the index of the item to insert
   --  "text": text of the item to insert

   procedure Insert_Prefix
      (Completion : not null access Gtk_Entry_Completion_Record);
   --  Requests a prefix insertion.
   --  Since: gtk+ 2.6

   procedure Set_Match_Func
      (Completion : not null access Gtk_Entry_Completion_Record;
       Func       : Gtk_Entry_Completion_Match_Func);
   --  Sets the match function for Completion to be Func. The match function
   --  is used to determine if a row should or should not be in the completion
   --  list.
   --  Since: gtk+ 2.4
   --  "func": the Gtk_Entry_Completion_Match_Func to use

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Match_Func_User_Data is

      type Gtk_Entry_Completion_Match_Func is access function
        (Completion : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
         Key        : UTF8_String;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         User_Data  : User_Data_Type) return Boolean;
      --  A function which decides whether the row indicated by Iter matches a
      --  given Key, and should be displayed as a possible completion for Key.
      --  Note that Key is normalized and case-folded (see g_utf8_normalize and
      --  g_utf8_casefold). If this is not appropriate, match functions have
      --  access to the unmodified key via `gtk_entry_get_text (GTK_ENTRY
      --  (gtk_entry_completion_get_entry ()))`.
      --  "completion": the Gtk.Entry_Completion.Gtk_Entry_Completion
      --  "key": the string to match, normalized and case-folded
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to match
      --  "user_data": user data given to Gtk.Entry_Completion.Set_Match_Func

      procedure Set_Match_Func
         (Completion : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
          Func       : Gtk_Entry_Completion_Match_Func;
          Func_Data  : User_Data_Type);
      --  Sets the match function for Completion to be Func. The match
      --  function is used to determine if a row should or should not be in the
      --  completion list.
      --  Since: gtk+ 2.4
      --  "func": the Gtk_Entry_Completion_Match_Func to use
      --  "func_data": user data for Func

   end Set_Match_Func_User_Data;

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
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
         (Cell_Layout : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
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
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear
      (Cell_Layout : not null access Gtk_Entry_Completion_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Entry_Completion_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The Gtk.Cell_Area.Gtk_Cell_Area used to layout cell renderers in the
   --  treeview column.
   --
   --  If no area is specified when creating the entry completion with
   --  Gtk.Entry_Completion.Gtk_New_With_Area a horizontally oriented
   --  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box will be used.

   Inline_Completion_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the common prefix of the possible completions should
   --  be inserted automatically in the entry. Note that this requires
   --  text-column to be set, even if you are using a custom match function.

   Inline_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the possible completions on the popup will appear in
   --  the entry as you navigate through them.

   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int;

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model

   Popup_Completion_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the possible completions should be shown in a popup
   --  window.

   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the completions popup window will be resized to the
   --  width of the entry.

   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the completions popup window will shown for a single
   --  possible completion. You probably want to set this to False if you are
   --  using [inline completion][GtkEntryCompletion--inline-completion].

   Text_Column_Property : constant Glib.Properties.Property_Int;
   --  The column of the model containing the strings. Note that the strings
   --  must be UTF-8.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Entry_Completion_Gint_Void is not null access procedure
     (Self  : access Gtk_Entry_Completion_Record'Class;
      Index : Glib.Gint);

   type Cb_GObject_Gint_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Index : Glib.Gint);

   Signal_Action_Activated : constant Glib.Signal_Name := "action-activated";
   procedure On_Action_Activated
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Gint_Void;
       After : Boolean := False);
   procedure On_Action_Activated
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when an action is activated.

   type Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean is not null access function
     (Self  : access Gtk_Entry_Completion_Record'Class;
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   type Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   Signal_Cursor_On_Match : constant Glib.Signal_Name := "cursor-on-match";
   procedure On_Cursor_On_Match
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After : Boolean := False);
   procedure On_Cursor_On_Match
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when a match from the cursor is on a match of the list.
   --  The default behaviour is to replace the contents of the entry with the
   --  contents of the text column in the row pointed to by Iter.
   --
   --  Note that Model is the model that was passed to
   --  Gtk.Entry_Completion.Set_Model.
   -- 
   --  Callback parameters:
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model containing the matches
   --    --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter positioned at the selected match
   --    --  Returns True if the signal has been handled

   type Cb_Gtk_Entry_Completion_UTF8_String_Boolean is not null access function
     (Self   : access Gtk_Entry_Completion_Record'Class;
      Prefix : UTF8_String) return Boolean;

   type Cb_GObject_UTF8_String_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Prefix : UTF8_String) return Boolean;

   Signal_Insert_Prefix : constant Glib.Signal_Name := "insert-prefix";
   procedure On_Insert_Prefix
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_UTF8_String_Boolean;
       After : Boolean := False);
   procedure On_Insert_Prefix
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the inline autocompletion is triggered. The default
   --  behaviour is to make the entry display the whole prefix and select the
   --  newly inserted part.
   --
   --  Applications may connect to this signal in order to insert only a
   --  smaller part of the Prefix into the entry - e.g. the entry used in the
   --  Gtk.File_Chooser.Gtk_File_Chooser inserts only the part of the prefix up
   --  to the next '/'.
   -- 
   --  Callback parameters:
   --    --  "prefix": the common prefix of all possible completions
   --    --  Returns True if the signal has been handled

   Signal_Match_Selected : constant Glib.Signal_Name := "match-selected";
   procedure On_Match_Selected
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After : Boolean := False);
   procedure On_Match_Selected
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when a match from the list is selected. The default
   --  behaviour is to replace the contents of the entry with the contents of
   --  the text column in the row pointed to by Iter.
   --
   --  Note that Model is the model that was passed to
   --  Gtk.Entry_Completion.Set_Model.
   -- 
   --  Callback parameters:
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model containing the matches
   --    --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter positioned at the selected match
   --    --  Returns True if the signal has been handled

   type Cb_Gtk_Entry_Completion_Void is not null access procedure
     (Self : access Gtk_Entry_Completion_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_No_Matches : constant Glib.Signal_Name := "no-matches";
   procedure On_No_Matches
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Void;
       After : Boolean := False);
   procedure On_No_Matches
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the filter model has zero number of rows in
   --  completion_complete method. (In other words when GtkEntryCompletion is
   --  out of suggestions)

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Entry_Completion_Record, Gtk_Entry_Completion);
   function "+"
     (Widget : access Gtk_Entry_Completion_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Entry_Completion
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Entry_Completion_Record, Gtk_Entry_Completion);
   function "+"
     (Widget : access Gtk_Entry_Completion_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Entry_Completion
   renames Implements_Gtk_Cell_Layout.To_Object;

private
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");
   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-single-match");
   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-set-width");
   Popup_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-completion");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("minimum-key-length");
   Inline_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-selection");
   Inline_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-completion");
   Cell_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area");
end Gtk.Entry_Completion;
