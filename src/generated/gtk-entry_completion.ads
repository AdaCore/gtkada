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

--  `GtkEntryCompletion` is an auxiliary object to provide completion
--  functionality for `GtkEntry`.
--
--  It implements the [ifaceGtk.CellLayout] interface, to allow the user to
--  add extra cells to the `GtkTreeView` with completion matches.
--
--  "Completion functionality" means that when the user modifies the text in
--  the entry, `GtkEntryCompletion` checks which rows in the model match the
--  current content of the entry, and displays a list of matches. By default,
--  the matching is done by comparing the entry text case-insensitively against
--  the text column of the model (see
--  [methodGtk.EntryCompletion.set_text_column]), but this can be overridden
--  with a custom match function (see
--  [methodGtk.EntryCompletion.set_match_func]).
--
--  When the user selects a completion, the content of the entry is updated.
--  By default, the content of the entry is replaced by the text column of the
--  model, but this can be overridden by connecting to the
--  [signalGtk.EntryCompletion::match-selected] signal and updating the entry
--  in the signal handler. Note that you should return True from the signal
--  handler to suppress the default behaviour.
--
--  To add completion functionality to an entry, use
--  [methodGtk.Entry.set_completion].
--
--  `GtkEntryCompletion` uses a [classGtk.TreeModelFilter] model to represent
--  the subset of the entire model that is currently matching. While the
--  `GtkEntryCompletion` signals [signalGtk.EntryCompletion::match-selected]
--  and [signalGtk.EntryCompletion::cursor-on-match] take the original model
--  and an iter pointing to that model as arguments, other callbacks and
--  signals (such as `GtkCellLayoutDataFunc` or
--  [signalGtk.CellArea::apply-attributes)] will generally take the filter
--  model as argument. As long as you are only calling
--  [methodGtk.TreeModel.get], this will make no difference to you. If for some
--  reason, you need the original model, use
--  [methodGtk.TreeModelFilter.get_model]. Don't forget to use
--  [methodGtk.TreeModelFilter.convert_iter_to_child_iter] to obtain a matching
--  iter.

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
   --  access to the unmodified key via `gtk_editable_get_text (GTK_EDITABLE
   --  (gtk_entry_completion_get_entry ()))`.
   --  @param Completion the `GtkEntryCompletion`
   --  @param Key the string to match, normalized and case-folded
   --  @param Iter a `GtkTreeIter` indicating the row to match
   --  @return True if Iter should be displayed as a possible completion for
   --  Key

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

   procedure Gtk_New (Self : out Gtk_Entry_Completion);
   procedure Initialize
      (Self : not null access Gtk_Entry_Completion_Record'Class);
   --  Creates a new `GtkEntryCompletion` object.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Entry_Completion_New return Gtk_Entry_Completion;
   --  Creates a new `GtkEntryCompletion` object.

   procedure Gtk_New_With_Area
      (Self : out Gtk_Entry_Completion;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area
      (Self : not null access Gtk_Entry_Completion_Record'Class;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new `GtkEntryCompletion` object using the specified Area.
   --  The `GtkCellArea` is used to layout cells in the underlying
   --  `GtkTreeViewColumn` for the drop-down menu.
   --  Initialize_With_Area does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Area the `GtkCellArea` used to layout cells

   function Gtk_Entry_Completion_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Entry_Completion;
   --  Creates a new `GtkEntryCompletion` object using the specified Area.
   --  The `GtkCellArea` is used to layout cells in the underlying
   --  `GtkTreeViewColumn` for the drop-down menu.
   --  @param Area the `GtkCellArea` used to layout cells

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_entry_completion_get_type");

   -------------
   -- Methods --
   -------------

   procedure Complete (Self : not null access Gtk_Entry_Completion_Record);
   pragma Obsolescent (Complete);
   --  Requests a completion operation, or in other words a refiltering of the
   --  current list with completions, using the current key.
   --  The completion list view will be updated accordingly.
   --  Deprecated since 4.10, 1

   function Compute_Prefix
      (Self : not null access Gtk_Entry_Completion_Record;
       Key  : UTF8_String) return UTF8_String;
   pragma Obsolescent (Compute_Prefix);
   --  Computes the common prefix that is shared by all rows in Completion
   --  that start with Key.
   --  If no row matches Key, null will be returned. Note that a text column
   --  must have been set for this function to work, see
   --  [methodGtk.EntryCompletion.set_text_column] for details.
   --  Deprecated since 4.10, 1
   --  @param Key The text to complete for
   --  @return The common prefix all rows starting with Key

   function Get_Completion_Prefix
      (Self : not null access Gtk_Entry_Completion_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Completion_Prefix);
   --  Get the original text entered by the user that triggered the completion
   --  or null if there's no completion ongoing.
   --  Deprecated since 4.10, 1
   --  @return the prefix for the current completion

   function Get_Entry
      (Self : not null access Gtk_Entry_Completion_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Entry);
   --  Gets the entry Completion has been attached to.
   --  Deprecated since 4.10, 1
   --  @return The entry Completion has been attached to

   function Get_Inline_Completion
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean;
   pragma Obsolescent (Get_Inline_Completion);
   --  Returns whether the common prefix of the possible completions should be
   --  automatically inserted in the entry.
   --  Deprecated since 4.10, 1
   --  @return True if inline completion is turned on

   procedure Set_Inline_Completion
      (Self              : not null access Gtk_Entry_Completion_Record;
       Inline_Completion : Boolean);
   pragma Obsolescent (Set_Inline_Completion);
   --  Sets whether the common prefix of the possible completions should be
   --  automatically inserted in the entry.
   --  Deprecated since 4.10, 1
   --  @param Inline_Completion True to do inline completion

   function Get_Inline_Selection
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean;
   pragma Obsolescent (Get_Inline_Selection);
   --  Returns True if inline-selection mode is turned on.
   --  Deprecated since 4.10, 1
   --  @return True if inline-selection mode is on

   procedure Set_Inline_Selection
      (Self             : not null access Gtk_Entry_Completion_Record;
       Inline_Selection : Boolean);
   pragma Obsolescent (Set_Inline_Selection);
   --  Sets whether it is possible to cycle through the possible completions
   --  inside the entry.
   --  Deprecated since 4.10, 1
   --  @param Inline_Selection True to do inline selection

   function Get_Minimum_Key_Length
      (Self : not null access Gtk_Entry_Completion_Record) return Glib.Gint;
   pragma Obsolescent (Get_Minimum_Key_Length);
   --  Returns the minimum key length as set for Completion.
   --  Deprecated since 4.10, 1
   --  @return The currently used minimum key length

   procedure Set_Minimum_Key_Length
      (Self   : not null access Gtk_Entry_Completion_Record;
       Length : Glib.Gint);
   pragma Obsolescent (Set_Minimum_Key_Length);
   --  Requires the length of the search key for Completion to be at least
   --  Length.
   --  This is useful for long lists, where completing using a small key takes
   --  a lot of time and will come up with meaningless results anyway (ie, a
   --  too large dataset).
   --  Deprecated since 4.10, 1
   --  @param Length the minimum length of the key in order to start
   --  completing

   function Get_Model
      (Self : not null access Gtk_Entry_Completion_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   pragma Obsolescent (Get_Model);
   --  Returns the model the `GtkEntryCompletion` is using as data source.
   --  Returns null if the model is unset.
   --  Deprecated since 4.10, 1
   --  @return A `GtkTreeModel`

   procedure Set_Model
      (Self  : not null access Gtk_Entry_Completion_Record;
       Model : Gtk.Tree_Model.Gtk_Tree_Model);
   pragma Obsolescent (Set_Model);
   --  Sets the model for a `GtkEntryCompletion`.
   --  If Completion already has a model set, it will remove it before setting
   --  the new model. If model is null, then it will unset the model.
   --  Deprecated since 4.10, 1
   --  @param Model the `GtkTreeModel`

   function Get_Popup_Completion
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean;
   pragma Obsolescent (Get_Popup_Completion);
   --  Returns whether the completions should be presented in a popup window.
   --  Deprecated since 4.10, 1
   --  @return True if popup completion is turned on

   procedure Set_Popup_Completion
      (Self             : not null access Gtk_Entry_Completion_Record;
       Popup_Completion : Boolean);
   pragma Obsolescent (Set_Popup_Completion);
   --  Sets whether the completions should be presented in a popup window.
   --  Deprecated since 4.10, 1
   --  @param Popup_Completion True to do popup completion

   function Get_Popup_Set_Width
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean;
   pragma Obsolescent (Get_Popup_Set_Width);
   --  Returns whether the completion popup window will be resized to the
   --  width of the entry.
   --  Deprecated since 4.10, 1
   --  @return True if the popup window will be resized to the width of the
   --  entry

   procedure Set_Popup_Set_Width
      (Self            : not null access Gtk_Entry_Completion_Record;
       Popup_Set_Width : Boolean);
   pragma Obsolescent (Set_Popup_Set_Width);
   --  Sets whether the completion popup window will be resized to be the same
   --  width as the entry.
   --  Deprecated since 4.10, 1
   --  @param Popup_Set_Width True to make the width of the popup the same as
   --  the entry

   function Get_Popup_Single_Match
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean;
   pragma Obsolescent (Get_Popup_Single_Match);
   --  Returns whether the completion popup window will appear even if there
   --  is only a single match.
   --  Deprecated since 4.10, 1
   --  @return True if the popup window will appear regardless of the number
   --  of matches

   procedure Set_Popup_Single_Match
      (Self               : not null access Gtk_Entry_Completion_Record;
       Popup_Single_Match : Boolean);
   pragma Obsolescent (Set_Popup_Single_Match);
   --  Sets whether the completion popup window will appear even if there is
   --  only a single match.
   --  You may want to set this to False if you are using
   --  [propertyGtk.EntryCompletion:inline-completion].
   --  Deprecated since 4.10, 1
   --  @param Popup_Single_Match True if the popup should appear even for a
   --  single match

   function Get_Text_Column
      (Self : not null access Gtk_Entry_Completion_Record) return Glib.Gint;
   pragma Obsolescent (Get_Text_Column);
   --  Returns the column in the model of Completion to get strings from.
   --  Deprecated since 4.10, 1
   --  @return the column containing the strings

   procedure Set_Text_Column
      (Self   : not null access Gtk_Entry_Completion_Record;
       Column : Glib.Gint);
   pragma Obsolescent (Set_Text_Column);
   --  Convenience function for setting up the most used case of this code: a
   --  completion list with just strings.
   --  This function will set up Completion to have a list displaying all (and
   --  just) strings in the completion list, and to get those strings from
   --  Column in the model of Completion.
   --  This functions creates and adds a `GtkCellRendererText` for the
   --  selected column. If you need to set the text column, but don't want the
   --  cell renderer, use g_object_set to set the
   --  [propertyGtk.EntryCompletion:text-column] property directly.
   --  Deprecated since 4.10, 1
   --  @param Column the column in the model of Completion to get strings from

   procedure Insert_Prefix
      (Self : not null access Gtk_Entry_Completion_Record);
   pragma Obsolescent (Insert_Prefix);
   --  Requests a prefix insertion.
   --  Deprecated since 4.10, 1

   procedure Set_Match_Func
      (Self : not null access Gtk_Entry_Completion_Record;
       Func : Gtk_Entry_Completion_Match_Func);
   pragma Obsolescent (Set_Match_Func);
   --  Sets the match function for Completion to be Func.
   --  The match function is used to determine if a row should or should not
   --  be in the completion list.
   --  Deprecated since 4.10, 1
   --  @param Func the `GtkEntryCompletion`MatchFunc to use

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
      --  access to the unmodified key via `gtk_editable_get_text (GTK_EDITABLE
      --  (gtk_entry_completion_get_entry ()))`.
      --  @param Completion the `GtkEntryCompletion`
      --  @param Key the string to match, normalized and case-folded
      --  @param Iter a `GtkTreeIter` indicating the row to match
      --  @param User_Data user data given to Gtk.Entry_Completion.Set_Match_Func
      --  @return True if Iter should be displayed as a possible completion for
      --  Key

      procedure Set_Match_Func
         (Self      : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
          Func      : Gtk_Entry_Completion_Match_Func;
          Func_Data : User_Data_Type);
      pragma Obsolescent (Set_Match_Func);
      --  Sets the match function for Completion to be Func.
      --  The match function is used to determine if a row should or should
      --  not be in the completion list.
      --  Deprecated since 4.10, 1
      --  @param Func the `GtkEntryCompletion`MatchFunc to use
      --  @param Func_Data user data for Func

   end Set_Match_Func_User_Data;

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Entry_Completion_Record;
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
         (Self      : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
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

   procedure Add_Attribute
      (Self      : not null access Gtk_Entry_Completion_Record;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   pragma Obsolescent (Add_Attribute);

   procedure Clear (Self : not null access Gtk_Entry_Completion_Record);
   pragma Obsolescent (Clear);

   procedure Clear_Attributes
      (Self : not null access Gtk_Entry_Completion_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Clear_Attributes);

   function Get_Cells
      (Self : not null access Gtk_Entry_Completion_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   pragma Obsolescent (Get_Cells);

   procedure Pack_End
      (Self   : not null access Gtk_Entry_Completion_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_End);

   procedure Pack_Start
      (Self   : not null access Gtk_Entry_Completion_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_Start);

   procedure Reorder
      (Self     : not null access Gtk_Entry_Completion_Record;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint);
   pragma Obsolescent (Reorder);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The `GtkCellArea` used to layout cell renderers in the treeview column.
   --
   --  If no area is specified when creating the entry completion with
   --  [ctorGtk.EntryCompletion.new_with_area], a horizontally oriented
   --  [classGtk.CellAreaBox] will be used.

   Inline_Completion_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the common prefix of the possible completions should
   --  be inserted automatically in the entry.
   --
   --  Note that this requires text-column to be set, even if you are using a
   --  custom match function.

   Inline_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the possible completions on the popup will appear in
   --  the entry as you navigate through them.

   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int;
   --  The minimum key length as set for completion.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  The model used as data source.

   Popup_Completion_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the possible completions should be shown in a popup
   --  window.

   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the completions popup window will be resized to the
   --  width of the entry.

   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the completions popup window will shown for a single
   --  possible completion.
   --
   --  You probably want to set this to False if you are using
   --  [propertyGtk.EntryCompletion:inline-completion].

   Text_Column_Property : constant Glib.Properties.Property_Int;
   --  The column of the model containing the strings.
   --
   --  Note that the strings must be UTF-8.

   -------------
   -- Signals --
   -------------

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
   --  Emitted when a match from the cursor is on a match of the list.
   --
   --  The default behaviour is to replace the contents of the entry with the
   --  contents of the text column in the row pointed to by Iter.
   --
   --  Note that Model is the model that was passed to
   --  [methodGtk.EntryCompletion.set_model].
   -- 
   --  Callback parameters:
   --    --  @param Model the `GtkTreeModel` containing the matches
   --    --  @param Iter a `GtkTreeIter` positioned at the selected match

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
   --  Emitted when the inline autocompletion is triggered.
   --
   --  The default behaviour is to make the entry display the whole prefix and
   --  select the newly inserted part.
   --
   --  Applications may connect to this signal in order to insert only a
   --  smaller part of the Prefix into the entry - e.g. the entry used in the
   --  `GtkFileChooser` inserts only the part of the prefix up to the next '/'.
   -- 
   --  Callback parameters:
   --    --  @param Prefix the common prefix of all possible completions

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
   --  Emitted when a match from the list is selected.
   --
   --  The default behaviour is to replace the contents of the entry with the
   --  contents of the text column in the row pointed to by Iter.
   --
   --  Note that Model is the model that was passed to
   --  [methodGtk.EntryCompletion.set_model].
   -- 
   --  Callback parameters:
   --    --  @param Model the `GtkTreeModel` containing the matches
   --    --  @param Iter a `GtkTreeIter` positioned at the selected match

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
   --  Emitted when the filter model has zero number of rows in
   --  completion_complete method.
   --
   --  In other words when `GtkEntryCompletion` is out of suggestions.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.CellLayout"

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
