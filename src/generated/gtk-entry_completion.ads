------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
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
--  reason, you need the original model, use gtk_tree_model_filter_get_model.
--  Don't forget to use gtk_tree_model_filter_convert_iter_to_child_iter to
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

   type Gtk_Entry_Completion_Match_Func is access function
     (Completion : not null access Gtk_Entry_Completion_Record'Class;
      Key        : UTF8_String;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  A function which decides whether the row indicated by Iter matches a
   --  given Key, and should be displayed as a possible completion for Key.
   --  Note that Key is normalized and case-folded (see g_utf8_normalize and
   --  g_utf8_casefold). If this is not appropriate, match functions have
   --  access to the unmodified key via 'gtk_entry_get_text (GTK_ENTRY
   --  (gtk_entry_completion_get_entry (<!-- -->)))'.
   --  for Key
   --  "completion": the Gtk.Entry_Completion.Gtk_Entry_Completion
   --  "key": the string to match, normalized and case-folded
   --  "iter": a Gtk.Tree_Iter.Gtk_Tree_Iter indicating the row to match

   type Cell_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Iter.Gtk_Tree_Iter indicating the row to set the
   --  value for

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Completion : out Gtk_Entry_Completion);
   procedure Initialize
      (Completion : not null access Gtk_Entry_Completion_Record'Class);
   --  Creates a new Gtk.Entry_Completion.Gtk_Entry_Completion object.
   --  Since: gtk+ 2.4

   procedure Gtk_New_With_Area
      (Completion : out Gtk_Entry_Completion;
       Area       : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
      ;
   procedure Initialize_With_Area
      (Completion : not null access Gtk_Entry_Completion_Record'Class;
       Area       : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
      ;
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

   procedure Delete_Action
      (Completion : not null access Gtk_Entry_Completion_Record;
       Index      : Gint);
   --  Deletes the action at Index_ from Completion's action list.
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
   procedure Set_Inline_Selection
      (Completion       : not null access Gtk_Entry_Completion_Record;
       Inline_Selection : Boolean);
   --  Sets whether it is possible to cycle through the possible completions
   --  inside the entry.
   --  Since: gtk+ 2.12
   --  "inline_selection": True to do inline selection

   function Get_Minimum_Key_Length
      (Completion : not null access Gtk_Entry_Completion_Record) return Gint;
   procedure Set_Minimum_Key_Length
      (Completion : not null access Gtk_Entry_Completion_Record;
       Length     : Gint);
   --  Requires the length of the search key for Completion to be at least
   --  Length. This is useful for long lists, where completing using a small
   --  key takes a lot of time and will come up with meaningless results anyway
   --  (ie, a too large dataset).
   --  Since: gtk+ 2.4
   --  "length": the minimum length of the key in order to start completing

   function Get_Model
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   procedure Set_Model
      (Completion : not null access Gtk_Entry_Completion_Record;
       Model      : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Sets the model for a Gtk.Entry_Completion.Gtk_Entry_Completion. If
   --  Completion already has a model set, it will remove it before setting the
   --  new model. If model is null, then it will unset the model.
   --  Since: gtk+ 2.4
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model

   function Get_Popup_Completion
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
   procedure Set_Popup_Completion
      (Completion       : not null access Gtk_Entry_Completion_Record;
       Popup_Completion : Boolean);
   --  Sets whether the completions should be presented in a popup window.
   --  Since: gtk+ 2.6
   --  "popup_completion": True to do popup completion

   function Get_Popup_Set_Width
      (Completion : not null access Gtk_Entry_Completion_Record)
       return Boolean;
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
   procedure Set_Popup_Single_Match
      (Completion         : not null access Gtk_Entry_Completion_Record;
       Popup_Single_Match : Boolean);
   --  Sets whether the completion popup window will appear even if there is
   --  only a single match. You may want to set this to False if you are using
   --  <link linkend="GtkEntryCompletion--inline-completion">inline
   --  completion</link>.
   --  Since: gtk+ 2.8
   --  "popup_single_match": True if the popup should appear even for a single
   --  match

   function Get_Text_Column
      (Completion : not null access Gtk_Entry_Completion_Record) return Gint;
   procedure Set_Text_Column
      (Completion : not null access Gtk_Entry_Completion_Record;
       Column     : Gint);
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
       Index      : Gint;
       Markup     : UTF8_String);
   --  Inserts an action in Completion's action item list at position Index_
   --  with markup Markup.
   --  Since: gtk+ 2.4
   --  "index_": the index of the item to insert
   --  "markup": markup of the item to insert

   procedure Insert_Action_Text
      (Completion : not null access Gtk_Entry_Completion_Record;
       Index      : Gint;
       Text       : UTF8_String);
   --  Inserts an action in Completion's action item list at position Index_
   --  with text Text. If you want the action item to have markup, use
   --  Gtk.Entry_Completion.Insert_Action_Markup.
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
      --  access to the unmodified key via 'gtk_entry_get_text (GTK_ENTRY
      --  (gtk_entry_completion_get_entry (<!-- -->)))'.
      --  for Key
      --  "completion": the Gtk.Entry_Completion.Gtk_Entry_Completion
      --  "key": the string to match, normalized and case-folded
      --  "iter": a Gtk.Tree_Iter.Gtk_Tree_Iter indicating the row to match
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
       Func        : Gtk.Cell_Layout.Cell_Data_Func);
   --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk.Cell_Layout.Cell_Data_Func to use, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Cell_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function which should set the value of Cell_Layout's cell renderer(s)
      --  as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Iter.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Entry_Completion.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Cell_Layout : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Cell_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Since: gtk+ 2.4
      --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
      --  "func": the Gtk.Cell_Layout.Cell_Data_Func to use, or null
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
       Column      : Gint);

   procedure Clear
      (Cell_Layout : not null access Gtk_Entry_Completion_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Entry_Completion_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      ;

   function Get_Cells
      (Cell_Layout : not null access Gtk_Entry_Completion_Record)
       return Glib.Object.Object_Simple_List.GList;

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
       Position    : Gint);

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

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Cell_Area_Property
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  Flags: read-write
   --  The Gtk.Cell_Area.Gtk_Cell_Area used to layout cell renderers in the
   --  treeview column.
   --
   --  If no area is specified when creating the entry completion with
   --  Gtk.Entry_Completion.Gtk_New_With_Area a horizontally oriented
   --  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box will be used.
   --
   --  Name: Inline_Completion_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the common prefix of the possible completions should
   --  be inserted automatically in the entry. Note that this requires
   --  text-column to be set, even if you are using a custom match function.
   --
   --  Name: Inline_Selection_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the possible completions on the popup will appear in
   --  the entry as you navigate through them.
   --
   --  Name: Minimum_Key_Length_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Model_Property
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model
   --  Flags: read-write
   --
   --  Name: Popup_Completion_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the possible completions should be shown in a popup
   --  window.
   --
   --  Name: Popup_Set_Width_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the completions popup window will be resized to the
   --  width of the entry.
   --
   --  Name: Popup_Single_Match_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Determines whether the completions popup window will shown for a single
   --  possible completion. You probably want to set this to False if you are
   --  using <link linkend="GtkEntryCompletion--inline-completion">inline
   --  completion</link>.
   --
   --  Name: Text_Column_Property
   --  Type: Gint
   --  Flags: read-write
   --  The column of the model containing the strings. Note that the strings
   --  must be UTF-8.

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   Inline_Completion_Property : constant Glib.Properties.Property_Boolean;
   Inline_Selection_Property : constant Glib.Properties.Property_Boolean;
   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int;
   Model_Property : constant Glib.Properties.Property_Object;
   Popup_Completion_Property : constant Glib.Properties.Property_Boolean;
   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean;
   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean;
   Text_Column_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "action-activated"
   --     procedure Handler
   --       (Self  : access Gtk_Entry_Completion_Record'Class;
   --        Index : Gint);
   --    --  "index": the index of the activated action
   --  Gets emitted when an action is activated.
   --
   --  "cursor-on-match"
   --     function Handler
   --       (Self  : access Gtk_Entry_Completion_Record'Class;
   --        Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
   --        Iter  : Gtk.Tree_Iter.Gtk_Tree_Iter) return Boolean;
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model containing the matches
   --    --  "iter": a Gtk.Tree_Iter.Gtk_Tree_Iter positioned at the selected match
   --  Gets emitted when a match from the cursor is on a match of the list.
   --  The default behaviour is to replace the contents of the entry with the
   --  contents of the text column in the row pointed to by Iter.
   --
   --  Note that Model is the model that was passed to
   --  Gtk.Entry_Completion.Set_Model.
   --
   --  Returns True if the signal has been handled
   --
   --  "insert-prefix"
   --     function Handler
   --       (Self   : access Gtk_Entry_Completion_Record'Class;
   --        Prefix : UTF8_String) return Boolean;
   --    --  "prefix": the common prefix of all possible completions
   --  Gets emitted when the inline autocompletion is triggered. The default
   --  behaviour is to make the entry display the whole prefix and select the
   --  newly inserted part.
   --
   --  Applications may connect to this signal in order to insert only a
   --  smaller part of the Prefix into the entry - e.g. the entry used in the
   --  Gtk.File_Chooser.Gtk_File_Chooser inserts only the part of the prefix up
   --  to the next '/'.
   --
   --  Returns True if the signal has been handled
   --
   --  "match-selected"
   --     function Handler
   --       (Self  : access Gtk_Entry_Completion_Record'Class;
   --        Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
   --        Iter  : Gtk.Tree_Iter.Gtk_Tree_Iter) return Boolean;
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model containing the matches
   --    --  "iter": a Gtk.Tree_Iter.Gtk_Tree_Iter positioned at the selected match
   --  Gets emitted when a match from the list is selected. The default
   --  behaviour is to replace the contents of the entry with the contents of
   --  the text column in the row pointed to by Iter.
   --
   --  Note that Model is the model that was passed to
   --  Gtk.Entry_Completion.Set_Model.
   --
   --  Returns True if the signal has been handled

   Signal_Action_Activated : constant Glib.Signal_Name := "action-activated";
   Signal_Cursor_On_Match : constant Glib.Signal_Name := "cursor-on-match";
   Signal_Insert_Prefix : constant Glib.Signal_Name := "insert-prefix";
   Signal_Match_Selected : constant Glib.Signal_Name := "match-selected";

private
   Cell_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area");
   Inline_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-completion");
   Inline_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-selection");
   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("minimum-key-length");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
   Popup_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-completion");
   Popup_Set_Width_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-set-width");
   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-single-match");
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");
end Gtk.Entry_Completion;
