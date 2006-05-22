-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This widget provides completion functionality for Gtk.Gentry.Gtk_Entry.
--
--  "Completion functionality" means that when the user modifies the text in
--  the entry, GtkEntryCompletion checks which rows in the model match the
--  current content of the entry, and displays a list of matches. By default,
--  the matching is done by comparing the entry text case-insensitively against
--  the text column of the model (see Set_Text_Column), but this can be
--  overridden with a custom match function (see Set_Match_Func).
--
--  When the user selects a completion, the content of the entry is updated. By
--  default, the content of the entry is replaced by the text column of the
--  model, but this can be overridden by connecting to the ::match-selected
--  signal and updating the entry in the signal handler. Note that you should
--  return TRUE from the signal handler to suppress the default behaviour.
--
--  To add completion functionality to an entry, use Gtk.Entry.Set_Completion.
--
--  In addition to regular completion matches, which will be inserted into the
--  entry when they are selected, GtkEntryCompletion also allows to display
--  "actions" in the popup window. Their appearance is similar to menuitems, to
--  differentiate them clearly from completion strings. When an action is
--  selected, the ::action-activated signal is emitted.
--  </description>
--  <c_version>2.8.17</c_version>

with Glib.Object;
with Glib.Properties;
with Gtk.Tree_Model;
with Gtk.Widget;

package Gtk.Entry_Completion is
   type Gtk_Entry_Completion_Record is new Glib.Object.GObject_Record
      with null record;
   type Gtk_Entry_Completion is access all Gtk_Entry_Completion_Record'Class;

   procedure Gtk_New (Completion : out Gtk_Entry_Completion);
   procedure Initialize
     (Completion : access Gtk_Entry_Completion_Record'Class);
   --  Creates or initializes a new completion object

   function Get_Type return Glib.GType;
   --  Return the internal type used for this object

   procedure Complete
     (Completion : access Gtk_Entry_Completion_Record);
   --  Requests a completion operation, or in other words a refiltering of the
   --  current list with completions, using the current key. The completion
   --  list view will be updated accordingly.

   procedure Delete_Action
     (Completion : access Gtk_Entry_Completion_Record;
      Index      : Gint);
   --  Deletes the action at index from completion's action list.

   function Get_Entry
     (Completion : access Gtk_Entry_Completion_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Gets the entry completion has been attached to.

   procedure Set_Inline_Completion
     (Completion        : access Gtk_Entry_Completion_Record;
      Inline_Completion : Boolean);
   function Get_Inline_Completion
     (Completion : access Gtk_Entry_Completion_Record)
      return Boolean;
   --  Returns whether the common prefix of the possible completions should
   --  be automatically inserted in the entry.
   --  This text appears greyed out, and is removed when the user types some
   --  text not compatible with the possible completions

   procedure Set_Minimum_Key_Length
     (Completion : access Gtk_Entry_Completion_Record;
      Length     : Gint);
   function Get_Minimum_Key_Length
     (Completion : access Gtk_Entry_Completion_Record) return Gint;
   --  Requires the length of the search key for completion to be at least
   --  length. This is useful for long lists, where completing using a small
   --  key takes a lot of time and will come up with meaningless results anyway
   --  (ie, a too large dataset).
   --  This is the minimal number of characters the user must start typing
   --  before any completion is attempted

   procedure Set_Model
     (Completion : access Gtk_Entry_Completion_Record;
      Model      : Gtk.Tree_Model.Gtk_Tree_Model);
   function Get_Model
     (Completion : access Gtk_Entry_Completion_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model the completion is using as data source.
   --  Returns null if the model is unset (setting it to null unsets the
   --  current model)

   procedure Set_Popup_Completion
     (Completion       : access Gtk_Entry_Completion_Record;
      Popup_Completion : Boolean);
   function Get_Popup_Completion
     (Completion : access Gtk_Entry_Completion_Record)
      return Boolean;
   --  Returns whether the completions should be presented in a popup window.
   --  This is to be used in addition to, or instead of, Get_Inline_Completion.

   procedure Set_Popup_Set_Width
     (Completion      : access Gtk_Entry_Completion_Record;
      Popup_Set_Width : Boolean);
   function Get_Popup_Set_Width
     (Completion : access Gtk_Entry_Completion_Record)
      return Boolean;
   --  Returns whether the  completion popup window will be resized to the
   --  width of the entry.

   procedure Set_Popup_Single_Match
     (Completion         : access Gtk_Entry_Completion_Record;
      Popup_Single_Match : Boolean);
   function Get_Popup_Single_Match
     (Completion : access Gtk_Entry_Completion_Record)
      return Boolean;
   --  Returns whether the completion popup window will appear even if there is
   --  only a single match.
   --  You may want to set this to False if you are using inline completion.

   procedure Set_Text_Column
     (Completion : access Gtk_Entry_Completion_Record;
      Column     : Gint);
   function Get_Text_Column
     (Completion : access Gtk_Entry_Completion_Record) return Gint;
   --  Convenience function for setting up the most used case of this code: a
   --  completion list with just strings. This function will set up completion
   --  to have a list displaying all (and just) strings in the completion list,
   --  and to get those strings from column in the model of completion.
   --
   --  This functions creates and adds a #GtkCellRendererText for the selected
   --  column. If you need to set the text column, but don't want the cell
   --  renderer, use Set_Property to set the ::text_column property directly.

   procedure Insert_Action_Markup
     (Completion : access Gtk_Entry_Completion_Record;
      Index      : Gint;
      Markup     : String);
   --  Inserts an action in ccompletion's action item list at position index
   --  with the given markup. Markup can be used to represent bold text, for
   --  instance with "<b>bold</b> text"

   procedure Insert_Action_Text
     (Completion : access Gtk_Entry_Completion_Record;
      Index     : Gint;
      Text       : String);
   --  Inserts an action in completion's action item list at position index
   --  with text Text. If you want the action item to have markup, use
   --  Insert_Action_Markup.

   procedure Insert_Prefix (Completion : access Gtk_Entry_Completion_Record);
   --  Requests a prefix insertion.

   generic
      type Data_Type (<>) is private;
   package Match_Functions is
      type Gtk_Entry_Completion_Match_Func is access
        function (Completion : access Gtk_Entry_Completion_Record'Class;
                  Key        : String;
                  Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
                  User_Data  : Data_Type) return Boolean;

      type Destroy_Notify is access procedure (Data : in out Data_Type);

      procedure Set_Match_Func
        (Completion  : access Gtk_Entry_Completion_Record;
         Func        : Gtk_Entry_Completion_Match_Func;
         Func_Data   : Data_Type;
         Func_Notify : Destroy_Notify);
      --  Sets the match function for completion to be Func. The match function
      --  is used to determine if a row should or should not be in the
      --  completion list.
   end Match_Functions;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "insert_prefix"
   --    procedure Handler
   --      (Completion : access Gtk_Entry_Completion_Record'Class;
   --       Prefix     : String);
   --    Gets emitted when the inline autocompletion is triggered. The default
   --    behaviour is to make the entry display the whole prefix and select the
   --    newly inserted part.
   --    Applications may connect to this signal in order to insert only
   --    smaller part of the Prefix into the entry - e.g. the entry used in
   --    the #GtkFileChooser inserts only the part of the prefix up to the next
   --    '/'.
   --    Return value: %TRUE if the signal has been handled
   --
   --  - "action_activated"
   --    procedure Handler
   --       (Completion : access Gtk_Entry_Completion_Record'Class;
   --        Index      : Gint);
   --    Gets emitted when an action is activated.
   --
   --  - "match_selected"
   --    procedure Handler
   --       (Completion : access Gtk_Entry_Completion_Record'Class;
   --        Model      : Gtk_Tree_Model;
   --        Iter       : Gtk_Tree_Iter);
   --    Gets emitted when a match from the list is selected. The default
   --    behaviour is to replace the contents of the entry with the contents of
   --    the text column in the row pointed to by Iter.
   --    Return value: %TRUE if the signal has been handled
   --
   --  </signals>

   Signal_Action_Activated : constant String := "action_activated";
   Signal_Insert_Prefix    : constant String := "insert_prefix";
   Signal_Match_Selected   : constant String := "match_selected";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Inline_Completion_Property
   --  Type:  Boolean
   --  Descr: Whether the common prefix should be inserted automatically
   --
   --  Name:  Minimum_Key_Length_Property
   --  Type:  Int
   --  Descr: Minimum length of the search key in order to look up matches
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model to find matches in
   --
   --  Name:  Popup_Completion_Property
   --  Type:  Boolean
   --  Descr: Whether the completions should be shown in a popup window
   --
   --  Name:  Popup_Set_Width_Property
   --  Type:  Boolean
   --  Descr: If TRUE, the popup window will have the same size as the entry
   --
   --  Name:  Popup_Single_Match_Property
   --  Type:  Boolean
   --  Descr: If TRUE, the popup window will appear for a single match.
   --
   --  Name:  Text_Column_Property
   --  Type:  Int
   --  Descr: The column of the model containing the strings.
   --
   --  </properties>

   Inline_Completion_Property  : constant Glib.Properties.Property_Boolean;
   Minimum_Key_Length_Property : constant Glib.Properties.Property_Int;
   Model_Property              : constant Glib.Properties.Property_Object;
   Popup_Completion_Property   : constant Glib.Properties.Property_Boolean;
   Popup_Set_Width_Property    : constant Glib.Properties.Property_Boolean;
   Popup_Single_Match_Property : constant Glib.Properties.Property_Boolean;
   Text_Column_Property        : constant Glib.Properties.Property_Int;

private
   Inline_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inline-completion");
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

   pragma Import (C, Get_Type, "gtk_entry_completion_get_type");
end Gtk.Entry_Completion;
