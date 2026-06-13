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

--  Displays the contents of a [classGtk.TextBuffer].
--
--  <picture> <source srcset="multiline-text-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkTextView"
--  src="multiline-text.png"> </picture>
--  You may wish to begin by reading the [conceptual
--  overview](section-text-widget.html), which gives an overview of all the
--  objects and data types related to the text widget and how they work
--  together.
--
--  ## Shortcuts and Gestures
--
--  `GtkTextView` supports the following keyboard shortcuts:
--
--  - <kbd>Shift</kbd>+<kbd>F10</kbd> or <kbd>Menu</kbd> opens the context
--  menu. - <kbd>Ctrl</kbd>+<kbd>Z</kbd> undoes the last modification. -
--  <kbd>Ctrl</kbd>+<kbd>Y</kbd> or
--  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Z</kbd> redoes the last undone
--  modification. - <kbd>Clear</kbd> clears the content.
--
--  Additionally, the following signals have default keybindings:
--
--  - [signalGtk.TextView::backspace] - [signalGtk.TextView::copy-clipboard] -
--  [signalGtk.TextView::cut-clipboard] -
--  [signalGtk.TextView::delete-from-cursor] -
--  [signalGtk.TextView::insert-emoji] - [signalGtk.TextView::move-cursor] -
--  [signalGtk.TextView::paste-clipboard] - [signalGtk.TextView::select-all] -
--  [signalGtk.TextView::toggle-cursor-visible] -
--  [signalGtk.TextView::toggle-overwrite]
--
--  ## Actions
--
--  `GtkTextView` defines a set of built-in actions:
--
--  - `clipboard.copy` copies the contents to the clipboard. - `clipboard.cut`
--  copies the contents to the clipboard and deletes it from the widget. -
--  `clipboard.paste` inserts the contents of the clipboard into the widget. -
--  `menu.popup` opens the context menu. - `misc.insert-emoji` opens the Emoji
--  chooser. - `selection.delete` deletes the current selection. -
--  `selection.select-all` selects all of the widgets content. - `text.redo`
--  redoes the last change to the contents. - `text.undo` undoes the last
--  change to the contents. - `text.clear` clears the content.
--
--  ## CSS nodes
--
--  ``` textview.view ├── border.top ├── border.left ├── text │ ╰──
--  [selection] ├── border.right ├── border.bottom ╰── [window.popup] ```
--
--  `GtkTextView` has a main css node with name textview and style class
--  .view, and subnodes for each of the border windows, and the main text area,
--  with names border and text, respectively. The border nodes each get one of
--  the style classes .left, .right, .top or .bottom.
--
--  A node representing the selection will appear below the text node.
--
--  If a context menu is opened, the window node will appear as a subnode of
--  the main node.
--
--  ## Accessibility
--
--  `GtkTextView` uses the [enumGtk.AccessibleRole.text_box] role.
--
--  <screenshot>gnome-textfile</screenshot>
--  <group>Multiline Text Editor</group>
--  <gtkada_demo>create_text_view.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Menu_Model;         use Glib.Menu_Model;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Accessible_Text;     use Gtk.Accessible_Text;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Child_Anchor;   use Gtk.Text_Child_Anchor;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Mark;           use Gtk.Text_Mark;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Context;           use Pango.Context;
with Pango.Tabs;              use Pango.Tabs;

package Gtk.Text_View is

   type Gtk_Text_View_Record is new Gtk_Widget_Record with null record;
   type Gtk_Text_View is access all Gtk_Text_View_Record'Class;

   type Gtk_Text_View_Layer is (
      Text_View_Layer_Below_Text,
      Text_View_Layer_Above_Text);
   pragma Convention (C, Gtk_Text_View_Layer);
   --  Used to reference the layers of `GtkTextView` for the purpose of
   --  customized drawing with the ::snapshot_layer vfunc.

   type Gtk_Text_Extend_Selection is (
      Text_Extend_Selection_Word,
      Text_Extend_Selection_Line);
   pragma Convention (C, Gtk_Text_Extend_Selection);
   --  Granularity types that extend the text selection. Use the
   --  `GtkTextView::extend-selection` signal to customize the selection.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Text_View_Layer_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_View_Layer);
   type Property_Gtk_Text_View_Layer is new Gtk_Text_View_Layer_Properties.Property;

   package Gtk_Text_Extend_Selection_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Text_Extend_Selection);
   type Property_Gtk_Text_Extend_Selection is new Gtk_Text_Extend_Selection_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (View : out Gtk_Text_View);
   procedure Initialize (View : not null access Gtk_Text_View_Record'Class);
   --  Creates a new `GtkTextView`.
   --  If you don't call [methodGtk.TextView.set_buffer] before using the text
   --  view, an empty default buffer will be created for you. Get the buffer
   --  with [methodGtk.TextView.get_buffer]. If you want to specify your own
   --  buffer, consider [ctorGtk.TextView.new_with_buffer].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Text_View_New return Gtk_Text_View;
   --  Creates a new `GtkTextView`.
   --  If you don't call [methodGtk.TextView.set_buffer] before using the text
   --  view, an empty default buffer will be created for you. Get the buffer
   --  with [methodGtk.TextView.get_buffer]. If you want to specify your own
   --  buffer, consider [ctorGtk.TextView.new_with_buffer].

   procedure Gtk_New
      (View   : out Gtk_Text_View;
       Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   procedure Initialize
      (View   : not null access Gtk_Text_View_Record'Class;
       Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Creates a new `GtkTextView` widget displaying the buffer Buffer.
   --  One buffer can be shared among many widgets. Buffer may be null to
   --  create a default buffer, in which case this function is equivalent to
   --  [ctorGtk.TextView.new]. The text view adds its own reference count to
   --  the buffer; it does not take over an existing reference.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Buffer a `GtkTextBuffer`

   function Gtk_Text_View_New_With_Buffer
      (Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
       return Gtk_Text_View;
   --  Creates a new `GtkTextView` widget displaying the buffer Buffer.
   --  One buffer can be shared among many widgets. Buffer may be null to
   --  create a default buffer, in which case this function is equivalent to
   --  [ctorGtk.TextView.new]. The text view adds its own reference count to
   --  the buffer; it does not take over an existing reference.
   --  @param Buffer a `GtkTextBuffer`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_view_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Child_At_Anchor
      (View   : not null access Gtk_Text_View_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class);
   --  Adds a child widget in the text buffer, at the given Anchor.
   --  @param Child a `GtkWidget`
   --  @param Anchor a `GtkTextChildAnchor` in the `GtkTextBuffer` for
   --  Text_View

   procedure Add_Overlay
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Xpos  : Glib.Gint;
       Ypos  : Glib.Gint);
   --  Adds Child at a fixed coordinate in the `GtkTextView`'s text window.
   --  The Xpos and Ypos must be in buffer coordinates (see
   --  [methodGtk.TextView.get_iter_location] to convert to buffer
   --  coordinates).
   --  Child will scroll with the text view.
   --  If instead you want a widget that will not move with the `GtkTextView`
   --  contents see `GtkOverlay`.
   --  @param Child a `GtkWidget`
   --  @param Xpos X position of child in window coordinates
   --  @param Ypos Y position of child in window coordinates

   function Backward_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter backward by one display (wrapped) line.
   --  A display line is different from a paragraph. Paragraphs are separated
   --  by newlines or other paragraph separator characters. Display lines are
   --  created by line-wrapping a paragraph. If wrapping is turned off, display
   --  lines and paragraphs will be the same. Display lines are divided
   --  differently for each view, since they depend on the view's width;
   --  paragraphs are the same in all views, since they depend on the contents
   --  of the `GtkTextBuffer`.
   --  @param Iter a `GtkTextIter`
   --  @return True if Iter was moved and is not on the end iterator

   function Backward_Display_Line_Start
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter backward to the next display line start.
   --  A display line is different from a paragraph. Paragraphs are separated
   --  by newlines or other paragraph separator characters. Display lines are
   --  created by line-wrapping a paragraph. If wrapping is turned off, display
   --  lines and paragraphs will be the same. Display lines are divided
   --  differently for each view, since they depend on the view's width;
   --  paragraphs are the same in all views, since they depend on the contents
   --  of the `GtkTextBuffer`.
   --  @param Iter a `GtkTextIter`
   --  @return True if Iter was moved and is not on the end iterator

   procedure Buffer_To_Window_Coords
      (View     : not null access Gtk_Text_View_Record;
       Win      : Gtk.Enums.Gtk_Text_Window_Type;
       Buffer_X : Glib.Gint;
       Buffer_Y : Glib.Gint;
       Window_X : out Glib.Gint;
       Window_Y : out Glib.Gint);
   --  Converts buffer coordinates to window coordinates.
   --  @param Win a `GtkTextWindowType`
   --  @param Buffer_X buffer x coordinate
   --  @param Buffer_Y buffer y coordinate
   --  @param Window_X window x coordinate return location
   --  @param Window_Y window y coordinate return location

   function Forward_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter forward by one display (wrapped) line.
   --  A display line is different from a paragraph. Paragraphs are separated
   --  by newlines or other paragraph separator characters. Display lines are
   --  created by line-wrapping a paragraph. If wrapping is turned off, display
   --  lines and paragraphs will be the same. Display lines are divided
   --  differently for each view, since they depend on the view's width;
   --  paragraphs are the same in all views, since they depend on the contents
   --  of the `GtkTextBuffer`.
   --  @param Iter a `GtkTextIter`
   --  @return True if Iter was moved and is not on the end iterator

   function Forward_Display_Line_End
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter forward to the next display line end.
   --  A display line is different from a paragraph. Paragraphs are separated
   --  by newlines or other paragraph separator characters. Display lines are
   --  created by line-wrapping a paragraph. If wrapping is turned off, display
   --  lines and paragraphs will be the same. Display lines are divided
   --  differently for each view, since they depend on the view's width;
   --  paragraphs are the same in all views, since they depend on the contents
   --  of the `GtkTextBuffer`.
   --  @param Iter a `GtkTextIter`
   --  @return True if Iter was moved and is not on the end iterator

   function Get_Accepts_Tab
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Returns whether pressing the <kbd>Tab</kbd> key inserts a tab
   --  characters.
   --  See [methodGtk.TextView.set_accepts_tab].
   --  @return True if pressing the Tab key inserts a tab character, False if
   --  pressing the Tab key moves the keyboard focus.

   procedure Set_Accepts_Tab
      (View        : not null access Gtk_Text_View_Record;
       Accepts_Tab : Boolean);
   --  Sets the behavior of the text widget when the <kbd>Tab</kbd> key is
   --  pressed.
   --  If Accepts_Tab is True, a tab character is inserted. If Accepts_Tab is
   --  False the keyboard focus is moved to the next widget in the focus chain.
   --  Focus can always be moved using <kbd>Ctrl</kbd>+<kbd>Tab</kbd>.
   --  @param Accepts_Tab True if pressing the Tab key should insert a tab
   --  character, False, if pressing the Tab key should move the keyboard
   --  focus.

   function Get_Bottom_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the bottom margin for text in the Text_View.
   --  @return bottom margin in pixels

   procedure Set_Bottom_Margin
      (View          : not null access Gtk_Text_View_Record;
       Bottom_Margin : Glib.Gint);
   --  Sets the bottom margin for text in Text_View.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  @param Bottom_Margin bottom margin in pixels

   function Get_Buffer
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Returns the `GtkTextBuffer` being displayed by this text view.
   --  The reference count on the buffer is not incremented; the caller of
   --  this function won't own a new reference.
   --  @return a `GtkTextBuffer`

   procedure Set_Buffer
      (View   : not null access Gtk_Text_View_Record;
       Buffer : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Sets Buffer as the buffer being displayed by Text_View.
   --  The previous buffer displayed by the text view is unreferenced, and a
   --  reference is added to Buffer. If you owned a reference to Buffer before
   --  passing it to this function, you must remove that reference yourself;
   --  `GtkTextView` will not "adopt" it.
   --  @param Buffer a `GtkTextBuffer`

   procedure Get_Cursor_Locations
      (View   : not null access Gtk_Text_View_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Strong : out Gdk.Rectangle.Gdk_Rectangle;
       Weak   : out Gdk.Rectangle.Gdk_Rectangle);
   --  Determine the positions of the strong and weak cursors if the insertion
   --  point is at Iter.
   --  The position of each cursor is stored as a zero-width rectangle. The
   --  strong cursor location is the location where characters of the
   --  directionality equal to the base direction of the paragraph are
   --  inserted. The weak cursor location is the location where characters of
   --  the directionality opposite to the base direction of the paragraph are
   --  inserted.
   --  If Iter is null, the actual cursor position is used.
   --  Note that if Iter happens to be the actual cursor position, and there
   --  is currently an IM preedit sequence being entered, the returned
   --  locations will be adjusted to account for the preedit cursor's offset
   --  within the preedit sequence.
   --  The rectangle position is in buffer coordinates; use
   --  [methodGtk.TextView.buffer_to_window_coords] to convert these
   --  coordinates to coordinates for one of the windows in the text view.
   --  @param Iter a `GtkTextIter`
   --  @param Strong location to store the strong cursor position
   --  @param Weak location to store the weak cursor position

   function Get_Cursor_Visible
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Find out whether the cursor should be displayed.
   --  @return whether the insertion mark is visible

   procedure Set_Cursor_Visible
      (View    : not null access Gtk_Text_View_Record;
       Setting : Boolean);
   --  Toggles whether the insertion point should be displayed.
   --  A buffer with no editable text probably shouldn't have a visible
   --  cursor, so you may want to turn the cursor off.
   --  Note that this property may be overridden by the
   --  [propertyGtk.Settings:gtk-keynav-use-caret] setting.
   --  @param Setting whether to show the insertion cursor

   function Get_Editable
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Returns the default editability of the `GtkTextView`.
   --  Tags in the buffer may override this setting for some ranges of text.
   --  @return whether text is editable by default

   procedure Set_Editable
      (View    : not null access Gtk_Text_View_Record;
       Setting : Boolean);
   --  Sets the default editability of the `GtkTextView`.
   --  You can override this default setting with tags in the buffer, using
   --  the "editable" attribute of tags.
   --  @param Setting whether it's editable

   function Get_Extra_Menu
      (View : not null access Gtk_Text_View_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Gets the menu model that gets added to the context menu or null if none
   --  has been set.
   --  @return the menu model

   procedure Set_Extra_Menu
      (View  : not null access Gtk_Text_View_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets a menu model to add when constructing the context menu for
   --  Text_View.
   --  You can pass null to remove a previously set extra menu.
   --  @param Model a `GMenuModel`

   function Get_Gutter
      (View : not null access Gtk_Text_View_Record;
       Win  : Gtk.Enums.Gtk_Text_Window_Type) return Gtk.Widget.Gtk_Widget;
   --  Gets a `GtkWidget` that has previously been set as gutter.
   --  See [methodGtk.TextView.set_gutter].
   --  Win must be one of Gtk.Enums.Text_Window_Left,
   --  Gtk.Enums.Text_Window_Right, Gtk.Enums.Text_Window_Top, or
   --  Gtk.Enums.Text_Window_Bottom.
   --  @param Win a `GtkTextWindowType`
   --  @return a `GtkWidget`

   procedure Set_Gutter
      (View   : not null access Gtk_Text_View_Record;
       Win    : Gtk.Enums.Gtk_Text_Window_Type;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Places Widget into the gutter specified by Win.
   --  Win must be one of Gtk.Enums.Text_Window_Left,
   --  Gtk.Enums.Text_Window_Right, Gtk.Enums.Text_Window_Top, or
   --  Gtk.Enums.Text_Window_Bottom.
   --  @param Win a `GtkTextWindowType`
   --  @param Widget a `GtkWidget`

   function Get_Indent
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default indentation of paragraphs in Text_View.
   --  Tags in the view's buffer may override the default. The indentation may
   --  be negative.
   --  @return number of pixels of indentation

   procedure Set_Indent
      (View   : not null access Gtk_Text_View_Record;
       Indent : Glib.Gint);
   --  Sets the default indentation for paragraphs in Text_View.
   --  Tags in the buffer may override the default.
   --  @param Indent indentation in pixels

   function Get_Input_Hints
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Input_Hints;
   --  Gets the `input-hints` of the `GtkTextView`.
   --  @return the input hints

   procedure Set_Input_Hints
      (View  : not null access Gtk_Text_View_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints);
   --  Sets the `input-hints` of the `GtkTextView`.
   --  The `input-hints` allow input methods to fine-tune their behaviour.
   --  @param Hints the hints

   function Get_Input_Purpose
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Input_Purpose;
   --  Gets the `input-purpose` of the `GtkTextView`.
   --  @return the input purpose

   procedure Set_Input_Purpose
      (View    : not null access Gtk_Text_View_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose);
   --  Sets the `input-purpose` of the `GtkTextView`.
   --  The `input-purpose` can be used by on-screen keyboards and other input
   --  methods to adjust their behaviour.
   --  @param Purpose the purpose

   function Get_Iter_At_Location
      (View : not null access Gtk_Text_View_Record;
       Iter : access Gtk.Text_Iter.Gtk_Text_Iter;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Boolean;
   --  Retrieves the iterator at buffer coordinates X and Y.
   --  Buffer coordinates are coordinates for the entire buffer, not just the
   --  currently-displayed portion. If you have coordinates from an event, you
   --  have to convert those to buffer coordinates with
   --  [methodGtk.TextView.window_to_buffer_coords].
   --  @param Iter a `GtkTextIter`
   --  @param X x position, in buffer coordinates
   --  @param Y y position, in buffer coordinates
   --  @return True if the position is over text

   function Get_Iter_At_Position
      (View     : not null access Gtk_Text_View_Record;
       Iter     : access Gtk.Text_Iter.Gtk_Text_Iter;
       Trailing : access Glib.Gint;
       X        : Glib.Gint;
       Y        : Glib.Gint) return Boolean;
   --  Retrieves the iterator pointing to the character at buffer coordinates
   --  X and Y.
   --  Buffer coordinates are coordinates for the entire buffer, not just the
   --  currently-displayed portion. If you have coordinates from an event, you
   --  have to convert those to buffer coordinates with
   --  [methodGtk.TextView.window_to_buffer_coords].
   --  Note that this is different from
   --  [methodGtk.TextView.get_iter_at_location], which returns cursor
   --  locations, i.e. positions between characters.
   --  @param Iter a `GtkTextIter`
   --  @param Trailing if non-null, location to store an integer indicating
   --  where in the grapheme the user clicked. It will either be zero, or the
   --  number of characters in the grapheme. 0 represents the trailing edge of
   --  the grapheme.
   --  @param X x position, in buffer coordinates
   --  @param Y y position, in buffer coordinates
   --  @return True if the position is over text

   procedure Get_Iter_Location
      (View     : not null access Gtk_Text_View_Record;
       Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
       Location : out Gdk.Rectangle.Gdk_Rectangle);
   --  Gets a rectangle which roughly contains the character at Iter.
   --  The rectangle position is in buffer coordinates; use
   --  [methodGtk.TextView.buffer_to_window_coords] to convert these
   --  coordinates to coordinates for one of the windows in the text view.
   --  @param Iter a `GtkTextIter`
   --  @param Location bounds of the character at Iter

   function Get_Justification
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Justification;
   --  Gets the default justification of paragraphs in Text_View.
   --  Tags in the buffer may override the default.
   --  @return default justification

   procedure Set_Justification
      (View          : not null access Gtk_Text_View_Record;
       Justification : Gtk.Enums.Gtk_Justification);
   --  Sets the default justification of text in Text_View.
   --  Tags in the view's buffer may override the default.
   --  @param Justification justification

   function Get_Left_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default left margin size of paragraphs in the Text_View.
   --  Tags in the buffer may override the default.
   --  @return left margin in pixels

   procedure Set_Left_Margin
      (View        : not null access Gtk_Text_View_Record;
       Left_Margin : Glib.Gint);
   --  Sets the default left margin for text in Text_View.
   --  Tags in the buffer may override the default.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  @param Left_Margin left margin in pixels

   procedure Get_Line_At_Y
      (View        : not null access Gtk_Text_View_Record;
       Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
       Y           : Glib.Gint;
       Line_Top    : out Glib.Gint);
   --  Gets the `GtkTextIter` at the start of the line containing the
   --  coordinate Y.
   --  Y is in buffer coordinates, convert from window coordinates with
   --  [methodGtk.TextView.window_to_buffer_coords]. If non-null, Line_Top will
   --  be filled with the coordinate of the top edge of the line.
   --  @param Target_Iter a `GtkTextIter`
   --  @param Y a y coordinate
   --  @param Line_Top return location for top coordinate of the line

   procedure Get_Line_Yrange
      (View   : not null access Gtk_Text_View_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Y      : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the y coordinate of the top of the line containing Iter, and the
   --  height of the line.
   --  The coordinate is a buffer coordinate; convert to window coordinates
   --  with [methodGtk.TextView.buffer_to_window_coords].
   --  @param Iter a `GtkTextIter`
   --  @param Y return location for a y coordinate
   --  @param Height return location for a height

   function Get_Ltr_Context
      (View : not null access Gtk_Text_View_Record)
       return Pango.Context.Pango_Context;
   --  Gets the `PangoContext` that is used for rendering LTR directed text
   --  layouts.
   --  The context may be replaced when CSS changes occur.
   --  Since: gtk+ 4.4
   --  @return a `PangoContext`

   function Get_Monospace
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Gets whether the `GtkTextView` uses monospace styling.
   --  @return True if monospace fonts are desired

   procedure Set_Monospace
      (View      : not null access Gtk_Text_View_Record;
       Monospace : Boolean);
   --  Sets whether the `GtkTextView` should display text in monospace
   --  styling.
   --  @param Monospace True to request monospace styling

   function Get_Overwrite
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Returns whether the `GtkTextView` is in overwrite mode or not.
   --  @return whether Text_View is in overwrite mode or not.

   procedure Set_Overwrite
      (View      : not null access Gtk_Text_View_Record;
       Overwrite : Boolean);
   --  Changes the `GtkTextView` overwrite mode.
   --  @param Overwrite True to turn on overwrite mode, False to turn it off

   function Get_Pixels_Above_Lines
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default number of pixels to put above paragraphs.
   --  Adding this function with [methodGtk.TextView.get_pixels_below_lines]
   --  is equal to the line space between each paragraph.
   --  @return default number of pixels above paragraphs

   procedure Set_Pixels_Above_Lines
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Above_Lines : Glib.Gint);
   --  Sets the default number of blank pixels above paragraphs in Text_View.
   --  Tags in the buffer for Text_View may override the defaults.
   --  @param Pixels_Above_Lines pixels above paragraphs

   function Get_Pixels_Below_Lines
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default number of pixels to put below paragraphs.
   --  The line space is the sum of the value returned by this function and
   --  the value returned by [methodGtk.TextView.get_pixels_above_lines].
   --  @return default number of blank pixels below paragraphs

   procedure Set_Pixels_Below_Lines
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Below_Lines : Glib.Gint);
   --  Sets the default number of pixels of blank space to put below
   --  paragraphs in Text_View.
   --  May be overridden by tags applied to Text_View's buffer.
   --  @param Pixels_Below_Lines pixels below paragraphs

   function Get_Pixels_Inside_Wrap
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default number of pixels to put between wrapped lines inside a
   --  paragraph.
   --  @return default number of pixels of blank space between wrapped lines

   procedure Set_Pixels_Inside_Wrap
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Inside_Wrap : Glib.Gint);
   --  Sets the default number of pixels of blank space to leave between
   --  display/wrapped lines within a paragraph.
   --  May be overridden by tags in Text_View's buffer.
   --  @param Pixels_Inside_Wrap default number of pixels between wrapped
   --  lines

   function Get_Right_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default right margin for text in Text_View.
   --  Tags in the buffer may override the default.
   --  @return right margin in pixels

   procedure Set_Right_Margin
      (View         : not null access Gtk_Text_View_Record;
       Right_Margin : Glib.Gint);
   --  Sets the default right margin for text in the text view.
   --  Tags in the buffer may override the default.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  @param Right_Margin right margin in pixels

   function Get_Rtl_Context
      (View : not null access Gtk_Text_View_Record)
       return Pango.Context.Pango_Context;
   --  Gets the `PangoContext` that is used for rendering RTL directed text
   --  layouts.
   --  The context may be replaced when CSS changes occur.
   --  Since: gtk+ 4.4
   --  @return a `PangoContext`

   function Get_Tabs
      (View : not null access Gtk_Text_View_Record)
       return Pango.Tabs.Pango_Tab_Array;
   --  Gets the default tabs for Text_View.
   --  Tags in the buffer may override the defaults. The returned array will
   --  be null if "standard" (8-space) tabs are used. Free the return value
   --  with [methodPango.TabArray.free].
   --  @return copy of default tab array, or null if standard tabs are used;
   --  must be freed with [methodPango.TabArray.free].

   procedure Set_Tabs
      (View : not null access Gtk_Text_View_Record;
       Tabs : Pango.Tabs.Pango_Tab_Array);
   --  Sets the default tab stops for paragraphs in Text_View.
   --  Tags in the buffer may override the default.
   --  @param Tabs tabs as a `PangoTabArray`

   function Get_Top_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the top margin for text in the Text_View.
   --  @return top margin in pixels

   procedure Set_Top_Margin
      (View       : not null access Gtk_Text_View_Record;
       Top_Margin : Glib.Gint);
   --  Sets the top margin for text in Text_View.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  @param Top_Margin top margin in pixels

   procedure Get_Visible_Offset
      (View     : not null access Gtk_Text_View_Record;
       X_Offset : out Gdouble;
       Y_Offset : out Gdouble);
   --  Gets the X,Y offset in buffer coordinates of the top-left corner of the
   --  textview's text contents.
   --  This allows for more-precise positioning than what is provided by
   --  [methodGtk.TextView.get_visible_rect] as you can discover what device
   --  pixel is being quantized for text positioning.
   --  You might want this when making ulterior widgets align with quantized
   --  device pixels of the textview contents such as line numbers.
   --  Since: gtk+ 4.18
   --  @param X_Offset a location for the X offset
   --  @param Y_Offset a location for the Y offset

   procedure Get_Visible_Rect
      (View         : not null access Gtk_Text_View_Record;
       Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills Visible_Rect with the currently-visible region of the buffer, in
   --  buffer coordinates.
   --  Convert to window coordinates with
   --  [methodGtk.TextView.buffer_to_window_coords].
   --  @param Visible_Rect rectangle to fill

   function Get_Wrap_Mode
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Wrap_Mode;
   --  Gets the line wrapping for the view.
   --  @return the line wrap setting

   procedure Set_Wrap_Mode
      (View      : not null access Gtk_Text_View_Record;
       Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);
   --  Sets the line wrapping for the view.
   --  @param Wrap_Mode a `GtkWrapMode`

   function Move_Mark_Onscreen
      (View : not null access Gtk_Text_View_Record;
       Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
       return Boolean;
   --  Moves a mark within the buffer so that it's located within the
   --  currently-visible text area.
   --  @param Mark a `GtkTextMark`
   --  @return True if the mark moved (wasn't already onscreen)

   procedure Move_Overlay
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Xpos  : Glib.Gint;
       Ypos  : Glib.Gint);
   --  Updates the position of a child.
   --  See [methodGtk.TextView.add_overlay].
   --  @param Child a widget already added with
   --  [methodGtk.TextView.add_overlay]
   --  @param Xpos new X position in buffer coordinates
   --  @param Ypos new Y position in buffer coordinates

   function Move_Visually
      (View  : not null access Gtk_Text_View_Record;
       Iter  : Gtk.Text_Iter.Gtk_Text_Iter;
       Count : Glib.Gint) return Boolean;
   --  Move the iterator a given number of characters visually, treating it as
   --  the strong cursor position.
   --  If Count is positive, then the new strong cursor position will be Count
   --  positions to the right of the old cursor position. If Count is negative
   --  then the new strong cursor position will be Count positions to the left
   --  of the old cursor position.
   --  In the presence of bi-directional text, the correspondence between
   --  logical and visual order will depend on the direction of the current
   --  run, and there may be jumps when the cursor is moved off of the end of a
   --  run.
   --  @param Iter a `GtkTextIter`
   --  @param Count number of characters to move (negative moves left,
   --  positive moves right)
   --  @return True if Iter moved and is not on the end iterator

   function Place_Cursor_Onscreen
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Moves the cursor to the currently visible region of the buffer.
   --  @return True if the cursor had to be moved.

   procedure Remove
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a child widget from Text_View.
   --  @param Child the child to remove

   procedure Reset_Cursor_Blink
      (View : not null access Gtk_Text_View_Record);
   --  Ensures that the cursor is shown.
   --  This also resets the time that it will stay blinking (or visible, in
   --  case blinking is disabled).
   --  This function should be called in response to user input (e.g. from
   --  derived classes that override the textview's event handlers).

   procedure Reset_Im_Context (View : not null access Gtk_Text_View_Record);
   --  Reset the input method context of the text view if needed.
   --  This can be necessary in the case where modifying the buffer would
   --  confuse on-going input method behavior.

   procedure Scroll_Mark_Onscreen
      (View : not null access Gtk_Text_View_Record;
       Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Scrolls Text_View the minimum distance such that Mark is contained
   --  within the visible area of the widget.
   --  @param Mark a mark in the buffer for Text_View

   function Scroll_To_Iter
      (View          : not null access Gtk_Text_View_Record;
       Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
       Within_Margin : Gdouble;
       Use_Align     : Boolean;
       Xalign        : Gdouble;
       Yalign        : Gdouble) return Boolean;
   --  Scrolls Text_View so that Iter is on the screen in the position
   --  indicated by Xalign and Yalign.
   --  An alignment of 0.0 indicates left or top, 1.0 indicates right or
   --  bottom, 0.5 means center. If Use_Align is False, the text scrolls the
   --  minimal distance to get the mark onscreen, possibly not scrolling at
   --  all. The effective screen for purposes of this function is reduced by a
   --  margin of size Within_Margin.
   --  Note that this function uses the currently-computed height of the lines
   --  in the text buffer. Line heights are computed in an idle handler; so
   --  this function may not have the desired effect if it's called before the
   --  height computations. To avoid oddness, consider using
   --  [methodGtk.TextView.scroll_to_mark] which saves a point to be scrolled
   --  to after line validation.
   --  @param Iter a `GtkTextIter`
   --  @param Within_Margin margin as a [0.0,0.5) fraction of screen size
   --  @param Use_Align whether to use alignment arguments (if False, just get
   --  the mark onscreen)
   --  @param Xalign horizontal alignment of mark within visible area
   --  @param Yalign vertical alignment of mark within visible area
   --  @return True if scrolling occurred

   procedure Scroll_To_Mark
      (View          : not null access Gtk_Text_View_Record;
       Mark          : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Within_Margin : Gdouble;
       Use_Align     : Boolean;
       Xalign        : Gdouble;
       Yalign        : Gdouble);
   --  Scrolls Text_View so that Mark is on the screen in the position
   --  indicated by Xalign and Yalign.
   --  An alignment of 0.0 indicates left or top, 1.0 indicates right or
   --  bottom, 0.5 means center. If Use_Align is False, the text scrolls the
   --  minimal distance to get the mark onscreen, possibly not scrolling at
   --  all. The effective screen for purposes of this function is reduced by a
   --  margin of size Within_Margin.
   --  @param Mark a `GtkTextMark`
   --  @param Within_Margin margin as a [0.0,0.5) fraction of screen size
   --  @param Use_Align whether to use alignment arguments (if False, just get
   --  the mark onscreen)
   --  @param Xalign horizontal alignment of mark within visible area
   --  @param Yalign vertical alignment of mark within visible area

   function Starts_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter is at the start of a display line.
   --  See [methodGtk.TextView.forward_display_line] for an explanation of
   --  display lines vs. paragraphs.
   --  @param Iter a `GtkTextIter`
   --  @return True if Iter begins a wrapped line

   procedure Window_To_Buffer_Coords
      (View     : not null access Gtk_Text_View_Record;
       Win      : Gtk.Enums.Gtk_Text_Window_Type;
       Window_X : Glib.Gint;
       Window_Y : Glib.Gint;
       Buffer_X : out Glib.Gint;
       Buffer_Y : out Glib.Gint);
   --  Converts coordinates on the window identified by Win to buffer
   --  coordinates.
   --  @param Win a `GtkTextWindowType`
   --  @param Window_X window x coordinate
   --  @param Window_Y window y coordinate
   --  @param Buffer_X buffer x coordinate return location
   --  @param Buffer_Y buffer y coordinate return location

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Text_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Text_View_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Text_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Text_View_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Text_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Text_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Text_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Text_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Text_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Text_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Update_Caret_Position
      (Self : not null access Gtk_Text_View_Record);

   procedure Update_Contents
      (Self    : not null access Gtk_Text_View_Record;
       Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
       Start   : Guint;
       The_End : Guint);

   procedure Update_Selection_Bound
      (Self : not null access Gtk_Text_View_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accepts_Tab_Property : constant Glib.Properties.Property_Boolean;
   --  Whether Tab will result in a tab character being entered.

   Bottom_Margin_Property : constant Glib.Properties.Property_Int;
   --  The bottom margin for text in the text view.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.
   --
   --  Don't confuse this property with [propertyGtk.Widget:margin-bottom].

   Buffer_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Text_Buffer.Gtk_Text_Buffer
   --  The buffer which is displayed.

   Cursor_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  If the insertion cursor is shown.

   Editable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the text can be modified by the user.

   Extra_Menu_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  A menu model whose contents will be appended to the context menu.

   Im_Module_Property : constant Glib.Properties.Property_String;
   --  Which IM (input method) module should be used for this text_view.
   --
   --  See [classGtk.IMMulticontext].
   --
   --  Setting this to a non-null value overrides the system-wide IM module
   --  setting. See the GtkSettings [propertyGtk.Settings:gtk-im-module]
   --  property.

   Indent_Property : constant Glib.Properties.Property_Int;
   --  Amount to indent the paragraph, in pixels.
   --
   --  A negative value of indent will produce a hanging indentation. That is,
   --  the first line will have the full width, and subsequent lines will be
   --  indented by the absolute value of indent.

   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints;
   --  Additional hints (beyond [propertyGtk.TextView:input-purpose]) that
   --  allow input methods to fine-tune their behaviour.

   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose;
   --  The purpose of this text field.
   --
   --  This property can be used by on-screen keyboards and other input
   --  methods to adjust their behaviour.

   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification;
   --  Left, right, or center justification.

   Left_Margin_Property : constant Glib.Properties.Property_Int;
   --  The default left margin for text in the text view.
   --
   --  Tags in the buffer may override the default.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.

   Monospace_Property : constant Glib.Properties.Property_Boolean;
   --  Whether text should be displayed in a monospace font.
   --
   --  If True, set the .monospace style class on the text view to indicate
   --  that a monospace font is desired.

   Overwrite_Property : constant Glib.Properties.Property_Boolean;
   --  Whether entered text overwrites existing contents.

   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;
   --  Pixels of blank space above paragraphs.

   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;
   --  Pixels of blank space below paragraphs.

   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int;
   --  Pixels of blank space between wrapped lines in a paragraph.

   Right_Margin_Property : constant Glib.Properties.Property_Int;
   --  The default right margin for text in the text view.
   --
   --  Tags in the buffer may override the default.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.

   Tabs_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Tab_Array
   --  Custom tabs for this text.

   Top_Margin_Property : constant Glib.Properties.Property_Int;
   --  The top margin for text in the text view.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.
   --
   --  Don't confuse this property with [propertyGtk.Widget:margin-top].

   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode;
   --  Whether to wrap lines never, at word boundaries, or at character
   --  boundaries.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Text_View_Void is not null access procedure (Self : access Gtk_Text_View_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Backspace : constant Glib.Signal_Name := "backspace";
   procedure On_Backspace
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Backspace
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user asks for it.
   --
   --  The ::backspace signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Backspace</kbd> and
   --  <kbd>Shift</kbd>+<kbd>Backspace</kbd>.

   Signal_Copy_Clipboard : constant Glib.Signal_Name := "copy-clipboard";
   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to copy the selection to the clipboard.
   --
   --  The ::copy-clipboard signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>c</kbd>
   --  and <kbd>Ctrl</kbd>+<kbd>Insert</kbd>.

   Signal_Cut_Clipboard : constant Glib.Signal_Name := "cut-clipboard";
   procedure On_Cut_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Cut_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to cut the selection to the clipboard.
   --
   --  The ::cut-clipboard signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>x</kbd>
   --  and <kbd>Shift</kbd>+<kbd>Delete</kbd>.

   type Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void is not null access procedure
     (Self     : access Gtk_Text_View_Record'Class;
      The_Type : Gtk.Enums.Gtk_Delete_Type;
      Count    : Glib.Gint);

   type Cb_GObject_Gtk_Delete_Type_Gint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      The_Type : Gtk.Enums.Gtk_Delete_Type;
      Count    : Glib.Gint);

   Signal_Delete_From_Cursor : constant Glib.Signal_Name := "delete-from-cursor";
   procedure On_Delete_From_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void;
       After : Boolean := False);
   procedure On_Delete_From_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Delete_Type_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user initiates a text deletion.
   --
   --  The ::delete-from-cursor signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  If the Type is Gtk.Enums.Delete_Chars, GTK deletes the selection if
   --  there is one, otherwise it deletes the requested number of characters.
   --
   --  The default bindings for this signal are <kbd>Delete</kbd> for deleting
   --  a character, <kbd>Ctrl</kbd>+<kbd>Delete</kbd> for deleting a word and
   --  <kbd>Ctrl</kbd>+<kbd>Backspace</kbd> for deleting a word backwards.
   -- 
   --  Callback parameters:
   --    --  @param The_Type the granularity of the deletion, as a `GtkDeleteType`
   --    --  @param Count the number of Type units to delete

   type Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean is not null access function
     (Self        : access Gtk_Text_View_Record'Class;
      Granularity : Gtk_Text_Extend_Selection;
      Location    : Gtk.Text_Iter.Gtk_Text_Iter;
      Start       : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End     : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;

   type Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean is not null access function
     (Self        : access Glib.Object.GObject_Record'Class;
      Granularity : Gtk_Text_Extend_Selection;
      Location    : Gtk.Text_Iter.Gtk_Text_Iter;
      Start       : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End     : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;

   Signal_Extend_Selection : constant Glib.Signal_Name := "extend-selection";
   procedure On_Extend_Selection
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       After : Boolean := False);
   procedure On_Extend_Selection
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the selection needs to be extended at Location.
   -- 
   --  Callback parameters:
   --    --  @param Granularity the granularity type
   --    --  @param Location the location where to extend the selection
   --    --  @param Start where the selection should start
   --    --  @param The_End where the selection should end

   type Cb_Gtk_Text_View_UTF8_String_Void is not null access procedure
     (Self   : access Gtk_Text_View_Record'Class;
      String : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      String : UTF8_String);

   Signal_Insert_At_Cursor : constant Glib.Signal_Name := "insert-at-cursor";
   procedure On_Insert_At_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Insert_At_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user initiates the insertion of a fixed string at
   --  the cursor.
   --
   --  The ::insert-at-cursor signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  This signal has no default bindings.

   Signal_Insert_Emoji : constant Glib.Signal_Name := "insert-emoji";
   procedure On_Insert_Emoji
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Insert_Emoji
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to present the Emoji chooser for the Text_View.
   --
   --  The ::insert-emoji signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>.</kbd>
   --  and <kbd>Ctrl</kbd>+<kbd>;</kbd>

   type Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void is not null access procedure
     (Self             : access Gtk_Text_View_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Glib.Gint;
      Extend_Selection : Boolean);

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void is not null access procedure
     (Self             : access Glib.Object.GObject_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Glib.Gint;
      Extend_Selection : Boolean);

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user initiates a cursor movement.
   --
   --  The ::move-cursor signal is a [keybinding
   --  signal](class.SignalAction.html). If the cursor is not visible in
   --  Text_View, this signal causes the viewport to be moved instead.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the <kbd>Shift</kbd> modifier extends the selection, the variant
   --  without it does not. There are too many key combinations to list them
   --  all here.
   --
   --  - <kbd>←</kbd>, <kbd>→</kbd>, <kbd>↑</kbd>, <kbd>↓</kbd> move by
   --  individual characters/lines - <kbd>Ctrl</kbd>+<kbd>←</kbd>, etc. move by
   --  words/paragraphs - <kbd>Home</kbd> and <kbd>End</kbd> move to the ends
   --  of the buffer - <kbd>PgUp</kbd> and <kbd>PgDn</kbd> move vertically by
   --  pages - <kbd>Ctrl</kbd>+<kbd>PgUp</kbd> and
   --  <kbd>Ctrl</kbd>+<kbd>PgDn</kbd> move horizontally by pages
   -- 
   --  Callback parameters:
   --    --  @param Step the granularity of the move, as a `GtkMovementStep`
   --    --  @param Count the number of Step units to move
   --    --  @param Extend_Selection True if the move should extend the selection

   type Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void is not null access procedure
     (Self  : access Gtk_Text_View_Record'Class;
      Step  : Gtk.Enums.Gtk_Scroll_Step;
      Count : Glib.Gint);

   type Cb_GObject_Gtk_Scroll_Step_Gint_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Step  : Gtk.Enums.Gtk_Scroll_Step;
      Count : Glib.Gint);

   Signal_Move_Viewport : constant Glib.Signal_Name := "move-viewport";
   procedure On_Move_Viewport
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void;
       After : Boolean := False);
   procedure On_Move_Viewport
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Scroll_Step_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to move the viewport.
   --
   --  The ::move-viewport signal is a [keybinding
   --  signal](class.SignalAction.html), which can be bound to key combinations
   --  to allow the user to move the viewport, i.e. change what part of the
   --  text view is visible in a containing scrolled window.
   --
   --  There are no default bindings for this signal.
   -- 
   --  Callback parameters:
   --    --  @param Step the granularity of the movement, as a `GtkScrollStep`
   --    --  @param Count the number of Step units to move

   Signal_Paste_Clipboard : constant Glib.Signal_Name := "paste-clipboard";
   procedure On_Paste_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Paste_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to paste the contents of the clipboard into the text view.
   --
   --  The ::paste-clipboard signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>v</kbd>
   --  and <kbd>Shift</kbd>+<kbd>Insert</kbd>.

   Signal_Preedit_Changed : constant Glib.Signal_Name := "preedit-changed";
   procedure On_Preedit_Changed
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Preedit_Changed
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when preedit text of the active IM changes.
   --
   --  If an input method is used, the typed text will not immediately be
   --  committed to the buffer. So if you are interested in the text, connect
   --  to this signal.
   --
   --  This signal is only emitted if the text at the given position is
   --  actually editable.

   type Cb_Gtk_Text_View_Boolean_Void is not null access procedure
     (Self       : access Gtk_Text_View_Record'Class;
      Gtk_Select : Boolean);

   type Cb_GObject_Boolean_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Gtk_Select : Boolean);

   Signal_Select_All : constant Glib.Signal_Name := "select-all";
   procedure On_Select_All
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Boolean_Void;
       After : Boolean := False);
   procedure On_Select_All
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to select or unselect the complete contents of the text
   --  view.
   --
   --  The ::select-all signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are <kbd>Ctrl</kbd>+<kbd>a</kbd>
   --  and <kbd>Ctrl</kbd>+<kbd>/</kbd> for selecting and
   --  <kbd>Shift</kbd>+<kbd>Ctrl</kbd>+<kbd>a</kbd> and
   --  <kbd>Ctrl</kbd>+<kbd>\</kbd> for unselecting.

   Signal_Set_Anchor : constant Glib.Signal_Name := "set-anchor";
   procedure On_Set_Anchor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Set_Anchor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user initiates settings the "anchor" mark.
   --
   --  The ::set-anchor signal is a [keybinding
   --  signal](class.SignalAction.html) which gets emitted when the user
   --  initiates setting the "anchor" mark. The "anchor" mark gets placed at
   --  the same position as the "insert" mark.
   --
   --  This signal has no default bindings.

   Signal_Toggle_Cursor_Visible : constant Glib.Signal_Name := "toggle-cursor-visible";
   procedure On_Toggle_Cursor_Visible
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Toggle_Cursor_Visible
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to toggle the `cursor-visible` property.
   --
   --  The ::toggle-cursor-visible signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>F7</kbd>.

   Signal_Toggle_Overwrite : constant Glib.Signal_Name := "toggle-overwrite";
   procedure On_Toggle_Overwrite
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False);
   procedure On_Toggle_Overwrite
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted to toggle the overwrite mode of the text view.
   --
   --  The ::toggle-overwrite signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Insert</kbd>.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.AccessibleText"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Text_View_Record, Gtk_Text_View);
   function "+"
     (Widget : access Gtk_Text_View_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Text_View
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Accessible_Text is new Glib.Types.Implements
     (Gtk.Accessible_Text.Gtk_Accessible_Text, Gtk_Text_View_Record, Gtk_Text_View);
   function "+"
     (Widget : access Gtk_Text_View_Record'Class)
   return Gtk.Accessible_Text.Gtk_Accessible_Text
   renames Implements_Gtk_Accessible_Text.To_Interface;
   function "-"
     (Interf : Gtk.Accessible_Text.Gtk_Accessible_Text)
   return Gtk_Text_View
   renames Implements_Gtk_Accessible_Text.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Text_View_Record, Gtk_Text_View);
   function "+"
     (Widget : access Gtk_Text_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Text_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Text_View_Record, Gtk_Text_View);
   function "+"
     (Widget : access Gtk_Text_View_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Text_View
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap-mode");
   Top_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("top-margin");
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Right_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-margin");
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-inside-wrap");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-below-lines");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-above-lines");
   Overwrite_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overwrite");
   Monospace_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("monospace");
   Left_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left-margin");
   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose :=
     Gtk.Enums.Build ("input-purpose");
   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints :=
     Gtk.Enums.Build ("input-hints");
   Indent_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Im_Module_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("im-module");
   Extra_Menu_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("extra-menu");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Cursor_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("cursor-visible");
   Buffer_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("buffer");
   Bottom_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("bottom-margin");
   Accepts_Tab_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accepts-tab");
end Gtk.Text_View;
