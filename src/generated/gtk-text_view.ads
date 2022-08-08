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
--  You may wish to begin by reading the [text widget conceptual
--  overview][TextWidget] which gives an overview of all the objects and data
--  types related to the text widget and how they work together.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> textview.view ├── border.top ├── border.left
--  ├── text │ ╰── [selection] ├── border.right ├── border.bottom ╰──
--  [window.popup] ]|
--
--  GtkTextView has a main css node with name textview and style class .view,
--  and subnodes for each of the border windows, and the main text area, with
--  names border and text, respectively. The border nodes each get one of the
--  style classes .left, .right, .top or .bottom.
--
--  A node representing the selection will appear below the text node.
--
--  If a context menu is opened, the window node will appear as a subnode of
--  the main node.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Gdk;                     use Gdk;
with Gdk.Event;               use Gdk.Event;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Container;           use Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Scrollable;          use Gtk.Scrollable;
with Gtk.Style;               use Gtk.Style;
with Gtk.Text_Attributes;     use Gtk.Text_Attributes;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Child_Anchor;   use Gtk.Text_Child_Anchor;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Mark;           use Gtk.Text_Mark;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Tabs;              use Pango.Tabs;

package Gtk.Text_View is

   type Gtk_Text_View_Record is new Gtk_Container_Record with null record;
   type Gtk_Text_View is access all Gtk_Text_View_Record'Class;

   type Gtk_Text_View_Layer is (
      Text_View_Layer_Below,
      Text_View_Layer_Above,
      Text_View_Layer_Below_Text,
      Text_View_Layer_Above_Text);
   pragma Convention (C, Gtk_Text_View_Layer);
   --  Used to reference the layers of Gtk.Text_View.Gtk_Text_View for the
   --  purpose of customized drawing with the ::draw_layer vfunc.

   type Gtk_Text_Extend_Selection is (
      Text_Extend_Selection_Word,
      Text_Extend_Selection_Line);
   pragma Convention (C, Gtk_Text_Extend_Selection);
   --  Granularity types that extend the text selection. Use the
   --  Gtk.Text_View.Gtk_Text_View::extend-selection signal to customize the
   --  selection.

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
   --  Creates a new Gtk.Text_View.Gtk_Text_View. If you don't call
   --  Gtk.Text_View.Set_Buffer before using the text view, an empty default
   --  buffer will be created for you. Get the buffer with
   --  Gtk.Text_View.Get_Buffer. If you want to specify your own buffer,
   --  consider Gtk.Text_View.Gtk_New.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Text_View_New return Gtk_Text_View;
   --  Creates a new Gtk.Text_View.Gtk_Text_View. If you don't call
   --  Gtk.Text_View.Set_Buffer before using the text view, an empty default
   --  buffer will be created for you. Get the buffer with
   --  Gtk.Text_View.Get_Buffer. If you want to specify your own buffer,
   --  consider Gtk.Text_View.Gtk_New.

   procedure Gtk_New
      (View   : out Gtk_Text_View;
       Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   procedure Initialize
      (View   : not null access Gtk_Text_View_Record'Class;
       Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Creates a new Gtk.Text_View.Gtk_Text_View widget displaying the buffer
   --  Buffer. One buffer can be shared among many widgets. Buffer may be null
   --  to create a default buffer, in which case this function is equivalent to
   --  Gtk.Text_View.Gtk_New. The text view adds its own reference count to the
   --  buffer; it does not take over an existing reference.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "buffer": a Gtk.Text_Buffer.Gtk_Text_Buffer

   function Gtk_Text_View_New_With_Buffer
      (Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
       return Gtk_Text_View;
   --  Creates a new Gtk.Text_View.Gtk_Text_View widget displaying the buffer
   --  Buffer. One buffer can be shared among many widgets. Buffer may be null
   --  to create a default buffer, in which case this function is equivalent to
   --  Gtk.Text_View.Gtk_New. The text view adds its own reference count to the
   --  buffer; it does not take over an existing reference.
   --  "buffer": a Gtk.Text_Buffer.Gtk_Text_Buffer

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
   --  "child": a Gtk.Widget.Gtk_Widget
   --  "anchor": a Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor in the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer for Text_View

   procedure Add_Child_In_Window
      (View         : not null access Gtk_Text_View_Record;
       Child        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Which_Window : Gtk.Enums.Gtk_Text_Window_Type;
       Xpos         : Glib.Gint;
       Ypos         : Glib.Gint);
   --  Adds a child at fixed coordinates in one of the text widget's windows.
   --  The window must have nonzero size (see
   --  Gtk.Text_View.Set_Border_Window_Size). Note that the child coordinates
   --  are given relative to scrolling. When placing a child in
   --  GTK_TEXT_WINDOW_WIDGET, scrolling is irrelevant, the child floats above
   --  all scrollable areas. But when placing a child in one of the scrollable
   --  windows (border windows or text window) it will move with the scrolling
   --  as needed.
   --  "child": a Gtk.Widget.Gtk_Widget
   --  "which_window": which window the child should appear in
   --  "xpos": X position of child in window coordinates
   --  "ypos": Y position of child in window coordinates

   function Backward_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter backward by one display (wrapped) line. A display
   --  line is different from a paragraph. Paragraphs are separated by newlines
   --  or other paragraph separator characters. Display lines are created by
   --  line-wrapping a paragraph. If wrapping is turned off, display lines and
   --  paragraphs will be the same. Display lines are divided differently for
   --  each view, since they depend on the view's width; paragraphs are the
   --  same in all views, since they depend on the contents of the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter

   function Backward_Display_Line_Start
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter backward to the next display line start. A display
   --  line is different from a paragraph. Paragraphs are separated by newlines
   --  or other paragraph separator characters. Display lines are created by
   --  line-wrapping a paragraph. If wrapping is turned off, display lines and
   --  paragraphs will be the same. Display lines are divided differently for
   --  each view, since they depend on the view's width; paragraphs are the
   --  same in all views, since they depend on the contents of the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter

   procedure Buffer_To_Window_Coords
      (View     : not null access Gtk_Text_View_Record;
       Win      : Gtk.Enums.Gtk_Text_Window_Type;
       Buffer_X : Glib.Gint;
       Buffer_Y : Glib.Gint;
       Window_X : out Glib.Gint;
       Window_Y : out Glib.Gint);
   --  Converts coordinate (Buffer_X, Buffer_Y) to coordinates for the window
   --  Win, and stores the result in (Window_X, Window_Y).
   --  Note that you can't convert coordinates for a nonexisting window (see
   --  Gtk.Text_View.Set_Border_Window_Size).
   --  "win": a Gtk.Enums.Gtk_Text_Window_Type, except
   --  Gtk.Enums.Text_Window_Private
   --  "buffer_x": buffer x coordinate
   --  "buffer_y": buffer y coordinate
   --  "window_x": window x coordinate return location or null
   --  "window_y": window y coordinate return location or null

   function Forward_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter forward by one display (wrapped) line. A display
   --  line is different from a paragraph. Paragraphs are separated by newlines
   --  or other paragraph separator characters. Display lines are created by
   --  line-wrapping a paragraph. If wrapping is turned off, display lines and
   --  paragraphs will be the same. Display lines are divided differently for
   --  each view, since they depend on the view's width; paragraphs are the
   --  same in all views, since they depend on the contents of the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter

   function Forward_Display_Line_End
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Moves the given Iter forward to the next display line end. A display
   --  line is different from a paragraph. Paragraphs are separated by newlines
   --  or other paragraph separator characters. Display lines are created by
   --  line-wrapping a paragraph. If wrapping is turned off, display lines and
   --  paragraphs will be the same. Display lines are divided differently for
   --  each view, since they depend on the view's width; paragraphs are the
   --  same in all views, since they depend on the contents of the
   --  Gtk.Text_Buffer.Gtk_Text_Buffer.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter

   function Get_Accepts_Tab
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Returns whether pressing the Tab key inserts a tab characters.
   --  Gtk.Text_View.Set_Accepts_Tab.
   --  Since: gtk+ 2.4

   procedure Set_Accepts_Tab
      (View        : not null access Gtk_Text_View_Record;
       Accepts_Tab : Boolean);
   --  Sets the behavior of the text widget when the Tab key is pressed. If
   --  Accepts_Tab is True, a tab character is inserted. If Accepts_Tab is
   --  False the keyboard focus is moved to the next widget in the focus chain.
   --  Since: gtk+ 2.4
   --  "accepts_tab": True if pressing the Tab key should insert a tab
   --  character, False, if pressing the Tab key should move the keyboard
   --  focus.

   function Get_Border_Window_Size
      (View     : not null access Gtk_Text_View_Record;
       The_Type : Gtk.Enums.Gtk_Text_Window_Type) return Glib.Gint;
   --  Gets the width of the specified border window. See
   --  Gtk.Text_View.Set_Border_Window_Size.
   --  "type": window to return size from

   procedure Set_Border_Window_Size
      (View     : not null access Gtk_Text_View_Record;
       The_Type : Gtk.Enums.Gtk_Text_Window_Type;
       Size     : Glib.Gint);
   --  Sets the width of Gtk.Enums.Text_Window_Left or
   --  Gtk.Enums.Text_Window_Right, or the height of Gtk.Enums.Text_Window_Top
   --  or Gtk.Enums.Text_Window_Bottom. Automatically destroys the
   --  corresponding window if the size is set to 0, and creates the window if
   --  the size is set to non-zero. This function can only be used for the
   --  "border windows", and it won't work with Gtk.Enums.Text_Window_Widget,
   --  Gtk.Enums.Text_Window_Text, or Gtk.Enums.Text_Window_Private.
   --  "type": window to affect
   --  "size": width or height of the window

   function Get_Bottom_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the bottom margin for text in the Text_View.
   --  Since: gtk+ 3.18

   procedure Set_Bottom_Margin
      (View          : not null access Gtk_Text_View_Record;
       Bottom_Margin : Glib.Gint);
   --  Sets the bottom margin for text in Text_View.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  Since: gtk+ 3.18
   --  "bottom_margin": bottom margin in pixels

   function Get_Buffer
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Returns the Gtk.Text_Buffer.Gtk_Text_Buffer being displayed by this
   --  text view. The reference count on the buffer is not incremented; the
   --  caller of this function won't own a new reference.

   procedure Set_Buffer
      (View   : not null access Gtk_Text_View_Record;
       Buffer : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Sets Buffer as the buffer being displayed by Text_View. The previous
   --  buffer displayed by the text view is unreferenced, and a reference is
   --  added to Buffer. If you owned a reference to Buffer before passing it to
   --  this function, you must remove that reference yourself;
   --  Gtk.Text_View.Gtk_Text_View will not "adopt" it.
   --  "buffer": a Gtk.Text_Buffer.Gtk_Text_Buffer

   procedure Get_Cursor_Locations
      (View   : not null access Gtk_Text_View_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Strong : out Gdk.Rectangle.Gdk_Rectangle;
       Weak   : out Gdk.Rectangle.Gdk_Rectangle);
   --  Given an Iter within a text layout, determine the positions of the
   --  strong and weak cursors if the insertion point is at that iterator. The
   --  position of each cursor is stored as a zero-width rectangle. The strong
   --  cursor location is the location where characters of the directionality
   --  equal to the base direction of the paragraph are inserted. The weak
   --  cursor location is the location where characters of the directionality
   --  opposite to the base direction of the paragraph are inserted.
   --  If Iter is null, the actual cursor position is used.
   --  Note that if Iter happens to be the actual cursor position, and there
   --  is currently an IM preedit sequence being entered, the returned
   --  locations will be adjusted to account for the preedit cursor's offset
   --  within the preedit sequence.
   --  The rectangle position is in buffer coordinates; use
   --  Gtk.Text_View.Buffer_To_Window_Coords to convert these coordinates to
   --  coordinates for one of the windows in the text view.
   --  Since: gtk+ 3.0
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "strong": location to store the strong cursor position (may be null)
   --  "weak": location to store the weak cursor position (may be null)

   function Get_Cursor_Visible
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Find out whether the cursor should be displayed.

   procedure Set_Cursor_Visible
      (View    : not null access Gtk_Text_View_Record;
       Setting : Boolean);
   --  Toggles whether the insertion point should be displayed. A buffer with
   --  no editable text probably shouldn't have a visible cursor, so you may
   --  want to turn the cursor off.
   --  Note that this property may be overridden by the
   --  Gtk.Settings.Gtk_Settings:gtk-keynave-use-caret settings.
   --  "setting": whether to show the insertion cursor

   function Get_Default_Attributes
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Text_Attributes.Gtk_Text_Attributes;
   --  Obtains a copy of the default text attributes. These are the attributes
   --  used for text unless a tag overrides them. You'd typically pass the
   --  default attributes in to Gtk.Text_Iter.Get_Attributes in order to get
   --  the attributes in effect at a given text position.
   --  The return value is a copy owned by the caller of this function, and
   --  should be freed with Gtk.Text_Attributes.Unref.

   function Get_Editable
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Returns the default editability of the Gtk.Text_View.Gtk_Text_View.
   --  Tags in the buffer may override this setting for some ranges of text.

   procedure Set_Editable
      (View    : not null access Gtk_Text_View_Record;
       Setting : Boolean);
   --  Sets the default editability of the Gtk.Text_View.Gtk_Text_View. You
   --  can override this default setting with tags in the buffer, using the
   --  "editable" attribute of tags.
   --  "setting": whether it's editable

   function Get_Indent
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default indentation of paragraphs in Text_View. Tags in the
   --  view's buffer may override the default. The indentation may be negative.

   procedure Set_Indent
      (View   : not null access Gtk_Text_View_Record;
       Indent : Glib.Gint);
   --  Sets the default indentation for paragraphs in Text_View. Tags in the
   --  buffer may override the default.
   --  "indent": indentation in pixels

   function Get_Input_Hints
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Input_Hints;
   --  Gets the value of the Gtk.Text_View.Gtk_Text_View:input-hints property.
   --  Since: gtk+ 3.6

   procedure Set_Input_Hints
      (View  : not null access Gtk_Text_View_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints);
   --  Sets the Gtk.Text_View.Gtk_Text_View:input-hints property, which allows
   --  input methods to fine-tune their behaviour.
   --  Since: gtk+ 3.6
   --  "hints": the hints

   function Get_Input_Purpose
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Input_Purpose;
   --  Gets the value of the Gtk.Text_View.Gtk_Text_View:input-purpose
   --  property.
   --  Since: gtk+ 3.6

   procedure Set_Input_Purpose
      (View    : not null access Gtk_Text_View_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose);
   --  Sets the Gtk.Text_View.Gtk_Text_View:input-purpose property which can
   --  be used by on-screen keyboards and other input methods to adjust their
   --  behaviour.
   --  Since: gtk+ 3.6
   --  "purpose": the purpose

   function Get_Iter_At_Location
      (View : not null access Gtk_Text_View_Record;
       Iter : access Gtk.Text_Iter.Gtk_Text_Iter;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Boolean;
   --  Retrieves the iterator at buffer coordinates X and Y. Buffer
   --  coordinates are coordinates for the entire buffer, not just the
   --  currently-displayed portion. If you have coordinates from an event, you
   --  have to convert those to buffer coordinates with
   --  Gtk.Text_View.Window_To_Buffer_Coords.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "x": x position, in buffer coordinates
   --  "y": y position, in buffer coordinates

   function Get_Iter_At_Position
      (View     : not null access Gtk_Text_View_Record;
       Iter     : access Gtk.Text_Iter.Gtk_Text_Iter;
       Trailing : access Glib.Gint;
       X        : Glib.Gint;
       Y        : Glib.Gint) return Boolean;
   --  Retrieves the iterator pointing to the character at buffer coordinates
   --  X and Y. Buffer coordinates are coordinates for the entire buffer, not
   --  just the currently-displayed portion. If you have coordinates from an
   --  event, you have to convert those to buffer coordinates with
   --  Gtk.Text_View.Window_To_Buffer_Coords.
   --  Note that this is different from Gtk.Text_View.Get_Iter_At_Location,
   --  which returns cursor locations, i.e. positions between characters.
   --  Since: gtk+ 2.6
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "trailing": if non-null, location to store an integer indicating where
   --  in the grapheme the user clicked. It will either be zero, or the number
   --  of characters in the grapheme. 0 represents the trailing edge of the
   --  grapheme.
   --  "x": x position, in buffer coordinates
   --  "y": y position, in buffer coordinates

   procedure Get_Iter_Location
      (View     : not null access Gtk_Text_View_Record;
       Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
       Location : out Gdk.Rectangle.Gdk_Rectangle);
   --  Gets a rectangle which roughly contains the character at Iter. The
   --  rectangle position is in buffer coordinates; use
   --  Gtk.Text_View.Buffer_To_Window_Coords to convert these coordinates to
   --  coordinates for one of the windows in the text view.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "location": bounds of the character at Iter

   function Get_Justification
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Justification;
   --  Gets the default justification of paragraphs in Text_View. Tags in the
   --  buffer may override the default.

   procedure Set_Justification
      (View          : not null access Gtk_Text_View_Record;
       Justification : Gtk.Enums.Gtk_Justification);
   --  Sets the default justification of text in Text_View. Tags in the view's
   --  buffer may override the default.
   --  "justification": justification

   function Get_Left_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default left margin size of paragraphs in the Text_View. Tags
   --  in the buffer may override the default.

   procedure Set_Left_Margin
      (View        : not null access Gtk_Text_View_Record;
       Left_Margin : Glib.Gint);
   --  Sets the default left margin for text in Text_View. Tags in the buffer
   --  may override the default.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  "left_margin": left margin in pixels

   procedure Get_Line_At_Y
      (View        : not null access Gtk_Text_View_Record;
       Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
       Y           : Glib.Gint;
       Line_Top    : out Glib.Gint);
   --  Gets the Gtk.Text_Iter.Gtk_Text_Iter at the start of the line
   --  containing the coordinate Y. Y is in buffer coordinates, convert from
   --  window coordinates with Gtk.Text_View.Window_To_Buffer_Coords. If
   --  non-null, Line_Top will be filled with the coordinate of the top edge of
   --  the line.
   --  "target_iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "y": a y coordinate
   --  "line_top": return location for top coordinate of the line

   procedure Get_Line_Yrange
      (View   : not null access Gtk_Text_View_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Y      : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the y coordinate of the top of the line containing Iter, and the
   --  height of the line. The coordinate is a buffer coordinate; convert to
   --  window coordinates with Gtk.Text_View.Buffer_To_Window_Coords.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "y": return location for a y coordinate
   --  "height": return location for a height

   function Get_Monospace
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Gets the value of the Gtk.Text_View.Gtk_Text_View:monospace property.
   --  Since: gtk+ 3.16

   procedure Set_Monospace
      (View      : not null access Gtk_Text_View_Record;
       Monospace : Boolean);
   --  Sets the Gtk.Text_View.Gtk_Text_View:monospace property, which
   --  indicates that the text view should use monospace fonts.
   --  Since: gtk+ 3.16
   --  "monospace": True to request monospace styling

   function Get_Overwrite
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Returns whether the Gtk.Text_View.Gtk_Text_View is in overwrite mode or
   --  not.
   --  Since: gtk+ 2.4

   procedure Set_Overwrite
      (View      : not null access Gtk_Text_View_Record;
       Overwrite : Boolean);
   --  Changes the Gtk.Text_View.Gtk_Text_View overwrite mode.
   --  Since: gtk+ 2.4
   --  "overwrite": True to turn on overwrite mode, False to turn it off

   function Get_Pixels_Above_Lines
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default number of pixels to put above paragraphs. Adding this
   --  function with Gtk.Text_View.Get_Pixels_Below_Lines is equal to the line
   --  space between each paragraph.

   procedure Set_Pixels_Above_Lines
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Above_Lines : Glib.Gint);
   --  Sets the default number of blank pixels above paragraphs in Text_View.
   --  Tags in the buffer for Text_View may override the defaults.
   --  "pixels_above_lines": pixels above paragraphs

   function Get_Pixels_Below_Lines
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the value set by Gtk.Text_View.Set_Pixels_Below_Lines.
   --  The line space is the sum of the value returned by this function and
   --  the value returned by Gtk.Text_View.Get_Pixels_Above_Lines.

   procedure Set_Pixels_Below_Lines
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Below_Lines : Glib.Gint);
   --  Sets the default number of pixels of blank space to put below
   --  paragraphs in Text_View. May be overridden by tags applied to
   --  Text_View's buffer.
   --  "pixels_below_lines": pixels below paragraphs

   function Get_Pixels_Inside_Wrap
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the value set by Gtk.Text_View.Set_Pixels_Inside_Wrap.

   procedure Set_Pixels_Inside_Wrap
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Inside_Wrap : Glib.Gint);
   --  Sets the default number of pixels of blank space to leave between
   --  display/wrapped lines within a paragraph. May be overridden by tags in
   --  Text_View's buffer.
   --  "pixels_inside_wrap": default number of pixels between wrapped lines

   function Get_Right_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the default right margin for text in Text_View. Tags in the buffer
   --  may override the default.

   procedure Set_Right_Margin
      (View         : not null access Gtk_Text_View_Record;
       Right_Margin : Glib.Gint);
   --  Sets the default right margin for text in the text view. Tags in the
   --  buffer may override the default.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  "right_margin": right margin in pixels

   function Get_Tabs
      (View : not null access Gtk_Text_View_Record)
       return Pango.Tabs.Pango_Tab_Array;
   --  Gets the default tabs for Text_View. Tags in the buffer may override
   --  the defaults. The returned array will be null if "standard" (8-space)
   --  tabs are used. Free the return value with Pango.Tabs.Free.

   procedure Set_Tabs
      (View : not null access Gtk_Text_View_Record;
       Tabs : Pango.Tabs.Pango_Tab_Array);
   --  Sets the default tab stops for paragraphs in Text_View. Tags in the
   --  buffer may override the default.
   --  "tabs": tabs as a Pango.Tabs.Pango_Tab_Array

   function Get_Top_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint;
   --  Gets the top margin for text in the Text_View.
   --  Since: gtk+ 3.18

   procedure Set_Top_Margin
      (View       : not null access Gtk_Text_View_Record;
       Top_Margin : Glib.Gint);
   --  Sets the top margin for text in Text_View.
   --  Note that this function is confusingly named. In CSS terms, the value
   --  set here is padding.
   --  Since: gtk+ 3.18
   --  "top_margin": top margin in pixels

   procedure Get_Visible_Rect
      (View         : not null access Gtk_Text_View_Record;
       Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills Visible_Rect with the currently-visible region of the buffer, in
   --  buffer coordinates. Convert to window coordinates with
   --  Gtk.Text_View.Buffer_To_Window_Coords.
   --  "visible_rect": rectangle to fill

   function Get_Window
      (View : not null access Gtk_Text_View_Record;
       Win  : Gtk.Enums.Gtk_Text_Window_Type) return Gdk.Gdk_Window;
   --  Retrieves the Gdk.Gdk_Window corresponding to an area of the text view;
   --  possible windows include the overall widget window, child windows on the
   --  left, right, top, bottom, and the window that displays the text buffer.
   --  Windows are null and nonexistent if their width or height is 0, and are
   --  nonexistent before the widget has been realized.
   --  "win": window to get

   function Get_Window_Type
      (View   : not null access Gtk_Text_View_Record;
       Window : Gdk.Gdk_Window) return Gtk.Enums.Gtk_Text_Window_Type;
   --  Usually used to find out which window an event corresponds to.
   --  If you connect to an event signal on Text_View, this function should be
   --  called on `event->window` to see which window it was.
   --  "window": a window type

   function Get_Wrap_Mode
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Wrap_Mode;
   --  Gets the line wrapping for the view.

   procedure Set_Wrap_Mode
      (View      : not null access Gtk_Text_View_Record;
       Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);
   --  Sets the line wrapping for the view.
   --  "wrap_mode": a Gtk.Enums.Gtk_Wrap_Mode

   function Im_Context_Filter_Keypress
      (View  : not null access Gtk_Text_View_Record;
       Event : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Allow the Gtk.Text_View.Gtk_Text_View input method to internally handle
   --  key press and release events. If this function returns True, then no
   --  further processing should be done for this key event. See
   --  Gtk.IM_Context.Filter_Keypress.
   --  Note that you are expected to call this function from your handler when
   --  overriding key event handling. This is needed in the case when you need
   --  to insert your own key handling between the input method and the default
   --  key event handling of the Gtk.Text_View.Gtk_Text_View.
   --  |[<!-- language="C" --> static gboolean gtk_foo_bar_key_press_event
   --  (GtkWidget *widget, GdkEventKey *event) { guint keyval;
   --  gdk_event_get_keyval ((GdkEvent*)event, &keyval);
   --  if (keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter) { if
   --  (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (widget),
   --  event)) return TRUE; }
   --  // Do some stuff
   --  return GTK_WIDGET_CLASS (gtk_foo_bar_parent_class)->key_press_event
   --  (widget, event); } ]|
   --  Since: gtk+ 2.22
   --  "event": the key event

   procedure Move_Child
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Xpos  : Glib.Gint;
       Ypos  : Glib.Gint);
   --  Updates the position of a child, as for
   --  Gtk.Text_View.Add_Child_In_Window.
   --  "child": child widget already added to the text view
   --  "xpos": new X position in window coordinates
   --  "ypos": new Y position in window coordinates

   function Move_Mark_Onscreen
      (View : not null access Gtk_Text_View_Record;
       Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
       return Boolean;
   --  Moves a mark within the buffer so that it's located within the
   --  currently-visible text area.
   --  "mark": a Gtk.Text_Mark.Gtk_Text_Mark

   function Move_Visually
      (View  : not null access Gtk_Text_View_Record;
       Iter  : Gtk.Text_Iter.Gtk_Text_Iter;
       Count : Glib.Gint) return Boolean;
   --  Move the iterator a given number of characters visually, treating it as
   --  the strong cursor position. If Count is positive, then the new strong
   --  cursor position will be Count positions to the right of the old cursor
   --  position. If Count is negative then the new strong cursor position will
   --  be Count positions to the left of the old cursor position.
   --  In the presence of bi-directional text, the correspondence between
   --  logical and visual order will depend on the direction of the current
   --  run, and there may be jumps when the cursor is moved off of the end of a
   --  run.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "count": number of characters to move (negative moves left, positive
   --  moves right)

   function Place_Cursor_Onscreen
      (View : not null access Gtk_Text_View_Record) return Boolean;
   --  Moves the cursor to the currently visible region of the buffer, it it
   --  isn't there already.

   procedure Reset_Cursor_Blink
      (View : not null access Gtk_Text_View_Record);
   --  Ensures that the cursor is shown (i.e. not in an 'off' blink interval)
   --  and resets the time that it will stay blinking (or visible, in case
   --  blinking is disabled).
   --  This function should be called in response to user input (e.g. from
   --  derived classes that override the textview's
   --  Gtk.Widget.Gtk_Widget::key-press-event handler).
   --  Since: gtk+ 3.20

   procedure Reset_Im_Context (View : not null access Gtk_Text_View_Record);
   --  Reset the input method context of the text view if needed.
   --  This can be necessary in the case where modifying the buffer would
   --  confuse on-going input method behavior.
   --  Since: gtk+ 2.22

   procedure Scroll_Mark_Onscreen
      (View : not null access Gtk_Text_View_Record;
       Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Scrolls Text_View the minimum distance such that Mark is contained
   --  within the visible area of the widget.
   --  "mark": a mark in the buffer for Text_View

   function Scroll_To_Iter
      (View          : not null access Gtk_Text_View_Record;
       Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
       Within_Margin : Gdouble;
       Use_Align     : Boolean;
       Xalign        : Gdouble;
       Yalign        : Gdouble) return Boolean;
   --  Scrolls Text_View so that Iter is on the screen in the position
   --  indicated by Xalign and Yalign. An alignment of 0.0 indicates left or
   --  top, 1.0 indicates right or bottom, 0.5 means center. If Use_Align is
   --  False, the text scrolls the minimal distance to get the mark onscreen,
   --  possibly not scrolling at all. The effective screen for purposes of this
   --  function is reduced by a margin of size Within_Margin.
   --  Note that this function uses the currently-computed height of the lines
   --  in the text buffer. Line heights are computed in an idle handler; so
   --  this function may not have the desired effect if it's called before the
   --  height computations. To avoid oddness, consider using
   --  Gtk.Text_View.Scroll_To_Mark which saves a point to be scrolled to after
   --  line validation.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter
   --  "within_margin": margin as a [0.0,0.5) fraction of screen size
   --  "use_align": whether to use alignment arguments (if False, just get the
   --  mark onscreen)
   --  "xalign": horizontal alignment of mark within visible area
   --  "yalign": vertical alignment of mark within visible area

   procedure Scroll_To_Mark
      (View          : not null access Gtk_Text_View_Record;
       Mark          : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Within_Margin : Gdouble;
       Use_Align     : Boolean;
       Xalign        : Gdouble;
       Yalign        : Gdouble);
   --  Scrolls Text_View so that Mark is on the screen in the position
   --  indicated by Xalign and Yalign. An alignment of 0.0 indicates left or
   --  top, 1.0 indicates right or bottom, 0.5 means center. If Use_Align is
   --  False, the text scrolls the minimal distance to get the mark onscreen,
   --  possibly not scrolling at all. The effective screen for purposes of this
   --  function is reduced by a margin of size Within_Margin.
   --  "mark": a Gtk.Text_Mark.Gtk_Text_Mark
   --  "within_margin": margin as a [0.0,0.5) fraction of screen size
   --  "use_align": whether to use alignment arguments (if False, just get the
   --  mark onscreen)
   --  "xalign": horizontal alignment of mark within visible area
   --  "yalign": vertical alignment of mark within visible area

   function Starts_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Determines whether Iter is at the start of a display line. See
   --  Gtk.Text_View.Forward_Display_Line for an explanation of display lines
   --  vs. paragraphs.
   --  "iter": a Gtk.Text_Iter.Gtk_Text_Iter

   procedure Window_To_Buffer_Coords
      (View     : not null access Gtk_Text_View_Record;
       Win      : Gtk.Enums.Gtk_Text_Window_Type;
       Window_X : Glib.Gint;
       Window_Y : Glib.Gint;
       Buffer_X : out Glib.Gint;
       Buffer_Y : out Glib.Gint);
   --  Converts coordinates on the window identified by Win to buffer
   --  coordinates, storing the result in (Buffer_X,Buffer_Y).
   --  Note that you can't convert coordinates for a nonexisting window (see
   --  Gtk.Text_View.Set_Border_Window_Size).
   --  "win": a Gtk.Enums.Gtk_Text_Window_Type except
   --  Gtk.Enums.Text_Window_Private
   --  "window_x": window x coordinate
   --  "window_y": window y coordinate
   --  "buffer_x": buffer x coordinate return location or null
   --  "buffer_y": buffer y coordinate return location or null

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Border
      (Self   : not null access Gtk_Text_View_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean;

   function Get_Hadjustment
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Text_View_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Text_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   function Get_Vadjustment
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Text_View_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Text_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accepts_Tab_Property : constant Glib.Properties.Property_Boolean;

   Bottom_Margin_Property : constant Glib.Properties.Property_Int;
   --  The bottom margin for text in the text view.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.
   --
   --  Don't confuse this property with Gtk.Widget.Gtk_Widget:margin-bottom.

   Buffer_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Text_Buffer.Gtk_Text_Buffer

   Cursor_Visible_Property : constant Glib.Properties.Property_Boolean;

   Editable_Property : constant Glib.Properties.Property_Boolean;

   Im_Module_Property : constant Glib.Properties.Property_String;
   --  Which IM (input method) module should be used for this text_view. See
   --  Gtk.IM_Context.Gtk_IM_Context.
   --
   --  Setting this to a non-null value overrides the system-wide IM module
   --  setting. See the GtkSettings Gtk.Settings.Gtk_Settings:gtk-im-module
   --  property.

   Indent_Property : constant Glib.Properties.Property_Int;

   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints;
   --  Additional hints (beyond Gtk.Text_View.Gtk_Text_View:input-purpose)
   --  that allow input methods to fine-tune their behaviour.

   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose;
   --  The purpose of this text field.
   --
   --  This property can be used by on-screen keyboards and other input
   --  methods to adjust their behaviour.

   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification;

   Left_Margin_Property : constant Glib.Properties.Property_Int;
   --  The default left margin for text in the text view. Tags in the buffer
   --  may override the default.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.
   --
   --  Don't confuse this property with Gtk.Widget.Gtk_Widget:margin-left.

   Monospace_Property : constant Glib.Properties.Property_Boolean;

   Overwrite_Property : constant Glib.Properties.Property_Boolean;

   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;

   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;

   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int;

   Populate_All_Property : constant Glib.Properties.Property_Boolean;
   --  If :populate-all is True, the
   --  Gtk.Text_View.Gtk_Text_View::populate-popup signal is also emitted for
   --  touch popups.

   Right_Margin_Property : constant Glib.Properties.Property_Int;
   --  The default right margin for text in the text view. Tags in the buffer
   --  may override the default.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.
   --
   --  Don't confuse this property with Gtk.Widget.Gtk_Widget:margin-right.

   Tabs_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Tab_Array

   Top_Margin_Property : constant Glib.Properties.Property_Int;
   --  The top margin for text in the text view.
   --
   --  Note that this property is confusingly named. In CSS terms, the value
   --  set here is padding, and it is applied in addition to the padding from
   --  the theme.
   --
   --  Don't confuse this property with Gtk.Widget.Gtk_Widget:margin-top.

   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode;

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
   --  The ::backspace signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted when the user asks for it.
   --
   --  The default bindings for this signal are Backspace and Shift-Backspace.

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
   --  The ::copy-clipboard signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to copy the selection to the clipboard.
   --
   --  The default bindings for this signal are Ctrl-c and Ctrl-Insert.

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
   --  The ::cut-clipboard signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to cut the selection to the clipboard.
   --
   --  The default bindings for this signal are Ctrl-x and Shift-Delete.

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
   --  The ::delete-from-cursor signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user initiates a
   --  text deletion.
   --
   --  If the Type is Gtk.Enums.Delete_Chars, GTK+ deletes the selection if
   --  there is one, otherwise it deletes the requested number of characters.
   --
   --  The default bindings for this signal are Delete for deleting a
   --  character, Ctrl-Delete for deleting a word and Ctrl-Backspace for
   --  deleting a word backwords.
   -- 
   --  Callback parameters:
   --    --  "type": the granularity of the deletion, as a Gtk.Enums.Gtk_Delete_Type
   --    --  "count": the number of Type units to delete

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
   --  The ::extend-selection signal is emitted when the selection needs to be
   --  extended at Location.
   -- 
   --  Callback parameters:
   --    --  "granularity": the granularity type
   --    --  "location": the location where to extend the selection
   --    --  "start": where the selection should start
   --    --  "end": where the selection should end
   --    --  Returns GDK_EVENT_STOP to stop other handlers from being invoked for the
   --   event. GDK_EVENT_PROPAGATE to propagate the event further.

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
   --  The ::insert-at-cursor signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user initiates the
   --  insertion of a fixed string at the cursor.
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
   --  The ::insert-emoji signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to present the Emoji chooser for the Text_View.
   --
   --  The default bindings for this signal are Ctrl-. and Ctrl-;

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
   --  The ::move-cursor signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates a cursor movement. If the
   --  cursor is not visible in Text_View, this signal causes the viewport to
   --  be moved instead.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the Shift modifier extends the selection, the variant without the
   --  Shift modifer does not. There are too many key combinations to list them
   --  all here. - Arrow keys move by individual characters/lines - Ctrl-arrow
   --  key combinations move by words/paragraphs - Home/End keys move to the
   --  ends of the buffer - PageUp/PageDown keys move vertically by pages -
   --  Ctrl-PageUp/PageDown keys move horizontally by pages
   -- 
   --  Callback parameters:
   --    --  "step": the granularity of the move, as a Gtk.Enums.Gtk_Movement_Step
   --    --  "count": the number of Step units to move
   --    --  "extend_selection": True if the move should extend the selection

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
   --  The ::move-viewport signal is a [keybinding signal][GtkBindingSignal]
   --  which can be bound to key combinations to allow the user to move the
   --  viewport, i.e. change what part of the text view is visible in a
   --  containing scrolled window.
   --
   --  There are no default bindings for this signal.
   -- 
   --  Callback parameters:
   --    --  "step": the granularity of the movement, as a Gtk.Enums.Gtk_Scroll_Step
   --    --  "count": the number of Step units to move

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
   --  The ::paste-clipboard signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to paste the contents of the clipboard into the text
   --  view.
   --
   --  The default bindings for this signal are Ctrl-v and Shift-Insert.

   type Cb_Gtk_Text_View_Gtk_Widget_Void is not null access procedure
     (Self  : access Gtk_Text_View_Record'Class;
      Popup : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Popup : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Populate_Popup : constant Glib.Signal_Name := "populate-popup";
   procedure On_Populate_Popup
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Populate_Popup
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::populate-popup signal gets emitted before showing the context
   --  menu of the text view.
   --
   --  If you need to add items to the context menu, connect to this signal
   --  and append your items to the Popup, which will be a Gtk.Menu.Gtk_Menu in
   --  this case.
   --
   --  If Gtk.Text_View.Gtk_Text_View:populate-all is True, this signal will
   --  also be emitted to populate touch popups. In this case, Popup will be a
   --  different container, e.g. a Gtk.Toolbar.Gtk_Toolbar.
   --
   --  The signal handler should not make assumptions about the type of
   --  Widget, but check whether Popup is a Gtk.Menu.Gtk_Menu or
   --  Gtk.Toolbar.Gtk_Toolbar or another kind of container.

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
   --  The ::select-all signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to select or unselect the complete contents of the
   --  text view.
   --
   --  The default bindings for this signal are Ctrl-a and Ctrl-/ for
   --  selecting and Shift-Ctrl-a and Ctrl-\ for unselecting.

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
   --  The ::set-anchor signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates setting the "anchor" mark.
   --  The "anchor" mark gets placed at the same position as the "insert" mark.
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
   --  The ::toggle-cursor-visible signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted to toggle the
   --  Gtk.Text_View.Gtk_Text_View:cursor-visible property.
   --
   --  The default binding for this signal is F7.

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
   --  The ::toggle-overwrite signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted to toggle the overwrite
   --  mode of the text view.
   --
   --  The default bindings for this signal is Insert.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Scrollable"

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

   package Implements_Gtk_Scrollable is new Glib.Types.Implements
     (Gtk.Scrollable.Gtk_Scrollable, Gtk_Text_View_Record, Gtk_Text_View);
   function "+"
     (Widget : access Gtk_Text_View_Record'Class)
   return Gtk.Scrollable.Gtk_Scrollable
   renames Implements_Gtk_Scrollable.To_Interface;
   function "-"
     (Interf : Gtk.Scrollable.Gtk_Scrollable)
   return Gtk_Text_View
   renames Implements_Gtk_Scrollable.To_Object;

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Draw_Layer is access procedure
     (View  : System.Address;
      Layer : Gtk_Text_View_Layer;
      Cr    : Cairo.Cairo_Context);
   pragma Convention (C, Virtual_Draw_Layer);
   --  The draw_layer virtual function is called before and after the text
   --  view is drawing its own text. Applications can override this vfunc in a
   --  subclass to draw customized content underneath or above the text. Since:
   --  3.14

   subtype Text_View_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Draw_Layer
     (Self    : Glib.Object.GObject_Class;
      Handler : Virtual_Draw_Layer);
   pragma Import (C, Set_Draw_Layer, "gtkada_Text_View_set_draw_layer");
   --  See Glib.Object.Add_Interface

private
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap-mode");
   Top_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("top-margin");
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Right_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-margin");
   Populate_All_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("populate-all");
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
