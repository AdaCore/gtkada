-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2003 ACT-Europe                 --
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
--  This is the View of the Model/View/Controller text capabilities of GtkAda.
--  </description>
--  <c_version>1.3.11</c_version>

with Gdk.Rectangle;
with Gdk.Window;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Text_Buffer;
with Gtk.Text_Child;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Widget;

package Gtk.Text_View is

   type Gtk_Text_View_Record is
     new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Text_View is access all Gtk_Text_View_Record'Class;

   procedure Gtk_New
     (Widget : out Gtk_Text_View;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer := null);
   --  Create a new Gtk_Text_View.
   --  If Buffer is null, an empty default buffer will be created for you. Get
   --  the buffer with Get_Buffer.
   --  Otherwise, create a new text view widget displaying Buffer.
   --  One buffer can be shared among many widgets.
   --  The text view adds its own reference count to the buffer; it does not
   --  take over an existing reference.

   procedure Initialize
     (Widget : access Gtk_Text_View_Record'Class;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   procedure Set_Buffer
     (Text_View : access Gtk_Text_View_Record;
      Buffer    : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Set Buffer as the buffer being displayed by Text_View.
   --  The previous buffer displayed by the text view is unreferenced, and a
   --  reference is added to Buffer. If you owned a reference to Buffer before
   --  passing it to this function, you must remove that reference yourself;
   --  Gtk_Text_View will not "adopt" it.

   function Get_Buffer
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  Return the Gtk_Text_Buffer being displayed by this text view.
   --  The reference count on the buffer is not incremented; the caller of this
   --  function won't own a new reference.

   function Scroll_To_Iter
     (Text_View     : access Gtk_Text_View_Record;
      Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
      Within_Margin : Gdouble;
      Use_Align     : Boolean;
      Xalign        : Gdouble;
      Yalign        : Gdouble) return Boolean;
   --  Scroll Text_View so that Iter is on the screen in the position
   --  indicated by Xalign and Yalign. An alignment of 0.0 indicates left or
   --  top, 1.0 indicates right or bottom, 0.5 means center. If Use_Align is
   --  False, the text scrolls the minimal distance to get the mark onscreen,
   --  possibly not scrolling at all. The effective screen for purposes of this
   --  function is reduced by a margin of size Within_Margin.
   --  Note: This function uses the currently-computed height of the lines in
   --  the text buffer. Note that line heights are computed in an idle handler;
   --  so this function may not have the desired effect if it's called before
   --  the height computations. To avoid oddness, consider using
   --  Scroll_To_Mark which saves a point to be scrolled to after line
   --  validation.

   procedure Scroll_To_Mark
     (Text_View     : access Gtk_Text_View_Record;
      Mark          : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Within_Margin : Gdouble := 0.0;
      Use_Align     : Boolean := False;
      Xalign        : Gdouble := 0.0;
      Yalign        : Gdouble := 0.0);
   --  Scroll Text_View so that Mark is on the screen in the position indicated
   --  by Xalign and Yalign. An alignment of 0.0 indicates left or top, 1.0
   --  indicates right or bottom, 0.5 means center. If Use_Align is False, the
   --  text scrolls the minimal distance to get the mark onscreen, possibly not
   --  scrolling at all. The effective screen for purposes of this function is
   --  reduced by a margin of size Within_Margin.

   procedure Scroll_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --  Same as the above with the default values

   function Move_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
      return Boolean;
   --  Move a mark within the buffer so that it's located within the
   --  currently-visible text area.
   --  Return value: True if the mark moved (wasn't already onscreen).

   function Place_Cursor_Onscreen
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Move the cursor to the currently visible region of the buffer, if it
   --  isn't there already.
   --  Return value: True if the cursor had to be moved.

   procedure Get_Visible_Rect
     (Text_View    : access Gtk_Text_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fill Visible_Rect with the currently-visible region of the buffer, in
   --  buffer coordinates. Convert to window coordinates with
   --  Buffer_To_Window_Coords.

   procedure Set_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True);
   --  Toggle whether the insertion point is displayed.
   --  A buffer with no editable text probably shouldn't have a visible cursor,
   --  so you may want to turn the cursor off.

   function Get_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Whether the cursor is being displayed.

   procedure Get_Iter_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Location  : out Gdk.Rectangle.Gdk_Rectangle);
   --  Get a rectangle which roughly contains the character at iter.
   --  The rectangle position is in buffer coordinates; use
   --  Buffer_To_Window_Coords to convert these coordinates to coordinates for
   --  one of the windows in the text view.

   procedure Get_Iter_At_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : out Gtk.Text_Iter.Gtk_Text_Iter;
      X         : Gint;
      Y         : Gint);
   --  Retrieve the iterator at buffer coordinates X and Y.
   --  Buffer coordinates are coordinates for the entire buffer, not just the
   --  currently-displayed portion. If you have coordinates from an event, you
   --  have to convert those to buffer coordinates with
   --  Window_To_Buffer_Coords.

   procedure Get_Line_Yrange
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Y         : out Gint;
      Height    : out Gint);
   --  Get the Y coordinate of the top of the line containing Iter,
   --  and the Height of the line. The coordinate is a buffer coordinate;
   --  convert to window coordinates with Buffer_To_Window_Coords.

   procedure Get_Line_At_Y
     (Text_View   : access Gtk_Text_View_Record;
      Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
      Y           : Gint;
      Line_Top    : out Gint);
   --  Get the Gtk_Text_Iter at the start of the line containing the
   --  coordinate Y. Y is in buffer coordinates, convert from window
   --  coordinates with Window_To_Buffer_Coords.
   --  Line_Top will be filled with the coordinate of the top edge of the line.

   procedure Buffer_To_Window_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Window_X  : out Gint;
      Window_Y  : out Gint);
   --  Convert coordinate (Buffer_X, Buffer_Y) to coordinates for the window
   --  Win, and store the result in (Window_X, Window_Y).

   procedure Window_To_Buffer_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Window_X  : Gint;
      Window_Y  : Gint;
      Buffer_X  : out Gint;
      Buffer_Y  : out Gint);
   --  Convert coordinates on the window identified by Win to buffer
   --  coordinates, storing the result in (Buffer_X, Buffer_Y).

   function Get_Window
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type) return Gdk.Window.Gdk_Window;
   --  Retrieve the Gdk_Window corresponding to an area of the text view;
   --  possible windows include the overall widget window, child windows on the
   --  left, right, top, bottom, and the window that displays the text buffer.
   --  Windows are null and nonexistent if their width or height is 0, and are
   --  nonexistent before the widget has been realized.

   function Get_Window_Type
     (Text_View : access Gtk_Text_View_Record;
      Window    : Gdk.Window.Gdk_Window) return Gtk.Enums.Gtk_Text_Window_Type;
   --  Usually used to find out which window an event corresponds to.
   --  If you connect to an event signal on Text_View, this function should be
   --  called on Get_Window (Event) to see which window it was.

   procedure Set_Border_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      The_Type  : Gtk.Enums.Gtk_Text_Window_Type;
      Size      : Gint);
   --  Set the width of Text_Window_Left or Text_Window_Right,
   --  or the height of Text_Window_Top or Text_Window_Bottom.
   --  Automatically destroy the corresponding window if the size is set to 0,
   --  and create the window if the size is set to non-zero.

   procedure Set_Disable_Scroll_On_Focus
     (Text_View : access Gtk_Text_View_Record;
      Set       : Boolean);
   --  Set whether the Text_View should scroll to the cursor when it gets the
   --  focus. (This is the default behaviour)

   function Get_Disable_Scroll_On_Focus
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Return True when the behaviour to scroll on the cursor when grabbing the
   --  focus is disabled.

   procedure Forward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   --  ???

   procedure Backward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   --  ???

   procedure Forward_Display_Line_End
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   --  ???

   procedure Backward_Display_Line_Start
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);
   --  ???

   function Starts_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  ???

   procedure Move_Visually
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count     : Gint;
      Result    : out Boolean);
   --  ???

   procedure Add_Child_At_Anchor
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Anchor    : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);
   --  ???

   procedure Add_Child_In_Window
     (Text_View    : access Gtk_Text_View_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Which_Window : Gtk.Enums.Gtk_Text_Window_Type;
      Xpos         : Gint;
      Ypos         : Gint);
   --  ???

   procedure Move_Child
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Xpos      : Gint;
      Ypos      : Gint);
   --  ???

   procedure Set_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);
   --  Set the line wrapping for the view.

   function Get_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record) return Gtk.Enums.Gtk_Wrap_Mode;
   --  Get the line wrapping for the view.

   procedure Set_Editable
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True);
   --  Set the default editability of the Gtk_Text_View.
   --  You can override this default setting with tags in the buffer, using the
   --  "editable" attribute of tags.

   function Get_Editable
     (Text_View : access Gtk_Text_View_Record) return Boolean;
   --  Return the default editability of the Gtk_Text_View.
   --  Tags in the buffer may override this setting for some ranges of text.

   procedure Set_Pixels_Above_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Above_Lines : Gint);
   --  ???

   function Get_Pixels_Above_Lines
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  ???

   procedure Set_Pixels_Below_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Below_Lines : Gint);
   --  ???

   function Get_Pixels_Below_Lines
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  ???

   procedure Set_Pixels_Inside_Wrap
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Inside_Wrap : Gint);
   --  ???

   function Get_Pixels_Inside_Wrap
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  ???

   procedure Set_Justification
     (Text_View     : access Gtk_Text_View_Record;
      Justification : Gtk.Enums.Gtk_Justification);
   --  ???

   function Get_Justification
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Enums.Gtk_Justification;
   --  ???

   procedure Set_Left_Margin
     (Text_View   : access Gtk_Text_View_Record;
      Left_Margin : Gint);
   --  ???

   function Get_Left_Margin
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  ???

   procedure Set_Right_Margin
     (Text_View    : access Gtk_Text_View_Record;
      Right_Margin : Gint);
   --  ???

   function Get_Right_Margin
     (Text_View : access Gtk_Text_View_Record) return Gint;
   --  ???

   procedure Set_Indent
     (Text_View : access Gtk_Text_View_Record; Indent : Gint);
   --  ???

   function Get_Indent (Text_View : access Gtk_Text_View_Record) return Gint;
   --  ???

   --  ??? not bound because PangoTabArray isn't bound either:
   --  Set_Tabs
   --  Get_Tabs

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler
   --      (Widget      : access Gtk_Text_View_Record'Class;
   --       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
   --       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --
   --  - "populate_popup"
   --    procedure Handler
   --      (Widget : access Gtk_Text_View_Record'Class;
   --       Menu   : access Gtk.Menu.Gtk_Menu_Record'Class);
   --
   --  - "move_cursor"
   --    procedure Handler
   --      (Widget           : access Gtk_Text_View_Record'Class;
   --       Step             : Gtk_Movement_Step;
   --       Count            : Gint;
   --       Extend_Selection : Boolean);
   --
   --  - "set_anchor"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "insert_at_cursor"
   --    procedure Handler
   --      (Widget : access Gtk_Text_View_Record'Class; Str : String);
   --
   --  - "delete_from_cursor"
   --    procedure Handler
   --      (Widget   : access Gtk_Text_View_Record'Class;
   --       The_Type : Gtk_Delete_Type;
   --       Count    : Gint);
   --
   --  - "cut_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "copy_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "paste_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "toggle_overwrite"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  </signals>

private
   type Gtk_Text_View_Record is new Gtk.Container.Gtk_Container_Record with
     null record;

   pragma Import (C, Get_Type, "gtk_text_view_get_type");
end Gtk.Text_View;
