-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--
--  This widget displays a multi-column list. Each line is made of
--  a number of column, each being able to display any kind of widget.
--
--  This is one of the most powerful widgets in GtkAda, that can be used to
--  display an kind of information. Look also into using Gtk_Ctree, which is
--  a similar widget.
--
--  You can add scrolling in a Gtk_Clist by adding it in a Gtk_Scrolled_Window.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gdk.Window;
with Glib.Glist;
with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Style;
with Gtk.Object;
with Gtk.Widget;
with Gtkada.Types;

package Gtk.Clist is

   type Gtk_Clist_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Clist is access all Gtk_Clist_Record'Class;

   type Gtk_Clist_Row is new Object_Type;

   ------------------------------------------------
   -- Creating a list and setting the attributes --
   ------------------------------------------------

   procedure Gtk_New (Widget : out Gtk_Clist; Columns : in Gint);
   --  This procedure creates a list with COLUMNS columns.
   --  Each line will have this exact number of column
   --  The number of columns can not be changed once the widget has been
   --  created.

   procedure Initialize
     (Widget : access Gtk_Clist_Record'Class; Columns : in Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New
     (Widget  : out Gtk_Clist;
      Columns : in  Gint;
      Titles  : in  Gtkada.Types.Chars_Ptr_Array);
   --  This procedure creates a new list with COLUMNS columns. The title
   --  of the columns is specified in TITLES.
   --  The results are undefined (and can raise an error) if TITLES does
   --  not have at least COLUMNS items.

   procedure Initialize
     (Widget  : access Gtk_Clist_Record'Class;
      Columns : in Gint;
      Titles  : in Gtkada.Types.Chars_Ptr_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Returns the internal value associated with a Gtk_Clist internally.

   procedure Set_Hadjustment
      (Clist      : access Gtk_Clist_Record;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Sets the horizontal adjustment used for the clist.
   --  Note that such an adjustment is automatically created when the clist
   --  is added to a Gtk_Scrolled_Window. You should rather use
   --  Gtk.Scrolled_Window.Set_Hadjustment if you want to modify the
   --  adjustment.
   --  If there was already such an adjustment, it is unref-ed, and might
   --  be deleted.

   procedure Set_Vadjustment
     (Clist      : access Gtk_Clist_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Sets the vertical adjustment used for the clist.
   --  Note that such an adjustment is automatically created when the clist
   --  is added to a Gtk_Scrolled_Window. You should rather use
   --  Gtk.Scrolled_Window.Set_Hadjustment if you want to modify the
   --  adjustment.
   --  If there was already such an adjustment, it is unref-ed, and might
   --  be deleted.

   function Get_Hadjustment (Clist  : access  Gtk_Clist_Record)
                             return           Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the horizontal adjustment used for the clist.
   --  This indicates what position the clist is presently displaying, and
   --  by changing its value, the clist is automatically scrolled horizontally.
   --  This is done automatically when the clist's parent is a
   --  Gtk_Scrolled_Window.

   function Get_Vadjustment (Clist  : access Gtk_Clist_Record)
                             return          Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the vertical adjustment used for the clist.
   --  This indicates what position the clist is presently displaying, and
   --  by changing its value, the clist is automatically scrolled vertically.
   --  This is done automatically when the clist's parent is a
   --  Gtk_Scrolled_Window.

   procedure Set_Selection_Mode (Clist : access Gtk_Clist_Record;
                                 Mode  : in Gtk.Enums.Gtk_Selection_Mode);
   --  Modifies the selection mode for the clist.
   --  This indicates whether one or more lines can be selected at the
   --  same time in the clist, and how this selection can done by the
   --  user (does he have to click explictly on an item, or can he
   --  browse through the clist and select the last item he was on, etc.)
   --
   --  Note that changing the selection mode to Selection_Single or
   --  Selection_Browse will deselect all the items in the clist.

   --------------------
   -- Visual aspects --
   --------------------

   procedure Freeze (Clist : access Gtk_Clist_Record);
   --  Freezes all visual updates on the list, while you make big changes.
   --  This is more efficient than working on an unfrozen list.

   procedure Thaw (Clist : access Gtk_Clist_Record);
   --  Thaw the list, ie reactivates all the visual updates. This also forces
   --  an immediate refresh of the list.
   --  Note that each Freeze must be followed by a Thaw. The visual updates
   --  are not reactivated until the last Thaw has been emitted, but there is
   --  an immediate refresh every time anyway.

   procedure Set_Shadow_Type (Clist    : access Gtk_Clist_Record;
                              The_Type : in Gtk.Enums.Gtk_Shadow_Type);
   --  Sets the border style of the clist.

   ----------------------------
   -- Modifying the contents --
   ----------------------------

   function Append (Clist : access Gtk_Clist_Record;
                    Text  : in     Gtkada.Types.Chars_Ptr_Array)
                   return      Gint;
   --  Appends a new row to the clist, and returns the index of the row that
   --  was created. The row is added at the end of the CLISt.
   --  The behavior is undefined if TEXT does not have at least as many items
   --  as there are columns in the CLIST.

   function Prepend (Clist : access Gtk_Clist_Record;
                     Text  : in     Gtkada.Types.Chars_Ptr_Array)
                     return         Gint;
   --  Adds a new row at the beginning of the clist, and returns the index of
   --  the row that was created.
   --  The behavior is undefined if TEXT does not have at least as many items
   --  as there are columns in the CLIST.

   -------------
   -- Columns --
   -------------

   procedure Column_Titles_Hide (Clist : access Gtk_Clist_Record);
   --  Hides the column titles for the list. This is the default behavior if
   --  no column titles were given when the list was created.

   procedure Column_Titles_Show (Clist : access Gtk_Clist_Record);
   --  Shows the column titles for the list. This is the default behavior if
   --  some column titles were given when the list was created.

   procedure Column_Title_Active (Clist : access Gtk_Clist_Record;
                                  Column : in Gint);
   --  Sets the column title to be an activate title, ie answer all button
   --  presses, highlights when the mouse is over it, ...

   procedure Column_Title_Passive (Clist : access Gtk_Clist_Record;
                                   Column : in Gint);
   --  Sets the column title to be passive, ie acts just as a title, and does
   --  not react to mouse events.

   procedure Column_Titles_Active (Clist : access Gtk_Clist_Record);
   --  Sets all column titles to be active.

   procedure Column_Titles_Passive (Clist : access Gtk_Clist_Record);
   --  Sets all column titles to be passive.

   procedure Set_Column_Title (Clist  : access Gtk_Clist_Record;
                               Column : in Gint;
                               Title  : in String);
   --  Sets the text for the button of the column's title.
   --  See Set_Column_Widget if you want to put a pixmap inside the button.

   function Get_Column_Title (Clist  : access Gtk_Clist_Record;
                              Column : in Gint)
                             return String;
   --  Returns the text used for the title's column. This is a copy of the
   --  title, so you can't modify it to automatically change the column's
   --  title.

   procedure Set_Column_Widget
     (Clist  : access Gtk_Clist_Record;
      Column : in     Gint;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Modifies the widget used in the Gtk_Button that is the column's title.
   --  By default, this button contains a simple Gtk_Label, which is replaced
   --  by WIDGET. This is the function to use if you want to put a pixmap
   --  (or a Gtk_Box that contains both a pixmap and some text) in a column's
   --  title.

   function Get_Column_Widget (Clist  : access Gtk_Clist_Record;
                               Column : in Gint)
                              return Gtk.Widget.Gtk_Widget;
   --  Returns the child of the button that makes the column's title.
   --  Unless you changed it with Set_Column_Widget, this will return a
   --  Gtk_Label. Note also that if this widget was not created in Ada, but
   --  transparently by gtk+, you have to 'with' Gtk.Type_Conversion so that
   --  the correct type of the widget is created (See the user's guide for
   --  more information on type conversion).

   procedure Set_Column_Justification
     (Clist         : access Gtk_Clist_Record;
      Column        : in Gint;
      Justification : in Gtk.Enums.Gtk_Justification);
   --  Changes the way the text in the whole column is justified.
   --  This function has no effect on the title if you used Set_Column_Widget
   --  before.

   procedure Set_Column_Visibility
     (Clist   : access Gtk_Clist_Record;
      Column  : in Gint;
      Visible : in Boolean);
   --  Modifies the visibility of a column.

   procedure Set_Column_Resizeable
     (Clist    : access Gtk_Clist_Record;
      Column   : in Gint;
      Resizeable : in Boolean);
   --  Sets whether the column can be dynamically resized by the user with the
   --  mouse. If RESIZEABLE is true, then the column can be resized by clicking
   --  and draging the lines that separates the column from the next one.

   procedure Set_Column_Auto_Resize
     (Clist       : access Gtk_Clist_Record;
      Column      : in Gint;
      Auto_Resize : in Boolean);
   --  Sets whether the column should automatically be resized to the optimal
   --  size (based on its contents). Note that this operation could slow things
   --  down a lot if you have a lot of items in your list.

   function Columns_Autosize (Clist  : access Gtk_Clist_Record) return Gint;
   --  Sets all the columns' width to their optimal size, and returns the total
   --  width of the clist after this operation.

   function Optimal_Column_Width (Clist : access Gtk_Clist_Record;
                                  Column : Gint)
                                 return Gint;
   --  Returns the optimal width for COLUMN, based on its contents. This is
   --  the maximal cell width in the column.

   procedure Set_Column_Width
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Width  : in Gint);
   --  Sets the column width in pixels. By default, the column's width is
   --  chosen from the column's title.

   procedure Set_Column_Min_Width
     (Clist     : access Gtk_Clist_Record;
      Column    : Gint;
      Min_Width : Gint);
   --  Sets the minimal width for the column, in pixels.
   --  if MIN_WIDTH is negative, there is no limit on the minimal width for
   --  the column.

   procedure Set_Column_Max_Width
     (Clist     : access Gtk_Clist_Record;
      Column    : Gint;
      Max_Width : Gint);
   --  Sets the maximal width for the column, in pixels.
   --  if MAX_WIDTH is negative, there is no limit on the maximal width for
   --  the column.

   ----------
   -- Rows --
   ----------

   procedure Set_Row_Height
     (Clist  : access Gtk_Clist_Record;
      Height : Gint);
   --  Sets the height of the rows, in pixels.
   --  if HEIGHT is 0, the chosen height will be the current's font height.

   function Row_Is_Visible (Clist : access Gtk_Clist_Record;
                            Row   : in Gint)
                           return Gtk.Enums.Gtk_Visibility;
   --  Returns the visibility status of the row.

   procedure Set_Foreground
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color);
   --  Sets the foreground color for the row.
   --  The color must already be allocated.
   --  If no such row exists in the list, nothing is done.

   procedure Set_Background
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color);
   --  Sets the background color for the row.
   --  The color must already be allocated.
   --  If no such row exists in the list, nothing is done.

   procedure Set_Row_Style
     (Clist : access Gtk_Clist_Record; Row : Gint;
      Style : in Gtk.Style.Gtk_Style'Class);
   --  Sets the default style for the cells in the row. This can be
   --  overriden for each cell with Set_Cell_Style.

   function Get_Row_Style (Clist  : access Gtk_Clist_Record;
                           Row    : in     Gint)
                           return          Gtk.Style.Gtk_Style'Class;
   --  Gets the default style used for the row.

   procedure Set_Selectable (Clist      : access Gtk_Clist_Record;
                             Row        : Gint;
                             Selectable : Boolean);
   --  Indicates whether the row can be selected or not. The default value
   --  is True.

   function Get_Selectable (Clist : access Gtk_Clist_Record;
                            Row   : Gint)
                           return Boolean;
   --  Returns the selectable status of the row.

   -----------
   -- Cells --
   -----------

   function Get_Cell_Type (Clist  : access Gtk_Clist_Record;
                           Row    : in Gint;
                           Column : in Gint)
                          return Gtk.Enums.Gtk_Cell_Type;
   --  Returns the type of the cell at ROW/COLUMN.
   --  This indicates which of the functions Get_Text. Get_Pixmap, etc.
   --  below you can use.

   procedure Set_Text
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Text   : in String);
   --  Sets the cell's text, replacing its current contents.

   function Get_Text
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint) return String;
   --  If there was a problem, a null-length string is returned.
   --  The problem might appear in case the row or the column are
   --  invalid, or if the cell does not contain any text.

   procedure Set_Pixmap
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   --  Sets the cell's pixmap, replacing its current contents.

   procedure Get_Pixmap
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap'Class;
      Is_Valid : out Boolean);
   --  Gets the pixmap contained in a cell.
   --  The result is meaningful only if Is_Valid is True. If the Cell did not
   --  contain a pixmap, Is_Valid is set to False

   procedure Set_Pixtext
     (Clist   : access Gtk_Clist_Record;
      Row     : in Gint;
      Column  : in Gint;
      Text    : in String;
      Spacing : in Guint8;
      Pixmap  : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask    : in Gdk.Bitmap.Gdk_Bitmap'Class);
   --  Sets both the text and the pixmap for the cell, replacing its current
   --  contents.

   procedure  Get_Pixtext
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Spacing  : in out Guint8;
      Pixmap   : in out Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask     : in out Gdk.Bitmap.Gdk_Bitmap'Class;
      Is_Valid : out Boolean);
   --  The result is not meaningful if Is_Valid is False.
   --  The only way to get the string is to use Get_Text, since a String is
   --  an unconstrained type in Ada and is not really convenient to use as an
   --  out parameter.

   procedure Set_Cell_Style (Clist  : access Gtk_Clist_Record;
                             Row    : in Gint;
                             Column : in Gint;
                             Style  : in Gtk.Style.Gtk_Style'Class);
   --  Sets the style (font, color, ...) use for the cell.
   --  This overrides the row's style.

   function Get_Cell_Style (Clist  : access Gtk_Clist_Record;
                            Row    : in     Gint;
                            Column : in     Gint)
                            return          Gtk.Style.Gtk_Style'Class;
   --  Returns the style of the cell.

   procedure Set_Shift
     (Clist      : access Gtk_Clist_Record;
      Row        : in Gint;
      Column     : in Gint;
      Vertical   : in Gint;
      Horizontal : in Gint);
   --  Sets a horizontal and vertical shift for drawing the content of the
   --  cell. Both shifts can be either positive or negative.
   --  This is particularly useful for indenting items in a columns.

   -------------------------
   -- Reordering the list --
   -------------------------

   procedure Set_Reorderable
     (Clist : access Gtk_Clist_Record; Reorderable : Boolean);
   --  Sets whether the list can be dynamically reordered by the user, using
   --  a simple drag-n-drop protocol.

   procedure Set_Use_Drag_Icons
     (Clist : access Gtk_Clist_Record; Use_Icons : Boolean);
   --  Sets whether drag icons are shown while the user is reordering the
   --  list. The default value is True.

   procedure Set_Button_Actions
     (Clist         : access Gtk_Clist_Record;
      Button        :        Guint;
      Button_Action :        Gtk.Enums.Gtk_Button_Action);
   --  Sets the action for a specific buton on the list.
   --  The default if for the left mouse button to select or drag and item,
   --  the other buttons are ignored.
   --  The Button_Expands action has no effect on a clist.

   procedure Moveto
     (Clist     : access Gtk_Clist_Record;
      Row       : in Gint;
      Column    : in Gint;
      Row_Align : in Gfloat;
      Col_Align : in Gfloat);
   --  Scrolls the list so that ROW/COLUMN is visible.
   --  If ROW is -1, the clist is not scrolled vertically.
   --  If COLUMN is -1, the clist is not scrolled horizontally.
   --  The new location of ROW/COLUMN depends on the value of ROW_ALIGN and
   --  COL_ALIGN (from 0.0x0.0 (top-left) to 1.0x1.0 (bottom-right), all
   --  intermediate values are possible).

   ----------
   -- Misc --
   ----------

   function Convert (C : in Gtk_Clist_Row) return System.Address;
   function Convert (W : System.Address) return Gtk_Clist_Row;
   package Row_List is new Glib.Glist.Generic_List (Gtk_Clist_Row);


   --  Return the index of the row

   procedure Clear (Clist : access Gtk_Clist_Record);


   function Get_Clist_Window (Clist : access Gtk_Clist_Record)
                              return Gdk.Window.Gdk_Window;



   function Get_Focus_Row (Clist : access Gtk_Clist_Record) return Gint;


   function Get_Row_List (Clist : access Gtk_Clist_Record)
                          return         Row_List.Glist;

   function Get_Selection (Widget : access Gtk_Clist_Record)
                           return Gtk.Enums.Gint_List.Glist;

   procedure Get_Selection_Info
     (Clist    : access Gtk_Clist_Record;
      X        : in Gint;
      Y        : in Gint;
      Row      : out Gint;
      Column   : out Gint;
      Is_Valid : out Boolean);
   --  The result is valid only if Is_Valid is true

   function Get_Selection_Mode (Clist : access Gtk_Clist_Record)
                                return Gtk.Enums.Gtk_Selection_Mode;


   procedure Insert (Clist : access Gtk_Clist_Record;
                     Row   : in     Gint;
                     Text  : in     Gtkada.Types.Chars_Ptr_Array);

   procedure Remove (Clist : access Gtk_Clist_Record; Row : in Gint);

   procedure Row_Move (Clist      : access Gtk_Clist_Record;
                       Source_Row : in     Gint;
                       Dest_Row   : in     Gint);

   procedure Select_All (Clist : access Gtk_Clist_Record);

   procedure Select_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint);

   --  gtk_clist_set_auto_sort



   --  gtk_clist_set_compare_func ([...])

   procedure Set_Show_Titles (Clist : access Gtk_Clist_Record; Show : Boolean);
   --  If show is true, call Column_Titles_Show. Do nothing otherwise.
   --  this procedure is primarily used by GATE generated code.

   --  gtk_clist_set_sort_column
   --  gtk_clist_set_sort_type

   --  gtk_clist_sort

   procedure Swap_Rows (Clist : access Gtk_Clist_Record;
                        Row1  : in     Gint;
                        Row2  : in     Gint);

   procedure Undo_Selection (Clist  : access Gtk_Clist_Record);

   procedure Unselect_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint);

   procedure Unselect_All (Clist : access Gtk_Clist_Record);

   ---------------
   -- Row_Data --
   ---------------

   generic
      type Data_Type (<>) is private;
   package Row_Data is
      function Get (Object : access Gtk_Clist_Record'Class;
                    Row    : in     Gint)
                    return Data_Type;

      procedure Set (Object : access Gtk_Clist_Record'Class;
                     Row    : in Gint;
                     Data   : in Data_Type);
      --  maps gtk_clist_set_row_data_full

      --  gint gtk_clist_find_row_from_data ([...])

   end Row_Data;

   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Clist : in out Gtk.Object.Gtk_Object; N : in Node_Ptr);


private
   type Gtk_Clist_Record is new Gtk.Container.Gtk_Container_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_clist_get_type");
end Gtk.Clist;
