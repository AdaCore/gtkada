-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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
--  A Gtk_Plot_Canvas is a special kind of drawing area used with Gtk_Plot
--  widgets.
--  It provides drag-and-drop capabilities for the texts, legends, points...
--  available in a Gtk_Plot.
--  Note that this widget is specifically designed for Gtk_Plot widgets, and
--  won't provide any other capability for other kinds of widgets.
--
--  Like any child of Gtk_Layout, this widget can have an almost unlimited
--  size for its children, and provides scrolling.
--  </description>
--  <c_version>gtk+extra 0.99.14</c_version>

with Gdk;
with Gdk.Pixmap;
with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data;
with Gtk.Fixed;
with Gdk.Color;
with Gtk.Enums;

package Gtk.Extra.Plot_Canvas is

   type Gtk_Plot_Canvas_Record is new Gtk.Fixed.Gtk_Fixed_Record with private;
   type Gtk_Plot_Canvas is access all Gtk_Plot_Canvas_Record'Class;

   ----------------
   -- Enum types --
   ----------------

   type Plot_Canvas_Action is (Action_Inactive,
                               Action_Selection,
                               Action_Drag,
                               Action_Resize);
   --  The action being performed on the canvas.

   type Plot_Canvas_Flag is new Gint;
   Frozen       : constant Plot_Canvas_Flag;
   Can_Move     : constant Plot_Canvas_Flag;
   Can_X_Resize : constant Plot_Canvas_Flag;
   Can_Y_Resize : constant Plot_Canvas_Flag;

   type Plot_Canvas_Type is (None,
                             Plot,
                             Axis,
                             Legends,
                             Title,
                             Text,
                             Data,
                             Line,
                             Rectangle,
                             Ellipse,
                             Custom);
   --  The type of data that can be put in a canvas.
   --  Plot is only for a Gtk.Extra.Plot.Gtk_Plot widget.

   type Plot_Canvas_Pos is (Canvas_Out,
                            Canvas_In,
                            Canvas_Left,
                            Canvas_Right,
                            Canvas_Top,
                            Canvas_Bottom,
                            Canvas_Top_Left,
                            Canvas_Top_Right,
                            Canvas_Bottom_Left,
                            Canvas_Bottom_Right);
   --  The position of the items in the canvas.

   type Plot_Canvas_Arrow is new Gint;
   Arrow_None   : constant Plot_Canvas_Arrow;
   Arrow_Origin : constant Plot_Canvas_Arrow;
   Arrow_End    : constant Plot_Canvas_Arrow;

   ------------------------------------------
   -- Creating and manipulating the canvas --
   ------------------------------------------

   procedure Gtk_New
     (Widget        : out Gtk_Plot_Canvas;
      Width         : in Gint;
      Height        : in Gint;
      Magnification : in Gdouble := 1.0);
   --  Create a new Gtk_Plot_Canvas, with a specific screen size.
   --  Since the widget can have an unlimited internal size, it does not try
   --  to set its size to accommodate all of its children.

   procedure Initialize
     (Widget        : access Gtk_Plot_Canvas_Record'Class;
      Width         : in Gint;
      Height        : in Gint;
      Magnification : in Gdouble := 1.0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Canvas.

   function Child_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Canvas_Child.

   procedure Refresh (Canvas : access Gtk_Plot_Canvas_Record);
   --  Force a refresh of the canvas on the screen. The screen is updated from
   --  the contents of the double-buffer.

   procedure Paint (Canvas : access Gtk_Plot_Canvas_Record);
   --  Redraw each of the items included in the canvas. The painting is done
   --  in the double-buffer, and must be drawn on the screen with Refresh.

   function Get_Pixmap (Canvas : access Gtk_Plot_Canvas_Record)
                       return Gdk.Pixmap.Gdk_Pixmap;
   --  Return the pixmap associated with the Canvas.
   --  If you add your own items on the canvas (see Child_New below), you
   --  can draw them on this pixmap to make them visible on the canvas. You
   --  need to call Refresh to send this pixmap to the screen.

   procedure Grid_Set_Visible (Canvas  : access Gtk_Plot_Canvas_Record;
                               Visible : in Boolean);
   --  Indicate whether the grid should be visible or not.

   procedure Grid_Set_Step (Canvas : access Gtk_Plot_Canvas_Record;
                            Step   : in Gint);
   --  Set the space between two lines of the grid.

   procedure Grid_Set_Attributes
     (Canvas : access Gtk_Plot_Canvas_Record;
      Style  : in Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width  : in Gint;
      Color  : in Gdk.Color.Gdk_Color);
   --  Set the attributes of the grid.

   procedure Add_Plot
      (Plot_Canvas : access Gtk_Plot_Canvas_Record;
       Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       X           : in Gdouble;
       Y           : in Gdouble);
   --  Add a new plot to the list handled by the canvas.
   --  The canvas will then provide drag-and-drop functionalities for that
   --  plot.
   --  The plot is displayed at the relative screen coordinates (X, Y).
   --  Plot becomes the new active plot, and its associated pixmap becomes the
   --  Plot_Canvas itself.

   procedure Set_Active_Plot
      (Plot_Canvas : access Gtk_Plot_Canvas_Record;
       Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class);
   --  Modify the active plot in the canvas.
   --  The active plot is generally the one that received the last click
   --  event (drag-and-drop, etc.). This should be set before emitting any
   --  of the signals in this class.

   procedure Cancel_Action (Plot_Canvas : access Gtk_Plot_Canvas_Record);
   --  Cancel the current action.
   --  This can be called in the user callbacks to ignore temporarily some of
   --  the signals below.

   function Get_Active_Plot (Canvas : access Gtk_Plot_Canvas_Record)
                            return      Gtk.Extra.Plot.Gtk_Plot;
   --  Return the active plot.
   --  In the callbacks for the signals below, this is the widget that got
   --  the signal.

   function Get_Active_Data (Canvas : access Gtk_Plot_Canvas_Record)
      return Gtk.Extra.Plot_Data.Gtk_Plot_Data;
   --  Return the active dataset (which of course belongs to the active plot).
   --  This is the dataset that was last clicked on.

   procedure Get_Active_Point (Canvas : access Gtk_Plot_Canvas_Record;
                               X      : out Gdouble;
                               Y      : out Gdouble;
                               Index  : out Gint);
   --  Return the relative coordinates of the active point in the
   --  active dataset. Index will contain the index of the active point in the
   --  list of the canvas'children, or -1 if there is no active point.
   --  This is the index in the coordinates arrays of Get_Active_Data.
   --  This is the one that was last clicked on.

   procedure Set_Size (Canvas  : access Gtk_Plot_Canvas_Record;
                       Width   : in Gint;
                       Height  : in Gint);
   --  Modify the size allocated for the canvas, and the size of the pixmap
   --  the plots are displayed on.

   procedure Unselect (Canvas : access Gtk_Plot_Canvas_Record);
   --  Unselect the currently selected item.

   procedure Set_Magnification
     (Canvas        : access Gtk_Plot_Canvas_Record;
      Magnification : Gdouble := 1.0);
   --  Changes the magnification for the canvas.
   --  1.0 is the default value. Higher values will zoom in, while lower values
   --  will zoom out.

   procedure Set_Background
     (Canvas     : access Gtk_Plot_Canvas_Record;
      Background : Gdk.Color.Gdk_Color);
   --  Set the background color for the canvas.

   procedure Get_Pixel
     (Canvas : access Gtk_Plot_Canvas_Record;
      Px     : in Gdouble;
      Py     : in Gdouble;
      X      : out Gint;
      Y      : out Gint);
   --  Convert from relative coordinates to absolute ones.

   procedure Get_Position
     (Canvas : access Gtk_Plot_Canvas_Record;
      X      : in Gint;
      Y      : in Gint;
      Px     : out Gdouble;
      Py     : out Gdouble);
   --  Convert from absolute coordinates to relative ones.

   ------------------
   -- Canvas items --
   ------------------
   --  There are several different types of items that can be put on the
   --  canvas, and then manipulated interactively by the user.

   type Gtk_Plot_Canvas_Child is new Gdk.C_Proxy;

   type Child_Draw_Func is access
     procedure (Canvas : System.Address;
                Child  : Gtk_Plot_Canvas_Child);
   --  Generic format of functions used to draw a child of the canvas.
   --  Canvas is a System.Address since these functions are called directly
   --  from C and GtkAda can't insert its hooks. However, you can use the
   --  Convert function below to convert to a Gtk_Plot_Canvas.

   function Convert (Canvas : System.Address) return Gtk_Plot_Canvas;
   --  Convert from a System.Address returned by C to a real Gtk_Plot_Canvas
   --  structure.

   procedure Set_Draw_Func (Child : Gtk_Plot_Canvas_Child;
                            Draw  : Child_Draw_Func);
   --  Set the function used to draw the item.
   --  This should be used only for items whose type is Custom, since other
   --  items have their own drawing functions.

   function Get_Active_Item (Canvas  : access Gtk_Plot_Canvas_Record)
                            return Gtk_Plot_Canvas_Child;
   --  Return the currently selected item.

   function Put_Text
     (Canvas        : access Gtk_Plot_Canvas_Record;
      X             : in Gdouble;
      Y             : in Gdouble;
      Ps_Font       : in String;
      Height        : in Gint;
      Angle         : in Gint;
      Fg            : in Gdk.Color.Gdk_Color;
      Bg            : in Gdk.Color.Gdk_Color;
      Transparent   : in Boolean;
      Justification : in Gtk.Enums.Gtk_Justification;
      Text          : in String)
     return Gtk_Plot_Canvas_Child;
   --  Put an arbitrary text in the layout.
   --  Ps_Font should be the name of a postscript font.
   --  (X, Y) are the relative coordinates to which the text should be drawn.
   --  The only legal values for Angle are 0, 90, 180 and 270 degrees.
   --
   --  Text can contain some special characters, that change is renderering.
   --  They all begin with a '\' (backslash) character, followed by one of:
   --  - '0' .. '9' : Change the font (take the nth font in the family
   --  - 'g' : Select the "Symbol" font
   --  - 'B' : Activate bold characters.
   --  - 'i' : Activate italic characters.
   --  - 'S' or '^' : Activate superscripts.
   --  - 's' or '_' : Activate subscripts.
   --  - '+' : Increment the fontsize by 3 pixels.
   --  - '-' : Decrement the fontsize by 3 pixels.
   --  - 'N' : Restore the default characteristics of the font.
   --  - 'b' : Move back one character.

   function Put_Line
     (Canvas     : access Gtk_Plot_Canvas_Record;
      X1         : Gdouble;
      Y1         : Gdouble;
      X2         : Gdouble;
      Y2         : Gdouble;
      Style      : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width      : Gfloat;
      Color      : Gdk.Color.Gdk_Color;
      Arrow_Mask : Plot_Canvas_Arrow)
     return Gtk_Plot_Canvas_Child;
   --  Draw a line in the background of the canvas.

   function Put_Rectangle
     (Canvas     : access Gtk_Plot_Canvas_Record;
      X1         : Gdouble;
      Y1         : Gdouble;
      X2         : Gdouble;
      Y2         : Gdouble;
      Style      : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width      : Gfloat;
      Fg         : Gdk.Color.Gdk_Color;
      Bg         : Gdk.Color.Gdk_Color;
      Border     : Gtk.Extra.Plot.Plot_Border_Style;
      Fill       : Boolean := False)
     return Gtk_Plot_Canvas_Child;
   --  Draw a rectangle in the canvas.

   function Put_Ellipse
     (Canvas     : access Gtk_Plot_Canvas_Record;
      X1         : Gdouble;
      Y1         : Gdouble;
      X2         : Gdouble;
      Y2         : Gdouble;
      Style      : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width      : Gfloat;
      Fg         : Gdk.Color.Gdk_Color;
      Bg         : Gdk.Color.Gdk_Color;
      Fill       : Boolean := False)
     return Gtk_Plot_Canvas_Child;
   --  Draw an ellipse in the canvas.

   procedure Line_Set_Attributes
     (Child : Gtk_Plot_Canvas_Child;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color;
      Mask  : Plot_Canvas_Arrow);
   --  Change the attributes of a line.

   procedure Rectangle_Set_Attributes
     (Child  : Gtk_Plot_Canvas_Child;
      Style  : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width  : Gfloat;
      Fg     : Gdk.Color.Gdk_Color;
      Bg     : Gdk.Color.Gdk_Color;
      Border : Gtk.Extra.Plot.Plot_Border_Style;
      Fill   : Boolean := False);
   --  Change the attributes of a rectangle.

   procedure Ellipse_Set_Attributes
     (Child  : Gtk_Plot_Canvas_Child;
      Style  : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width  : Gfloat;
      Fg     : Gdk.Color.Gdk_Color;
      Bg     : Gdk.Color.Gdk_Color;
      Fill   : Boolean := False);
   --  Change the attributes for an ellipse.

   ---------------------
   -- Custom children --
   ---------------------
   --  You can insert your own items in a canvas.
   --  While the canvas will take care of moving the item, it is your
   --  responsability to provide a visual rendering for it.

   function Child_New (Child_Type : Plot_Canvas_Type := Custom)
                      return Gtk_Plot_Canvas_Child;
   --  Create a new child.

   procedure Put_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble;
      Y2     : Gdouble);
   --  Insert a new item in the canvas. It will occupy the area defined by
   --   the four coordinates.

   procedure Child_Move
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child;
      X1     : Gdouble;
      Y1     : Gdouble);
   --  Move an item, but does not change its size.

   procedure Child_Move_Resize
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble;
      Y2     : Gdouble);
   --  Move an resize an item in the canvas.

   function Get_Item_Type (Item : Gtk_Plot_Canvas_Child)
                          return Plot_Canvas_Type;
   --  Return the type of the item.

   function Get_Allocation_Width (Child : Gtk_Plot_Canvas_Child) return Guint;
   --  Return the current width of the child.

   function Get_Allocation_Height (Child : Gtk_Plot_Canvas_Child) return Guint;
   --  Return the current height of the child.

   function Get_Allocation_X (Child : Gtk_Plot_Canvas_Child) return Gint;
   --  Return the current position of the child, relative to its canvas.

   function Get_Allocation_Y (Child : Gtk_Plot_Canvas_Child) return Gint;
   --  Return the current position of the child, relative to its canvas.

   function Get_Flags (Child : Gtk_Plot_Canvas_Child) return Plot_Canvas_Flag;
   --  Return the list of actions currently possible on the child.

   procedure Set_Flags (Child : Gtk_Plot_Canvas_Child;
                        Flags : Plot_Canvas_Flag);
   --  Modify the list of actions possible for a child.

   -----------
   -- Flags --
   -----------
   --  Some flags are defined for this widget. You can not access them through
   --  the usual interface in Gtk.Object.Flag_Is_Set since this widget is not
   --  part of the standard gtk+ packages. Instead, use the functions below.
   --
   --  - "can_select"
   --    True if it is possible to select a region of the canvas
   --
   --  - "can_select_item"
   --    True if it is possible to select any of the item on the canvas.
   --
   --  - "can_select_point"
   --    True if the individual points in the plots can be selected and
   --    interactively moved by the user.
   --
   --  - "can_dnd"
   --    True if it is possible to drag an item on the canvas.
   --
   --  - "can_dnd_point"
   --    True if the points of the plots can be moved interactively.
   --

   Can_Select       : constant := 2 ** 0;
   Can_Select_Item  : constant := 2 ** 1;
   Can_Select_Point : constant := 2 ** 2;
   Can_Dnd          : constant := 2 ** 3;
   Can_Dnd_Point    : constant := 2 ** 4;

   Dnd_Flags        : constant := Can_Select_Item
                                  + Can_Select_Point
                                  + Can_Dnd
                                  + Can_Dnd_Point;

   function Plot_Canvas_Flag_Is_Set
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Flag        : in Guint16)
     return Boolean;
   --  Test whether one of the flags for a Gtk_Plot_Canvas widget or its
   --  children is set.

   procedure Plot_Canvas_Set_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : in Guint16);
   --  Set the flags for a Gtk_Plot_Canvas widget or its children.
   --  Note that the flags currently set are not touched by this function.
   --  This can only be used for the flags defined in the
   --  Gtk.Extra.Gtk_Plot_Canvas package.

   procedure Plot_Canvas_Unset_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : in Guint16);
   --  Unset the flags for a Gtk_Plot_Canvas.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "select_item"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event;
   --                      Item   : Gtk_Plot_Canvas_Child)
   --                     return Boolean;
   --
   --    Called when an item was selected.
   --    An item can be anything, from a text to a plot
   --    When this signal is called, the item was simply selected, but not
   --    dragged.
   --    The handler should return False if the item can not be selected.
   --
   --  - "move_item"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Item   : Gtk_Plot_Canvas_Child;
   --                      New_X  : Gdouble;
   --                      New_Y  : Gdouble)
   --                     return Boolean;
   --
   --    An item was moved on the canvas.
   --    Its coordinates have not changed yet, but if the handler returns True
   --    they will become (New_X, New_Y). If the handler returns False,
   --    nothing happens.
   --
   --  - "resize_item"
   --    function Handler (Canvas     : access Gtk_Plot_Canvas_Record'Class;
   --                      Item       : Gtk_Plot_Canvas_Child;
   --                      New_Width  : Gdouble;
   --                      New_Height : Gdouble)
   --                     return Boolean;
   --
   --    An item is being resized.
   --    Its size has not changed yet, but if the handler returns True
   --    it will become (New_Width, New_Height). If the handler returns False,
   --    nothing happens.
   --
   --  - "delete_item"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                       Item   : Gtk_Plot_Canvas_Child);
   --
   --    Called when an item is being removed from the canvas
   --
   --  - "select_region"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                       X_Min  : Gdouble;
   --                       Y_Min  : Gdouble;
   --                       X_Max  : Gdouble;
   --                       Y_Max  : Gdouble);
   --
   --  - "changed"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class);
   --
   --    Called when the contents of the canvas has changed (an item was
   --    moved interactively by the user).
   --
   --    A region of the canvas was selected by the user.
   --  </signals>

private
   type Gtk_Plot_Canvas_Record is new Gtk.Fixed.Gtk_Fixed_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot_canvas_get_type");
   pragma Import (C, Child_Get_Type, "gtk_plot_canvas_child_get_type");
   pragma Import (C, Get_Item_Type, "ada_gtk_plot_canvas_get_item_type");
   pragma Import (C, Child_New, "gtk_plot_canvas_child_new");
   pragma Import (C, Set_Draw_Func, "ada_gtk_plot_canvas_set_draw_func");
   pragma Import (C, Get_Allocation_Width,
                    "ada_gtk_plot_canvas_get_alloc_width");
   pragma Import (C, Get_Allocation_Height,
                    "ada_gtk_plot_canvas_get_alloc_height");
   pragma Import (C, Get_Allocation_X, "ada_gtk_plot_canvas_get_alloc_x");
   pragma Import (C, Get_Allocation_Y, "ada_gtk_plot_canvas_get_alloc_y");
   pragma Import (C, Get_Flags, "ada_gtk_plot_canvas_get_child_flags");
   pragma Import (C, Set_Flags, "ada_gtk_plot_canvas_set_child_flags");

   Frozen       : constant Plot_Canvas_Flag := 0;
   Can_Move     : constant Plot_Canvas_Flag := 1;
   Can_X_Resize : constant Plot_Canvas_Flag := 2;
   Can_Y_Resize : constant Plot_Canvas_Flag := 4;

   Arrow_None   : constant Plot_Canvas_Arrow := 0;
   Arrow_Origin : constant Plot_Canvas_Arrow := 1;
   Arrow_End    : constant Plot_Canvas_Arrow := 2;
end Gtk.Extra.Plot_Canvas;
