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
--  <c_version>gtk+extra 0.99.5</c_version>

with Gdk;
with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Layout;

package Gtk.Extra.Plot_Canvas is

   type Gtk_Plot_Canvas_Record is
     new Gtk.Extra.Plot_Layout.Gtk_Plot_Layout_Record with private;
   type Gtk_Plot_Canvas is access all Gtk_Plot_Canvas_Record'Class;

   ----------------
   -- Enum types --
   ----------------

   type Plot_Canvas_Action is (Action_Inactive,
                               Action_Selection,
                               Action_Drag,
                               Action_Resize);
   --  The action being performed on the canvas.

   type Plot_Canvas_Flag is (Frozen,
                             Can_Move,
                             Can_X_Resize,
                             Can_Y_Resize);
   --  Flags used by the canvas

   type Plot_Canvas_Type is (None,
                             Plot,
                             Axis,
                             Legends,
                             Title,
                             Text,
                             Data);
   --  The type of data that can be put in a canvas.
   --  A 'plot' is specifically a Gtk.Extra.Plot.Gtk_Plot.

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

   ------------------------------------------
   -- Creating and manipulating the canvas --
   ------------------------------------------

   procedure Gtk_New (Widget : out Gtk_Plot_Canvas;
                      Width  : in Gint;
                      Height : in Gint);
   --  Create a new Gtk_Plot_Canvas, with a specific screen size.
   --  Since the widget can have an unlimited internal size, it does not try
   --  to set its size to accommodate all of its children.

   procedure Initialize (Widget : access Gtk_Plot_Canvas_Record'Class;
                         Width  : in Gint;
                         Height : in Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Canvas.

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

   --  procedure Cancel_Action (Plot_Canvas : access Gtk_Plot_Canvas_Record);
   --  Cancel the current action.
   --  This can be called in the user callbacks to ignore temporarily some of
   --  the signals below.
   --  !! This function is defined but has no body in gtk+extra !!

   function Get_Active_Plot (Canvas : access Gtk_Plot_Canvas_Record)
                            return      Gtk.Extra.Plot.Gtk_Plot;
   --  Return the active plot.
   --  In the callbacks for the signals below, this is the widget that got
   --  the signal.

   function Get_Active_Dataset (Canvas : access Gtk_Plot_Canvas_Record)
                               return      Gtk.Extra.Plot.Gtk_Plot_Data;
   --  Return the active dataset (which of course belongs to the active plot).
   --  This is the dataset that was last clicked on.

   procedure Get_Active_Point (Canvas : access Gtk_Plot_Canvas_Record;
                               X      : out Gdouble;
                               Y      : out Gdouble);
   --  Return the relative coordinates of the active point in the
   --  active dataset.
   --  This is the one that was last clicked on.

   procedure Set_Size (Canvas  : access Gtk_Plot_Canvas_Record;
                       Width   : in Gint;
                       Height  : in Gint);
   --  Modify the size allocated for the canvas, and the size of the pixmap
   --  the plots are displayed on.

   procedure Unselect (Canvas : access Gtk_Plot_Canvas_Record);
   --  Unselect the currently selected item.

   ------------------
   -- Canvas items --
   ------------------
   --  There are several different types of items that can be put on the
   --  canvas, and then manipulated interactively by the user.

   type Gtk_Plot_Canvas_Item is new Gdk.C_Proxy;

   function Get_Item_Type (Item : Gtk_Plot_Canvas_Item)
                          return Plot_Canvas_Type;
   --  Return the type of the item.

   function Get_Active_Item (Canvas  : access Gtk_Plot_Canvas_Record)
                            return Gtk_Plot_Canvas_Item;
   --  Return the currently selected item.

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
   --                      Item   : Gtk_Plot_Canvas_Item)
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
   --                      Item   : Gtk_Plot_Canvas_Item;
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
   --                      Item       : Gtk_Plot_Canvas_Item;
   --                      New_Width  : Gdouble;
   --                      New_Height : Gdouble)
   --                     return Boolean;
   --
   --    An item is being resized.
   --    Its size has not changed yet, but if the handler returns True
   --    it will become (New_Width, New_Height). If the handler returns False,
   --    nothing happens.
   --
   --  - "select_region"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                       X_Min  : Gdouble;
   --                       X_Max  : Gdouble;
   --                       Y_Min  : Gdouble;
   --                       Y_Max  : Gdouble);
   --
   --    A region of the canvas was selected by the user.
   --  </signals>

private
   type Gtk_Plot_Canvas_Record is
     new Gtk.Extra.Plot_Layout.Gtk_Plot_Layout_Record with null record;
   pragma Import (C, Get_Type, "gtk_plot_canvas_get_type");
   pragma Import (C, Get_Item_Type, "ada_gtk_plot_canvas_get_item_type");
   for Plot_Canvas_Flag use (Frozen        => 0,
                             Can_Move      => 1,
                             Can_X_Resize  => 2,
                             Can_Y_Resize  => 4);
end Gtk.Extra.Plot_Canvas;
