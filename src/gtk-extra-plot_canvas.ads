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
--  <c_version>gtk+extra 0.99</c_version>

with Gtk.Enums;
with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Layout;

package Gtk.Extra.Plot_Canvas is

   type Gtk_Plot_Canvas_Record is
     new Gtk.Extra.Plot_Layout.Gtk_Plot_Layout_Record with private;
   type Gtk_Plot_Canvas is access all Gtk_Plot_Canvas_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Plot_Canvas;
                      Width  : in Gint;
                      Height : in Gint);
   --  Create a new Gtk_Plot_Canvas, with a specific screen size.
   --  Since the widget can have an unlimited internal size, it does not try
   --  to set its size to accomodate all of its children.

   procedure Initialize (Widget : access Gtk_Plot_Canvas_Record;
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
   --  The canvas will then provide drag-and-drop functionnalities for that
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

   function Get_Active_Text (Canvas : access Gtk_Plot_Canvas_Record)
                            return      Gtk.Extra.Plot.Gtk_Plot_Text;
   --  Return the active text.
   --  This is the one that was last clicked on.

   -----------
   -- Flags --
   -----------
   --  Some flags are defined for this widget. You can not access them through
   --  the usual interface in Gtk.Object.Flag_Is_Set since this widget is not
   --  part of the standard gtk+ packages. Instead, use the functions below.
   --
   --  - "can_resize_plot"
   --    True if the plots in the canvas can be resized.
   --
   --  - "can_move_plot"
   --    True if the plots can be moved within the canvas.
   --
   --  - "can_move_legends"
   --    True if the plots'legends can be moved.
   --
   --  - "can_move_titles"
   --    True if the plots'titles can be moved.
   --
   --  - "can_move_text"
   --    True if the plots'text or the canvas's texts can be moved.
   --
   --  - "can_dnd_point"
   --    True if the points of the plots can be moved interactively.
   --
   --  - "allocate_titles"
   --    Should be set if the plot have titles associated with their axis,
   --    since this provides a better position for the titles.

   Can_Resize_Plot  : constant := 2 ** 0;
   Can_Move_Plot    : constant := 2 ** 1;
   Can_Move_Legends : constant := 2 ** 2;
   Can_Move_Titles  : constant := 2 ** 3;
   Can_Move_Text    : constant := 2 ** 4;
   Can_Dnd_Point    : constant := 2 ** 5;
   Allocate_Titles  : constant := 2 ** 6;

   Dnd_Flags        : constant := Can_Resize_Plot
                                  + Can_Move_Plot
                                  + Can_Move_Legends
                                  + Can_Move_Titles
                                  + Can_Move_Text
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
   --  - "click_on_plot"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event)
   --                     return Boolean;
   --
   --    Called when a plot (any area of it) was clicked on.
   --    See Get_Active_Plot to get the exact plot that was clicked on.
   --    Should return False if the event should not be propagated.
   --
   --  - "click_on_point"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event)
   --                     return Boolean;
   --
   --    Called when a point was clicked on.
   --    See Get_Active_Plot and Get_Active_Point to find which one was
   --    clicked on.
   --    The handler should return False if the event should not be propagated.
   --
   --  - "click_on_legends"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event)
   --                     return Boolean;
   --
   --    Called when a plot's legend was clicked on.
   --    Should return False if the event should not be propagated.
   --
   --  - "click_on_axis"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event;
   --                      Axis   : Gtk.Extra.Plot.Gtk_Plot_Axis)
   --                     return Boolean;
   --
   --    Called when one of the axis of a plot was clicked on.
   --    Should return False if the event should not be propagated.
   --
   --  - "click_on_title"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event;
   --                      Axis   : Gtk.Extra.Plot.Gtk_Plot_Axis)
   --                     return Boolean;
   --
   --    Called when the title of one of the axis was clicked on.
   --    Should return False if the event should not be propagated.
   --
   --  - "click_on_text"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event)
   --                     return Boolean;
   --
   --    Called when one of the texts of the canvas was clicked on.
   --    Should return False if the event should not be propagated.
   --
   --  - "select_region"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Xmin, Xmax : Gdouble;
   --                      Ymin, Ymax : Gdouble)
   --                     return Boolean;
   --
   --    Called when the user selected a region of a plot.
   --    Should return False if the event should not be propagated.
   --
   --  </signals>

private
   type Gtk_Plot_Canvas_Record is
     new Gtk.Extra.Plot_Layout.Gtk_Plot_Layout_Record with null record;
   pragma Import (C, Get_Type, "gtk_plot_canvas_get_type");
end Gtk.Extra.Plot_Canvas;
