-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Glib;             use Glib;
with Glib.Graphs;      use Glib.Graphs;
with Glib.Object;      use Glib.Object;
with Glib.Values;      use Glib.Values;
with Gdk.Color;        use Gdk.Color;
with Gdk.Cursor;       use Gdk.Cursor;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;           use Gdk.GC;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Region;       use Gdk.Region;
with Gdk.Types;        use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Visual;       use Gdk.Visual;
with Gdk.Window;       use Gdk.Window;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Object;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtk.Main;         use Gtk.Main;
pragma Elaborate_All (Gtk.Main);
with Pango.Font;       use Pango.Font;
with Pango.Layout;     use Pango.Layout;

with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Unchecked_Deallocation;

--  TODO:
--   - would be nice to have a pixbuf item directly (for alpha layers)

package body Gtkada.Canvas is

   use type Gdk_Font;
   use type Gdk_GC;
   use type Gdk_Window, Gdk_Pixmap;
   use type System.Address;

   Class_Record : GObject_Class := Uninitialized_Class;
   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused.
   --  ??? This is a global variable.

   Timeout_Between_Scrolls : constant := 10;
   --  Time between two scrollings when the mouse is in the bounding box.

   Timeout_Between_Zooms : constant := 15;
   --  Time between two zooms when smooth-scrolling the canvas

   Scrolling_Margin : constant := 10;
   --  Width and height of the surrounding box in which "infinite"
   --  scrolling is started (it will continue while the mouse is kept in this
   --  area or moved outside of the canvas)

   Scrolling_Amount_Min      : constant Gfloat := 10.0;
   Scrolling_Amount_Max      : constant Gfloat := 50.0;
   Scrolling_Amount_Increase : constant Gfloat := 1.05;  --  +5% every step
   --  Number of pixels to scroll while the mouse is in the surrounding
   --  box. This is the initial value, and will keep increasing while the mouse
   --  is left in the box.

   Links_Threshold_While_Moving : constant := 20;
   --  Maximal number of links that are drawn while moving an item. This is
   --  used to make the canvas still usable when there are lots of links to a
   --  given item.

   Signals : constant chars_ptr_array :=
     (1 => New_String ("background_click"),
      2 => New_String ("item_selected"),
      3 => New_String ("zoomed"),
      4 => New_String ("set_scroll_adjustments"),
      5 => New_String ("item_unselected"));
   --  Array of the signals created for this widget

   -----------------
   -- Subprograms --
   -----------------
   --  Note: Some callbacks take Gtk_Widget_Record parameters, so that we can
   --  reuse the callbacks in Gtkada.Handlers, and thus save a lot of space
   --  in the GtkAda library.

   procedure Free is new Unchecked_Deallocation (String, String_Access);
   procedure Free is new Unchecked_Deallocation
     (Item_Selection_List_Record, Item_Selection_List);

   package Canvas_Timeout is new Gtk.Main.Timeout (Interactive_Canvas);

   function Expose
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle the expose events for a canvas.

   procedure Realized (Canvas : access Gtk_Widget_Record'Class);
   --  Create all the graphic contexts required for the animation.

   procedure Canvas_Destroyed (Canvas : access Gtk_Widget_Record'Class);
   --  Called when the canvas is being destroyed. All the items and links
   --  are removed, and the double-buffer is freed

   procedure Update_Links
     (Canvas      : access Interactive_Canvas_Record'Class;
      GC          : Gdk.GC.Gdk_GC;
      Invert_Mode : Boolean;
      Selected    : Item_Selection_List := null);
   --  Redraw all the links in the canvas.
   --  If Item is not null, only the links to or from Item are redrawn.

   procedure Size_Allocate
     (Canv : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  When the item is resized.

   function Button_Pressed
     (Canv : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  This tests whether an item was selected.

   function Button_Release
     (Canv : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when the user has released the mouse button.
   --  If an item was selected, this refreshed the canvas.

   function Button_Motion
     (Canv : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when the user moves the mouse while a button is pressed.
   --  If an item was selected, the item is moved.

   function Key_Press
     (Canv : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Handle key events, to provide scrolling through Page Up, Page Down, and
   --  arrow keys.

   procedure Draw_Orthogonal_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class);
   --  Draw a link on the screen, as possibly several orthogonal lines.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Straight_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class);
   --  Draw Link on the screen as a straight line.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Arc_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class;
      Offset : Gint);
   --  Draw Link on the screen.
   --  The link is drawn as a curved link (ie there is an extra handle in its
   --  middle).
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Self_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class;
      Offset : Gint);
   --  Draw a link when its source and destination items are the same

   procedure Update_Adjustments
     (Canvas : access Interactive_Canvas_Record'Class);
   --  Update the adjustments of the canvas.
   --  The bounds for the adjustments are automatically computed, given the
   --  list of items in it.

   procedure Draw_Arrow_Head
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Angle  : in Float);
   --  Draw an arrow head at the position (X, Y) on the canvas. The position
   --  is given in pixels, and should include zoom processing.
   --  Angle is the angle of the main axis of the arrow.

   procedure Draw_Annotation
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Link   : access Canvas_Link_Record'Class);
   --  Print an annotation on the canvas.
   --  The annotation is centered around (X, Y), in pixels. These coordinates
   --  should already include zoom processing.

   procedure Set_Scroll_Adjustments
     (Canvas : access Gtk_Widget_Record'Class;
      Args   : GValues);
   --  Change the two adjustments used for the canvas (in a callback)

   procedure Scrolled (Canvas : access Gtk_Widget_Record'Class);
   --  Called everytime the value of one of the adjustments is changed.

   procedure Get_Bounding_Box
     (Canvas : access Interactive_Canvas_Record'Class;
      X_Min, X_Max, Y_Min, Y_Max : out Gint);
   --  Find the smallest bounding box for all the items in the canvas.
   --  Note that this does not include links, which might thus be found
   --  outside of this box.
   --  The returned values are in world coordinates

   procedure Test_Scrolling_Box
     (Canvas   : access Gtk.Widget.Gtk_Widget_Record'Class;
      X_Scroll : out Gint;
      Y_Scroll : out Gint);
   --  We keep moving the selection (and scrolling the canvas) as long as the
   --  mouse remains in a surrounding box around the canvas, or even outside
   --  the canvas. This is done even if the mouse doesn't move, so at to make
   --  it easier to move items.  This subprogram tests whether the pointer is
   --  found in that box, and returns the extra scrolling that should be
   --  done. (0, 0) is returned if the mouse is not in that box.

   function Scrolling_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Function called repeatedly while the mouse is in the scrolling box.
   --  This provides scrolling even when the mouse doesn't move

   function Move_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      Delta_X, Delta_Y : Gint) return Boolean;
   --  Moves the selected items by a given number of pixels.
   --  Return True if the selection was actually moved, False if for some
   --  reason nothing happened

   procedure Show_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Gint;
      Report_Adj_Changed : Boolean := True);
   --  Like Show_Item, but use X, Y for the coordinates instead of the
   --  ones available in Item.
   --  If Report_Adj_Changed is true, the "changed" signal might be sent if the
   --  adjustments are changed. However, this might result in flickering.

   procedure Draw_Dashed_Selection
     (Canvas : access Interactive_Canvas_Record'Class);
   --  Draw all the selected items and links with dashed-lines

   function Zoom_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Timeout function used to provide smooth zooming.

   procedure Zoom_Internal
     (Canvas : access Interactive_Canvas_Record'Class; Percent : Guint);
   --  Internal function to implement zooming

   function Get_Background_Selection_Rectangle
     (Canvas : access Interactive_Canvas_Record'Class)
      return Gdk_Rectangle;
   --  Return the coordinates of the rectangle representing the background
   --  selection (when the user clicks in the background and drags the mouse).
   --  Return coordinates are in canvas coordinates

   procedure Emit_By_Name_Item
     (Object : System.Address;
      Name   : String;
      Param  : access Canvas_Item_Record'Class);
   pragma Import (C, Emit_By_Name_Item, "gtk_signal_emit_by_name");

   ---------------------------
   -- To_Canvas_Coordinates --
   ---------------------------

   function To_Canvas_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Gint) return Gint is
   begin
      return X * Gint (Canvas.Zoom) / 100;
   end To_Canvas_Coordinates;

   --------------------------
   -- To_World_Coordinates --
   --------------------------

   function To_World_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Gint) return Gint is
   begin
      return X * 100 / Gint (Canvas.Zoom);
   end To_World_Coordinates;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Canvas : out Interactive_Canvas; Auto_Layout : Boolean := True)
   is
   begin
      Canvas := new Interactive_Canvas_Record;
      Gtkada.Canvas.Initialize (Canvas, Auto_Layout);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Canvas      : access Interactive_Canvas_Record'Class;
      Auto_Layout : Boolean := True)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => Gdk.Event.Get_Type,      2 => GType_None),
         2 => (1 => GType_Pointer,           2 => GType_None),
         3 => (1 => GType_Uint,              2 => GType_None),
         4 => (1 => Gtk.Adjustment.Get_Type, 2 => Gtk.Adjustment.Get_Type),
         5 => (1 => GType_Pointer,           2 => GType_None));
      --  the parameters for the above signals.
      --  This must be defined in this function rather than at the
      --  library-level, or the value of Gdk_Event.Get_Type is not yet
      --  initialized.

   begin
      Gtk.Drawing_Area.Initialize (Canvas);

      Set_Directed (Canvas.Children, True);
      Canvas.Auto_Layout := Auto_Layout;

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note also that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Initialize_Class_Record
        (Canvas, Signals, Class_Record,
         "GtkAdaCanvas", Signal_Parameters);
      Set_Scroll_Adjustments_Signal
        (Class_Record, "set_scroll_adjustments");

      Return_Callback.Connect
        (Canvas, "expose_event",
         Return_Callback.To_Marshaller (Expose'Access));
      Widget_Callback.Connect
        (Canvas, "realize",
         Widget_Callback.To_Marshaller (Realized'Access));
      Return_Callback.Connect
        (Canvas, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Canvas, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release'Access));
      Return_Callback.Connect
        (Canvas, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Return_Callback.Connect
        (Canvas, "key_press_event",
         Return_Callback.To_Marshaller (Key_Press'Access));
      Widget_Callback.Connect
        (Canvas, "size_allocate", Size_Allocate'Access);
      Widget_Callback.Connect
        (Canvas, "set_scroll_adjustments", Set_Scroll_Adjustments'Access);
      Widget_Callback.Connect
        (Canvas, "destroy",
         Widget_Callback.To_Marshaller (Canvas_Destroyed'Access));

      Canvas.Annotation_Layout := Create_Pango_Layout (Canvas);

      --  We want to be sure to get all the mouse events, that are required
      --  for the animation.

      Add_Events
        (Canvas,
         Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Key_Press_Mask
           or Key_Release_Mask);
      Set_Flags (Canvas, Can_Focus);

      --  Configure with default values
      Configure (Canvas);
      Set_Scroll_Adjustments (Canvas, Null_Adjustment, Null_Adjustment);
   end Initialize;

   ----------------------
   -- Canvas_Destroyed --
   ----------------------

   procedure Canvas_Destroyed (Canvas : access Gtk_Widget_Record'Class) is
      C : Interactive_Canvas := Interactive_Canvas (Canvas);
      Child : Vertex_Iterator;
      V : Vertex_Access;
   begin
      if C.Scrolling_Timeout_Id /= 0 then
         Timeout_Remove (C.Scrolling_Timeout_Id);
      end if;

      Clear_Selection (C);

      --  Delete nodes and links
      Child := First (C.Children);
      while not At_End (Child) loop
         V := Get (Child);
         Next (Child);
         Remove (C.Children, V);
      end loop;

      Unref (C.Annotation_Layout);
      Unref (C.Clear_GC);
      Unref (C.Black_GC);
      Unref (C.Link_GC);
      Unref (C.Anim_GC);
      Destroy (C.Hadj);
      Destroy (C.Vadj);
   end Canvas_Destroyed;

   ----------------------------
   -- Set_Scroll_Adjustments --
   ----------------------------

   procedure Set_Scroll_Adjustments
     (Canvas : access Gtk_Widget_Record'Class;
      Args   : GValues)
   is
      Hadj : constant System.Address := Get_Address (Nth (Args, 1));
      Vadj : constant System.Address := Get_Address (Nth (Args, 2));
      Canv : Interactive_Canvas := Interactive_Canvas (Canvas);
      Stub : Gtk_Adjustment_Record;

   begin
      if Canv.Hadj /= null then
         Unref (Canv.Hadj);
      end if;

      if Hadj /= System.Null_Address then
         Canv.Hadj := Gtk_Adjustment (Get_User_Data (Hadj, Stub));
      else
         Gtk_New (Canv.Hadj, 0.0, 0.0, 0.0, 1.0, 1.0, 10.0);
      end if;
      Ref (Canv.Hadj);
      Sink (Canv.Hadj);

      if Canv.Vadj /= null then
         Unref (Canv.Vadj);
      end if;

      if Vadj /= System.Null_Address then
         Canv.Vadj := Gtk_Adjustment (Get_User_Data (Vadj, Stub));
      else
         Gtk_New (Canv.Vadj, 0.0, 0.0, 0.0, 1.0, 1.0, 10.0);
      end if;
      Ref (Canv.Vadj);
      Sink (Canv.Vadj);

      Widget_Callback.Object_Connect
        (Canv.Hadj, "value_changed",
         Widget_Callback.To_Marshaller (Scrolled'Access), Canv);
      Widget_Callback.Object_Connect
        (Canv.Vadj, "value_changed",
         Widget_Callback.To_Marshaller (Scrolled'Access), Canv);

      Update_Adjustments (Canv);
   end Set_Scroll_Adjustments;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Canvas : access Interactive_Canvas_Record;
      Grid_Size         : Guint := Default_Grid_Size;
      Annotation_Font   : Pango.Font.Pango_Font_Description :=
        Pango.Font.From_String (Default_Annotation_Font);
      Arc_Link_Offset   : Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Gint := Default_Arrow_Angle;
      Arrow_Length      : Gint := Default_Arrow_Length;
      Motion_Threshold  : Gint := Default_Motion_Threshold) is
   begin
      Canvas.Grid_Size := Grid_Size;

      if Grid_Size < 2 then
         Canvas.Align_On_Grid := False;
      end if;

      Set_Font_Description (Canvas.Annotation_Layout, Annotation_Font);

      Canvas.Arc_Link_Offset := Arc_Link_Offset;
      Canvas.Arrow_Angle := Float (Arrow_Angle) * Ada.Numerics.Pi / 180.0;
      Canvas.Arrow_Length := Arrow_Length;
      Canvas.Motion_Threshold := Motion_Threshold;
   end Configure;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Canv : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Alloc  : constant Gtk_Allocation_Access := To_Allocation (Args, 1);
      Canvas : constant Interactive_Canvas := Interactive_Canvas (Canv);

   begin
      Set_Page_Size (Canvas.Hadj, Gdouble (Alloc.Width));
      Set_Step_Increment (Canvas.Hadj, 10.0);
      Set_Page_Increment (Canvas.Hadj, Get_Page_Size (Canvas.Hadj) / 2.0);

      Set_Page_Size (Canvas.Vadj, Gdouble (Alloc.Height));
      Set_Step_Increment (Canvas.Vadj, 10.0);
      Set_Page_Increment (Canvas.Vadj, Get_Page_Size (Canvas.Vadj) / 2.0);

      Update_Adjustments (Canvas);
   end Size_Allocate;

   -------------------
   -- Align_On_Grid --
   -------------------

   procedure Align_On_Grid
     (Canvas : access Interactive_Canvas_Record;
      Align  : Boolean := True) is
   begin
      Canvas.Align_On_Grid := (Canvas.Grid_Size >= 2) and then Align;
   end Align_On_Grid;

   ----------------------
   -- Get_Bounding_Box --
   ----------------------

   procedure Get_Bounding_Box
     (Canvas : access Interactive_Canvas_Record'Class;
      X_Min, X_Max, Y_Min, Y_Max : out Gint)
   is
      Current : Vertex_Iterator := First (Canvas.Children);
      Item : Canvas_Item;
   begin
      if At_End (Current) then
         X_Min := 0;
         X_Max := 0;
         Y_Min := 0;
         Y_Max := 0;

      else
         X_Min := Gint'Last;
         X_Max := Gint'First;
         Y_Min := Gint'Last;
         Y_Max := Gint'First;

         while not At_End (Current) loop
            Item := Canvas_Item (Get (Current));
            if Item.Visible then
               X_Max := Gint'Max
                 (X_Max, Item.Coord.X + Gint (Item.Coord.Width));
               X_Min := Gint'Min (X_Min, Item.Coord.X);
               Y_Max := Gint'Max
                 (Y_Max, Item.Coord.Y + Gint (Item.Coord.Height));
               Y_Min := Gint'Min (Y_Min, Item.Coord.Y);
            end if;

            Next (Current);
         end loop;
      end if;
   end Get_Bounding_Box;

   ------------------------
   -- Update_Adjustments --
   ------------------------

   procedure Update_Adjustments
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      X_Max, Y_Max, X_Min, Y_Min : Gint;
   begin
      Get_Bounding_Box (Canvas, X_Min, X_Max, Y_Min, Y_Max);
      X_Min := To_Canvas_Coordinates (Canvas, X_Min);
      Y_Min := To_Canvas_Coordinates (Canvas, Y_Min);
      X_Max := To_Canvas_Coordinates (Canvas, X_Max);
      Y_Max := To_Canvas_Coordinates (Canvas, Y_Max);

      --  If the non-visible part of the canvas (left of the displayed area)
      --  contains no item, we can delete that part. However, avoid any visible
      --  scrolling, which is disturbing for the user.
      Set_Lower
        (Canvas.Hadj, Gdouble'Min (Get_Value (Canvas.Hadj), Gdouble (X_Min)));
      Set_Upper
        (Canvas.Hadj,
         Gdouble'Max
           (Get_Value (Canvas.Hadj) + Get_Page_Size (Canvas.Hadj),
            Gdouble (X_Max)));
      Changed (Canvas.Hadj);

      Set_Lower
        (Canvas.Vadj, Gdouble'Min (Get_Value (Canvas.Vadj), Gdouble (Y_Min)));
      Set_Upper
        (Canvas.Vadj,
         Gdouble'Max
           (Get_Value (Canvas.Vadj) + Get_Page_Size (Canvas.Vadj),
            Gdouble (Y_Max)));
      Changed (Canvas.Vadj);
   end Update_Adjustments;

   ------------------------------
   -- Default_Layout_Algorithm --
   ------------------------------

   procedure Default_Layout_Algorithm
     (Canvas : access Interactive_Canvas_Record'Class;
      Graph : Glib.Graphs.Graph;
      Force : Boolean;
      Vertical_Layout : Boolean)
   is
      pragma Unreferenced (Force);
      Step       : Gint := Gint (Canvas.Grid_Size);
      Region     : Gdk_Region;
      Items      : Vertex_Iterator;
      Item       : Canvas_Item;
      Links      : Edge_Iterator;
      Src_Item   : Canvas_Item := null;
      X1, X3, Y1 : Gint;
      Num        : Gint;
      Coord      : Gdk_Rectangle;

   begin
      if Step = 0 then
         Step := Gint (Default_Grid_Size);
      end if;

      --  First, check every item that won't be moved

      Gdk_New (Region);
      Items := First (Graph);
      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         if Item.Coord.X /= Gint'First
           or else Item.Coord.Y /= Gint'First
         then
            Union_With_Rect (Region, Item.Coord);
         end if;

         Next (Items);
      end loop;

      Items := First (Graph);
      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         if Item.Coord.X = Gint'First or else Item.Coord.Y = Gint'First then
            --  Check if there is any link that has for destination or source
            --  the widget we are adding.

            Links := First (Canvas.Children, Src => Vertex_Access (Item));
            if not At_End (Links) then
               Src_Item := Canvas_Item (Get_Dest (Get (Links)));
            else
               Links := First (Canvas.Children, Dest => Vertex_Access (Item));
               if not At_End (Links) then
                  Src_Item := Canvas_Item (Get_Src (Get (Links)));
               else
                  Src_Item := null;
               end if;
            end if;

            --  The rule is the following when we have a link to an existing
            --  item: We first try to put the new item below the old one, then,
            --  if that place is already occupied, to the bottom-right, then
            --  the bottom-left, then two down, ...

            if Src_Item /= null then
               Num := 0;

               if Vertical_Layout then
                  X3 := Src_Item.Coord.Y;
                  Y1 := Src_Item.Coord.X + Gint (Src_Item.Coord.Width) + Step;

                  loop
                     case Num mod 3 is
                        when 0 =>
                           X1 := X3;
                        when 1 =>
                           X1 := X3 - Step - Gint (Item.Coord.Height);
                        when 2 =>
                           X1 := X3 + Step + Gint (Item.Coord.Height);
                        when others =>
                           null;
                     end case;

                     Coord := (Y1, X1, Item.Coord.Width, Item.Coord.Height);
                     exit when Rect_In (Region, Coord) = Overlap_Rectangle_Out;

                     Num := Num + 1;
                     if Num mod 3 = 0 then
                        Y1 := Y1 + 2 * Step;
                     end if;
                  end loop;

                  Item.Coord.X := Y1;
                  Item.Coord.Y := X1;

               else
                  X3 := Src_Item.Coord.X;
                  Y1 := Src_Item.Coord.Y + Gint (Src_Item.Coord.Height) + Step;

                  loop
                     case Num mod 3 is
                        when 0 =>
                           X1 := X3;
                        when 1 =>
                           X1 := X3 - Step - Gint (Item.Coord.Width);
                        when 2 =>
                           X1 := X3 + Step + Gint (Item.Coord.Width);
                        when others =>
                           null;
                     end case;

                     Coord := (X1, Y1, Item.Coord.Width, Item.Coord.Height);
                     exit when Rect_In (Region, Coord) = Overlap_Rectangle_Out;

                     Num := Num + 1;
                     if Num mod 3 = 0 then
                        Y1 := Y1 + 2 * Step;
                     end if;
                  end loop;

                  Item.Coord.X := X1;
                  Item.Coord.Y := Y1;
               end if;

            else
               --  Else put the item in the first line, at the first possible
               --  location
               X1 := Gint (Get_Lower (Canvas.Hadj)) + Step;
               Y1 := Gint (Get_Lower (Canvas.Vadj)) + Step;

               loop
                  Coord := (X1, Y1, Item.Coord.Width, Item.Coord.Height);
                  exit when Rect_In (Region, Coord) = Overlap_Rectangle_Out;

                  if Vertical_Layout then
                     Y1 := Y1 + 2 * Step;
                  else
                     X1 := X1 + 2 * Step;
                  end if;
               end loop;

               Item.Coord.X := X1;
               Item.Coord.Y := Y1;
            end if;

            Union_With_Rect (Region, Item.Coord);
         end if;

         Next (Items);
      end loop;

      Destroy (Region);
   end Default_Layout_Algorithm;

   ---------------------
   -- Set_Auto_Layout --
   ---------------------

   procedure Set_Auto_Layout
     (Canvas : access Interactive_Canvas_Record;
      Auto_Layout : Boolean) is
   begin
      Canvas.Auto_Layout := Auto_Layout;
   end Set_Auto_Layout;

   ------------
   -- Layout --
   ------------

   procedure Layout
     (Canvas : access Interactive_Canvas_Record;
      Force  : Boolean := False;
      Vertical_Layout : Boolean := False)
   is
      Step  : constant Gint := Gint (Canvas.Grid_Size);
      Items : Vertex_Iterator;
      Item : Canvas_Item;
      Min_X, Min_Y : Gint := Gint'Last;
   begin
      Canvas.Layout (Canvas, Canvas.Children, Force, Vertical_Layout);

      Items := First (Canvas.Children);
      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         Min_X := Gint'Min (Min_X, Item.Coord.X);
         Min_Y := Gint'Min (Min_Y, Item.Coord.Y);

         if Force then
            Item.From_Auto_Layout := True;
         end if;
         Next (Items);
      end loop;

      Items := First (Canvas.Children);
      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         if Item.From_Auto_Layout then
            Item.Coord.X := Item.Coord.X - Min_X;
            Item.Coord.Y := Item.Coord.Y - Min_Y;

            if Canvas.Align_On_Grid then
               Item.Coord.X := Item.Coord.X - Item.Coord.X mod Step;
               Item.Coord.Y := Item.Coord.Y - Item.Coord.Y mod Step;
            end if;
         end if;

         Next (Items);
      end loop;
   end Layout;

   --------------------------
   -- Set_Layout_Algorithm --
   --------------------------

   procedure Set_Layout_Algorithm
     (Canvas    : access Interactive_Canvas_Record;
      Algorithm : Layout_Algorithm) is
   begin
      if Canvas.Layout /= null then
         Canvas.Layout := Algorithm;
      end if;
   end Set_Layout_Algorithm;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First)
   is
      pragma Unreferenced (Canvas);
   begin
      Item.Coord.X := X;
      Item.Coord.Y := Y;
   end Move_To;

   ---------
   -- Put --
   ---------

   procedure Put
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Gint := Gint'First) is
   begin
      Add_Vertex (Canvas.Children, Item);
      Move_To (Canvas, Item, X, Y);

      --  Make sure that the item will be properly moved by the layout
      --  algorithm.
      Item.From_Auto_Layout :=
        X = Gint'First and then Y = Gint'First;

      if Canvas.Auto_Layout
        and then Item.From_Auto_Layout
      then
         Layout (Canvas);
      end if;

      Update_Adjustments (Canvas);
   end Put;

   ---------------
   -- Set_Items --
   ---------------

   procedure Set_Items
     (Canvas : access Interactive_Canvas_Record;
      Items  : Glib.Graphs.Graph) is
   begin
      Destroy (Canvas.Children);
      Canvas.Children := Items;
   end Set_Items;

   --------------
   -- Realized --
   --------------

   procedure Realized (Canvas : access Gtk_Widget_Record'Class) is
      Canv : Interactive_Canvas := Interactive_Canvas (Canvas);
   begin
      --  Create all the graphic contexts if necessary.
      --  Set Exposures to False, since we want to handle the redraw
      --  events ourselves, and not have them generated automatically
      --  everytime we do a Draw_Pixmap (for optimization purposes)

      if Canv.Black_GC = null then
         Gdk_New (Canv.Black_GC, Get_Window (Canvas));
         Set_Foreground
           (Canv.Black_GC, Black (Gtk.Widget.Get_Default_Colormap));
         Set_Exposures (Canv.Black_GC, False);

         Gdk_New (Canv.Link_GC, Get_Window (Canvas));
         Set_Foreground
           (Canv.Link_GC, Black (Gtk.Widget.Get_Default_Colormap));
         Set_Exposures (Canv.Link_GC, False);

         Gdk_New (Canv.Clear_GC, Get_Window (Canvas));
         Set_Foreground
           (Canv.Clear_GC, Get_Background (Get_Style (Canvas), State_Normal));
         Set_Exposures (Canv.Clear_GC, False);

         --  Note: when setting the line attributes below, it is very important
         --  for the Line_Width to be 0 so has to get algorithms as fast as
         --  possible (1 is way too slow for a proper interaction with the
         --  user).

         Gdk_New (Canv.Anim_GC, Get_Window (Canvas));
         Set_Function (Canv.Anim_GC, Invert);

         --  Do not draw the lines dashed while we are moving items, since this
         --  becomes too slow when there are a lot of links to move around.
         --  Set_Line_Attributes
         --    (Canv.Anim_GC,
         --     Line_Width => 0,
         --     Line_Style => Line_On_Off_Dash,
         --     Cap_Style  => Cap_Butt,
         --     Join_Style => Join_Miter);

         Set_Exposures (Canv.Anim_GC, False);
      end if;
   end Realized;

   -------------------
   -- For_Each_Item --
   -------------------

   procedure For_Each_Item
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Item_Processor;
      Linked_From_Or_To : Canvas_Item := null)
   is
      Iter : Item_Iterator := Start (Canvas, Linked_From_Or_To);
      It : Canvas_Item;
   begin
      loop
         It := Get (Iter);
         exit when It = null;

         Next (Iter);
         exit when not Execute (Canvas, It);
      end loop;
   end For_Each_Item;

   -----------
   -- Start --
   -----------

   function Start
     (Canvas : access Interactive_Canvas_Record;
      Linked_From_Or_To : Canvas_Item := null) return Item_Iterator is
   begin
      if Linked_From_Or_To = null then
         return (Vertex            => First (Canvas.Children),
                 Edge              => Null_Edge_Iterator,
                 Linked_From_Or_To => null);
      else
         return (Vertex => Null_Vertex_Iterator,
                 Edge   => First (Canvas.Children,
                                  Vertex_Access (Linked_From_Or_To),
                                  Directed => False),
                 Linked_From_Or_To => Linked_From_Or_To);
      end if;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Item_Iterator) is
   begin
      if Iter.Linked_From_Or_To = null then
         Next (Iter.Vertex);
      else
         Next (Iter.Edge);
      end if;
   end Next;

   --------------------
   -- Is_Linked_From --
   --------------------

   function Is_Linked_From (Iter : Item_Iterator) return Boolean is
   begin
      return Iter.Linked_From_Or_To /= null
        and then not At_End (Iter.Edge)
        and then Canvas_Item (Get_Src (Get (Iter.Edge))) /=
          Iter.Linked_From_Or_To;
   end Is_Linked_From;

   ---------
   -- Get --
   ---------

   function Get (Iter : Item_Iterator) return Canvas_Item is
      Item : Canvas_Item;
   begin
      if Iter.Linked_From_Or_To = null then
         if At_End (Iter.Vertex) then
            return null;
         else
            return Canvas_Item (Get (Iter.Vertex));
         end if;

      else
         if At_End (Iter.Edge) then
            return null;
         end if;

         Item  := Canvas_Item (Get_Src (Get (Iter.Edge)));
         if Item /= Iter.Linked_From_Or_To then
            return Item;
         end if;

         --  If Get_Src was the item, we want to return Dest (which might
         --  actually be the item itself).
         --  Else, if Get_Src wasn't the item, then Get_Dest is the item, and
         --  we do not want to return it.
         return Canvas_Item (Get_Dest (Get (Iter.Edge)));
      end if;
   end Get;

   ---------------
   -- Clip_Line --
   ---------------

   procedure Clip_Line
     (Src   : access Canvas_Item_Record;
      To_X  : Gint;
      To_Y  : Gint;
      X_Pos : Gfloat;
      Y_Pos : Gfloat;
      Side  : out Item_Side;
      X_Out : out Gint;
      Y_Out : out Gint)
   is
      Rect     : constant Gdk_Rectangle := Src.Coord;
      Src_X    : constant Gint := Rect.X + Gint (Gfloat (Rect.Width) * X_Pos);
      Src_Y    : constant Gint := Rect.Y + Gint (Gfloat (Rect.Height) * Y_Pos);
      Delta_X  : constant Gint := To_X - Src_X;
      Delta_Y  : constant Gint := To_Y - Src_Y;
      Offset   : Gint;

   begin
      --  Intersection with horizontal sides

      if Delta_Y /= 0 then
         Offset := (Src_X * To_Y - To_X * Src_Y);

         if Delta_Y < 0 then
            Side := North;
            Y_Out := Rect.Y;
         else
            Side := South;
            Y_Out := Rect.Y + Gint (Rect.Height);
         end if;

         X_Out := (Delta_X * Y_Out + Offset) / Delta_Y;

         if Rect.X <= X_Out
           and then X_Out <= Rect.X + Gint (Rect.Width)
         then
            return;
         end if;
      end if;

      --  Intersection with vertical sides

      if Delta_X /= 0 then
         Offset := (To_X * Src_Y - Src_X * To_Y);

         if Delta_X < 0 then
            Side := West;
            X_Out := Rect.X;
         else
            Side := East;
            X_Out := Rect.X + Gint (Rect.Width);
         end if;

         Y_Out := (Delta_Y * X_Out + Offset) / Delta_X;

         if Rect.Y <= Y_Out
           and then Y_Out <= Rect.Y + Gint (Rect.Height)
         then
            return;
         end if;
      end if;

      X_Out := 0;
      Y_Out := 0;
   end Clip_Line;

   ---------------------
   -- Draw_Arrow_Head --
   ---------------------

   procedure Draw_Arrow_Head
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Angle  : in Float)
   is
      Length : constant Float :=
        Float (To_Canvas_Coordinates (Canvas, Canvas.Arrow_Length));
   begin
      Draw_Polygon
        (Get_Window (Canvas),
         GC,
         Filled => True,
         Points =>
           ((X => X, Y => Y),
            (X => X + Gint (Length * Cos (Angle + Canvas.Arrow_Angle)),
             Y => Y + Gint (Length * Sin (Angle + Canvas.Arrow_Angle))),
            (X => X + Gint (Length * Cos (Angle - Canvas.Arrow_Angle)),
             Y => Y + Gint (Length * Sin (Angle - Canvas.Arrow_Angle)))));
   end Draw_Arrow_Head;

   ---------------------
   -- Draw_Annotation --
   ---------------------

   procedure Draw_Annotation
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Link   : access Canvas_Link_Record'Class)
   is
      Pixmap : Gdk_Pixmap;
      Tmp    : Gdk_Pixbuf;
      W, H   : Gint;
   begin
      if Link.Descr /= null
        and then Link.Descr.all /= ""
        and then Canvas.Annotation_Layout /= null
      then
         if Link.Pixbuf = Null_Pixbuf then
            Set_Text (Canvas.Annotation_Layout, Link.Descr.all);
            Get_Pixel_Size (Canvas.Annotation_Layout, W, H);

            Gdk_New (Pixmap, Get_Window (Canvas), W, H);
            Draw_Rectangle (Pixmap, Canvas.Clear_GC, True, 0, 0, W, H);
            Draw_Layout
              (Drawable => Pixmap,
               GC       => Get_Black_GC (Get_Style (Canvas)),
               X        => 0,
               Y        => 0,
               Layout   => Canvas.Annotation_Layout);

            Link.Pixbuf := Get_From_Drawable
              (Dest   => null,
               Src    => Pixmap,
               Cmap   => Get_Colormap (Canvas),
               Src_X  => 0,
               Src_Y  => 0,
               Dest_X => 0,
               Dest_Y => 0,
               Width  => W,
               Height => H);
            Gdk.Pixmap.Unref (Pixmap);
         end if;

         --  Do not draw the text in Xor mode, since this doesn't work on
         --  Windows systems, and doesn't provide any real information anyway.
         if GC /= Canvas.Anim_GC then
            if Canvas.Zoom = 100 then
               Tmp := Link.Pixbuf;
            else
               Tmp := Scale_Simple
                 (Src         => Link.Pixbuf,
                  Dest_Width  =>
                    Get_Width (Link.Pixbuf) * Gint (Canvas.Zoom) / 100,
                  Dest_Height =>
                    Get_Height (Link.Pixbuf) * Gint (Canvas.Zoom) / 100);
            end if;

            Render_To_Drawable
              (Pixbuf   => Tmp,
               Drawable => Get_Window (Canvas),
               GC       => Canvas.Black_GC,
               Src_X    => 0,
               Src_Y    => 0,
               Dest_X   => X,
               Dest_Y   => Y,
               Width    => Get_Width (Tmp),
               Height   => Get_Height (Tmp));

            if Canvas.Zoom /= 100 then
               Unref (Tmp);
            end if;
         end if;
      end if;
   end Draw_Annotation;

   ----------------------
   -- Compute_Line_Pos --
   ----------------------

   function Compute_Line_Pos
     (Canvas : access Interactive_Canvas_Record'Class)
     return Gint_Array;

   function Compute_Line_Pos
     (Canvas : access Interactive_Canvas_Record'Class)
     return Gint_Array
   is
      type Graph_Range is record
         From, To : Gint;
      end record;

      type Range_Array is array (Positive range <>) of Graph_Range;
      type Range_Array_Access is access all Range_Array;

      procedure Free is new Unchecked_Deallocation
        (Range_Array, Range_Array_Access);

      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));

      Free_Ranges : Range_Array_Access := new Range_Array (1 .. 1000);
      Tmp : Range_Array_Access;
      Last_Range : Positive := Free_Ranges'First;
      Iter : Vertex_Iterator := First (Canvas.Children);
      E : Canvas_Item;
      Right : Gint;
   begin
      Free_Ranges (Free_Ranges'First) := (From => Gint'First, To => Gint'Last);

      while not At_End (Iter) loop
         E := Canvas_Item (Get (Iter));
         Right := E.Coord.X + Gint (E.Coord.Width);

         for R in Free_Ranges'First .. Last_Range loop
            if Free_Ranges (R).From <= E.Coord.X
              and then Free_Ranges (R).To >= E.Coord.X
              and then Free_Ranges (R).To <= Right
            then
               Free_Ranges (R) :=
                 (From => Free_Ranges (R).From, To => E.Coord.X - 1);

            elsif Free_Ranges (R).From <= E.Coord.X
              and then Free_Ranges (R).To >= Right
            then
               if Last_Range >= Free_Ranges'Last then
                  Tmp := new Range_Array (1 .. Free_Ranges'Last * 2);
                  Tmp (1 .. Free_Ranges'Last) := Free_Ranges.all;
                  Free (Free_Ranges);
                  Free_Ranges := Tmp;
               end if;

               Free_Ranges (R + 1 .. Last_Range + 1) :=
                 Free_Ranges (R .. Last_Range);
               Free_Ranges (R + 1) :=
                 (From => Right + 1, To => Free_Ranges (R).To);
               Free_Ranges (R) :=
                 (From => Free_Ranges (R).From, To => E.Coord.X - 1);
               Last_Range := Last_Range + 1;

            elsif Free_Ranges (R).From >= E.Coord.X
              and then Free_Ranges (R).From <= Right
              and then Free_Ranges (R).To >= Right
            then
               Free_Ranges (R) :=
                 (From => Right + 1, To => Free_Ranges (R).To);
            end if;

            exit when Free_Ranges (R).From > Right;
         end loop;

         Next (Iter);
      end loop;

      declare
         Result : Gint_Array (1 .. Last_Range);
      begin
         for R in Result'Range loop
            --  ??? Should handle vertical layout and horizontal layout
            Result (R) := To_Canvas_Coordinates
              (Canvas, (Free_Ranges (R).From + Free_Ranges (R).To) / 2)
              - Xbase;
         end loop;

         Free (Free_Ranges);
         return Result;
      end;
   end Compute_Line_Pos;

   --------------------------
   -- Draw_Orthogonal_Link --
   --------------------------

   procedure Draw_Orthogonal_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class)
   is
      X1, Y1, Xp1, Yp1, X2, Y2, Xp2, Yp2, X3, Y3 : Gint;
      Xc1, Xc2, Yc1, Yc2 : Gint;
      Xarr_End, Yarr_End, Xarr_Start, Yarr_Start : Gint;
      Angle_Arr_End, Angle_Arr_Start : Float;
      Xbase    : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase    : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Src      : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest     : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Line_Pos : constant Gint_Array := Compute_Line_Pos (Canvas);

   begin
      X1 := To_Canvas_Coordinates (Canvas, Src.Coord.X) - Xbase;
      Y1 := To_Canvas_Coordinates (Canvas, Src.Coord.Y) - Ybase;
      Xp1 := To_Canvas_Coordinates
        (Canvas, Src.Coord.X + Gint (Src.Coord.Width)) - Xbase;
      Yp1 := To_Canvas_Coordinates
        (Canvas, Src.Coord.Y + Gint (Src.Coord.Height)) - Ybase;

      X2 := To_Canvas_Coordinates (Canvas, Dest.Coord.X) - Xbase;
      Y2 := To_Canvas_Coordinates (Canvas, Dest.Coord.Y) - Ybase;
      Xp2 := To_Canvas_Coordinates
        (Canvas, Dest.Coord.X + Gint (Dest.Coord.Width)) - Xbase;
      Yp2 := To_Canvas_Coordinates
        (Canvas, Dest.Coord.Y + Gint (Dest.Coord.Height)) - Ybase;

      Xc1 := (X1 + Xp1) / 2;
      Xc1 := Xc1 - Xc1 mod Gint (Canvas.Grid_Size);

      Xc2 := (X2 + Xp2) / 2;
      Xc2 := Xc2 - Xc2 mod Gint (Canvas.Grid_Size);

      Yc1 := (Y1 + Yp1) / 2;
      Yc2 := (Y2 + Yp2) / 2;

      --  The preferred case will be
      --     A ---
      --         |____ B
      --  The separation line should be at equal distance of the center of A
      --  and the center of B, so that multiple items lined up in a column
      --  above B all have the vertical line at the same location.
      --
      --  If the vertical line can be drawn at exact distance of the centers,
      --  then we try and display the vertical line at equal distance of the
      --  adjacent edges of A and B

      X3 := Gint'First;

      --  Put_Line ("Xp1=" & Xp1'Img & " X2=" & X2'Img);

      for L in Line_Pos'Range loop
         if Line_Pos (L) >= Xp1
           and then Line_Pos (L) <= X2
         then
            X3 := Line_Pos (L);
            exit;

         elsif Line_Pos (L) >= Xp2
           and then Line_Pos (L) <= X1
         then
            X3 := Line_Pos (L);
            exit;
         end if;
      end loop;

      --  X3 := (X1 + Xp1 + X2 + Xp2) / 4;
      --  X3 := X3 - X3 mod Gint (Canvas.Grid_Size);

      --  if ((X1 <= X3 and then X3 <= Xp1)
      --      or else (X2 <= X3 and then X3 <= Xp2))
      --    and then (Xp1 <= X2 or else Xp2 <= X1)
      --  then
      --     X3 := (Xp1 + X2) / 2;
      --     X3 := X3 - X3 mod Gint (Canvas.Grid_Size);
      --  end if;

      if X3 /= Gint'First then
      --  if (X3 >= Xp1 and then X3 <= X2)
      --    or else (X3 <= X1 and then X3 >= Xp2)
      --  then
         Draw_Line (Get_Window (Canvas), GC, X3, Yc1, X3, (Y2 + Yp2) / 2);
         Yarr_Start := Yc1;
         Yarr_End := Yc2;

         if X3 >= Xp1 then
            Draw_Line (Get_Window (Canvas), GC, Xp1, Yc1, X3, Yc1);
            Draw_Line (Get_Window (Canvas), GC, X3, Yc2, X2, Yc2);
            Xarr_Start := Xp1;
            Xarr_End := X2;
            Angle_Arr_Start := 0.0;
            Angle_Arr_End := -Ada.Numerics.Pi;
         else
            Draw_Line (Get_Window (Canvas), GC, X1, Yc1, X3, Yc1);
            Draw_Line (Get_Window (Canvas), GC, X3, Yc2, Xp2, Yc2);
            Xarr_Start := X1;
            Xarr_End := Xp2;
            Angle_Arr_Start := -Ada.Numerics.Pi;
            Angle_Arr_End := 0.0;
         end if;

      --  Third case is when we didn't have enough space to draw the
      --  intermediate line. In that case, the layout is similar to
      --      A ----
      --           |
      --           B
      --  with the vertical line drawn at the same location as in the first
      --  algorithm.

      --  elsif X3 >= Xp1 or else X3 <= X1 then
      --     if X3 >= Xp1 then
      --        Draw_Line (Window, GC, Xp1, Yc1, X3, Yc1);
      --        Xarr_Start := Xp1;
      --        Angle_Arr_Start := -Ada.Numerics.Pi;
      --     else
      --        Draw_Line (Window, GC, X1, Yc1, X3, Yc1);
      --        Xarr_Start := X1;
      --        Angle_Arr_Start := 0.0;
      --     end if;

      --     Yarr_Start := Yc1;
      --     Xarr_End := X3;

      --     if Y2 < Yc1 then
      --        Draw_Line (Window, GC, X3, Yc1, X3, Yp2);
      --        Yarr_End := Yp2;
      --        Angle_Arr_End := Ada.Numerics.Pi / 2.0;
      --     else
      --        Draw_Line (Window, GC, X3, Yc1, X3, Y2);
      --        Yarr_End := Y2;
      --        Angle_Arr_End := -Ada.Numerics.Pi / 2.0;
      --     end if;

      --  Second case is when one of the item is below the other one. In that
      --  case, the layout should look like
      --       AAA
      --       |_
      --         |
      --        BB
      --  ie the link connects the top side of one item and the bottom side of
      --  the other item.

      else
      --  elsif (X1 <= X2 and then X2 <= Xp1)
      --    or else (X2 <= X1 and then X1 <= Xp2)
      --  then
         Y3 := (Y1 + Yp1 + Y2 + Yp2) / 4;
         Y3 := Y3 - Y3 mod Gint (Canvas.Grid_Size);
         Xarr_Start := Xc1;
         Xarr_End := Xc2;

         Draw_Line (Get_Window (Canvas), GC, Xc1, Y3, Xc2, Y3);

         if Y2 > Y3 then
            Draw_Line (Get_Window (Canvas), GC, Xc1, Yp1, Xc1, Y3);
            Draw_Line (Get_Window (Canvas), GC, Xc2, Y3, Xc2, Y2);
            Yarr_Start := Yp1;
            Yarr_End := Y2;
            Angle_Arr_End := -Ada.Numerics.Pi / 2.0;
            Angle_Arr_Start := Ada.Numerics.Pi / 2.0;
         else
            Draw_Line (Get_Window (Canvas), GC, Xc1, Y1, Xc1, Y3);
            Draw_Line (Get_Window (Canvas), GC, Xc2, Y3, Xc2, Yp2);
            Yarr_Start := Y1;
            Yarr_End := Yp2;
            Angle_Arr_End := Ada.Numerics.Pi / 2.0;
            Angle_Arr_Start := -Ada.Numerics.Pi / 2.0;
         end if;
      end if;

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, GC, Xarr_End, Yarr_End, Angle_Arr_End);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, GC, Xarr_Start, Yarr_Start, Angle_Arr_Start);
      end if;

      --  Draw the text if any

      --  if Link.Descr /= null then
      --     Draw_Annotation (Canvas, Window, GC, X3, (Y1 + Y2) / 2, Link);
      --  end if;
   end Draw_Orthogonal_Link;

   ------------------------
   -- Draw_Straight_Line --
   ------------------------

   procedure Draw_Straight_Line
     (Link : access Canvas_Link_Record;
      Window : Gdk_Window;
      GC : Gdk.GC.Gdk_GC;
      Src_Side : Item_Side;
      X1, Y1 : Glib.Gint;
      Dest_Side : Item_Side;
      X2, Y2 : Glib.Gint)
   is
      pragma Unreferenced (Link, Src_Side, Dest_Side);
   begin
      Draw_Line (Window, GC, X1, Y1, X2, Y2);
   end Draw_Straight_Line;

   ------------------------
   -- Draw_Straight_Link --
   ------------------------

   procedure Draw_Straight_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class)
   is
      X1, Y1, X2, Y2 : Gint;
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Src   : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest  : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Src_Side, Dest_Side : Item_Side;

   begin
      Clip_Line
        (Src,
         Dest.Coord.X + Gint (Gfloat (Dest.Coord.Width) * Link.Dest_X_Pos),
         Dest.Coord.Y + Gint (Gfloat (Dest.Coord.Height) * Link.Dest_Y_Pos),
         X_Pos => Link.Src_X_Pos, Y_Pos => Link.Src_Y_Pos,
         Side => Src_Side, X_Out => X1, Y_Out => Y1);
      Clip_Line
        (Dest,
         Src.Coord.X + Gint (Gfloat (Src.Coord.Width) * Link.Src_X_Pos),
         Src.Coord.Y + Gint (Gfloat (Src.Coord.Height) * Link.Src_Y_Pos),
         X_Pos => Link.Dest_X_Pos, Y_Pos => Link.Dest_Y_Pos,
         Side => Dest_Side, X_Out => X2, Y_Out => Y2);
      X1 := To_Canvas_Coordinates (Canvas, X1) - Xbase;
      Y1 := To_Canvas_Coordinates (Canvas, Y1) - Ybase;
      X2 := To_Canvas_Coordinates (Canvas, X2) - Xbase;
      Y2 := To_Canvas_Coordinates (Canvas, Y2) - Ybase;

      Draw_Straight_Line
        (Link, Get_Window (Canvas), GC, Src_Side, X1, Y1, Dest_Side, X2, Y2);

      --  Draw the end arrow head

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Draw_Arrow_Head
              (Canvas, GC, X2, Y2,
               Arctan (Float (Y1 - Y2), Float (X1 - X2)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head
              (Canvas, GC, X2, Y2, Ada.Numerics.Pi / 2.0);
         else
            Draw_Arrow_Head
              (Canvas, GC, X2, Y2, -Ada.Numerics.Pi / 2.0);
         end if;
      end if;

      --  Draw the start arrow head

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Draw_Arrow_Head
              (Canvas, GC, X1, Y1,
               Arctan (Float (Y2 - Y1), Float (X2 - X1)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head
              (Canvas, GC, X1, Y1, -Ada.Numerics.Pi / 2.0);
         else
            Draw_Arrow_Head
              (Canvas, GC, X1, Y1, +Ada.Numerics.Pi / 2.0);
         end if;
      end if;

      --  Draw the text if any

      if Link.Descr /= null then
         Draw_Annotation (Canvas, GC, (X1 + X2) / 2, (Y1 + Y2) / 2, Link);
      end if;
   end Draw_Straight_Link;

   --------------------
   -- Draw_Self_Link --
   --------------------

   procedure Draw_Self_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class;
      Offset : Gint)
   is
      Xbase      : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase      : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Arc_Offset : constant Float :=
        Float (To_Canvas_Coordinates (Canvas, Canvas.Arc_Link_Offset));
      Right_Angle : constant Float := Ada.Numerics.Pi / 2.0;
      X1, Y1, X3, Y3, Xc, Yc, Radius : Gint;
      Src        : constant Canvas_Item := Canvas_Item (Get_Src (Link));

   begin
      pragma Assert (Src = Canvas_Item (Get_Dest (Link)));
      Xc := To_Canvas_Coordinates (Canvas, Src.Coord.X + Src.Coord.Width)
        - Xbase;
      Yc := To_Canvas_Coordinates (Canvas, Src.Coord.Y) - Ybase;
      Radius := Gint (Arc_Offset) / 2 * Offset;

      --  Location of the arrow and the annotation
      X3 := Xc - Radius;
      Y3 := Yc;
      X1 := Xc;
      Y1 := Yc + Radius;

      Draw_Arc (Get_Window (Canvas),
                GC,
                Filled => False,
                X      => Xc - Radius,
                Y      => Yc - Radius,
                Width  => Radius * 2,
                Height => Radius * 2,
                Angle1 => -90 * 64,
                Angle2 => 270 * 64);

      --  Draw the arrows

      if Link.Arrow /= No_Arrow then
         Draw_Arrow_Head (Canvas, GC, X3, Y3, -Right_Angle);
      end if;

      if Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, GC, X1, Y1, 0.0);
      end if;

      --  Draw the annotations
      if Link.Descr /= null then
         Draw_Annotation (Canvas, GC, Xc + Radius / 2, Yc + Radius / 2, Link);
      end if;
   end Draw_Self_Link;

   -------------------
   -- Draw_Arc_Link --
   -------------------

   procedure Draw_Arc_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      Link   : access Canvas_Link_Record'Class;
      Offset : Gint)
   is
      procedure Bezier_One_Control
        (X1, Y1, X2, Y2, X3, Y3 : Gint; Step : Float);
      --  Draw a bezier curve with one control point (X2, Y2).

      ------------------------
      -- Bezier_One_Control --
      ------------------------

      procedure Bezier_One_Control
        (X1, Y1, X2, Y2, X3, Y3 : Gint; Step : Float)
      is
         T : Float := 0.0;
         A, B, B2, C : Float;
         Old_X : Gint := X1;
         Old_Y : Gint := Y1;
         Old_X2 : Gint := X3;
         Old_Y2 : Gint := Y3;
         New_X, New_Y, New_X2, New_Y2 : Gint;
      begin
         --  The general formula for a quadratic bezier curve is:
         --    P(t)=(1-t)^2 * P0 + 2t(1-t) * P1 + t^2 * P2,  0 <= t <= 1
         --  For optimization purposes, we compute only for 0 <= t <= 0.5, and
         --  draw two points at once.

         while T <= 0.5 loop
            A := (1.0 - T);
            B := 2.0 * T * A;
            B2 := B * Float (X2);
            B := B * Float (Y2);
            A := A * A;
            C := T * T;
            New_X := Gint (A * Float (X1) + B2 + C * Float (X3));
            New_Y := Gint (A * Float (Y1) + B + C * Float (Y3));
            if Old_X /= New_X or else Old_Y /= New_Y then
               Draw_Line (Get_Window (Canvas), GC, Old_X, Old_Y, New_X, New_Y);
               Old_X := New_X;
               Old_Y := New_Y;
            end if;

            New_X2 := Gint (C * Float (X1) + B2 + A * Float (X3));
            New_Y2 := Gint (C * Float (Y1) + B + A * Float (Y3));
            if Old_X2 /= New_X2 or else Old_Y2 /= New_Y2 then
               Draw_Line
                 (Get_Window (Canvas), GC, Old_X2, Old_Y2, New_X2, New_Y2);
               Old_X2 := New_X2;
               Old_Y2 := New_Y2;
            end if;

            T := T + Step;
         end loop;
      end Bezier_One_Control;

      Angle      : Float;
      X1, Y1, X2, Y2, X3, Y3 : Gint;
      Right_Angle : constant Float := Ada.Numerics.Pi / 2.0;
      Xbase       : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase       : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Arc_Offset  : constant Float :=
        Float (To_Canvas_Coordinates (Canvas, Canvas.Arc_Link_Offset));
      Src         : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest        : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Src_Side, Dest_Side : Item_Side;

   begin
      --  We will first compute the extra intermediate point between the
      --  center of the two items. Once we have this intermediate point, we
      --  will be able to use the intersection point between the two items
      --  and the two lines from the centers to the middle point. This extra
      --  point is used as a control point for the Bezier curve.

      X1 := Src.Coord.X + Gint (Gfloat (Src.Coord.Width) * Link.Src_X_Pos);
      Y1 := Src.Coord.Y + Gint (Gfloat (Src.Coord.Height) * Link.Src_Y_Pos);
      X3 := Dest.Coord.X + Gint (Gfloat (Dest.Coord.Width) * Link.Dest_X_Pos);
      Y3 := Dest.Coord.Y + Gint (Gfloat (Dest.Coord.Height) * Link.Dest_Y_Pos);

      --  Compute the middle point for the arc, and create a dummy item for it
      --  that the user can move.

      if X1 /= X3 then
         Angle := Arctan (Float (Y3 - Y1), Float (X3 - X1));
      elsif Y3 > Y1 then
         Angle := Right_Angle;
      else
         Angle := -Right_Angle;
      end if;

      if Offset < 0 then
         Angle := Angle - Right_Angle;
      else
         Angle := Angle + Right_Angle;
      end if;

      X2 := (X1 + X3) / 2 + abs (Offset) * Gint (Arc_Offset * Cos (Angle));
      Y2 := (Y1 + Y3) / 2 + abs (Offset) * Gint (Arc_Offset * Sin (Angle));

      --  Clip to the border of the boxes

      Clip_Line
        (Src, X2, Y2, Link.Src_X_Pos, Link.Src_Y_Pos, Src_Side, X1, Y1);
      Clip_Line
        (Dest, X2, Y2, Link.Dest_X_Pos, Link.Dest_Y_Pos,
         Dest_Side, X3, Y3);

      X1 := To_Canvas_Coordinates (Canvas, X1) - Xbase;
      Y1 := To_Canvas_Coordinates (Canvas, Y1) - Ybase;
      X2 := To_Canvas_Coordinates (Canvas, X2) - Xbase;
      Y2 := To_Canvas_Coordinates (Canvas, Y2) - Ybase;
      X3 := To_Canvas_Coordinates (Canvas, X3) - Xbase;
      Y3 := To_Canvas_Coordinates (Canvas, Y3) - Ybase;

      if GC /= Canvas.Anim_GC then
         Bezier_One_Control (X1, Y1, X2, Y2, X3, Y3, 0.005);
      else
         Bezier_One_Control (X1, Y1, X2, Y2, X3, Y3, 0.01);
      end if;

      --  Draw the arrows

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         if X3 /= X2 then
            Angle := Arctan (Float (Y2 - Y3), Float (X2 - X3));
         elsif Y3 > Y2 then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;
         Draw_Arrow_Head (Canvas, GC, X3, Y3, Angle);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Angle := Arctan (Float (Y2 - Y1), Float (X2 - X1));
         elsif Y2 > Y1 then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;
         Draw_Arrow_Head (Canvas,  GC, X1, Y1, Angle);
      end if;

      --  Draw the annotations, if any, in the middle of the link
      if Link.Descr /= null then
         X2 := Gint (0.25 * Float (X1) + 0.5 * Float (X2) + 0.25 * Float (X3));
         Y2 := Gint (0.25 * Float (Y1) + 0.5 * Float (Y2) + 0.25 * Float (Y3));
         Draw_Annotation (Canvas, GC, X2, Y2, Link);
      end if;
   end Draw_Arc_Link;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Canvas_Link_Record;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Gint)
   is
      pragma Unreferenced (Invert_Mode);
   begin
      --  Self-referencing links
      if Get_Src (Link) = Get_Dest (Link) then
         Draw_Self_Link (Canvas, GC, Link, Edge_Number);

      elsif Edge_Number = 1 then
         --  The first link in the list is always straight
         if Canvas.Orthogonal_Links then
            Draw_Orthogonal_Link (Canvas, GC, Link);
         else
            Draw_Straight_Link (Canvas, GC, Link);
         end if;

      elsif Edge_Number mod 2 = 1 then
         Draw_Arc_Link (Canvas, GC, Link, Edge_Number / 2);

      else
         Draw_Arc_Link (Canvas, GC, Link, -(Edge_Number / 2));

      end if;
   end Draw_Link;

   ------------------
   -- Update_Links --
   ------------------

   procedure Update_Links
     (Canvas      : access Interactive_Canvas_Record'Class;
      GC          : Gdk.GC.Gdk_GC;
      Invert_Mode : Boolean;
      Selected    : Item_Selection_List := null)
   is
      Current : Edge_Iterator;
      L : Canvas_Link;
      X, Y    : Gint;
      Count   : Natural := 0;
   begin
      if Selected /= null then
         X := Selected.Item.Coord.X;
         Y := Selected.Item.Coord.Y;
         Selected.Item.Coord.X := Selected.X;
         Selected.Item.Coord.Y := Selected.Y;
      end if;

      --  Temporarily set the graph as not-directed, so that we get all links
      --  to or from the item
      Set_Directed (Canvas.Children, False);

      if Selected /= null then
         Current := First
           (Canvas.Children, Src => Vertex_Access (Selected.Item));
      else
         Current := First (Canvas.Children);
      end if;

      while not At_End (Current) loop
         L := Canvas_Link (Get (Current));
         if Is_Visible (Canvas_Item (Get_Src (L)))
           and then Is_Visible (Canvas_Item (Get_Dest (L)))
         then
            Draw_Link (Canvas, L, Invert_Mode, GC,
                       Gint (Repeat_Count (Current)));
         end if;

         --  To save time, we limit the number of links that are drawn while
         --  moving items.
         Count := Count + 1;
         exit when Selected /= null
           and then Count > Links_Threshold_While_Moving;

         Next (Current);
      end loop;

      --  Restore the directed status of the graph
      Set_Directed (Canvas.Children, True);

      if Selected /= null then
         Selected.Item.Coord.X := X;
         Selected.Item.Coord.Y := Y;
      end if;
   end Update_Links;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid
     (Canvas        : access Interactive_Canvas_Record;
      GC            : Gdk.GC.Gdk_GC;
      Screen_Rect   : Gdk.Rectangle.Gdk_Rectangle;
      X_Left, Y_Top : Glib.Gint)
   is
      Grid   : constant Gint :=
        Gint (Canvas.Grid_Size) * Gint (Canvas.Zoom) / 100;
      X, Y, Xmin : Gint;
   begin
      if Grid >= 5 then
         if (Screen_Rect.Y + Y_Top) mod Grid = 0 then
            Y := Screen_Rect.Y;
         else
            Y := Screen_Rect.Y + Grid - (Screen_Rect.Y + Y_Top) mod Grid;
         end if;
         if (Screen_Rect.X + X_Left) mod Grid = 0 then
            Xmin := Screen_Rect.X;
         else
            Xmin := Screen_Rect.X + Grid - (Screen_Rect.X + X_Left) mod Grid;
         end if;

         while Y <= Screen_Rect.Y + Gint (Screen_Rect.Height) loop
            X := Xmin;

            while X <= Screen_Rect.X + Gint (Screen_Rect.Width) loop
               Draw_Point (Get_Window (Canvas), GC, X, Y);
               X := X + Grid;
            end loop;

            Y := Y + Grid;
         end loop;
      end if;
   end Draw_Grid;

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background
     (Canvas        : access Interactive_Canvas_Record;
      Screen_Rect   : Gdk_Rectangle;
      X_Left, Y_Top : Gint) is
   begin
      Draw_Rectangle
        (Get_Window (Canvas),
         Canvas.Clear_GC,
         Filled => True,
         X      => Screen_Rect.X,
         Y      => Screen_Rect.Y,
         Width  => Gint (Screen_Rect.Width),
         Height => Gint (Screen_Rect.Height));
      Draw_Grid (Interactive_Canvas (Canvas), Canvas.Black_GC,
                 Screen_Rect, X_Left, Y_Top);
   end Draw_Background;

   ------------
   -- Expose --
   ------------

   function Expose
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      Canvas : constant Interactive_Canvas := Interactive_Canvas (Canv);
      Rect   : constant Gdk_Rectangle := Get_Area (Event);
      Item   : Canvas_Item;
      Tmp    : Vertex_Iterator := First (Canvas.Children);
      X, Y   : Gint;
      Dest   : Gdk_Rectangle;
      Inters : Boolean;
      Xbase  : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase  : constant Gint := Gint (Get_Value (Canvas.Vadj));

   begin
      --  If the GC was not created, do not do anything

      if Canvas.Clear_GC = Null_GC then
         return False;
      end if;

      --  Clear the canvas

      --  Set_Clip_Rectangle (Canvas.Black_GC, Rect);

      Draw_Background (Canvas, Rect, Xbase, Ybase);

      --  Draw the links first, so that they appear to be below the items.
      --  ??? Should redraw only the required links

      Update_Links (Canvas, Canvas.Link_GC, False);

      --  Draw each of the items.

      while not At_End (Tmp) loop
         Item := Canvas_Item (Get (Tmp));
         if Item.Visible then
            X := To_Canvas_Coordinates (Canvas, Item.Coord.X) - Xbase;
            Y := To_Canvas_Coordinates (Canvas, Item.Coord.Y) - Ybase;
            Intersect
              (Rect,
               (X,
                Y,
                Item.Coord.Width * GRectangle_Length (Canvas.Zoom) / 100,
                Item.Coord.Height * GRectangle_Length (Canvas.Zoom) / 100),
               Dest, Inters);
            if Inters then
               Draw (Item, Canvas, Canvas.Black_GC, X, Y);
            end if;
         end if;

         Next (Tmp);
      end loop;

      --  The dashed line (while moving items) have been deleted, and are no
      --  longer visible
      if Canvas.Dashed_Line_Visible then
         Draw_Dashed_Selection (Canvas);
      end if;

      --  Set_Clip_Mask (Canvas.Black_GC, null);
      return False;
   end Expose;

   ---------------------
   -- Set_Screen_Size --
   ---------------------

   procedure Set_Screen_Size
     (Item   : access Canvas_Item_Record;
      Width, Height  : Gint) is
   begin
      Item.Coord.Width  := Width;
      Item.Coord.Height := Height;
   end Set_Screen_Size;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Canv : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas    : constant Interactive_Canvas := Interactive_Canvas (Canv);
      Value     : constant Gdouble := Get_Value (Canvas.Vadj);
      Upper     : constant Gdouble := Get_Upper (Canvas.Vadj);
      Lower     : constant Gdouble := Get_Lower (Canvas.Vadj);
      Page_Incr : constant Gdouble := Gdouble (Scrolling_Amount_Max);
      Page_Size : constant Gdouble := Get_Page_Size (Canvas.Vadj);
      Step_Incr : constant Gdouble := Gdouble (Scrolling_Amount_Min);

   begin
      case Get_Key_Val (Event) is
         when GDK_Home =>
            Set_Value (Canvas.Vadj, Lower);
            Changed (Canvas.Vadj);
            return True;

         when GDK_End =>
            Set_Value (Canvas.Vadj, Upper - Page_Size);
            Changed (Canvas.Vadj);
            return True;

         when GDK_Page_Up =>
            if Value >= Lower + Page_Incr then
               Set_Value (Canvas.Vadj, Value - Page_Incr);
            else
               Set_Value (Canvas.Vadj, Lower);
            end if;

            Changed (Canvas.Vadj);
            return True;

         when GDK_Page_Down =>
            if Value + Page_Incr + Page_Size <= Upper then
               Set_Value (Canvas.Vadj, Value + Page_Incr);
            else
               Set_Value (Canvas.Vadj, Upper - Page_Size);
            end if;

            Changed (Canvas.Vadj);
            return True;

         when GDK_Up | GDK_KP_Up =>
            if Value - Step_Incr >= Lower then
               Set_Value (Canvas.Vadj, Value - Step_Incr);
            else
               Set_Value (Canvas.Vadj, Lower);
            end if;

            Changed (Canvas.Vadj);
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when GDK_Down | GDK_KP_Down =>
            if Value + Step_Incr + Page_Size <= Upper then
               Set_Value (Canvas.Vadj, Value + Step_Incr);
            else
               Set_Value (Canvas.Vadj, Upper - Page_Size);
            end if;

            Changed (Canvas.Vadj);
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when GDK_Left | GDK_KP_Left =>
            if Get_Value (Canvas.Hadj) -
              Get_Step_Increment (Canvas.Hadj) >=
                Get_Lower (Canvas.Hadj)
            then
               Set_Value (Canvas.Hadj,
                          Get_Value (Canvas.Hadj)
                          - Get_Step_Increment (Canvas.Hadj));
            else
               Set_Value (Canvas.Hadj,
                          Get_Lower (Canvas.Hadj));
            end if;

            Changed (Canvas.Hadj);
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when GDK_Right | GDK_KP_Right =>
            if Get_Value (Canvas.Hadj) +
              Get_Step_Increment (Canvas.Hadj) +
              Get_Page_Size (Canvas.Hadj) <=
                Get_Upper (Canvas.Hadj)
            then
               Set_Value (Canvas.Hadj,
                          Get_Value (Canvas.Hadj) +
                            Get_Step_Increment (Canvas.Hadj));
            else
               Set_Value (Canvas.Hadj,
                          Get_Upper (Canvas.Hadj) -
                            Get_Page_Size (Canvas.Hadj));
            end if;

            Changed (Canvas.Hadj);
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when others =>
            null;
      end case;

      return False;

   exception
      when others =>
         return False;
   end Key_Press;

   -------------------
   -- Point_In_Item --
   -------------------

   function Point_In_Item
     (Item   : access Canvas_Item_Record;
      X, Y   : Gint) return Boolean is
   begin
      return X >= Item.Coord.X
        and then X <= Item.Coord.X + Gint (Item.Coord.Width)
        and then Y >= Item.Coord.Y
        and then Y <= Item.Coord.Y + Gint (Item.Coord.Height);
   end Point_In_Item;

   -------------------------
   -- Item_At_Coordinates --
   -------------------------

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record;
      X, Y : Glib.Gint) return Canvas_Item
   is
      Tmp  : Vertex_Iterator := First (Canvas.Children);
      Result : Canvas_Item := null;
      Item : Canvas_Item;
   begin
      --  Keep the last item found, since this is the one on top.
      --  ??? Not the most efficient way to search, since we have to traverse
      --  the whole list every time.

      while not At_End (Tmp) loop
         Item := Canvas_Item (Get (Tmp));

         if Item.Visible and then Point_In_Item (Item, X, Y) then
            Result := Item;
         end if;

         Next (Tmp);
      end loop;

      return Result;
   end Item_At_Coordinates;

   -------------------------
   -- Item_At_Coordinates --
   -------------------------

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record; Event : Gdk_Event)
      return Canvas_Item
   is
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      X     : constant Gint := To_World_Coordinates
        (Canvas, Gint (Get_X (Event)) + Xbase);
      Y     : constant Gint := To_World_Coordinates
        (Canvas, Gint (Get_Y (Event)) + Ybase);

   begin
      return Item_At_Coordinates (Canvas, X, Y);
   end Item_At_Coordinates;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas   : Interactive_Canvas := Interactive_Canvas (Canv);
      Item     : Canvas_Item;
      Xbase    : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase    : constant Gint := Gint (Get_Value (Canvas.Vadj));
      X        : constant Gint := To_World_Coordinates
        (Canvas, Gint (Get_X (Event)) + Xbase);
      Y        : constant Gint := To_World_Coordinates
        (Canvas, Gint (Get_Y (Event)) + Ybase);
      Cursor   : Gdk.Cursor.Gdk_Cursor;

   begin
      if Get_Window (Event) /= Get_Window (Canvas) then
         return False;
      end if;

      Grab_Focus (Canvas);
      Set_Flags (Canvas, Has_Focus);

      --  Find the selected item.

      Item := Item_At_Coordinates (Canvas, X, Y);

      if (Get_State (Event) and Control_Mask) = 0
        and then (Item = null or else not Is_Selected (Canvas, Item))
      then
         Clear_Selection (Canvas);
      end if;

      if Item /= null then
         Add_To_Selection (Canvas, Item);
         Set_X (Event, Gdouble (X - Item.Coord.X));
         Set_Y (Event, Gdouble (Y - Item.Coord.Y));
      else
         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);
      end if;

      --  Double-click events are transmitted directly to the item, and are
      --  not used to move an item.
      --  Clicks other than left mouse button are also transmitted directly.
      --  These only need to be transmitted to the last item we clicked on,
      --  not to the whole selection

      if Get_Event_Type (Event) = Gdk_2button_Press
        or else Get_Button (Event) /= 1
      then
         Set_Cursor (Get_Window (Canvas), null);
         if Item /= null then
            On_Button_Click (Item, Event);
         end if;
         return False;
      end if;

      --  Change the cursor to give visual feedback

      Gdk_New (Cursor, Fleur);
      Set_Cursor (Get_Window (Canvas), Cursor);
      Destroy (Cursor);

      --  Initialize the move

      Canvas.Last_X_Event :=
        To_World_Coordinates (Canvas, Gint (Get_X_Root (Event)));
      Canvas.Last_Y_Event :=
        To_World_Coordinates (Canvas, Gint (Get_Y_Root (Event)));
      Canvas.Bg_Selection_Width := 0;
      Canvas.Bg_Selection_Height := 0;
      Canvas.Mouse_Has_Moved := False;
      Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;

      --  Make sure that no other widget steal the events while we are
      --  moving an item.

      Grab_Add (Canvas);
      Deep_Copy (From => Event, To => Canvas.Event_Press);
      return True;

   exception
      when others =>
         return False;
   end Button_Pressed;

   ----------------------------------------
   -- Get_Background_Selection_Rectangle --
   ----------------------------------------

   function Get_Background_Selection_Rectangle
     (Canvas : access Interactive_Canvas_Record'Class) return Gdk_Rectangle
   is
      X : Gint := 0;
      Y : Gint := 0;
      W : Gint := Canvas.Bg_Selection_Width;
      H : Gint := Canvas.Bg_Selection_Height;

   begin
      if Canvas.Event_Press /= null then
         X := Gint (Get_X (Canvas.Event_Press));
         Y := Gint (Get_Y (Canvas.Event_Press));
      end if;

      if W < 0 then
         W := -W;
         X := X - W;
      end if;

      if H < 0 then
         H := -H;
         Y := Y - H;
      end if;

      return (X, Y, W, H);
   end Get_Background_Selection_Rectangle;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas      : Interactive_Canvas := Interactive_Canvas (Canv);
      Tmp         : Item_Selection_List;
      Xbase       : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase       : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Rect, Coord : Gdk_Rectangle;
      Iter        : Item_Iterator;

   begin
      Grab_Remove (Canvas);

      --  Restore the standard cursor
      Set_Cursor (Get_Window (Canvas), null);

      if Get_Window (Event) /= Get_Window (Canvas) then
         return False;
      end if;

      if Canvas.Selection = null then
         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);

         if Canvas.Dashed_Line_Visible then
            Draw_Dashed_Selection (Canvas);
            Canvas.Dashed_Line_Visible := False;
         end if;

         --  Select all the items inside the rectangle

         Rect := Get_Background_Selection_Rectangle (Canvas);
         Rect := (To_World_Coordinates (Canvas, Rect.X + Xbase),
                  To_World_Coordinates (Canvas, Rect.Y + Ybase),
                  To_World_Coordinates (Canvas, Rect.Width),
                  To_World_Coordinates (Canvas, Rect.Height));

         Iter := Start (Canvas);
         while Get (Iter) /= null loop
            Coord := Get_Coord (Get (Iter));

            --  Only items fully contained in the rectangle are selected
            if Rect.X <= Coord.X
              and then Coord.X + Coord.Width <= Rect.X + Rect.Width
              and then Rect.Y <= Coord.Y
              and then Coord.Y + Coord.Height <= Rect.Y + Rect.Height
            then
               Add_To_Selection (Canvas, Get (Iter));
            end if;

            Next (Iter);
         end loop;

         return False;
      end if;

      if Canvas.Scrolling_Timeout_Id /= 0 then
         Timeout_Remove (Canvas.Scrolling_Timeout_Id);
         Canvas.Scrolling_Timeout_Id := 0;
      end if;

      if Canvas.Mouse_Has_Moved then
         Tmp := Canvas.Selection;
         while Tmp /= null loop
            if Canvas.Align_On_Grid then
               Tmp.Item.Coord.X := Tmp.X - Tmp.X mod Gint (Canvas.Grid_Size);
               Tmp.Item.Coord.Y := Tmp.Y - Tmp.Y mod Gint (Canvas.Grid_Size);
            else
               Tmp.Item.Coord.X := Tmp.X;
               Tmp.Item.Coord.Y := Tmp.Y;
            end if;
            Tmp.Item.From_Auto_Layout := False;
            Tmp := Tmp.Next;
         end loop;

         Canvas.Dashed_Line_Visible := False;

         --  Scroll the canvas so as to show the first item from the selection
         Refresh_Canvas (Canvas);

      --  If the user did not move the mouse while it was pressed, this is
      --  because he only wanted to select the item.
      --  Note that even if multiple items are currently selected, only the one
      --  we actually clicked on should receive the button_click event.

      elsif Canvas.Event_Press /= null then

         --  The button-press event wasn't forwarded, since we were expecting
         --  that the item would move. We thus forward it now
         On_Button_Click (Canvas.Selection.Item, Canvas.Event_Press);

         Set_X (Event,
                Gdouble (To_World_Coordinates
                         (Canvas, Gint (Get_X (Event)) + Xbase))
                - Gdouble (Canvas.Selection.Item.Coord.X));
         Set_Y (Event,
                Gdouble (To_World_Coordinates
                         (Canvas, Gint (Get_Y (Event)) + Ybase))
                - Gdouble (Canvas.Selection.Item.Coord.Y));
         On_Button_Click (Canvas.Selection.Item, Event);
      end if;

      if Canvas.Event_Press /= null then
         Free (Canvas.Event_Press);
      end if;

      return False;

   exception
      when others =>
         return False;
   end Button_Release;

   ------------------------
   -- Test_Scrolling_Box --
   ------------------------

   procedure Test_Scrolling_Box
     (Canvas   : access Gtk.Widget.Gtk_Widget_Record'Class;
      X_Scroll : out Gint;
      Y_Scroll : out Gint)
   is
      C : constant Interactive_Canvas := Interactive_Canvas (Canvas);
      X, Y, X2, Y2 : Gint;
      Success : Boolean;
      Mask : Gdk_Modifier_Type;
      W : Gdk_Window;
   begin
      X_Scroll := 0;
      Y_Scroll := 0;

      Get_Pointer (null, X, Y, Mask, W);
      Get_Origin (Get_Window (Canvas), X2, Y2, Success);
      X := X - X2;
      Y := Y - Y2;

      if X < Scrolling_Margin then
         X_Scroll := -Gint (C.Surround_Box_Scroll);
      elsif X > Gint (Get_Allocation_Width (Canvas)) - Scrolling_Margin then
         X_Scroll := Gint (C.Surround_Box_Scroll);
      end if;

      if Y < Scrolling_Margin then
         Y_Scroll := -Gint (C.Surround_Box_Scroll);
      elsif Y > Gint (Get_Allocation_Height (Canvas)) - Scrolling_Margin then
         Y_Scroll := Gint (C.Surround_Box_Scroll);
      end if;
   end Test_Scrolling_Box;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Canv : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas : Interactive_Canvas := Interactive_Canvas (Canv);
      X_Scroll, Y_Scroll : Gint;
   begin
      if Get_Window (Event) /= Get_Window (Canvas) then
         return False;
      end if;

      if Canvas.Selection /= null then
         --  Are we in the scrolling box ? If yes, do not move the item
         --  directly, but establish the timeout callbacks that will take care
         --  of the scrolling
         Test_Scrolling_Box (Canv, X_Scroll, Y_Scroll);

         if X_Scroll /= 0 or else Y_Scroll /= 0 then
            if Canvas.Scrolling_Timeout_Id = 0 then
               Canvas.Scrolling_Timeout_Id := Canvas_Timeout.Add
                 (Timeout_Between_Scrolls, Scrolling_Timeout'Access, Canvas);
            end if;
            return False;
         end if;
      end if;

      if Canvas.Scrolling_Timeout_Id /= 0 then
         Timeout_Remove (Canvas.Scrolling_Timeout_Id);
         Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;
         Canvas.Scrolling_Timeout_Id := 0;
      end if;

      X_Scroll := Canvas.Last_X_Event;
      Canvas.Last_X_Event :=
        To_World_Coordinates (Canvas, Gint (Get_X_Root (Event)));
      Y_Scroll := Canvas.Last_Y_Event;
      Canvas.Last_Y_Event :=
        To_World_Coordinates (Canvas, Gint (Get_Y_Root (Event)));

      if not Move_Selection
        (Canvas,
         Canvas.Last_X_Event - X_Scroll,
         Canvas.Last_Y_Event - Y_Scroll)
      then
         Canvas.Last_X_Event := X_Scroll;
         Canvas.Last_Y_Event := Y_Scroll;
      end if;
      return False;
   end Button_Motion;

   -----------------------
   -- Scrolling_Timeout --
   -----------------------

   function Scrolling_Timeout (Canvas : Interactive_Canvas) return Boolean is
      X_Scroll, Y_Scroll : Gint;
   begin
      Test_Scrolling_Box (Canvas, X_Scroll, Y_Scroll);
      if (X_Scroll /= 0 or else Y_Scroll /= 0)
        and then Move_Selection (Canvas, X_Scroll, Y_Scroll)
      then
         --  Keep increasing the speed
         if Canvas.Surround_Box_Scroll < Scrolling_Amount_Max then
            Canvas.Surround_Box_Scroll := Canvas.Surround_Box_Scroll
              * Scrolling_Amount_Increase;
         end if;

         --  Force an immediate draw, since Queue_Draw would only redraw in
         --  an idle event, and thus might not happen before the next timeout.
         --  With lots of items, this would break the scrolling.
         Draw (Canvas);
         return True;
      else
         Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;
         Canvas.Scrolling_Timeout_Id := 0;
         return False;
      end if;
   end Scrolling_Timeout;

   ---------------------------
   -- Draw_Dashed_Selection --
   ---------------------------

   procedure Draw_Dashed_Selection
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Selected : Item_Selection_List := Canvas.Selection;
      X, Y : Gint;
      Rect : Gdk_Rectangle;
   begin
      if Selected = null then
         Rect := Get_Background_Selection_Rectangle (Canvas);
         Draw_Rectangle
           (Get_Window (Canvas),
            GC     => Canvas.Anim_GC,
            Filled => False,
            X      => Rect.X,
            Y      => Rect.Y,
            Width  => Rect.Width,
            Height => Rect.Height);

      else
         while Selected /= null loop
            if Selected.Item.Visible then
               if Canvas.Align_On_Grid then
                  X := Selected.X;
                  Y := Selected.Y;
                  Selected.X := Selected.X -
                    Selected.X mod Gint (Canvas.Grid_Size);
                  Selected.Y := Selected.Y -
                    Selected.Y mod Gint (Canvas.Grid_Size);
               end if;

               Draw_Rectangle
                 (Get_Window (Canvas),
                  GC     => Canvas.Anim_GC,
                  Filled => False,
                  X      => To_Canvas_Coordinates (Canvas, Selected.X) - Xbase,
                  Y      => To_Canvas_Coordinates (Canvas, Selected.Y) - Ybase,
                  Width  => To_Canvas_Coordinates
                  (Canvas, Gint (Selected.Item.Coord.Width)),
                  Height => To_Canvas_Coordinates
                  (Canvas, Gint (Selected.Item.Coord.Height)));

               Update_Links (Canvas, Canvas.Anim_GC, True, Selected);

               if Canvas.Align_On_Grid then
                  Selected.X := X;
                  Selected.Y := Y;
               end if;
            end if;
            Selected := Selected.Next;
         end loop;
      end if;
   end Draw_Dashed_Selection;

   --------------------
   -- Move_Selection --
   --------------------

   function Move_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      Delta_X, Delta_Y : Gint) return Boolean
   is
      Selected : Item_Selection_List;
   begin
      if not Canvas.Mouse_Has_Moved then
         --  Is this a motion, or simply a selection ?

         if abs (Delta_X) <= Canvas.Motion_Threshold
           and then abs (Delta_Y) <= Canvas.Motion_Threshold
         then
            return False;
         end if;

      elsif Canvas.Dashed_Line_Visible then
         --  Delete the currently dashed lines
         Draw_Dashed_Selection (Canvas);
      end if;

      Canvas.Mouse_Has_Moved := True;
      Canvas.Dashed_Line_Visible := True;

      --  Move everything
      Selected := Canvas.Selection;
      while Selected /= null loop
         Selected.X := Selected.X + Delta_X;
         Selected.Y := Selected.Y + Delta_Y;
         Selected := Selected.Next;
      end loop;

      Canvas.Bg_Selection_Width  := Canvas.Bg_Selection_Width
        + To_Canvas_Coordinates (Canvas, Delta_X);
      Canvas.Bg_Selection_Height := Canvas.Bg_Selection_Height
        + To_Canvas_Coordinates (Canvas, Delta_Y);

      Draw_Dashed_Selection (Canvas);

      if Canvas.Selection /= null then
         --  We must set Report_Adj_Changed to False, otherwise we get flickers
         --  in the following scenario:
         --  Start with a canvas with no scrollbar. Then move one of the items
         --  to the left (in the scrollbox). The scrollbar would keep appearing
         --  and disappearing, and slow down the whole process.
         Show_Item
           (Canvas, Canvas.Selection.Item,
            Canvas.Selection.X, Canvas.Selection.Y,
            Report_Adj_Changed => False);
      end if;
      return True;
   end Move_Selection;

   ------------------
   -- Item_Updated --
   ------------------

   procedure Item_Updated
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
   begin
      if Item.Visible then
         Queue_Draw_Area
           (Canvas,
            To_Canvas_Coordinates (Canvas, Item.Coord.X) - Xbase,
            To_Canvas_Coordinates (Canvas, Item.Coord.Y) - Ybase,
            Gint (Item.Coord.Width
                  * GRectangle_Length (Canvas.Zoom) / 100),
            Gint (Item.Coord.Height
                  * GRectangle_Length (Canvas.Zoom) / 100));
      end if;
   end Item_Updated;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Remove_From_Selection (Canvas, Item);
      Remove (Canvas.Children, Item);

      --  Have to redraw everything, since there might have been some
      --  links.
      --  ??? Note very efficient when removing several items.
      Refresh_Canvas (Canvas);
   end Remove;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item   : access Canvas_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button)
   is
      pragma Unreferenced (Item, Event);
   begin
      null;
   end On_Button_Click;

   ---------------
   -- Get_Coord --
   ---------------

   function Get_Coord
     (Item : access Canvas_Item_Record) return Gdk.Rectangle.Gdk_Rectangle is
   begin
      return Item.Coord;
   end Get_Coord;

   --------------
   -- Has_Link --
   --------------

   function Has_Link
     (Canvas   : access Interactive_Canvas_Record;
      From, To : access Canvas_Item_Record'Class;
      Name     : String := "") return Boolean
   is
      Current : Edge_Iterator := First
        (Canvas.Children,
         Src  => Vertex_Access (From),
         Dest => Vertex_Access (To));
   begin
      while not At_End (Current) loop
         if Name = ""
           or else (Canvas_Link (Get (Current)).Descr /= null
                    and then Canvas_Link (Get (Current)).Descr.all = Name)
         then
            return True;
         end if;
         Next (Current);
      end loop;
      return False;
   end Has_Link;

   ----------------
   -- Lower_Item --
   ----------------

   procedure Lower_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Move_To_Front (Canvas.Children, Item);

      --  Redraw just the part of the canvas that is impacted.
      Item_Updated (Canvas, Item);
   end Lower_Item;

   ----------------
   -- Raise_Item --
   ----------------

   procedure Raise_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Move_To_Back (Canvas.Children, Item);

      --  Redraw just the part of the canvas that is impacted.
      Item_Updated (Canvas, Item);
   end Raise_Item;

   ---------------
   -- Is_On_Top --
   ---------------

   function Is_On_Top
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      Iter : Vertex_Iterator := First (Canvas.Children);
      Last : Canvas_Item := null;
   begin
      while not At_End (Iter) loop
         Last := Canvas_Item (Get (Iter));
         Next (Iter);
      end loop;
      return Last = Canvas_Item (Item);
   end Is_On_Top;

   ---------------
   -- Show_Item --
   ---------------

   procedure Show_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Gint;
      Report_Adj_Changed : Boolean := True)
   is
      X1 : constant Gint := To_Canvas_Coordinates (Canvas, X);
      Y1 : constant Gint := To_Canvas_Coordinates (Canvas, Y);
      X2 : constant Gint :=
        To_Canvas_Coordinates (Canvas, X + Gint (Item.Coord.Width));
      Y2 : constant Gint :=
        To_Canvas_Coordinates (Canvas, Y + Gint (Item.Coord.Height));
      Adj_Changed : Boolean := False;
   begin
      --  Do we need to scroll the canvas to the right to show the item?

      if X2 > Gint (Get_Upper (Canvas.Hadj)) then
         Set_Upper (Canvas.Hadj, Gdouble (X2));
         Adj_Changed := True;
      end if;

      --  In case the item is larger than the canvas, make sure we display
      --  its right-most part
      --      ----------------------------
      --     |                            |
      --     |                          ------
      --     |                         |      |
      --     |                         |      |
      --     |                          ------
      --     |                            |
      --      ----------------------------
      --     ^                            ^
      --   value                    value + page_size
      --                                      ^
      --                                      X2

      --  Do we need to scroll the canvas to the left ?

      if X1 < Gint (Get_Lower (Canvas.Hadj)) then
         Set_Lower (Canvas.Hadj, Gdouble (X1));
         Adj_Changed := True;
      end if;

      if Report_Adj_Changed and then Adj_Changed then
         Changed (Canvas.Hadj);
      end if;

      if X1 < Gint (Get_Value (Canvas.Hadj)) then
         Set_Value (Canvas.Hadj, Gdouble (X1));
      elsif Gdouble (X2) >
        Get_Value (Canvas.Hadj) + Get_Page_Size (Canvas.Hadj)
      then
         Set_Value (Canvas.Hadj, Gdouble (X2) - Get_Page_Size (Canvas.Hadj));
      end if;

      --  Do we need to scroll the canvas to the top to show the selection?

      Adj_Changed := False;

      if Y2 > Gint (Get_Upper (Canvas.Vadj)) then
         Set_Upper (Canvas.Vadj, Gdouble (Y2));
         Adj_Changed := True;
      end if;

      --  Do we need to scroll the canvas to the bottom ?

      if Y1 < Gint (Get_Lower (Canvas.Vadj)) then
         Set_Lower (Canvas.Vadj, Gdouble (Y1));
         Adj_Changed := True;
      end if;

      if Report_Adj_Changed and then Adj_Changed then
         Changed (Canvas.Vadj);
      end if;

      if Y1 < Gint (Get_Value (Canvas.Vadj)) then
         Set_Value (Canvas.Vadj, Gdouble (Y1));
      elsif Gdouble (Y2) >
        Get_Value (Canvas.Vadj) + Get_Page_Size (Canvas.Vadj)
      then
         Set_Value (Canvas.Vadj, Gdouble (Y2) - Get_Page_Size (Canvas.Vadj));
      end if;
   end Show_Item;

   ---------------
   -- Show_Item --
   ---------------

   procedure Show_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Show_Item (Canvas, Item, Item.Coord.X, Item.Coord.Y);
   end Show_Item;

   -----------------------
   -- Get_Align_On_Grid --
   -----------------------

   function Get_Align_On_Grid
     (Canvas : access Interactive_Canvas_Record) return Boolean is
   begin
      return Canvas.Align_On_Grid;
   end Get_Align_On_Grid;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (Item    : access Canvas_Item_Record;
      Visible : Boolean) is
   begin
      Item.Visible := Visible;
   end Set_Visibility;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Item : access Canvas_Item_Record) return Boolean is
   begin
      return Item.Visible;
   end Is_Visible;

   --------------------
   -- Refresh_Canvas --
   --------------------

   procedure Refresh_Canvas (Canvas : access Interactive_Canvas_Record) is
   begin
      Update_Adjustments (Canvas);
      Queue_Draw (Canvas);
   end Refresh_Canvas;

   ---------------------
   -- Clear_Selection --
   ---------------------

   procedure Clear_Selection (Canvas : access Interactive_Canvas_Record) is
      Tmp : Item_Selection_List;
   begin
      while Canvas.Selection /= null loop
         Tmp := Canvas.Selection;
         Canvas.Selection := Canvas.Selection.Next;

         if not Gtk.Object.In_Destruction_Is_Set (Canvas) then
            Selected (Tmp.Item, Canvas, Is_Selected => False);
         end if;

         Emit_By_Name_Item
           (Get_Object (Canvas), "item_unselected" & ASCII.NUL, Tmp.Item);
         Free (Tmp);
      end loop;
   end Clear_Selection;

   ----------------------
   -- Add_To_Selection --
   ----------------------

   procedure Add_To_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item : access Canvas_Item_Record'Class)
   is
      Tmp : Item_Selection_List := Canvas.Selection;
   begin
      --  Is the item already in the selection ?
      while Tmp /= null loop
         if Tmp.Item = Canvas_Item (Item) then
            return;
         end if;
         Tmp := Tmp.Next;
      end loop;

      --  No => Add it
      Canvas.Selection := new Item_Selection_List_Record'
        (Item => Canvas_Item (Item),
         X    => Item.Coord.X,
         Y    => Item.Coord.Y,
         Next => Canvas.Selection);

      Selected (Item, Canvas, Is_Selected => True);
      Emit_By_Name_Item
        (Get_Object (Canvas), "item_selected" & ASCII.NUL, Item);
   end Add_To_Selection;

   ---------------------------
   -- Remove_From_Selection --
   ---------------------------

   procedure Remove_From_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item : access Canvas_Item_Record'Class)
   is
      Tmp : Item_Selection_List := Canvas.Selection;
      Previous : Item_Selection_List := null;
   begin
      while Tmp /= null loop
         if Tmp.Item = Canvas_Item (Item) then
            if Previous = null then
               Canvas.Selection := Tmp.Next;
            else
               Previous.Next := Tmp.Next;
            end if;
            Free (Tmp);

            if not Gtk.Object.In_Destruction_Is_Set (Canvas) then
               Selected (Item, Canvas, Is_Selected => False);
            end if;

            Emit_By_Name_Item
              (Get_Object (Canvas), "item_unselected" & ASCII.NUL, Item);
            return;
         end if;
         Previous := Tmp;
         Tmp := Tmp.Next;
      end loop;

   end Remove_From_Selection;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Link   : access Canvas_Link_Record;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "") is
   begin
      Link.Arrow := Arrow;
      Free (Link.Descr);
      Link.Descr := new String'(Descr);
   end Configure;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "") is
   begin
      Configure (Link, Arrow, Descr);
      Add_Edge (Canvas.Children, Link, Src, Dest);
   end Add_Link;

   -----------------
   -- Remove_Link --
   -----------------

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class) is
   begin
      Remove (Canvas.Children, Link);
   end Remove_Link;

   -------------------
   -- For_Each_Link --
   -------------------

   procedure For_Each_Link
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Link_Processor;
      From, To : Canvas_Item := null)
   is
      Iter : Edge_Iterator := First
        (Canvas.Children, Vertex_Access (From), Vertex_Access (To));
      Link : Canvas_Link;
   begin
      while not At_End (Iter) loop
         Link := Canvas_Link (Get (Iter));
         Next (Iter);
         exit when not Execute (Canvas, Link);
      end loop;
   end For_Each_Link;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Link : in out Canvas_Link_Record) is
   begin
      if Link.Pixbuf /= null then
         Unref (Link.Pixbuf);
      end if;

      Free (Link.Descr);
   end Destroy;

   procedure Destroy (Item : in out Canvas_Item_Record) is
      pragma Unreferenced (Item);
   begin
      null;
   end Destroy;

   procedure Destroy (Item : in out Buffered_Item_Record) is
   begin
      if Item.Pixmap /= null then
         Gdk.Pixmap.Unref (Item.Pixmap);
      end if;
   end Destroy;

   ---------------
   -- Get_Descr --
   ---------------

   function Get_Descr (Link : access Canvas_Link_Record) return String is
   begin
      if Link.Descr = null then
         return "";
      else
         return Link.Descr.all;
      end if;
   end Get_Descr;

   -----------------
   -- Set_Src_Pos --
   -----------------

   procedure Set_Src_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Gfloat := 0.5) is
   begin
      Link.Src_X_Pos := X_Pos;
      Link.Src_Y_Pos := Y_Pos;
   end Set_Src_Pos;

   ------------------
   -- Set_Dest_Pos --
   ------------------

   procedure Set_Dest_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Gfloat := 0.5) is
   begin
      Link.Dest_X_Pos := X_Pos;
      Link.Dest_Y_Pos := Y_Pos;
   end Set_Dest_Pos;

   ------------------
   -- Zoom_Timeout --
   ------------------

   function Zoom_Timeout (Canvas : Interactive_Canvas) return Boolean is
      Z : constant Guint := Guint (Gint (Canvas.Zoom) + Canvas.Zoom_Step);
   begin
      if (Canvas.Zoom_Step > 0 and then Z >= Canvas.Target_Zoom)
        or else (Canvas.Zoom_Step < 0 and then Z <= Canvas.Target_Zoom)
      then
         Zoom_Internal (Canvas, Canvas.Target_Zoom);
         return False;
      else
         Zoom_Internal (Canvas, Z);

         --  Force an immediate draw, otherwise since Queue_Draw does its work
         --  in an idle loop, it might not complete before the next timeout
         Draw (Canvas);
         return True;
      end if;
   end Zoom_Timeout;

   -------------------
   -- Zoom_Internal --
   -------------------

   procedure Zoom_Internal
     (Canvas : access Interactive_Canvas_Record'Class; Percent : Guint)
   is
      Z  : constant Gdouble := Gdouble (Canvas.Zoom);
      H  : Gdouble := Get_Value (Canvas.Hadj) / Z * 100.0;
      V  : Gdouble := Get_Value (Canvas.Vadj) / Z * 100.0;
      Lh : Gdouble := Get_Page_Size (Canvas.Hadj) / Z * 100.0;
      Lv : Gdouble := Get_Page_Size (Canvas.Vadj) / Z * 100.0;
      L  : Gdouble;
   begin
      Canvas.Zoom := Percent;

      --  Display the proper area in the canvas
      --  When zooming out, we want to keep the old area centered into the
      --  new one.
      --  When zooming in, we want to keep the same center as before
      --  (reverse of zoom out)

      if Gdouble (Canvas.Zoom) < Z then  --  zoom out
         L := (Z / Gdouble (Canvas.Zoom) - 1.0) / 2.0;
         H := (H - L * Lh) * Gdouble (Canvas.Zoom) / 100.0;
         V := (V - L * Lv) * Gdouble (Canvas.Zoom) / 100.0;
         Lh := Lh * (L * 2.0 + 1.0) * Gdouble (Canvas.Zoom) / 100.0;
         Lv := Lv * (L * 2.0 + 1.0) * Gdouble (Canvas.Zoom) / 100.0;

      else  --  zoom in
         L := (1.0 - Z / Gdouble (Canvas.Zoom)) / 2.0;
         H := (H + Lh * L) * Gdouble (Canvas.Zoom) / 100.0;
         V := (V + Lv * L) * Gdouble (Canvas.Zoom) / 100.0;
         Lh := Lh * Z / 100.0;
         Lv := Lv * Z / 100.0;
      end if;

      Set_Lower (Canvas.Hadj, Gdouble'Min (H, Get_Lower (Canvas.Hadj)));
      Set_Upper (Canvas.Hadj, Gdouble'Max (H + Lh, Get_Upper (Canvas.Hadj)));

      Set_Lower (Canvas.Vadj, Gdouble'Min (V, Get_Lower (Canvas.Vadj)));
      Set_Upper (Canvas.Vadj, Gdouble'Max (V + Lv, Get_Upper (Canvas.Vadj)));

      if Canvas.Selection = null then
         Set_Value (Canvas.Hadj, H);
         Set_Value (Canvas.Vadj, V);
      else
         Show_Item
           (Canvas, Canvas.Selection.Item,
            Canvas.Selection.X, Canvas.Selection.Y,
            Report_Adj_Changed => True);
      end if;

      --  Do not report the modification to the scrollbar, since this does
      --  too much flickering and slows things done a lot when the canvas is
      --  in a scrolled_window whose policy is set on automatic.

      if Canvas.Zoom = Canvas.Target_Zoom then
         Changed (Canvas.Hadj);
         Changed (Canvas.Vadj);
      end if;

      Refresh_Canvas (Canvas);

      Widget_Callback.Emit_By_Name (Canvas, "zoomed");
   end Zoom_Internal;

   ----------
   -- Zoom --
   ----------

   procedure Zoom
     (Canvas : access Interactive_Canvas_Record;
      Percent : Guint := 100;
      Steps   : Guint := 1)
   is
      Id : Timeout_Handler_Id;
      pragma Warnings (Off, Id);
   begin
      if Canvas.Zoom = Percent then
         return;
      end if;
      Canvas.Target_Zoom := Percent;

      --  Do we want smooth scrolling ?
      if Steps > 1 then
         Canvas.Zoom_Step :=
           (Gint (Percent) - Gint (Canvas.Zoom)) / Gint (Steps);
         if Canvas.Zoom_Step = 0 then
            if Percent > Canvas.Zoom then
               Canvas.Zoom_Step := 1;
            else
               Canvas.Zoom_Step := -1;
            end if;
         end if;
         Id := Canvas_Timeout.Add
           (Timeout_Between_Zooms, Zoom_Timeout'Access,
            Interactive_Canvas (Canvas));

      else
         Zoom_Internal (Canvas, Percent);
      end if;
   end Zoom;

   --------------
   -- Get_Zoom --
   --------------

   function Get_Zoom
     (Canvas : access Interactive_Canvas_Record) return Glib.Guint is
   begin
      return Canvas.Zoom;
   end Get_Zoom;

   --------------
   -- Scrolled --
   --------------

   procedure Scrolled (Canvas : access Gtk_Widget_Record'Class) is
   begin
      Queue_Draw (Canvas);
   end Scrolled;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Item   : access Buffered_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      Xdest  : Glib.Gint;
      Ydest  : Glib.Gint)
   is
      Tmp, Tmp2 : Gdk_Pixbuf;
   begin
      if Canvas.Zoom = 100 then
         Draw_Pixmap
           (Drawable => Get_Window (Canvas),
            Gc       => GC,
            Src      => Item.Pixmap,
            Xsrc     => 0,
            Ysrc     => 0,
            Xdest    => Xdest,
            Ydest    => Ydest);

      else
         Tmp2 := Get_From_Drawable
           (Dest   => null,
            Src    => Item.Pixmap,
            Cmap   => Get_Colormap (Canvas),
            Src_X  => 0,
            Src_Y  => 0,
            Dest_X => 0,
            Dest_Y => 0,
            Width  => Item.Coord.Width,
            Height => Item.Coord.Height);

         Tmp := Scale_Simple
           (Src         => Tmp2,
            Dest_Width  => Get_Width (Tmp2) * Gint (Canvas.Zoom) / 100,
            Dest_Height => Get_Height (Tmp2) * Gint (Canvas.Zoom) / 100);

         Render_To_Drawable
           (Pixbuf   => Tmp,
            Drawable => Get_Window (Canvas),
            GC       => GC,
            Src_X    => 0,
            Src_Y    => 0,
            Dest_X   => Xdest,
            Dest_Y   => Ydest,
            Width    => Get_Width (Tmp),
            Height   => Get_Height (Tmp));

         Unref (Tmp2);
         Unref (Tmp);
      end if;
   end Draw;

   ---------------------
   -- Set_Screen_Size --
   ---------------------

   procedure Set_Screen_Size
     (Item   : access Buffered_Item_Record;
      Width, Height  : Glib.Gint) is
   begin
      Set_Screen_Size (Canvas_Item_Record (Item.all)'Access, Width, Height);

      if Item.Pixmap /= null then
         Gdk.Pixmap.Unref (Item.Pixmap);
      end if;

      Gdk_New (Item.Pixmap, null, Width, Height,
               Depth => Get_Best_Depth);
   end Set_Screen_Size;

   ------------
   -- Pixmap --
   ------------

   function Pixmap (Item : access Buffered_Item_Record) return Gdk_Pixmap is
   begin
      return Item.Pixmap;
   end Pixmap;

   --------------------
   -- Get_Arrow_Type --
   --------------------

   function Get_Arrow_Type
     (Link : access Canvas_Link_Record) return Arrow_Type is
   begin
      return Link.Arrow;
   end Get_Arrow_Type;

   --------------------------
   -- Set_Orthogonal_Links --
   --------------------------

   procedure Set_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record;
      Orthogonal : Boolean) is
   begin
      Canvas.Orthogonal_Links := Orthogonal;
   end Set_Orthogonal_Links;

   --------------------------
   -- Get_Orthogonal_Links --
   --------------------------

   function Get_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record) return Boolean is
   begin
      return Canvas.Orthogonal_Links;
   end Get_Orthogonal_Links;

   -------------------------
   -- Is_From_Auto_Layout --
   -------------------------

   function Is_From_Auto_Layout
     (Item : access Canvas_Item_Record) return Boolean is
   begin
      return Item.From_Auto_Layout;
   end Is_From_Auto_Layout;

   -----------
   -- Start --
   -----------

   function Start (Canvas : access Interactive_Canvas_Record)
      return Selection_Iterator is
   begin
      return Selection_Iterator (Canvas.Selection);
   end Start;

   ----------
   -- Next --
   ----------

   function Next (Iterator : Selection_Iterator)
      return Selection_Iterator is
   begin
      return Selection_Iterator (Iterator.Next);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Selection_Iterator) return Canvas_Item is
   begin
      if Iterator = null then
         return null;
      else
         return Iterator.Item;
      end if;
   end Get;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      Iter : Selection_Iterator := Start (Canvas);
   begin
      while Get (Iter) /= null loop
         if Get (Iter) = Canvas_Item (Item) then
            return True;
         end if;
         Iter := Next (Iter);
      end loop;
      return False;
   end Is_Selected;

   --------------
   -- Selected --
   --------------

   procedure Selected
     (Item        : access Canvas_Item_Record;
      Canvas      : access Interactive_Canvas_Record'Class;
      Is_Selected : Boolean)
   is
      pragma Unreferenced (Item, Canvas, Is_Selected);
   begin
      null;
   end Selected;

end Gtkada.Canvas;
