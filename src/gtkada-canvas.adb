-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
with Glib.GObjects;    use Glib.GObjects;
with Glib.Values;      use Glib.Values;
with Gdk.Color;        use Gdk.Color;
with Gdk.Cursor;       use Gdk.Cursor;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;           use Gdk.GC;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Types;        use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Window;       use Gdk.Window;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Handlers;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtk.Main;         use Gtk.Main;
pragma Elaborate_All (Gtk.Main);

with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtk.Object;       use Gtk.Object;
with System;
with Unchecked_Deallocation;

package body Gtkada.Canvas is

   use type Gdk_Font;
   use type Gdk_GC;
   use type Gdk_Window, Gdk_Pixmap;
   use type System.Address;

   Class_Record : System.Address := System.Null_Address;
   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused.
   --  ??? This is a global variable.

   Timeout_Between_Scrolls : constant := 10;
   --  Time between two scrollings when the mouse is in the bounding box.

   Timeout_Between_Zooms : constant := 20;
   --  Time between two zooms when smooth-scrolling the canvas

   Scrolling_Margin : constant := 10;
   --  Width and height of the surrounding box in which "infinite"
   --  scrolling is provided.

   Scrolling_Amount : constant := 4;
   --  Number of pixels to scroll while the mouse is in the surrounding box.

   Use_Double_Buffer : constant Boolean := True;
   --  Whether drawing should be done through a double buffer.
   --  The only reason to set this to False is if you don't have any link in
   --  the canvas and the items are fast to draw (for instance if they use
   --  double-buffer themselves).
   --  ??? This could be configurable at the canvas level
   --  ??? This might be set to False with gtk+2.0

   Scroll_Adj_Signal : constant := 4;
   Signals : constant chars_ptr_array :=
     (1 => New_String ("background_click"),
      2 => New_String ("item_selected"),
      3 => New_String ("zoomed"),
      Scroll_Adj_Signal => New_String ("set_scroll_adjustments"));
   --  Array of the signals created for this widget

   -----------------
   -- Subprograms --
   -----------------
   --  Note: Some callbacks take Gtk_Widget_Record parameters, so that we can
   --  reuse the callbacks in Gtkada.Handlers, and thus save a lot of space
   --  in the GtkAda library.

   procedure Free is new Unchecked_Deallocation (String, String_Access);
   procedure Free is new Unchecked_Deallocation
     (Canvas_Link_List_Record, Canvas_Link_List);
   procedure Free is new Unchecked_Deallocation
     (Canvas_Link_Record'Class, Canvas_Link);
   procedure Free is new Unchecked_Deallocation
     (Canvas_Item_List_Record, Canvas_Item_List);
   procedure Free is new Unchecked_Deallocation
     (Canvas_Item_Record'Class, Canvas_Item);
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
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Selected : in Item_Selection_List := null);
   --  Redraw all the links in the canvas.
   --  If Item is not null, only the links to or from Item are redrawn.

   function Configure_Handler
     (Canv : access Gtk_Widget_Record'Class) return Boolean;
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

   procedure Clip_Line
     (Rect  : Gdk.Rectangle.Gdk_Rectangle;
      To_X  : in Gint;
      To_Y  : in Gint;
      X_Pos : in Gfloat;
      Y_Pos : in Gfloat;
      X_Out : out Gint;
      Y_Out : out Gint);
   --  Clip the line that goes from the middle of Rect to (To_X, To_Y).
   --  The intersection between that line and the border of Rect is returned
   --  in (X_Out, Y_Out).
   --  X_Pos and Y_Pos have the same meaning as Src_X_Pos and Src_Y_Pos in the
   --  link record.

   procedure Draw_Straight_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link);
   --  Draw Link on the screen as a straight line.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Arc_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link;
      Offset : Gint);
   --  Draw Link on the screen.
   --  The link is drawn as a curved link (ie there is an extra handle in its
   --  middle).
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Self_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link;
      Offset : Gint);
   --  Draw a link when its source and destination items are the same

   procedure Update_Adjustments
     (Canvas : access Interactive_Canvas_Record'Class);
   --  Update the adjustments of the canvas.
   --  The bounds for the adjustments are automatically computed, given the
   --  list of items in it.

   procedure Draw_Arrow_Head
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Angle  : in Float);
   --  Draw an arrow head at the position (X, Y) on the canvas. The position
   --  is given in pixels, and should include zoom processing.
   --  Angle is the angle of the main axis of the arrow.

   procedure Draw_Annotation
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Str    : String_Access);
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
   --  The returned values are in canvas coordinates

   procedure Test_Scrolling_Box
     (Canvas   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Win      : Gdk_Window;
      X, Y     : Gint;
      X_Scroll : out Gint;
      Y_Scroll : out Gint);
   --  We keep moving the selection (and scrolling the canvas) as long as the
   --  mouse remains in a surrounding box around the canvas. This is done even
   --  if the mouse doesn't move, so at to make it easier to move items.
   --  This subprogram tests whether (X, Y) is found in that box, and returns
   --  the extra scrolling that should be done. (0, 0) is returned if the
   --  mouse is not in that box.

   function Scrolling_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Function called repeatedly while the mouse is in the scrolling box.
   --  This provides scrolling even when the mouse doesn't move

   function Move_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      Delta_X, Delta_Y : Gint) return Boolean;
   --  Moves the selected items by a given number of pixels.
   --  Return True if the selection was actually moved, False if for some
   --  reason nothing happened

   procedure Show_Item (Canvas : access Interactive_Canvas_Record'Class;
                        Item   : access Canvas_Item_Record'Class;
                        X, Y   : Gint);
   --  Like Show_Item, but use X, Y for the coordinates instead of the
   --  ones available in Item.

   procedure Draw_Dashed_Selection
     (Canvas : access Interactive_Canvas_Record'Class);
   --  Draw all the selected items and links with dashed-lines

   function Zoom_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Timeout function used to provide smooth zooming.

   procedure Zoom_Internal
     (Canvas : access Interactive_Canvas_Record'Class; Percent : Guint);
   --  Internal function to implement zooming

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

   procedure Gtk_New (Canvas : out Interactive_Canvas) is
   begin
      Canvas := new Interactive_Canvas_Record;
      Gtkada.Canvas.Initialize (Canvas);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Canvas : access Interactive_Canvas_Record'Class) is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => Gdk.Event.Get_Type, 2 => GType_None),
         2 => (1 => GType_Pointer,      2 => GType_None),
         3 => (1 => GType_Uint,         2 => GType_None),
         4 => (1 => GType_Object,       2 => GType_Object));
      --  the parameters for the above signals.
      --  This must be defined in this function rather than at the
      --  library-level, or the value of Gdk_Event.Get_Type is not yet
      --  initialized.

   begin
      Gtk.Drawing_Area.Initialize (Canvas);

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note also that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Gtk.Object.Initialize_Class_Record
        (Canvas, Signals, Class_Record, Signal_Parameters, Scroll_Adj_Signal);

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
      Return_Callback.Connect
        (Canvas, "configure_event",
         Return_Callback.To_Marshaller (Configure_Handler'Access));
      Widget_Callback.Connect
        (Canvas, "set_scroll_adjustments", Set_Scroll_Adjustments'Access);
      Widget_Callback.Connect
        (Canvas, "destroy",
         Widget_Callback.To_Marshaller (Canvas_Destroyed'Access));

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
      Set_Double_Buffered (Canvas, False);

      --  Configure with default values
      Configure (Canvas);
      Set_Scroll_Adjustments (Canvas, Null_Adjustment, Null_Adjustment);
   end Initialize;

   ----------------------
   -- Canvas_Destroyed --
   ----------------------

   procedure Canvas_Destroyed (Canvas : access Gtk_Widget_Record'Class) is
      C : Interactive_Canvas := Interactive_Canvas (Canvas);
      Tmp, Child : Canvas_Item_List;
      Tmp2, Link_L : Canvas_Link_List;
      Link, Tmp_Link : Canvas_Link;
   begin
      if C.Scrolling_Timeout_Id /= 0 then
         Timeout_Remove (C.Scrolling_Timeout_Id);
      end if;

      Clear_Selection (C);

      Child := C.Children;
      while Child /= null loop
         Tmp := Child.Next;
         Destroy (Child.Item);
         Free (Child.Item);
         Free (Child);
         Child := Tmp;
      end loop;

      Link_L := C.Links;
      while Link_L /= null loop
         Tmp2 := Link_L.Next;
         Link := Link_L.Link;
         while Link /= null loop
            Tmp_Link := Link.Sibling;
            Destroy (Link);
            Free (Link);
            Link := Tmp_Link;
         end loop;
         Free (Link_L);
         Link_L := Tmp2;
      end loop;

      Free (C.Annotation_Font);
      Unref (C.Clear_GC);
      Unref (C.Black_GC);
      Unref (C.Anim_GC);
      Unref (C.Font);
      Destroy (C.Hadj);
      Destroy (C.Vadj);
      Gdk.Pixmap.Unref (C.Double_Buffer);
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
      Annotation_Font   : String := Default_Annotation_Font;
      Annotation_Height : Gint := Default_Annotation_Height;
      Arc_Link_Offset   : Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Gint := Default_Arrow_Angle;
      Arrow_Length      : Gint := Default_Arrow_Length;
      Motion_Threshold  : Gint := Default_Motion_Threshold) is
   begin
      Canvas.Grid_Size := Grid_Size;

      if Grid_Size < 2 then
         Canvas.Align_On_Grid := False;
      end if;

      Free (Canvas.Annotation_Font);
      Canvas.Annotation_Font := new String'(Annotation_Font);
      Canvas.Annotation_Height := Annotation_Height;
      Canvas.Arc_Link_Offset := Arc_Link_Offset;
      Canvas.Arrow_Angle := Float (Arrow_Angle) * Ada.Numerics.Pi / 180.0;
      Canvas.Arrow_Length := Arrow_Length;
      Canvas.Motion_Threshold := Motion_Threshold;

      if Canvas.Font /= Null_Font then
         Unref (Canvas.Font);
      end if;

      Canvas.Font :=
        Get_Gdkfont (Canvas.Annotation_Font.all, Canvas.Annotation_Height);
   end Configure;

   -----------------------
   -- Configure_Handler --
   -----------------------

   function Configure_Handler
     (Canv : access Gtk_Widget_Record'Class) return Boolean
   is
      Canvas : Interactive_Canvas := Interactive_Canvas (Canv);
   begin
      Set_Page_Size (Canvas.Hadj, Gdouble (Get_Allocation_Width (Canvas)));
      Set_Step_Increment (Canvas.Hadj, 10.0);
      Set_Page_Increment (Canvas.Hadj, Get_Page_Size (Canvas.Hadj) / 2.0);

      Set_Page_Size (Canvas.Vadj, Gdouble (Get_Allocation_Height (Canvas)));
      Set_Step_Increment (Canvas.Vadj, 10.0);
      Set_Page_Increment (Canvas.Vadj, Get_Page_Size (Canvas.Vadj) / 2.0);

      Update_Adjustments (Canvas);

      if Canvas.Double_Buffer /= null then
         Gdk.Pixmap.Unref (Canvas.Double_Buffer);
      end if;
      Gdk_New
        (Canvas.Double_Buffer, Get_Window (Canvas),
         Gint (Get_Allocation_Width (Canvas)),
         Gint (Get_Allocation_Height (Canvas)));
      return False;
   end Configure_Handler;

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
      Current : Canvas_Item_List := Canvas.Children;
      X, Y : Gint;
   begin
      if Current = null then
         X_Min := 0;
         X_Max := 0;
         Y_Min := 0;
         Y_Max := 0;

      else
         X_Min := Gint'Last;
         X_Max := Gint'First;
         Y_Min := Gint'Last;
         Y_Max := Gint'First;

         while Current /= null loop
            if Current.Item.Visible then
               X := To_Canvas_Coordinates (Canvas, Current.Item.Coord.X);
               Y := To_Canvas_Coordinates (Canvas, Current.Item.Coord.Y);
               X_Max := Gint'Max (X_Max,  X + Gint (Current.Item.Coord.Width));
               X_Min := Gint'Min (X_Min, X);
               Y_Max := Gint'Max (Y_Max, Y + Gint (Current.Item.Coord.Height));
               Y_Min := Gint'Min (Y_Min, Y);
            end if;

            Current := Current.Next;
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

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First)
   is
      Step  : Gint := Gint (Canvas.Grid_Size);
      Zoom_Step : Gint;

      function Location_Is_Free (X, Y : Gint; Return_X : Boolean) return Gint;
      --  Return X if the location (X, Y) for the new item would be
      --  acceptable,
      --  Return the next X coordinate (if Return_X is true) or Y coordinate
      --  (otherwise) to test if (X, Y) overlaps an existing item.
      --  Keeps a space of at least Grid_Size around each item.

      ----------------------
      -- Location_Is_Free --
      ----------------------

      function Location_Is_Free (X, Y : Gint; Return_X : Boolean)
         return Gint
      is
         Tmp   : Canvas_Item_List := Canvas.Children;
         Dest  : Gdk.Rectangle.Gdk_Rectangle;
         Inter : Boolean := False;
         W     : constant Gint := Item.Coord.Width;
         H     : constant Gint := Item.Coord.Height;
         X_Tmp, Y_Tmp : Gint;
      begin
         --  Keep an appropriate marging around the item.
         Item.Coord.X := X - Zoom_Step;
         Item.Coord.Y := Y - Zoom_Step;
         Item.Coord.Width := W + 2 * Gint (Zoom_Step);
         Item.Coord.Height := H + 2 * Gint (Zoom_Step);

         while Tmp /= null loop
            if Tmp.Item.Visible
              and then Tmp.Item /= Canvas_Item (Item)
            then
               X_Tmp := To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.X);
               Y_Tmp := To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.Y);
               Intersect ((X_Tmp,
                           Y_Tmp,
                           Tmp.Item.Coord.Width,
                           Tmp.Item.Coord.Height),
                          Item.Coord, Dest, Inter);
               exit when Inter;
            end if;

            Tmp := Tmp.Next;
         end loop;

         Item.Coord.Width := W;
         Item.Coord.Height := H;
         if Return_X then
            if Inter then
               return X_Tmp + Zoom_Step + Gint (Tmp.Item.Coord.Width);
            end if;
            return X;
         else
            if Inter then
               return Y_Tmp + Zoom_Step + Gint (Tmp.Item.Coord.Height);
            end if;
            return Y;
         end if;
      end Location_Is_Free;

      Src_Item : Canvas_Item := null;
      Links    : Canvas_Link_List := Canvas.Links;
      X1, X2   : Gint;
      Y1       : Gint;

   begin
      --  When no grid is drawn...

      if Step = 0 then
         Step := Gint (Default_Grid_Size);
      end if;
      Zoom_Step := To_Canvas_Coordinates (Canvas, Step);

      if X /= Gint'First and then Y /= Gint'First then
         Item.Coord.X := X;
         Item.Coord.Y := Y;
      else
         --  Check if there is any link that has for destination the widget we
         --  are adding.

         while Links /= null loop
            if Links.Link.Src = Canvas_Item (Item)
              or else Links.Link.Dest = Canvas_Item (Item)
            then
               Src_Item := Links.Link.Src;
               exit;
            end if;
            Links := Links.Next;
         end loop;

         --  The rule is the following when we have a link to an existing
         --  item: We first try to put the new item below the old
         --  one, then, if that place is already occupied, to the
         --  bottom-right, then the bottom-left, then two down, ...

         if Src_Item /= null then
            declare
               Num : Gint := 0;
               Next_Y : array (Gint range 0 .. 2) of Gint;
               X3 : constant Gint :=
                 To_Canvas_Coordinates (Canvas, Src_Item.Coord.X);
            begin
               Y1 := To_Canvas_Coordinates (Canvas, Src_Item.Coord.Y)
                 + Gint (Src_Item.Coord.Height) + Zoom_Step;
               Next_Y := (others => Y1);

               loop
                  case Num mod 3 is
                     when 0 =>
                        X1 := X3;
                     when 1 =>
                        X1 := X3 - Zoom_Step - Gint (Item.Coord.Width);
                     when 2 =>
                        X1 := X3 + Zoom_Step + Gint (Src_Item.Coord.Width);
                     when others => null;
                  end case;

                  Y1 := Next_Y (Num mod 3);
                  X2 := Location_Is_Free (X1, Y1, False);
                  exit when  X2 = Y1;
                  Next_Y (Num mod 3) := X2;

                  Num := Num + 1;
               end loop;
            end;

         --  Else put the item in the first line, at the first possible
         --  location

         else
            X1 := Gint (Get_Lower (Canvas.Hadj)) + Zoom_Step;
            Y1 := Gint (Get_Lower (Canvas.Vadj)) + Zoom_Step;

            loop
               X2 := Location_Is_Free (X1, Y1, True);
               exit when X2 = X1;
               X1 := X2;
            end loop;
         end if;

         Item.Coord.X := To_World_Coordinates (Canvas, X1);
         Item.Coord.Y := To_World_Coordinates (Canvas, Y1);
      end if;

      if Canvas.Align_On_Grid then
         if Item.Coord.X mod Step /= 0 then
            Item.Coord.X := Item.Coord.X + Step - Item.Coord.X mod Step;
         end if;
         if Item.Coord.Y mod Step /= 0 then
            Item.Coord.Y := Item.Coord.Y + Step - Item.Coord.Y mod Step;
         end if;
      end if;
   end Move_To;

   ---------
   -- Put --
   ---------

   procedure Put
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Gint := Gint'First) is
   begin
      Canvas.Children := new Canvas_Item_List_Record
        '(Item => Canvas_Item (Item),
          Next => Canvas.Children);
      Move_To (Canvas, Item, X, Y);
      Update_Adjustments (Canvas);
   end Put;

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
         Set_Exposures
           (Get_Background_GC (Get_Style (Canvas), State_Normal), False);

         Gdk_New (Canv.Black_GC, Get_Window (Canvas));
         Set_Foreground
           (Canv.Black_GC, Black (Gtk.Widget.Get_Default_Colormap));
         Set_Exposures (Canv.Black_GC, False);

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
         Set_Line_Attributes
           (Canv.Anim_GC,
            Line_Width => 0,
            Line_Style => Line_On_Off_Dash,
            Cap_Style  => Cap_Butt,
            Join_Style => Join_Miter);
         Set_Exposures (Canv.Anim_GC, False);
      end if;
   end Realized;

   -------------------
   -- For_Each_Item --
   -------------------

   procedure For_Each_Item
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Item_Processor)
   is
      Item : Canvas_Item_List := Canvas.Children;
      Tmp  : Canvas_Item_List;
   begin
      while Item /= null loop
         Tmp := Item.Next;
         exit when not Execute (Canvas, Item.Item);
         Item := Tmp;
      end loop;
   end For_Each_Item;

   ---------------
   -- Clip_Line --
   ---------------

   procedure Clip_Line
     (Rect  : Gdk_Rectangle;
      To_X  : in Gint;
      To_Y  : in Gint;
      X_Pos : in Gfloat;
      Y_Pos : in Gfloat;
      X_Out : out Gint;
      Y_Out : out Gint)
   is
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
            --  Intersection with north side ?
            Y_Out := Rect.Y;
         else
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
            --  Intersection with West side ?
            X_Out := Rect.X;
         else
            --  Intersection with East side ?
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
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Angle  : in Float)
   is
      Length : constant Float :=
        Float (To_Canvas_Coordinates (Canvas, Canvas.Arrow_Length));
   begin
      Draw_Polygon
        (Window,
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
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      X, Y   : Gint;
      Str    : String_Access) is
   begin
      --  Do not draw the text in Xor mode, since this doesn't work on
      --  Windows systems, and doesn't provide any real information anyway.
      if GC /= Canvas.Anim_GC and then Canvas.Font /= null then
         Draw_Text
           (Window,
            Canvas.Font,
            GC,
            X - String_Width (Canvas.Font, Str.all) / 2,
            Y - String_Height (Canvas.Font, Str.all) / 2,
            Text => Str.all);
      end if;
   end Draw_Annotation;

   ------------------------
   -- Draw_Straight_Link --
   ------------------------

   procedure Draw_Straight_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link)
   is
      X1, Y1, X2, Y2 : Gint;
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Sx : constant Gint := Link.Src.Coord.X;
      Sy : constant Gint := Link.Src.Coord.Y;
      Dx : constant Gint := Link.Dest.Coord.X;
      Dy : constant Gint := Link.Dest.Coord.Y;
   begin
      Link.Src.Coord.X := To_Canvas_Coordinates (Canvas, Sx) - Xbase;
      Link.Src.Coord.Y := To_Canvas_Coordinates (Canvas, Sy) - Ybase;
      Link.Dest.Coord.X := To_Canvas_Coordinates (Canvas, Dx) - Xbase;
      Link.Dest.Coord.Y := To_Canvas_Coordinates (Canvas, Dy) - Ybase;
      Clip_Line
        (Link.Src.Coord,
         Link.Dest.Coord.X
            + Gint (Gfloat (Link.Dest.Coord.Width) * Link.Dest_X_Pos),
         Link.Dest.Coord.Y
            + Gint (Gfloat (Link.Dest.Coord.Height) * Link.Dest_Y_Pos),
         X_Pos => Link.Src_X_Pos, Y_Pos => Link.Src_Y_Pos,
         X_Out => X1, Y_Out => Y1);
      Clip_Line
        (Link.Dest.Coord,
         Link.Src.Coord.X
            + Gint (Gfloat (Link.Src.Coord.Width) * Link.Src_X_Pos),
         Link.Src.Coord.Y
            + Gint (Gfloat (Link.Src.Coord.Height) * Link.Src_Y_Pos),
         X_Pos => Link.Dest_X_Pos, Y_Pos => Link.Dest_Y_Pos,
         X_Out => X2, Y_Out => Y2);
      Link.Src.Coord.X := Sx;
      Link.Src.Coord.Y := Sy;
      Link.Dest.Coord.X := Dx;
      Link.Dest.Coord.Y := Dy;

      --  Draw the link itself

      Draw_Line (Window, GC, X1, Y1, X2, Y2);

      --  Draw the end arrow head

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Draw_Arrow_Head
              (Canvas, Window, GC, X2, Y2,
               Arctan (Float (Y1 - Y2), Float (X1 - X2)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head
              (Canvas, Window, GC, X2, Y2, Ada.Numerics.Pi / 2.0);
         else
            Draw_Arrow_Head
              (Canvas, Window, GC, X2, Y2, -Ada.Numerics.Pi / 2.0);
         end if;
      end if;

      --  Draw the start arrow head

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Draw_Arrow_Head
              (Canvas, Window, GC, X1, Y1,
               Arctan (Float (Y2 - Y1), Float (X2 - X1)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head
              (Canvas, Window, GC, X1, Y1, -Ada.Numerics.Pi / 2.0);
         else
            Draw_Arrow_Head
              (Canvas, Window, GC, X1, Y1, +Ada.Numerics.Pi / 2.0);
         end if;
      end if;

      --  Draw the text if any

      if Link.Descr /= null then
         Draw_Annotation
           (Canvas, Window, GC,
            (X1 + X2) / 2,
            (Y1 + Y2) / 2,
            Link.Descr);
      end if;
   end Draw_Straight_Link;

   --------------------
   -- Draw_Self_Link --
   --------------------

   procedure Draw_Self_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link;
      Offset : Gint)
   is
      Xbase      : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase      : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Arc_Offset : constant Float :=
        Float (To_Canvas_Coordinates (Canvas, Canvas.Arc_Link_Offset));
      Right_Angle : constant Float := Ada.Numerics.Pi / 2.0;
      X1, Y1, X3, Y3, Xc, Yc, Radius : Gint;

   begin
      pragma Assert (Link.Src = Link.Dest);
      Xc := To_Canvas_Coordinates
        (Canvas, Link.Src.Coord.X) + Gint (Link.Src.Coord.Width) - Xbase;
      Yc := To_Canvas_Coordinates (Canvas, Link.Src.Coord.Y) - Ybase;
      Radius := Gint (Arc_Offset) / 2 * Offset;

      --  Location of the arrow and the annotation
      X3 := Xc - Radius;
      Y3 := Yc;
      X1 := Xc;
      Y1 := Yc + Radius;

      Draw_Arc (Window,
                GC,
                Filled => False,
                X      => Xc - Radius,
                Y      => Yc - Radius,
                Width  => Radius * 2,
                Height => Radius * 2,
                Angle1 => -90 * 64,
                Angle2 => 270 * 64);

      --  Draw the arrows

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, Window, GC, X3, Y3, -Right_Angle);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, Window, GC, X1, Y1, 0.0);
      end if;

      --  Draw the annotations
      if Link.Descr /= null then
         Draw_Annotation
           (Canvas, Window, GC, Xc + Radius / 2, Yc + Radius / 2, Link.Descr);
      end if;
   end Draw_Self_Link;

   -------------------
   -- Draw_Arc_Link --
   -------------------

   procedure Draw_Arc_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link;
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
               Draw_Line (Window, GC, Old_X, Old_Y, New_X, New_Y);
               Old_X := New_X;
               Old_Y := New_Y;
            end if;

            New_X2 := Gint (C * Float (X1) + B2 + A * Float (X3));
            New_Y2 := Gint (C * Float (Y1) + B + A * Float (Y3));
            if Old_X2 /= New_X2 or else Old_Y2 /= New_Y2 then
               Draw_Line (Window, GC, Old_X2, Old_Y2, New_X2, New_Y2);
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
      Sx          : constant Gint := Link.Src.Coord.X;
      Sy          : constant Gint := Link.Src.Coord.Y;
      Dx          : constant Gint := Link.Dest.Coord.X;
      Dy          : constant Gint := Link.Dest.Coord.Y;
   begin
      --  We will first compute the extra intermediate point between the
      --  center of the two items. Once we have this intermediate point, we
      --  will be able to use the intersection point between the two items
      --  and the two lines from the centers to the middle point. This extra
      --  point is used as a control point for the Bezier curve.

      Link.Src.Coord.X := To_Canvas_Coordinates (Canvas, Sx) - Xbase;
      Link.Src.Coord.Y := To_Canvas_Coordinates (Canvas, Sy) - Ybase;
      Link.Dest.Coord.X := To_Canvas_Coordinates (Canvas, Dx) - Xbase;
      Link.Dest.Coord.Y := To_Canvas_Coordinates (Canvas, Dy) - Ybase;

      X1 := Link.Src.Coord.X +
        Gint (Gfloat (Link.Src.Coord.Width) * Link.Src_X_Pos);
      Y1 := Link.Src.Coord.Y +
        Gint (Gfloat (Link.Src.Coord.Height) * Link.Src_Y_Pos);
      X3 := Link.Dest.Coord.X +
        Gint (Gfloat (Link.Dest.Coord.Width) * Link.Dest_X_Pos);
      Y3 := Link.Dest.Coord.Y +
        Gint (Gfloat (Link.Dest.Coord.Height) * Link.Dest_Y_Pos);

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
        (Link.Src.Coord, X2, Y2, Link.Src_X_Pos, Link.Src_Y_Pos, X1, Y1);
      Clip_Line
        (Link.Dest.Coord, X2, Y2, Link.Dest_X_Pos, Link.Dest_Y_Pos, X3, Y3);

      Link.Src.Coord.X := Sx;
      Link.Src.Coord.Y := Sy;
      Link.Dest.Coord.X := Dx;
      Link.Dest.Coord.Y := Dy;

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
         Draw_Arrow_Head (Canvas, Window, GC, X3, Y3, Angle);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Angle := Arctan (Float (Y2 - Y1), Float (X2 - X1));
         elsif Y2 > Y1 then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;
         Draw_Arrow_Head (Canvas, Window, GC, X1, Y1, Angle);
      end if;

      --  Draw the annotations, if any, in the middle of the link
      if Link.Descr /= null then
         X2 := Gint (0.25 * Float (X1) + 0.5 * Float (X2) + 0.25 * Float (X3));
         Y2 := Gint (0.25 * Float (Y1) + 0.5 * Float (Y2) + 0.25 * Float (Y3));
         Draw_Annotation (Canvas, Window, GC, X2, Y2, Link.Descr);
      end if;
   end Draw_Arc_Link;

   ------------------
   -- Update_Links --
   ------------------

   procedure Update_Links
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Selected : in Item_Selection_List := null)
   is
      Current : Canvas_Link_List := Canvas.Links;
      Link    : Canvas_Link;
      X, Y    : Gint;
      Offset  : Gint;
      O       : Gint;
   begin
      if Selected /= null then
         X := Selected.Item.Coord.X;
         Y := Selected.Item.Coord.Y;
         Selected.Item.Coord.X := Selected.X;
         Selected.Item.Coord.Y := Selected.Y;
      end if;

      while Current /= null loop
         if Is_Visible (Current.Link.Src)
           and then Is_Visible (Current.Link.Dest)
           and then
           (Selected = null
            or else Current.Link.Src = Canvas_Item (Selected.Item)
            or else Current.Link.Dest = Canvas_Item (Selected.Item))
         then
            --  Self-referencing links
            if Current.Link.Src = Current.Link.Dest then
               Link := Current.Link;
               Offset := 1;
               while Link /= null loop
                  Draw_Self_Link (Canvas, Window, GC, Link, Offset);
                  Link := Link.Sibling;
                  Offset := Offset + 1;
               end loop;

            else
               --  The first link in the list is always straight
               Draw_Straight_Link (Canvas, Window, GC, Current.Link);

               Link := Current.Link.Sibling;
               Offset := 1;
               while Link /= null loop

                  --  Do we need to reverse the link ? If we don't do that,
                  --  then the link will hide another link.
                  O := -Offset;
                  if Link.Src /= Current.Link.Src then
                     O := Offset;
                  end if;
                  Draw_Arc_Link (Canvas, Window, GC, Link, O);
                  Link := Link.Sibling;

                  if Link /= null then
                     O := Offset;
                     if Link.Src /= Current.Link.Src then
                        O := -Offset;
                     end if;
                     Draw_Arc_Link (Canvas, Window, GC, Link, O);
                     Link := Link.Sibling;
                  end if;
                  Offset := Offset + 1;
               end loop;
            end if;
         end if;
         Current := Current.Next;
      end loop;

      if Selected /= null then
         Selected.Item.Coord.X := X;
         Selected.Item.Coord.Y := Y;
      end if;
   end Update_Links;

   ------------
   -- Expose --
   ------------

   function Expose
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      Canvas : Interactive_Canvas := Interactive_Canvas (Canv);
      Rect : Gdk_Rectangle := Get_Area (Event);
      Tmp : Canvas_Item_List := Canvas.Children;
      X, Y : Gint;
      Xmin : Gint;
      Dest : Gdk_Rectangle;
      Inters : Boolean;
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Grid  : constant Gint :=
        To_Canvas_Coordinates (Canvas, Gint (Canvas.Grid_Size));
      Pix : Gdk_Pixmap;

   begin
      if Use_Double_Buffer then
         Pix := Canvas.Double_Buffer;
      else
         Pix := Get_Window (Canvas);
      end if;

      --  If the GC was not created, do not do anything

      if Canvas.Clear_GC = Null_GC
        or else (Use_Double_Buffer and then Canvas.Double_Buffer = null)
      then
         return False;
      end if;

      --  Clear the canvas

      Draw_Rectangle
        (Pix,
         Get_Background_GC (Get_Style (Canvas), State_Normal),
         Filled => True,
         X => Rect.X, Y => Rect.Y,
         Width  => Gint (Rect.Width),
         Height => Gint (Rect.Height));

      Set_Clip_Rectangle (Canvas.Black_GC, Rect);

      --  Draw the background dots.

      if Grid >= 3 then
         if (Rect.Y + Ybase) mod Grid = 0 then
            Y := Rect.Y;
         else
            Y := Rect.Y + Grid - (Rect.Y + Ybase) mod Grid;
         end if;
         if (Rect.X + Xbase) mod Grid = 0 then
            Xmin := Rect.X;
         else
            Xmin := Rect.X + Grid - (Rect.X + Xbase) mod Grid;
         end if;

         while Y <= Rect.Y + Gint (Rect.Height) loop
            X := Xmin;

            while X <= Rect.X + Gint (Rect.Width) loop
               Draw_Point (Pix, Canvas.Black_GC, X, Y);
               X := X + Grid;
            end loop;

            Y := Y + Grid;
         end loop;
      end if;

      --  Draw the links first, so that they appear to be below the items.
      --  ??? Should redraw only the required links

      Update_Links (Canvas, Pix, Canvas.Black_GC);

      --  Draw each of the items.

      while Tmp /= null loop
         if Tmp.Item.Visible then
            Intersect
              (Rect,
               (To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.X) - Xbase,
                To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.Y) - Ybase,
                Tmp.Item.Coord.Width,
                Tmp.Item.Coord.Height),
               Dest, Inters);
            if Inters then
               Draw
                 (Tmp.Item, Canvas, Pix,
                  To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.X) - Xbase,
                  To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.Y) - Ybase);
            end if;
         end if;

         Tmp := Tmp.Next;
      end loop;

      if Use_Double_Buffer then
         Draw_Drawable
           (Get_Window (Canvas),
            GC     => Canvas.Black_GC,
            Src    => Pix,
            Xsrc   => Rect.X,
            Ysrc   => Rect.Y,
            Xdest  => Rect.X,
            Ydest  => Rect.Y,
            Width  => Gint (Rect.Width),
            Height => Gint (Rect.Height));
      end if;

      --  The dashed line (while moving items) have been deleted, and are no
      --  longer visible
      if Canvas.Dashed_Line_Visible then
         Draw_Dashed_Selection (Canvas);
      end if;

      Set_Clip_Mask (Canvas.Black_GC, null);
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
      Canvas    : Interactive_Canvas := Interactive_Canvas (Canv);
      Value     : constant Gdouble := Get_Value (Canvas.Vadj);
      Upper     : constant Gdouble := Get_Upper (Canvas.Vadj);
      Lower     : constant Gdouble := Get_Lower (Canvas.Vadj);
      Page_Incr : constant Gdouble := Get_Page_Increment (Canvas.Vadj);
      Page_Size : constant Gdouble := Get_Page_Size (Canvas.Vadj);
      Step_Incr : constant Gdouble := Get_Step_Increment (Canvas.Vadj);

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
   end Key_Press;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Canv : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas : Interactive_Canvas := Interactive_Canvas (Canv);
      Tmp : Canvas_Item_List := Canvas.Children;
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      X : constant Gint := Gint (Get_X (Event)) + Xbase;
      Y : constant Gint := Gint (Get_Y (Event)) + Ybase;
      X2, Y2 : Gint;
      Cursor   : Gdk.Cursor.Gdk_Cursor;

      procedure Emit_By_Name_Item
        (Object : in System.Address;
         Name   : in String;
         Param  : in Canvas_Item);
      pragma Import (C, Emit_By_Name_Item, "gtk_signal_emit_by_name");

   begin
      Grab_Focus (Canvas);
      Set_Flags (Canvas, Has_Focus);

      Clear_Selection (Canvas);

      --  Find the selected item.
      --  Note that we only keep the last item found, since this is the one
      --  on top, and thus the one the user really wanted to select

      while Tmp /= null loop
         X2 := To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.X);
         Y2 := To_Canvas_Coordinates (Canvas, Tmp.Item.Coord.Y);
         if Tmp.Item.Visible
           and then X >= X2
           and then X <= X2 + Gint (Tmp.Item.Coord.Width)
           and then Y >= Y2
           and then Y <= Y2 + Gint (Tmp.Item.Coord.Height)
         then
            Clear_Selection (Canvas);
            Add_To_Selection (Canvas, Tmp.Item);
         end if;
         Tmp := Tmp.Next;
      end loop;

      --  If there was none, nothing to do...

      if Canvas.Selection = null then
         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);
         return False;
      end if;

      Set_X (Event, Gdouble (Xbase) + Get_X (Event)
             - Gdouble (To_Canvas_Coordinates
                        (Canvas, Canvas.Selection.Item.Coord.X)));
      Set_Y (Event, Gdouble (Ybase) + Get_Y (Event)
             - Gdouble (To_Canvas_Coordinates
                        (Canvas, Canvas.Selection.Item.Coord.Y)));

      --  Double-click events are transmitted directly to the item, and are
      --  not used to move an item.
      --  Clicks other than left mouse button are also transmitted directly

      if Get_Event_Type (Event) = Gdk_2button_Press
        or else Get_Button (Event) /= 1
      then
         --  ??? Should do so for each item in the selection
         On_Button_Click (Canvas.Selection.Item, Event);
         return False;
      end if;

      --  Warn the user that a selection has been made
      --  ??? Should send one such event per item in the selection

      Emit_By_Name_Item
        (Get_Object (Canvas), "item_selected" & ASCII.NUL,
         Canvas.Selection.Item);

      --  Change the cursor to give visual feedback
      Gdk_New (Cursor, Fleur);
      Set_Cursor (Get_Window (Canvas), Cursor);
      Destroy (Cursor);

      --  Initialize the move

      Canvas.Last_X_Event :=
        To_World_Coordinates (Canvas, Gint (Get_X_Root (Event)));
      Canvas.Last_Y_Event :=
        To_World_Coordinates (Canvas, Gint (Get_Y_Root (Event)));
      Canvas.Mouse_Has_Moved := False;

      --  Make sure that no other widget steal the events while we are
      --  moving an item.

      Grab_Add (Canvas);
      Deep_Copy (From => Event, To => Canvas.Event_Press);
      return False;
   end Button_Pressed;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Canv : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Canvas : Interactive_Canvas := Interactive_Canvas (Canv);
      Tmp : Item_Selection_List;
      Cursor : Gdk.Cursor.Gdk_Cursor;
   begin
      Grab_Remove (Canvas);

      if Canvas.Selection = null then
         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);
         return False;
      end if;

      --  restore the standard cursor
      Gdk_New (Cursor, Left_Ptr);
      Set_Cursor (Get_Window (Canvas), Cursor);
      Destroy (Cursor);

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
            Tmp := Tmp.Next;
         end loop;

         Canvas.Dashed_Line_Visible := False;

         --  Scroll the canvas so as to show the first item from the selection
         Refresh_Canvas (Canvas);

      --  If the user did not move the mouse while it was pressed, this is
      --  because he only wanted to select the item.
      --  Note that even if multiple items are currently selected, only the one
      --  we actually clicked on should receive the button_click event.

      else
         --  The button-press event wasn't forwarded, since we were expecting
         --  that the item would move. We thus forward it now
         On_Button_Click (Canvas.Selection.Item, Canvas.Event_Press);

         Set_X (Event, Gdouble (Get_Value (Canvas.Hadj)) +  Get_X (Event)
                - Gdouble (To_Canvas_Coordinates
                           (Canvas, Canvas.Selection.Item.Coord.X)));
         Set_Y (Event, Gdouble (Get_Value (Canvas.Vadj)) + Get_Y (Event)
                - Gdouble (To_Canvas_Coordinates
                           (Canvas, Canvas.Selection.Item.Coord.Y)));
         On_Button_Click (Canvas.Selection.Item, Event);
      end if;

      Free (Canvas.Event_Press);

      return False;
   end Button_Release;

   ------------------------
   -- Test_Scrolling_Box --
   ------------------------

   procedure Test_Scrolling_Box
     (Canvas   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Win      : Gdk_Window;
      X, Y     : Gint;
      X_Scroll : out Gint;
      Y_Scroll : out Gint) is
   begin
      X_Scroll := 0;
      Y_Scroll := 0;

      if Win = Get_Window (Canvas) then
         if X < Scrolling_Margin then
            X_Scroll := -Scrolling_Amount;
         elsif X > Gint (Get_Allocation_Width (Canvas)) - Scrolling_Margin then
            X_Scroll := Scrolling_Amount;
         end if;

         if Y < Scrolling_Margin then
            Y_Scroll := -Scrolling_Amount;
         elsif
           Y > Gint (Get_Allocation_Height (Canvas)) - Scrolling_Margin
         then
            Y_Scroll := Scrolling_Amount;
         end if;
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
      --  Are we in the scrolling box ? If yes, do not move the item directly,
      --  but establish the timeout callbacks that will take care of the
      --  scrolling
      Test_Scrolling_Box
        (Canv, Get_Window (Event), Gint (Get_X (Event)), Gint (Get_Y (Event)),
         X_Scroll, Y_Scroll);

      if X_Scroll /= 0 or else Y_Scroll /= 0 then
         if Canvas.Scrolling_Timeout_Id = 0 then
            Canvas.Scrolling_Timeout_Id := Canvas_Timeout.Add
              (Timeout_Between_Scrolls, Scrolling_Timeout'Access, Canvas);
         end if;

      else
         if Canvas.Scrolling_Timeout_Id /= 0 then
            Timeout_Remove (Canvas.Scrolling_Timeout_Id);
            Canvas.Scrolling_Timeout_Id := 0;
         end if;

         X_Scroll := To_World_Coordinates (Canvas, Gint (Get_X_Root (Event)));
         Y_Scroll := To_World_Coordinates (Canvas, Gint (Get_Y_Root (Event)));
         if Move_Selection
           (Canvas, X_Scroll - Canvas.Last_X_Event,
            Y_Scroll - Canvas.Last_Y_Event)
         then
            Canvas.Last_X_Event := X_Scroll;
            Canvas.Last_Y_Event := Y_Scroll;
         end if;
      end if;
      return False;
   end Button_Motion;

   -----------------------
   -- Scrolling_Timeout --
   -----------------------

   function Scrolling_Timeout (Canvas : Interactive_Canvas) return Boolean is
      Win  : Gdk_Window;
      X, Y : Gint;
      X_Scroll, Y_Scroll : Gint;
   begin
      Window_At_Pointer (X, Y, Win);
      Test_Scrolling_Box (Canvas, Win, X, Y, X_Scroll, Y_Scroll);
      if (X_Scroll /= 0 or else Y_Scroll /= 0)
        and then Move_Selection (Canvas, X_Scroll, Y_Scroll)
      then
         --  Force an immediate draw, since Queue_Draw would only redraw in
         --  an idle event, and thus might not happen before the next timeout.
         --  With lots of items, this would break the scrolling.
         Draw (Canvas);
         return True;
      else
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
   begin
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
               Width  => Gint (Selected.Item.Coord.Width),
               Height => Gint (Selected.Item.Coord.Height));

            Update_Links
              (Canvas, Get_Window (Canvas), Canvas.Anim_GC, Selected);

            if Canvas.Align_On_Grid then
               Selected.X := X;
               Selected.Y := Y;
            end if;
         end if;
         Selected := Selected.Next;
      end loop;
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
      if Canvas.Selection = null then
         return False;
      end if;

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

      Draw_Dashed_Selection (Canvas);
      Show_Item
        (Canvas, Canvas.Selection.Item,
         Canvas.Selection.X, Canvas.Selection.Y);
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
            Gint (Item.Coord.Width), Gint (Item.Coord.Height));
      end if;
   end Item_Updated;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
      Previous      : Canvas_Item_List := null;
      Current       : Canvas_Item_List := Canvas.Children;
      Previous_Link : Canvas_Link_List;
      Current_Link  : Canvas_Link_List;
      Link, Tmp     : Canvas_Link;

   begin
      while Current /= null loop
         if Current.Item = Canvas_Item (Item) then

            --  Remove the item itself

            if Previous = null then
               Canvas.Children := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;

            --  Remove all the links

            Previous_Link := null;
            Current_Link := Canvas.Links;
            while Current_Link /= null loop
               if Current_Link.Link.Src = Canvas_Item (Item)
                 or else Current_Link.Link.Dest = Canvas_Item (Item)
               then
                  Link := Current_Link.Link;
                  while Link /= null loop
                     Tmp := Link.Sibling;
                     Destroy (Link);
                     Free (Link);
                     Link := Tmp;
                  end loop;

                  if Previous_Link = null then
                     Canvas.Links := Current_Link.Next;
                  else
                     Previous_Link.Next := Current_Link.Next;
                  end if;
               else
                  Previous_Link := Current_Link;
               end if;

               if Previous_Link /= null then
                  Current_Link := Previous_Link.Next;
               else
                  Current_Link := Canvas.Links;
               end if;
            end loop;

            --  Free the memory
            Destroy (Current.Item);
            Free (Current);

            --  Have to redraw everything, since there might have been some
            --  links.
            Refresh_Canvas (Canvas);
            return;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item   : access Canvas_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button) is
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
      Current : Canvas_Link_List := Canvas.Links;
      Link    : Canvas_Link;
   begin
      while Current /= null loop
         if Current.Link.Src = Canvas_Item (From)
           and then Current.Link.Dest = Canvas_Item (To)
         then
            Link := Current.Link;
            while Link /= null loop
               if (Name = "" or else Link.Descr.all = Name) then
                  return True;
               end if;
               Link := Link.Sibling;
            end loop;
            return False;
         end if;
         Current := Current.Next;
      end loop;
      return False;
   end Has_Link;

   ----------------
   -- Lower_Item --
   ----------------

   procedure Lower_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
      Previous : Canvas_Item_List := Canvas.Children;
      Current  : Canvas_Item_List;

   begin
      --  Already on top?

      if Canvas.Children = null
        or else Canvas.Children.Item = Canvas_Item (Item)
      then
         return;
      end if;

      Current := Canvas.Children.Next;

      --  Look for the item.

      while Current /= null loop
         if Current.Item = Canvas_Item (Item) then
            Previous.Next := Current.Next;
            Current.Next := Canvas.Children;
            Canvas.Children := Current;
            exit;
         end if;

         Previous := Current;
         Current := Current.Next;
      end loop;

      Queue_Draw (Canvas);
   end Lower_Item;

   ----------------
   -- Raise_Item --
   ----------------

   procedure Raise_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
      Previous : Canvas_Item_List := null;
      Current  : Canvas_Item_List := Canvas.Children;
      To_Move  : Canvas_Item_List := null;

   begin
      --  A single item => Nothing to do
      if Canvas.Children.Next = null then
         return;
      end if;

      while Current /= null loop
         if Current.Item = Canvas_Item (Item) then
            To_Move := Current;

            if Previous = null then
               Canvas.Children := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;
         else
            Previous := Current;
         end if;

         Current := Current.Next;
      end loop;

      if Previous /= null and then To_Move /= null then
         Previous.Next := To_Move;
         To_Move.Next := null;
      end if;

      --  Have to redraw everything, since there might have been some links.
      Queue_Draw (Canvas);
   end Raise_Item;

   ---------------
   -- Is_On_Top --
   ---------------

   function Is_On_Top
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      Current : Canvas_Item_List := Canvas.Children;
   begin
      if Current = null then
         return False;
      end if;

      while Current.Next /= null loop
         Current := Current.Next;
      end loop;

      return Current.Item = Canvas_Item (Item);
   end Is_On_Top;

   ---------------
   -- Show_Item --
   ---------------

   procedure Show_Item (Canvas : access Interactive_Canvas_Record'Class;
                        Item   : access Canvas_Item_Record'Class;
                        X, Y   : Gint)
   is
      X1 : constant Gint := To_Canvas_Coordinates (Canvas, X);
      Y1 : constant Gint := To_Canvas_Coordinates (Canvas, Y);
      X2 : constant Gint := X1 + Gint (Item.Coord.Width);
      Y2 : constant Gint := Y1 + Gint (Item.Coord.Height);
   begin
      --  Do we need to scroll the canvas to the right to show the item?

      if X2 > Gint (Get_Upper (Canvas.Hadj)) then
         Set_Upper (Canvas.Hadj, Gdouble (X2));
         Changed (Canvas.Hadj);
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

      if X2 > Gint (Get_Value (Canvas.Hadj) + Get_Page_Size (Canvas.Hadj)) then
         Set_Value (Canvas.Hadj, Gdouble (X2) - Get_Page_Size (Canvas.Hadj));
      end if;

      --  Do we need to scroll the canvas to the left ?

      if X1 < Gint (Get_Lower (Canvas.Hadj)) then
         Set_Lower (Canvas.Hadj, Gdouble (X1));
         Changed (Canvas.Hadj);
      end if;

      if X1 < Gint (Get_Value (Canvas.Hadj)) then
         Set_Value (Canvas.Hadj, Gdouble (X1));
      end if;

      --  Do we need to scroll the canvas to the top to show the selection?

      if Y2 > Gint (Get_Upper (Canvas.Vadj)) then
         Set_Upper (Canvas.Vadj, Gdouble (Y2));
         Changed (Canvas.Vadj);
      end if;

      if Y2 > Gint (Get_Value (Canvas.Vadj) + Get_Page_Size (Canvas.Vadj)) then
         Set_Value (Canvas.Vadj, Gdouble (Y2) - Get_Page_Size (Canvas.Vadj));
      end if;

      --  Do we need to scroll the canvas to the bottom ?

      if Y1 < Gint (Get_Lower (Canvas.Vadj)) then
         Set_Lower (Canvas.Vadj, Gdouble (Y1));
         Changed (Canvas.Vadj);
      end if;

      if Y1 < Gint (Get_Value (Canvas.Vadj)) then
         Set_Value (Canvas.Vadj, Gdouble (Y1));
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
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "") is
   begin
      Link.Src := Canvas_Item (Src);
      Link.Dest := Canvas_Item (Dest);
      Link.Arrow := Arrow;

      Free (Link.Descr);
      Link.Descr := new String'(Descr);
   end Configure;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class)
   is
      List : Canvas_Link_List := Canvas.Links;
      L : Canvas_Link;
   begin
      --  Do we already have links between the two items ?

      while List /= null loop
         if (List.Link.Src = Link.Src and then List.Link.Dest = Link.Dest)
           or else
           (List.Link.Src = Link.Dest and then List.Link.Dest = Link.Src)
         then
            L := List.Link;
            while L.Sibling /= null loop
               L := L.Sibling;
            end loop;
            L.Sibling := Canvas_Link (Link);
            return;
         end if;
         List := List.Next;
      end loop;

      --  If not, we simply add a new item to the list

      Canvas.Links := new Canvas_Link_List_Record'
        (Link => Canvas_Link (Link),
         Next => Canvas.Links);
   end Add_Link;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "")
   is
      L : Canvas_Link := new Canvas_Link_Record;
   begin
      Configure (L, Src, Dest, Arrow, Descr);
      Add_Link (Canvas, L);
   end Add_Link;

   -----------------
   -- Remove_Link --
   -----------------

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class)
   is
      Previous : Canvas_Link_List;
      Current  : Canvas_Link_List := Canvas.Links;
      L        : Canvas_Link;
   begin
      while Current /= null loop
         L := Current.Link;
         if L = Canvas_Link (Link) then
            if L.Sibling = null then
               if Previous = null then
                  Canvas.Links := Current.Next;
               else
                  Previous.Next := Current.Next;
               end if;
               Free (Current);
            else
               Current.Link := L.Sibling;
               Destroy (L);
               Free (L);
            end if;
            return;
         end if;

         while L /= null loop
            if L.Sibling = Canvas_Link (Link) then
               L.Sibling := Link.Sibling;
               Destroy (Link);
               L := Canvas_Link (Link);
               Free (L);
               return;
            end if;
            L := L.Sibling;
         end loop;
         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove_Link;

   -------------------
   -- For_Each_Link --
   -------------------

   procedure For_Each_Link
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Link_Processor)
   is
      Current : Canvas_Link_List := Canvas.Links;
      Tmp     : Canvas_Link_List;
      Link, Tmp_Link : Canvas_Link;
   begin
      while Current /= null loop
         Tmp := Current.Next;
         Link := Current.Link;
         while Link /= null loop
            Tmp_Link := Link.Sibling;
            exit when not Execute (Canvas, Link);
            Link := Tmp_Link;
         end loop;
         Current := Tmp;
      end loop;
   end For_Each_Link;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Link : access Canvas_Link_Record) is
   begin
      Free (Link.Descr);
   end Destroy;

   procedure Destroy (Item : access Canvas_Item_Record) is
   begin
      null;
   end Destroy;

   procedure Destroy (Item : access Buffered_Item_Record) is
   begin
      if Item.Pixmap /= null then
         Gdk.Pixmap.Unref (Item.Pixmap);
      end if;
   end Destroy;

   -------------
   -- Get_Src --
   -------------

   function Get_Src (Link : access Canvas_Link_Record) return Canvas_Item is
   begin
      return Link.Src;
   end Get_Src;

   --------------
   -- Get_Dest --
   --------------

   function Get_Dest (Link : access Canvas_Link_Record) return Canvas_Item is
   begin
      return Link.Dest;
   end Get_Dest;

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
      Font_Size : Gint;
   begin
      Canvas.Zoom := Percent;

      if Canvas.Font /= null then
         Unref (Canvas.Font);
      end if;

      Font_Size := To_Canvas_Coordinates (Canvas, Canvas.Annotation_Height);
      if Font_Size >= 1 then
         Canvas.Font := Get_Gdkfont (Canvas.Annotation_Font.all, Font_Size);
      else
         Canvas.Font := null;
      end if;

      Refresh_Canvas (Canvas);

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

      Set_Value (Canvas.Hadj, H);
      Set_Value (Canvas.Vadj, V);

      --  Do not report the modification to the scrollbar, since this does
      --  too much flickering and slows things done a lot when the canvas is
      --  in a scrolled_window whose policy is set on automatic.

      if Canvas.Zoom = Canvas.Target_Zoom then
         Changed (Canvas.Hadj);
         Changed (Canvas.Vadj);
      end if;

      Widget_Callback.Emit_By_Name (Canvas, "zoomed", Percent);
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
   begin
      if Canvas.Zoom = Percent then
         return;
      end if;
      Canvas.Target_Zoom := Percent;

      --  Do we want smooth scrolling ?
      if Steps > 1 then
         Canvas.Zoom_Step := (Gint (Percent - Canvas.Zoom) / Gint (Steps));
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
      Dest   : Gdk.Pixmap.Gdk_Pixmap;
      Xdest  : Glib.Gint;
      Ydest  : Glib.Gint) is
   begin
      Draw_Drawable (Dest, Canvas.Black_GC, Item.Pixmap, 0, 0, Xdest, Ydest);
   end Draw;

   --------------------------------
   -- Set_Screen_Size_And_Pixmap --
   --------------------------------

   procedure Set_Screen_Size_And_Pixmap
     (Item   : access Buffered_Item_Record;
      Win    : Gdk.Window.Gdk_Window;
      Width, Height  : Glib.Gint) is
   begin
      Set_Screen_Size (Item, Width, Height);

      if Item.Pixmap /= null then
         Gdk.Pixmap.Unref (Item.Pixmap);
      end if;

      Gdk_New (Item.Pixmap, Win, Width, Height);
   end Set_Screen_Size_And_Pixmap;

   ------------
   -- Pixmap --
   ------------

   function Pixmap (Item : access Buffered_Item_Record) return Gdk_Pixmap is
   begin
      return Item.Pixmap;
   end Pixmap;

end Gtkada.Canvas;
