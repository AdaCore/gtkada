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

with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Gdk.Color;        use Gdk.Color;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;           use Gdk.GC;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Types;        use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Window;       use Gdk.Window;
with Glib;             use Glib;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Handlers;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Main;         use Gtk.Main;
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
     (Item_Selection_List_Record, Item_Selection_List);

   package Canvas_Timeout is new Gtk.Main.Timeout (Interactive_Canvas);

   function Expose
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle the expose events for a canvas.

   procedure Realized (Canvas : access Gtk_Widget_Record'Class);
   --  Create all the graphic contexts required for the animation.

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
     (From  : access Canvas_Item_Record'Class;
      To_X  : in Gint;
      To_Y  : in Gint;
      X_Pos : in Gfloat;
      Y_Pos : in Gfloat;
      X_Out : out Gint;
      Y_Out : out Gint);
   --  Clip the line that goes from the middle of From to (To_X, To_Y).
   --  The intersection between that line and the border of From is returned
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
      Link   : in Canvas_Link);
   --  Draw Link on the screen.
   --  The link is drawn as a curved link (ie there is an extra handle in its
   --  middle).
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

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

   function Has_Link
     (Canvas    : access Interactive_Canvas_Record'Class;
      Src, Dest : access Canvas_Item_Record'Class;
      Side      : Link_Side;
      Offset    : Gint) return Boolean;
   --  Return True if there is already a link from Src to Dest with
   --  the specific side and offset specified in parameter.
   --  This is used to avoid having two link hidding each other.

   procedure Set_Scroll_Adjustments
     (Canvas : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
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

   function To_Canvas
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Gint) return Gint;
   --  Return the world coordinate X as coordinates in the canvas

   function To_World
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Gint) return Gint;
   --  Return the canvas coordinate X as coordinates in the world

   function Zoom_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Timeout function used to provide smooth zooming.

   procedure Zoom_Internal
     (Canvas : access Interactive_Canvas_Record'Class; Percent : Guint);
   --  Internal function to implement zooming

   ---------------
   -- To_Canvas --
   ---------------

   function To_Canvas
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Gint) return Gint is
   begin
      return X * Gint (Canvas.Zoom) / 100;
   end To_Canvas;

   --------------
   -- To_World --
   --------------

   function To_World
     (Canvas : access Interactive_Canvas_Record'Class;
      X      : Gint) return Gint is
   begin
      return X * 100 / Gint (Canvas.Zoom);
   end To_World;

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

   Scroll_Adj_Signal : constant := 4;
   Signals : constant chars_ptr_array :=
     (1 => New_String ("background_click"),
      2 => New_String ("item_selected"),
      3 => New_String ("zoomed"),
      Scroll_Adj_Signal => New_String ("set_scroll_adjustments"));
   --  Array of the signals created for this widget

   procedure Initialize (Canvas : access Interactive_Canvas_Record'Class) is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => Gtk.Gtk_Type_Gdk_Event, 2 => Gtk.Gtk_Type_None),
         2 => (1 => Gtk.Gtk_Type_Pointer,   2 => Gtk.Gtk_Type_None),
         3 => (1 => Gtk.Gtk_Type_Uint,      2 => Gtk.Gtk_Type_None),
         4 => (1 => Gtk.Gtk_Type_Object,    2 => Gtk.Gtk_Type_Object));
      --  the parameters for the above signals.
      --  This must be defined in this function rather than at the
      --  library-level, or the value of Gtk_Type_Gdk_Event is not yet
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

      --  We want to be sure to get all the mouse events, that are required
      --  for the animation.

      Add_Events (Canvas, Gdk.Types.Button_Press_Mask
                  or Gdk.Types.Button_Motion_Mask
                  or Gdk.Types.Button_Release_Mask
                  or Gdk.Types.Key_Press_Mask
                  or Gdk.Types.Key_Release_Mask);
      Set_Flags (Canvas, Can_Focus);

      --  Configure with default values
      Configure (Canvas);
      Set_Scroll_Adjustments (Canvas, Null_Adjustment, Null_Adjustment);
   end Initialize;

   ----------------------------
   -- Set_Scroll_Adjustments --
   ----------------------------

   procedure Set_Scroll_Adjustments
     (Canvas : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Hadj : constant System.Address := To_Address (Args, 1);
      Vadj : constant System.Address := To_Address (Args, 2);
      Canv : Interactive_Canvas := Interactive_Canvas (Canvas);
      Stub  : Gtk_Adjustment_Record;
   begin
      if Canv.Hadj /= null then
         Unref (Canv.Hadj);
      end if;

      if Hadj /= System.Null_Address then
         Canv.Hadj := Gtk_Adjustment (Gtk.Get_User_Data (Hadj, Stub));
      else
         Gtk_New (Canv.Hadj, 0.0, 0.0, 0.0, 1.0, 1.0, 10.0);
      end if;
      Ref (Canv.Hadj);
      Sink (Canv.Hadj);

      if Canv.Vadj /= null then
         Unref (Canv.Vadj);
      end if;

      if Vadj /= System.Null_Address then
         Canv.Vadj := Gtk_Adjustment (Gtk.Get_User_Data (Vadj, Stub));
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
      Set_Page_Size (Canvas.Hadj, Gfloat (Get_Allocation_Width (Canvas)));
      Set_Step_Increment (Canvas.Hadj, 10.0);
      Set_Page_Increment (Canvas.Hadj, Get_Page_Size (Canvas.Hadj) / 2.0);

      Set_Page_Size (Canvas.Vadj, Gfloat (Get_Allocation_Height (Canvas)));
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
         X_Max := Gint (Get_Allocation_Width (Canvas));
         Y_Min := 0;
         Y_Max := Gint (Get_Allocation_Height (Canvas));

      else
         X_Min := Gint'Last;
         X_Max := Gint'First;
         Y_Min := Gint'Last;
         Y_Max := Gint'First;

         while Current /= null loop
            if Current.Item.Visible then
               X := To_Canvas (Canvas, Current.Item.Coord.X);
               Y := To_Canvas (Canvas, Current.Item.Coord.Y);
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
        (Canvas.Hadj, Gfloat'Min (Get_Value (Canvas.Hadj), Gfloat (X_Min)));
      Set_Upper
        (Canvas.Hadj,
         Gfloat'Max (Get_Value (Canvas.Hadj) + Get_Page_Size (Canvas.Hadj),
                     Gfloat (X_Max)));
      Changed (Canvas.Hadj);

      Set_Lower
        (Canvas.Vadj, Gfloat'Min (Get_Value (Canvas.Vadj), Gfloat (Y_Min)));
      Set_Upper
        (Canvas.Vadj,
         Gfloat'Max (Get_Value (Canvas.Vadj) + Get_Page_Size (Canvas.Vadj),
                     Gfloat (Y_Max)));
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
      Step  : Guint := Canvas.Grid_Size;

      function Location_Is_Free (X, Y : Gint) return Boolean;
      --  Return True if the location X, Y for the new item would be
      --  acceptable, or if it would cross an existing item.
      --  Keeps a space of at least Grid_Size around each item.

      ----------------------
      -- Location_Is_Free --
      ----------------------

      function Location_Is_Free (X, Y : Gint) return Boolean is
         Tmp   : Canvas_Item_List := Canvas.Children;
         Dest  : Gdk.Rectangle.Gdk_Rectangle;
         Inter : Boolean := False;
      begin
         Item.Coord.X := X - Gint (Step);
         Item.Coord.Y := Y - Gint (Step);
         Item.Coord.Width := Item.Coord.Width + 2 * Step - 1;
         Item.Coord.Height := Item.Coord.Height + 2 * Step - 1;

         while not Inter and then Tmp /= null loop
            if Tmp.Item.Visible
              and then Tmp.Item /= Canvas_Item (Item)
            then
               Intersect (Tmp.Item.Coord, Item.Coord, Dest, Inter);
            end if;

            Tmp := Tmp.Next;
         end loop;

         Item.Coord.Width := Item.Coord.Width - 2 * Step + 1;
         Item.Coord.Height := Item.Coord.Height - 2 * Step + 1;
         return not Inter;
      end Location_Is_Free;

      Src_Item : Canvas_Item := null;
      Links    : Canvas_Link_List := Canvas.Links;
      X1       : Gint;
      Y1       : Gint;

   begin
      --  When no grid is drawn...

      if Step = 0 then
         Step := Default_Grid_Size;
      end if;

      if X /= Gint'First and then Y /= Gint'First then
         Item.Coord.X := X;
         Item.Coord.Y := Y;
      else
         --  Check if there is any link that has for destination the widget we
         --  are adding.

         while Links /= null loop
            if Src_Item = null
              and then Links.Link.Dest = Canvas_Item (Item)
            then
               Src_Item := Links.Link.Src;
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
            begin
               loop
                  X1 := Src_Item.Coord.X + ((Num + 1) mod 3 - 1)
                    * Gint (Step * 2 + Src_Item.Coord.Width);
                  Y1 := Src_Item.Coord.Y
                    + Gint (Src_Item.Coord.Height)
                    + (1 + Num / 3) * Gint (Step * 2);

                  if X1 >= 0 then
                     exit when Location_Is_Free (X1, Y1);
                  end if;

                  Num := Num + 1;
               end loop;
            end;

         --  Else put the item in the first line, at the first possible
         --  location

         else
            X1 := Gint (Step);
            Y1 := Gint (Step);

            while not Location_Is_Free (X1, Y1) loop
               X1 := X1 + Gint (Step) * 2;
            end loop;
         end if;

         Item.Coord.X := X1;
         Item.Coord.Y := Y1;
      end if;

      if Canvas.Align_On_Grid then
         Item.Coord.X := (Item.Coord.X / Gint (Canvas.Grid_Size))
           * Gint (Canvas.Grid_Size);
         Item.Coord.Y := (Item.Coord.Y / Gint (Canvas.Grid_Size))
           * Gint (Canvas.Grid_Size);
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
      Show_Item (Canvas, Item);
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
         exit when not  Execute (Canvas, Item.Item);
         Item := Tmp;
      end loop;
   end For_Each_Item;

   ---------------
   -- Clip_Line --
   ---------------

   procedure Clip_Line
     (From  : access Canvas_Item_Record'Class;
      To_X  : in Gint;
      To_Y  : in Gint;
      X_Pos : in Gfloat;
      Y_Pos : in Gfloat;
      X_Out : out Gint;
      Y_Out : out Gint)
   is
      Src_X    : constant Gint :=
        From.Coord.X + Gint (Gfloat (From.Coord.Width) * X_Pos);
      Src_Y    : constant Gint :=
        From.Coord.Y + Gint (Gfloat (From.Coord.Height) * Y_Pos);
      Delta_X  : constant Gint := To_X - Src_X;
      Delta_Y  : constant Gint := To_Y - Src_Y;
      Offset   : Gint;

   begin
      --  Intersection with horizontal sides

      if Delta_Y /= 0 then
         Offset := (Src_X * To_Y - To_X * Src_Y);

         if Delta_Y < 0 then
            --  Intersection with north side ?
            Y_Out := From.Coord.Y;
         else
            Y_Out := From.Coord.Y + Gint (From.Coord.Height);
         end if;

         X_Out := (Delta_X * Y_Out + Offset) / Delta_Y;

         if From.Coord.X <= X_Out
           and then X_Out <= From.Coord.X + Gint (From.Coord.Width)
         then
            return;
         end if;
      end if;

      --  Intersection with vertical sides

      if Delta_X /= 0 then
         Offset := (To_X * Src_Y - Src_X * To_Y);

         if Delta_X < 0 then
            --  Intersection with West side ?
            X_Out := From.Coord.X;
         else
            --  Intersection with East side ?
            X_Out := From.Coord.X + Gint (From.Coord.Width);
         end if;

         Y_Out := (Delta_Y * X_Out + Offset) / Delta_X;

         if From.Coord.Y <= Y_Out
           and then Y_Out <= From.Coord.Y + Gint (From.Coord.Height)
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
        Float (To_Canvas (Canvas, Canvas.Arrow_Length));
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
   begin
      Clip_Line
        (Link.Src,
         Link.Dest.Coord.X
            + Gint (Gfloat (Link.Dest.Coord.Width) * Link.Dest_X_Pos),
         Link.Dest.Coord.Y
            + Gint (Gfloat (Link.Dest.Coord.Height) * Link.Dest_Y_Pos),
         X_Pos => Link.Src_X_Pos, Y_Pos => Link.Src_Y_Pos,
         X_Out => X1, Y_Out => Y1);
      Clip_Line
        (Link.Dest,
         Link.Src.Coord.X
            + Gint (Gfloat (Link.Src.Coord.Width) * Link.Src_X_Pos),
         Link.Src.Coord.Y
            + Gint (Gfloat (Link.Src.Coord.Height) * Link.Src_Y_Pos),
         X_Pos => Link.Dest_X_Pos, Y_Pos => Link.Dest_Y_Pos,
         X_Out => X2, Y_Out => Y2);

      X1 := To_Canvas (Canvas, X1) - Xbase;
      Y1 := To_Canvas (Canvas, Y1) - Ybase;
      X2 := To_Canvas (Canvas, X2) - Xbase;
      Y2 := To_Canvas (Canvas, Y2) - Ybase;

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

   -------------------
   -- Draw_Arc_Link --
   -------------------

   procedure Draw_Arc_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Window : Gdk_Window;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Canvas_Link)
   is
      X1, Y1, X2, Y2, X3, Y3 : Gint;
      --  The three points that define the arc

      procedure Center (Center_X, Center_Y : out Gint);
      --  Compute the center of the circle that encloses the three points
      --  defined by (X1, Y1), (X2, Y2) or (X3, Y3).
      --  Center_X is set to Gint'First if the points are colinear

      ------------
      -- Center --
      ------------

      procedure Center (Center_X, Center_Y : out Gint) is
         A : constant Gint := X2 - X1;
         B : constant Gint := Y2 - Y1;
         C : constant Gint := X3 - X1;
         D : constant Gint := Y3 - Y1;
         E : constant Gint := A * (X1 + X2) + B * (Y1 + Y2);
         F : constant Gint := C * (X1 + X3) + D * (Y1 + Y3);
         G : constant Gint := 2 * (A * (Y3 - Y2) - B * (X3 - X2));

      begin
         --  Are the points colinear ? We just choose a circle that
         --  goes through two of them.

         if G = 0 then
            if A = 0 and then B = 0 then
               Center_X := (X1 + X3) / 2;
               Center_Y := (Y1 + Y3) / 2;
            else
               Center_X := (X2 + X3) / 2;
               Center_Y := (Y2 + Y3) / 2;
            end if;
            return;
         end if;

         Center_X := (D * E - B * F) / G;
         Center_Y := (A * F - C * E) / G;
      end Center;

      Offset_X, Offset_Y : Gint;
      Right_Angle : constant Float := Ada.Numerics.Pi / 2.0;
      --  The offsets to use to calculate the position of the middle point.

      Xc, Yc     : Gint;
      Radius     : Gint;
      Base       : constant := 360 * 64;
      Float_Base : constant := 180.0 * 64.0 / Ada.Numerics.Pi;
      Angle      : Float;
      Angle_From : Gint;
      Angle_To   : Gint;
      Path       : Gint;
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Arc_Offset : constant Float :=
        Float (To_Canvas (Canvas, Canvas.Arc_Link_Offset));

   begin
      --  Special case for self-referencing links

      if Link.Src = Link.Dest then
         Xc := To_Canvas
           (Canvas, Link.Src.Coord.X + Gint (Link.Src.Coord.Width)) - Xbase;
         Yc := To_Canvas (Canvas, Link.Src.Coord.Y) - Ybase;
         Radius := Gint (Arc_Offset) / 2 * Link.Offset;
         Angle_From := -90 * 64;
         Path       := 270 * 64;

         --  Location of the arrow
         X3 := Xc - Radius;
         Y3 := Yc;
         X1 := Xc;
         Y1 := Yc + Radius;

         --  Location of the annotation
         X2 := Xc + Radius / 2;
         Y2 := Yc - Radius / 2;

      else
         --  We will first compute the extra intermediate point between the
         --  center of the two items. Once we have this intermediate point, we
         --  will be able to use the intersection point between the two items
         --  and the two lines from the centers to the middle point.  Finally,
         --  we will be able to compute the circle enclosing the three points.

         X1 := Link.Src.Coord.X +
           Gint (Gfloat (Link.Src.Coord.Width) * Link.Src_X_Pos);
         Y1 := Link.Src.Coord.Y +
           Gint (Gfloat (Link.Src.Coord.Height) * Link.Src_Y_Pos);
         X3 := Link.Dest.Coord.X +
           Gint (Gfloat (Link.Dest.Coord.Width) * Link.Dest_X_Pos);
         Y3 := Link.Dest.Coord.Y +
           Gint (Gfloat (Link.Dest.Coord.Height) * Link.Dest_Y_Pos);

         --  Compute the middle point for the arc, and create a dummy item for
         --  it that the user can move.

         if X1 /= X3 then
            Angle := Arctan (Float (Y3 - Y1), Float (X3 - X1));
         elsif Y3 > Y1 then
            Angle := Ada.Numerics.Pi / 2.0;
         else
            Angle := -Ada.Numerics.Pi / 2.0;
         end if;

         if Link.Side = Right then
            Offset_X := Gint (Arc_Offset * Cos (Angle - Right_Angle));
            Offset_Y := Gint (Arc_Offset * Sin (Angle - Right_Angle));
         else
            Offset_X := Gint (Arc_Offset * Cos (Angle + Right_Angle));
            Offset_Y := Gint (Arc_Offset * Sin (Angle + Right_Angle));
         end if;

         X2 := (X1 + X3) / 2 + Offset_X * Link.Offset;
         Y2 := (Y1 + Y3) / 2 + Offset_Y * Link.Offset;

         --  Clip to the border of the boxes

         Clip_Line (Link.Src, X2, Y2,
                    X_Pos => Link.Src_X_Pos, Y_Pos => Link.Src_Y_Pos,
                    X_Out => X1, Y_Out => Y1);
         Clip_Line (Link.Dest, X2, Y2,
                    X_Pos => Link.Dest_X_Pos, Y_Pos => Link.Dest_Y_Pos,
                    X_Out => X3, Y_Out => Y3);

         X1 := To_Canvas (Canvas, X1) - Xbase;
         Y1 := To_Canvas (Canvas, Y1) - Ybase;
         X2 := To_Canvas (Canvas, X2) - Xbase;
         Y2 := To_Canvas (Canvas, Y2) - Ybase;
         X3 := To_Canvas (Canvas, X3) - Xbase;
         Y3 := To_Canvas (Canvas, Y3) - Ybase;

         --  Compute the circle's center and radius

         Center (Xc, Yc);
         Radius := Gint (Sqrt (Float ((Xc - X3) * (Xc - X3)
                                      + (Yc - Y3) * (Yc - Y3))));

         --  Compute the angles

         if X1 /= Xc then
            Angle_From :=
              (Gint (Float_Base * Arctan (Float (Yc - Y1), Float (X1 - Xc)))
               + Base) mod Base;
         elsif Yc > Y1 then
            Angle_From := Base / 4;
         else
            Angle_From := -Base / 4;
         end if;

         if X3 /= Xc then
            Angle_To :=
              (Gint (Float_Base * Arctan (Float (Yc - Y3), Float (X3 - Xc)))
               + Base) mod Base;
         elsif Yc > Y3 then
            Angle_To := Base / 4;
         else
            Angle_To := -Base / 4;
         end if;

         Path := (Base + Angle_To - Angle_From) mod Base;

         --  Make sure we chose the shortest arc

         if Path > Base / 2 then
            Path := Path - Base;
         end if;

      end if;

      --  Draw the arc

      Draw_Arc (Window,
                GC,
                Filled => False,
                X      => Xc - Radius,
                Y      => Yc - Radius,
                Width  => Radius * 2,
                Height => Radius * 2,
                Angle1 => Angle_From,
                Angle2 => Path);

      --  Draw the arrows

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         if X3 /= Xc then
            Angle := Arctan (Float (Y3 - Yc), Float (X3 - Xc));
         elsif Y3 > Yc then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;

         if Path > 0 then
            Angle := Angle + Right_Angle;
         else
            Angle := Angle - Right_Angle;
         end if;

         Draw_Arrow_Head (Canvas, Window, GC, X3, Y3, Angle);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= Xc then
            Angle := Arctan (Float (Y1 - Yc), Float (X1 - Xc));
         elsif Y1 > Yc then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;

         if Path > 0 then
            Angle := Angle - Right_Angle;
         else
            Angle := Angle + Right_Angle;
         end if;

         Draw_Arrow_Head (Canvas, Window, GC, X1, Y1, Angle);
      end if;

      --  Draw the text if any

      if Link.Descr /= null then
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
      X, Y : Gint;
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
         then
            if Selected = null
              or else Current.Link.Src = Canvas_Item (Selected.Item)
              or else Current.Link.Dest = Canvas_Item (Selected.Item)
            then
               if Current.Link.Side = Straight then
                  Draw_Straight_Link (Canvas, Window, GC, Current.Link);
               else
                  Draw_Arc_Link (Canvas, Window, GC, Current.Link);
               end if;
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
      Dest, Rect2 : Gdk_Rectangle;
      Inters : Boolean;
      Xbase : constant Gint := Gint (Get_Value (Canvas.Hadj));
      Ybase : constant Gint := Gint (Get_Value (Canvas.Vadj));
      Grid  : constant Gint := To_Canvas (Canvas, Gint (Canvas.Grid_Size));

   begin
      --  If the GC was not created, do not do anything

      if Canvas.Clear_GC = Null_GC
        or else Canvas.Double_Buffer = null
      then
         return False;
      end if;

      --  Clear the canvas

      Draw_Rectangle
        (Canvas.Double_Buffer,
         Get_Background_GC (Get_Style (Canvas), State_Normal),
         Filled => True,
         X => Rect.X, Y => Rect.Y,
         Width  => Gint (Rect.Width),
         Height => Gint (Rect.Height));

      Set_Clip_Rectangle (Canvas.Black_GC, Rect);

      --  Draw the background dots.

      if Grid >= 2 then
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
               Draw_Point (Canvas.Double_Buffer, Canvas.Black_GC, X, Y);
               X := X + Grid;
            end loop;

            Y := Y + Grid;
         end loop;
      end if;

      --  Draw the links first, so that they appear to be below the items.
      --  ??? Should redraw only the required links (for splines)

      Update_Links (Canvas, Canvas.Double_Buffer, Canvas.Black_GC);

      --  Draw each of the items.

      Rect2 :=
        (To_World (Canvas, Rect.X + Xbase),
         To_World (Canvas, Rect.Y + Ybase),
         Guint (To_World (Canvas, Gint (Rect.Width))),
         Guint (To_World (Canvas, Gint (Rect.Height))));

      while Tmp /= null loop
         if Tmp.Item.Visible then
            Intersect (Tmp.Item.Coord, Rect2, Dest, Inters);
            if Inters then
               Draw_Pixmap
                 (Canvas.Double_Buffer,
                  GC    => Canvas.Black_GC,
                  Src   => Pixmap (Tmp.Item),
                  Xsrc  => 0,
                  Ysrc  => 0,
                  Xdest => To_Canvas (Canvas, Tmp.Item.Coord.X) - Xbase,
                  Ydest => To_Canvas (Canvas, Tmp.Item.Coord.Y) - Ybase,
                  Width => Gint (Tmp.Item.Coord.Width),
                  Height => Gint (Tmp.Item.Coord.Height));
            end if;
         end if;

         Tmp := Tmp.Next;
      end loop;

      Draw_Pixmap
        (Get_Window (Canvas),
         GC     => Canvas.Black_GC,
         Src    => Canvas.Double_Buffer,
         Xsrc   => Rect.X,
         Ysrc   => Rect.Y,
         Xdest  => Rect.X,
         Ydest  => Rect.Y,
         Width  => Gint (Rect.Width),
         Height => Gint (Rect.Height));

      --  The dashed line (while moving items) have been deleted, and are no
      --  longer visible
      if Canvas.Dashed_Line_Visible then
         Draw_Dashed_Selection (Canvas);
      end if;

      Set_Clip_Mask (Canvas.Black_GC, null);
      return False;
   end Expose;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item   : access Canvas_Item_Record'Class;
      Win    : Gdk_Window;
      Width  : Gint;
      Height : Gint) is
   begin
      Item.Coord.Width  := Guint (Width);
      Item.Coord.Height := Guint (Height);

      --  Create the pixmap
      if Item.Pixmap /= Null_Pixmap then
         Gdk.Pixmap.Unref (Item.Pixmap);
      end if;

      Gdk_New (Item.Pixmap, Win, Width, Height);
   end Initialize;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Canv : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas    : Interactive_Canvas := Interactive_Canvas (Canv);
      Value     : constant Gfloat := Get_Value (Canvas.Vadj);
      Upper     : constant Gfloat := Get_Upper (Canvas.Vadj);
      Lower     : constant Gfloat := Get_Lower (Canvas.Vadj);
      Page_Incr : constant Gfloat := Get_Page_Increment (Canvas.Vadj);
      Page_Size : constant Gfloat := Get_Page_Size (Canvas.Vadj);
      Step_Incr : constant Gfloat := Get_Step_Increment (Canvas.Vadj);

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
         X2 := To_Canvas (Canvas, Tmp.Item.Coord.X);
         Y2 := To_Canvas (Canvas, Tmp.Item.Coord.Y);
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

      --  Double-click events are transmitted directly to the item, and are
      --  not used to move an item.
      --  Clicks other than left mouse button are also transmitted directly

      if Get_Event_Type (Event) = Gdk_2button_Press
        or else Get_Button (Event) /= 1
      then
         Set_X (Event, Gdouble (Xbase) + Get_X (Event)
                - Gdouble (To_Canvas (Canvas, Canvas.Selection.Item.Coord.X)));
         Set_Y (Event, Gdouble (Ybase) + Get_Y (Event)
                - Gdouble (To_Canvas (Canvas, Canvas.Selection.Item.Coord.Y)));
         --  ??? Should do so for each item in the selection
         On_Button_Click (Canvas.Selection.Item, Event);
         Clear_Selection (Canvas);
         return False;
      end if;

      --  Warn the user that a selection has been made
      --  ??? Should send one such event per item in the selection

      Emit_By_Name_Item
        (Gtk.Get_Object (Canvas), "item_selected" & ASCII.NUL,
         Canvas.Selection.Item);

      --  Initialize the move

      Canvas.Last_X_Event := To_World (Canvas, Gint (Get_X_Root (Event)));
      Canvas.Last_Y_Event := To_World (Canvas, Gint (Get_Y_Root (Event)));
      Canvas.Mouse_Has_Moved := False;

      --  Make sure that no other widget steal the events while we are
      --  moving an item.

      Grab_Add (Canvas);

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
   begin
      Grab_Remove (Canvas);

      if Canvas.Selection = null then
         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);
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
         Set_X (Event, Gdouble (Get_Value (Canvas.Hadj)) +  Get_X (Event)
                - Gdouble (To_Canvas (Canvas, Canvas.Selection.Item.Coord.X)));
         Set_Y (Event, Gdouble (Get_Value (Canvas.Vadj)) + Get_Y (Event)
                - Gdouble (To_Canvas (Canvas, Canvas.Selection.Item.Coord.Y)));
         On_Button_Click (Canvas.Selection.Item, Event);
      end if;

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

         X_Scroll := To_World (Canvas, Gint (Get_X_Root (Event)));
         Y_Scroll := To_World (Canvas, Gint (Get_Y_Root (Event)));
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
               X      => To_Canvas (Canvas, Selected.X) - Xbase,
               Y      => To_Canvas (Canvas, Selected.Y) - Ybase,
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

   ------------
   -- Pixmap --
   ------------

   function Pixmap
     (Item : access Canvas_Item_Record'Class) return Gdk.Pixmap.Gdk_Pixmap is
   begin
      return Item.Pixmap;
   end Pixmap;

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
            To_Canvas (Canvas, Item.Coord.X) - Xbase,
            To_Canvas (Canvas, Item.Coord.Y) - Ybase,
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

                  if Previous_Link = null then
                     Canvas.Links := Current_Link.Next;
                  else
                     Previous_Link.Next := Current_Link.Next;
                  end if;

                  Destroy (Current_Link.Link);
                  Free (Current_Link);
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
   begin
      while Current /= null loop
         if Current.Link.Src = Canvas_Item (From)
           and then Current.Link.Dest = Canvas_Item (To)
           and then (Name = "" or else Current.Link.Descr.all = Name)
         then
            return True;
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
      X1 : constant Gint := To_Canvas (Canvas, X);
      Y1 : constant Gint := To_Canvas (Canvas, Y);
      X2 : constant Gint := X1 + Gint (Item.Coord.Width);
      Y2 : constant Gint := Y1 + Gint (Item.Coord.Height);
   begin
      --  Do we need to scroll the canvas to the right to show the item?

      if X2 > Gint (Get_Upper (Canvas.Hadj)) then
         Set_Upper (Canvas.Hadj, Gfloat (X2));
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
         Set_Value (Canvas.Hadj, Gfloat (X2) - Get_Page_Size (Canvas.Hadj));
      end if;

      --  Do we need to scroll the canvas to the left ?

      if X1 < Gint (Get_Lower (Canvas.Hadj)) then
         Set_Lower (Canvas.Hadj, Gfloat (X1));
         Changed (Canvas.Hadj);
      end if;

      if X1 < Gint (Get_Value (Canvas.Hadj)) then
         Set_Value (Canvas.Hadj, Gfloat (X1));
      end if;

      --  Do we need to scroll the canvas to the top to show the selection?

      if Y2 > Gint (Get_Upper (Canvas.Vadj)) then
         Set_Upper (Canvas.Vadj, Gfloat (Y2));
         Changed (Canvas.Vadj);
      end if;

      if Y2 > Gint (Get_Value (Canvas.Vadj) + Get_Page_Size (Canvas.Vadj)) then
         Set_Value (Canvas.Vadj, Gfloat (Y2) - Get_Page_Size (Canvas.Vadj));
      end if;

      --  Do we need to scroll the canvas to the bottom ?

      if Y1 < Gint (Get_Lower (Canvas.Vadj)) then
         Set_Lower (Canvas.Vadj, Gfloat (Y1));
         Changed (Canvas.Vadj);
      end if;

      if Y1 < Gint (Get_Value (Canvas.Vadj)) then
         Set_Value (Canvas.Vadj, Gfloat (Y1));
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
         else
            Previous := Tmp;
            Tmp := Tmp.Next;
         end if;
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
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : in Arrow_Type := End_Arrow;
      Descr  : in String := "";
      Side   : Link_Side := Automatic)
   is
      L : Canvas_Link;
   begin
      L := new Canvas_Link_Record;
      Configure (L, Src, Dest, Arrow, Descr);
      Add_Link (Canvas, L, Side);
   end Add_Link;

   --------------
   -- Has_Link --
   --------------

   function Has_Link
     (Canvas    : access Interactive_Canvas_Record'Class;
      Src, Dest : access Canvas_Item_Record'Class;
      Side      : Link_Side;
      Offset    : Gint) return Boolean
   is
      Current    : Canvas_Link_List := Canvas.Links;
      Other_Side : Link_Side := Side;

   begin
      if Side = Left then
         Other_Side := Right;
      elsif Side = Right then
         Other_Side := Left;
      end if;

      while Current /= null loop
         if Current.Link.Src = Canvas_Item (Src)
           and then Current.Link.Dest = Canvas_Item (Dest)
           and then Current.Link.Side = Side
           and then Current.Link.Offset = Offset
         then
            return True;
         end if;

         if Current.Link.Src = Canvas_Item (Dest)
           and then Current.Link.Dest = Canvas_Item (Src)
           and then Current.Link.Side = Other_Side
           and then Current.Link.Offset = Offset
         then
            return True;
         end if;
         Current := Current.Next;
      end loop;
      return False;
   end Has_Link;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class;
      Side   : Link_Side := Automatic)
   is
      Offset    : Gint := 1;
      Auto_Side : Link_Side;
   begin
      --  Find the type of link that should be used.
      --  We can't use straight links for self referencing links.

      if Link.Src /= Link.Dest
        and then
        (Side = Straight
         or else
         (Side = Automatic
          and then not Has_Link (Canvas, Link.Src, Link.Dest, Straight, 0)))
      then
         Auto_Side := Straight;
         Offset := 0;
      else
         loop
            if Side /= Left and then
              not Has_Link (Canvas, Link.Src, Link.Dest, Right, Offset)
            then
               Auto_Side := Right;
               exit;
            elsif Side /= Right and then
              not Has_Link (Canvas, Link.Src, Link.Dest, Left, Offset)
            then
               Auto_Side := Left;
               exit;
            end if;
            Offset := Offset + 1;
         end loop;
      end if;

      Link.Side   := Auto_Side;
      Link.Offset := Offset;
      Canvas.Links := new Canvas_Link_List_Record'
        (Link => Canvas_Link (Link),
         Next => Canvas.Links);
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

   begin
      while Current /= null loop
         if Current.Link = Canvas_Link (Link) then
            if Previous = null then
               Canvas.Links := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;
            Free (Current);
            return;
         end if;
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
   begin
      while Current /= null loop
         Tmp := Current.Next;
         exit when not Execute (Canvas, Current.Link);
         Current := Tmp;
      end loop;
   end For_Each_Link;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Link : access Canvas_Link_Record) is
      L : Canvas_Link := Canvas_Link (Link);
   begin
      Free (Link.Descr);
      Free (L);
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
      Z : constant Gfloat := Gfloat (Canvas.Zoom);
      H : Gfloat := Get_Value (Canvas.Hadj) / Z * 100.0;
      V : Gfloat := Get_Value (Canvas.Vadj) / Z * 100.0;
      Lh : Gfloat := Get_Page_Size (Canvas.Hadj) / Z * 100.0;
      Lv : Gfloat := Get_Page_Size (Canvas.Vadj) / Z * 100.0;
      L : Gfloat;
      Font_Size : Gint;
      X_Min, X_Max, Y_Min, Y_Max : Gint;
   begin
      Canvas.Zoom := Percent;

      if Canvas.Font /= null then
         Unref (Canvas.Font);
      end if;

      Font_Size := To_Canvas (Canvas, Canvas.Annotation_Height);
      if Font_Size >= 4 then
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
      --  As a special rule, if we can display all the items in the canvas
      --  without requiring scroll bars, we do so.

      Get_Bounding_Box (Canvas, X_Min, X_Max, Y_Min, Y_Max);
      if X_Max - X_Min <= Gint (Get_Allocation_Width (Canvas))
        and then Y_Max - Y_Min <= Gint (Get_Allocation_Height (Canvas))
      then
         declare
            W : constant Gint := Gint (Get_Allocation_Width (Canvas));
            H : constant Gint := Gint (Get_Allocation_Height (Canvas));
         begin
            Set_Lower
              (Canvas.Hadj, Gfloat (X_Min - (W + X_Min - X_Max) / 2));
            Set_Upper
              (Canvas.Hadj, Gfloat (X_Max + (W + X_Min - X_Max) / 2));
            Set_Page_Size (Canvas.Hadj, Gfloat (X_Max - X_Min));
            Set_Value (Canvas.Hadj, Get_Lower (Canvas.Hadj));

            Set_Lower
              (Canvas.Vadj, Gfloat (Y_Min - (H + Y_Min - Y_Max) / 2));
            Set_Upper
              (Canvas.Vadj, Gfloat (Y_Max + (H + Y_Min - Y_Max) / 2));
            Set_Page_Size (Canvas.Vadj, Gfloat (Y_Max - Y_Min));
            Set_Value (Canvas.Vadj, Get_Lower (Canvas.Vadj));
         end;

      else

         --  zoom out
         if Gfloat (Canvas.Zoom) < Z then
            L := (Z / Gfloat (Canvas.Zoom) - 1.0) / 2.0;
            H := (H - L * Lh) * Gfloat (Canvas.Zoom) / 100.0;
            V := (V - L * Lv) * Gfloat (Canvas.Zoom) / 100.0;
            Lh := Lh * (L * 2.0 + 1.0) * Gfloat (Canvas.Zoom) / 100.0;
            Lv := Lv * (L * 2.0 + 1.0) * Gfloat (Canvas.Zoom) / 100.0;

            --  zoom in
         else
            L := (1.0 - Z / Gfloat (Canvas.Zoom)) / 2.0;
            H := (H + Lh * L) * Gfloat (Canvas.Zoom) / 100.0;
            V := (V + Lv * L) * Gfloat (Canvas.Zoom) / 100.0;
            Lh := Lh * Z / 100.0;
            Lv := Lv * Z / 100.0;
         end if;

         Set_Lower (Canvas.Hadj, Gfloat'Min (H, Get_Lower (Canvas.Hadj)));
         Set_Upper (Canvas.Hadj, Gfloat'Max (H + Lh, Get_Upper (Canvas.Hadj)));
         Set_Page_Size (Canvas.Hadj, Lh);

         Set_Lower (Canvas.Vadj, Gfloat'Min (V, Get_Lower (Canvas.Vadj)));
         Set_Upper (Canvas.Vadj, Gfloat'Max (V + Lv, Get_Upper (Canvas.Vadj)));
         Set_Page_Size (Canvas.Vadj, Lv);

         Set_Value (Canvas.Hadj, H);
         Set_Value (Canvas.Vadj, V);
      end if;

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
end Gtkada.Canvas;
