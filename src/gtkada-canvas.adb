with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Gdk.Color;        use Gdk.Color;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;           use Gdk.GC;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Types;        use Gdk.Types;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Window;       use Gdk.Window;
with Glib;             use Glib;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Main;         use Gtk.Main;
with Gtk.Style;        use Gtk.Style;
with Gtk.Viewport;     use Gtk.Viewport;
with Gtk.Widget;       use Gtk.Widget;
with Unchecked_Deallocation;

with Ada.Text_IO;      use Ada.Text_IO;

package body Gtkada.Canvas is

   -----------------
   -- Subprograms --
   -----------------

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   package Event_Handler is new Gtk.Handlers.Return_Callback
     (Widget_Type => Interactive_Canvas_Record, Return_Type => Boolean);

   package Realize_Handler is new Gtk.Handlers.Callback
     (Widget_Type => Interactive_Canvas_Record);

   function Expose (Canvas : access Interactive_Canvas_Record'Class;
                    Event         : Gdk.Event.Gdk_Event)
                   return Boolean;
   --  Handle the expose events for a canvas.

   procedure Realized (Canvas : access Interactive_Canvas_Record'Class);
   --  Create all the graphic contexts required for the animation.

   procedure Update_Links
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Item   : in Canvas_Item := null);
   --  Redraw all the links in the canvas.
   --  If Item is not null, only the links to or from Item are redrawn.

   procedure Redraw_Canvas (Canvas : access Interactive_Canvas_Record'Class);
   --  Clear and redraw the whole canvas.

   function Button_Pressed (Canvas : access Interactive_Canvas_Record'Class;
                            Event : Gdk_Event)
                           return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  This tests whether an item was selected.

   function Button_Release (Canvas : access Interactive_Canvas_Record'Class;
                           Event : Gdk_Event)
                           return Boolean;
   --  Called when the user has released the mouse button.
   --  If an item was selected, this refreshed the canvas.

   function Button_Motion (Canvas : access Interactive_Canvas_Record'Class;
                           Event : Gdk_Event)
                          return Boolean;
   --  Called when the user moves the mouse while a button is pressed.
   --  If an item was selected, the item is moved.

   procedure Clip_Line (From   : access Canvas_Item_Record'Class;
                        To_X   : in Gint;
                        To_Y   : in Gint;
                        X_Out  : out Gint;
                        Y_Out  : out Gint);
   --  Clip the line that goes from the middle of From to (To_X, To_Y).
   --  The intersection between that line and the border of From is returned
   --  in (X_Out, Y_Out).

   procedure Draw_Straight_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Link_Access);
   --  Draw Link on the screen as a straight line.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Arc_Link (Canvas : access Interactive_Canvas_Record'Class;
                            GC     : in Gdk.GC.Gdk_GC;
                            Link   : in Link_Access);
   --  Draw Link on the screen.
   --  The link is drawn as a curved link (ie there is an extra handle in its
   --  middle).
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Update_Adjustments
     (Canvas : access Interactive_Canvas_Record'Class);
   --  Update the adjustments of the canvas.
   --  This also resizes the Canvas itself, so that scrolling is usable if
   --  the canvas was put in a Scrolled_Window.
   --  This is the main function used to provide scrolling in case the widget
   --  was inserted in a scrolled window.

   procedure Draw_Arrow_Head (Canvas : access Interactive_Canvas_Record'Class;
                              GC     : in Gdk.GC.Gdk_GC;
                              X, Y   : Gint;
                              Angle  : in Float);
   --  Draw an arrow head at the position (X, Y) on the canvas.
   --  Angle is the angle of the main axis of the arrow.

   procedure Draw_Annotation (Canvas : access Interactive_Canvas_Record'Class;
                              GC     : in Gdk.GC.Gdk_GC;
                              X, Y   : Gint;
                              Str    : String_Access);
   --  Print an annotation on the canvas.
   --  The annotation is centered around (X, Y).

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
   begin
      --  Create the viewport.
      --  The adjustments will be added automatically when the canvas is
      --  put in a scrolled window.

      Gtk.Viewport.Initialize (Canvas, Null_Adjustment, Null_Adjustment);

      Gtk_New (Canvas.Drawing_Area);
      Add (Canvas, Canvas.Drawing_Area);

      Event_Handler.Object_Connect
        (Canvas.Drawing_Area, "expose_event",
         Event_Handler.To_Marshaller (Expose'Access),
         Slot_Object => Canvas);
      Realize_Handler.Object_Connect
        (Canvas.Drawing_Area, "realize",
         Realize_Handler.To_Marshaller (Realized'Access),
         Slot_Object => Canvas);
      Event_Handler.Object_Connect
        (Canvas.Drawing_Area, "button_press_event",
         Event_Handler.To_Marshaller (Button_Pressed'Access),
         Slot_Object => Canvas);
      Event_Handler.Object_Connect
        (Canvas.Drawing_Area, "button_release_event",
         Event_Handler.To_Marshaller (Button_Release'Access),
         Slot_Object => Canvas);
      Event_Handler.Object_Connect
        (Canvas.Drawing_Area, "motion_notify_event",
         Event_Handler.To_Marshaller (Button_Motion'Access),
         Slot_Object => Canvas);

      --  We want to be sure to get all the mouse events, that are required
      --  for the animation.

      Add_Events (Canvas.Drawing_Area, Gdk.Types.Button_Press_Mask
                  or Gdk.Types.Button_Motion_Mask
                  or Gdk.Types.Button_Release_Mask);

      Canvas.Annotation_Font := new String'(Default_Annotation_Font);
      Canvas.Arrow_Angle
        := Float (Default_Arrow_Angle) * Ada.Numerics.Pi / 180.0;
   end Initialize;

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
      Motion_Threshold  : Gint := Default_Motion_Threshold)
   is
   begin
      Canvas.Grid_Size := Grid_Size;
      Free (Canvas.Annotation_Font);
      Canvas.Annotation_Font := new String'(Annotation_Font);
      Canvas.Annotation_Height := Annotation_Height;
      Canvas.Arc_Link_Offset := Float (Arc_Link_Offset);
      Canvas.Arrow_Angle := Float (Arrow_Angle) * Ada.Numerics.Pi / 180.0;
      Canvas.Arrow_Length := Float (Arrow_Length);
      Canvas.Motion_Threshold := Motion_Threshold;

      Unref (Canvas.Font);
      Canvas.Font := Get_Gdkfont (Canvas.Annotation_Font.all,
                                  Canvas.Annotation_Height);

      --  Clean and redraw everything in the canvas, if the canvas is
      --  displayed on the screen.

      if Realized_Is_Set (Canvas) then
         Draw_Rectangle (Get_Window (Canvas.Drawing_Area),
                         GC     => Canvas.Clear_GC,
                         Filled => True,
                         X      => 0,
                         Y      => 0,
                         Width  => Gint (Get_Allocation_Width (Canvas)) - 1,
                         Height => Gint (Get_Allocation_Height (Canvas)) - 1);
         Queue_Draw (Canvas);
      end if;
   end Configure;

   -------------------
   -- Align_On_Grid --
   -------------------

   procedure Align_On_Grid (Canvas : access Interactive_Canvas_Record;
                            Align  : Boolean := True)
   is
   begin
      Canvas.Align_On_Grid := Align;
   end Align_On_Grid;

   ------------------------
   -- Update_Adjustments --
   ------------------------

   procedure Update_Adjustments
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Current      : Canvas_Item_List := Canvas.Children;
      X_Max, Y_Max : Gint := Gint'First;
   begin
      --  Find the smallest bounding box for all the items in the canvas.
      --  Note that this does not include links, which might thus be found
      --  outside of this box, but this does not really matter.
      --  Note also that we do not handle widgets whose location has negative
      --  coordinates, which we thus forbid in Button_Motion.

      while Current /= null loop
         if Current.Item.Coord.X + Gint (Current.Item.Coord.Width) > X_Max then
            X_Max := Current.Item.Coord.X + Gint16 (Current.Item.Coord.Width);
         end if;

         if Current.Item.Coord.Y + Gint (Current.Item.Coord.Height)
           > Y_Max
         then
            Y_Max := Current.Item.Coord.Y + Gint16 (Current.Item.Coord.Height);
         end if;
         Current := Current.Next;
      end loop;

      Canvas.Xmax := Gint (X_Max);
      Canvas.Ymax := Gint (Y_Max);

      --  Update the scrollbars.

      Set_Lower (Get_Hadjustment (Canvas), 0.0);
      Set_Upper (Get_Hadjustment (Canvas), Gfloat (X_Max));
      Changed (Get_Hadjustment (Canvas));

      Set_Lower (Get_Vadjustment (Canvas), 0.0);
      Set_Upper (Get_Vadjustment (Canvas), Gfloat (Y_Max));
      Changed (Get_Vadjustment (Canvas));

      --  Resize the canvas, so that scrolling can take place.

      Size (Canvas.Drawing_Area,
            X_Max + X_Thickness (Get_Style (Canvas)),
            Y_Max + Y_Thickness (Get_Style (Canvas)));
   end Update_Adjustments;

   ---------
   -- Put --
   ---------

   procedure Put (Canvas : access Interactive_Canvas_Record;
                  Item   : access Canvas_Item_Record'Class;
                  X, Y   : Gint := Gint'First)
   is
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
         Item.Coord.X := X - Gint (Canvas.Grid_Size);
         Item.Coord.Y := Y - Gint (Canvas.Grid_Size);
         Item.Coord.Width := Item.Coord.Width + 2 * Canvas.Grid_Size;
         Item.Coord.Height := Item.Coord.Height + 2 * Canvas.Grid_Size;

         while not Inter and then Tmp /= null loop

            if Tmp.Item /= Canvas_Item (Item) then
               Intersect (Tmp.Item.Coord, Item.Coord, Dest, Inter);
               Tmp := Tmp.Next;
            end if;
         end loop;
         Item.Coord.Width := Item.Coord.Width - 2 * Canvas.Grid_Size;
         Item.Coord.Height := Item.Coord.Height - 2 * Canvas.Grid_Size;
         return not Inter;
      end Location_Is_Free;

      Src_Item : Canvas_Item := null;
      Links    : Link_Access := Canvas.Links;
      X1       : Gint;
      Y1       : Gint;

   begin
      if X /= Gint'First and then Y /= Gint'First then
         Item.Coord.X := X;
         Item.Coord.Y := Y;
      else

         --  Check if there is any link that has for destination we widget we
         --  are adding.

         while Links /= null loop
            if Src_Item = null
              and then Links.Dest = Canvas_Item (Item)
            then
               Src_Item := Links.Src;
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
                  Num := Num + 1;
                  X1 := Src_Item.Coord.X + ((Num + 1) mod 3 - 1)
                    * Gint (Canvas.Grid_Size + Item.Coord.Width);
                  Y1 := Src_Item.Coord.Y + (Num / 3)
                    * Gint (Canvas.Grid_Size + Item.Coord.Height);
                  if X1 < 0 then
                     X1 := Src_Item.Coord.Y;
                  end if;
                  exit when Location_Is_Free (X1, Y1);
               end loop;
            end;

         --  Else put the item in the first line, at the first possible
         --  location

         else
            X1 := Gint (Canvas.Grid_Size);
            Y1 := Gint (Canvas.Grid_Size);
            while not Location_Is_Free (X1, Y1) loop
               X1 := X1 + Gint (Canvas.Grid_Size) * 2;
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

      Canvas.Children := new Canvas_Item_List_Record
        '(Item => Canvas_Item (Item),
          Next => Canvas.Children);

      Update_Adjustments (Canvas);
   end Put;

   --------------
   -- Realized --
   --------------

   procedure Realized (Canvas : access Interactive_Canvas_Record'Class) is
      use type Gdk.GC.Gdk_GC;
   begin

      --  Create all the graphic contexts if necessary.
      if Canvas.Black_GC = null then
         Gdk_New (Canvas.Black_GC, Get_Window (Canvas.Drawing_Area));
         Set_Foreground (Canvas.Black_GC,
                         Black (Gtk.Widget.Get_Default_Colormap));

         Gdk_New (Canvas.Clear_GC, Get_Window (Canvas.Drawing_Area));
         Set_Foreground (Canvas.Clear_GC,
                         Get_Background (Get_Style (Canvas), State_Normal));

         --  Note: when setting the line attributes below, it is very important
         --  for the Line_Width to be 0 so has to get algorithms as fast as
         --  possible (1 is way too slow for a proper interaction with the
         --  user).
         Gdk_New (Canvas.Anim_GC, Get_Window (Canvas.Drawing_Area));
         Set_Function (Canvas.Anim_GC, Invert);
         Set_Line_Attributes (Canvas.Anim_GC,
                              Line_Width => 0,
                              Line_Style => Line_On_Off_Dash,
                              Cap_Style  => Cap_Butt,
                              Join_Style => Join_Miter);

         Canvas.Font := Get_Gdkfont (Canvas.Annotation_Font.all,
                                     Canvas.Annotation_Height);
         if Canvas.Font = Null_Font then
            null;  --  ??  Should use a default font
         end if;
      end if;
   end Realized;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link (Canvas : access Interactive_Canvas_Record;
                       Src    : access Canvas_Item_Record'Class;
                       Dest   : access Canvas_Item_Record'Class;
                       Arrow  : in Arrow_Type := End_Arrow;
                       Descr  : in String := "")
   is
      function Has_Link (Side : Link_Side; Offset : Gint) return Boolean;
      --  Return True if there is already a link from Src to Dest with
      --  the specific side and offset specified in parameter.
      --  This is used to avoid having two link hidding each other.

      --------------
      -- Has_Link --
      --------------

      function Has_Link (Side : Link_Side; Offset : Gint) return Boolean is
         Current : Link_Access := Canvas.Links;
         Other_Side : Link_Side := Side;
      begin
         if Side = Left then
            Other_Side := Right;
         elsif Side = Right then
            Other_Side := Left;
         end if;

         while Current /= null loop
            if Current.Src = Canvas_Item (Src)
              and then Current.Dest = Canvas_Item (Dest)
              and then Current.Side = Side
              and then Current.Offset = Offset
            then
               return True;
            end if;

            if Current.Src = Canvas_Item (Dest)
              and then Current.Dest = Canvas_Item (Src)
              and then Current.Side = Other_Side
              and then Current.Offset = Offset
            then
               return True;
            end if;
            Current := Current.Next;
         end loop;
         return False;
      end Has_Link;

      Ptr    : String_Access := null;
      Offset : Gint := 1;
      Side   : Link_Side;
   begin
      if Descr /= "" then
         Ptr := new String'(Descr);
      end if;

      --  Find the type of link that should be used.
      --  We can't use straight links for self referencing links.

      if Canvas_Item (Src) /= Canvas_Item (Dest)
        and then not Has_Link (Straight, 0)
      then
         Side := Straight;
         Offset := 0;
      else
         loop
            if not Has_Link (Right, Offset) then
               Side := Right;
               exit;
            elsif not Has_Link (Left, Offset) then
               Side := Left;
               exit;
            end if;
            Offset := Offset + 1;
         end loop;
      end if;

      Canvas.Links := new Link'(Src    => Canvas_Item (Src),
                                Dest   => Canvas_Item (Dest),
                                Side   => Side,
                                Offset => Offset,
                                Descr  => Ptr,
                                Arrow  => Arrow,
                                Next   => Canvas.Links);
   end Add_Link;

   ---------------
   -- Clip_Line --
   ---------------

   procedure Clip_Line (From  : access Canvas_Item_Record'Class;
                        To_X  : in Gint;
                        To_Y  : in Gint;
                        X_Out : out Gint;
                        Y_Out : out Gint)
   is
      Center_X : constant Gint := From.Coord.X + Gint (From.Coord.Width) / 2;
      Center_Y : constant Gint := From.Coord.Y + Gint (From.Coord.Height) / 2;
      Delta_X  : constant Gint := abs (Center_X - To_X);
      Delta_Y  : constant Gint := abs (Center_Y - To_Y);
      Offset   : Gint := 1;
   begin
      --  East or West side

      if Gint (From.Coord.Height) * Delta_X
        > Gint (From.Coord.Width) * Delta_Y
      then
         if To_X <= Center_X then
            Offset := -1;
         end if;

         if Center_X /= To_X and then Center_Y /= To_Y then
            Y_Out := Center_Y + Offset * Gint (From.Coord.Width / 2)
              * (Center_Y - To_Y) / (Center_X - To_X);
         else
            Y_Out := Center_Y;
         end if;
         X_Out := Center_X + Offset * Gint (From.Coord.Width) / 2;

      --  North or South side
      else
         if To_Y <= Center_Y then
            Offset := -1;
         end if;

         if Center_X /= To_X and then Center_Y /= To_Y then
            X_Out := Center_X + Offset * Gint (From.Coord.Height / 2)
              * (Center_X - To_X) / (Center_Y - To_Y);
         else
            X_Out := Center_X;
         end if;
         Y_Out := Center_Y + Offset * Gint (From.Coord.Height) / 2;
      end if;
   end Clip_Line;

   ---------------------
   -- Draw_Arrow_Head --
   ---------------------

   procedure Draw_Arrow_Head (Canvas : access Interactive_Canvas_Record'Class;
                              GC     : in Gdk.GC.Gdk_GC;
                              X, Y   : Gint;
                              Angle  : in Float)
   is
   begin
      Draw_Polygon
        (Get_Window (Canvas.Drawing_Area),
         GC,
         Filled => True,
         Points =>
           ((X => X, Y => Y),
            (X => X
             + Gint (Canvas.Arrow_Length * Cos (Angle + Canvas.Arrow_Angle)),
             Y => Y
             + Gint (Canvas.Arrow_Length * Sin (Angle + Canvas.Arrow_Angle))),
            (X => X
             + Gint (Canvas.Arrow_Length * Cos (Angle - Canvas.Arrow_Angle)),
             Y => Y
             + Gint (Canvas.Arrow_Length
                     * Sin (Angle - Canvas.Arrow_Angle)))));
   end Draw_Arrow_Head;

   ---------------------
   -- Draw_Annotation --
   ---------------------

   procedure Draw_Annotation (Canvas : access Interactive_Canvas_Record'Class;
                              GC     : in Gdk.GC.Gdk_GC;
                              X, Y   : Gint;
                              Str    : String_Access)
   is
   begin
      Draw_Text (Get_Window (Canvas.Drawing_Area),
                 Canvas.Font,
                 GC,
                 X    => X - String_Width (Canvas.Font, Str.all) / 2,
                 Y    => Y - String_Height (Canvas.Font, Str.all) / 2,
                 Text => Str.all);
   end Draw_Annotation;

   ------------------------
   -- Draw_Straight_Link --
   ------------------------

   procedure Draw_Straight_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Link   : in Link_Access)
   is
      X1, Y1, X2, Y2 : Gint;
   begin
      Clip_Line (Link.Src, Link.Dest.Coord.X, Link.Dest.Coord.Y,
                 X_Out => X1, Y_Out => Y1);
      Clip_Line (Link.Dest, Link.Src.Coord.X, Link.Src.Coord.Y,
                 X_Out => X2, Y_Out => Y2);

      --  Draw the link itself

      Draw_Line (Get_Window (Canvas.Drawing_Area), GC, X1, Y1, X2, Y2);

      --  Draw the end arrow head

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then

         if X1 /= X2 then
            Draw_Arrow_Head (Canvas, GC, X2, Y2,
                             Arctan (Float (Y1 - Y2), Float (X1 - X2)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head (Canvas, GC, X2, Y2, Ada.Numerics.Pi / 2.0);
         else
            Draw_Arrow_Head (Canvas, GC, X2, Y2, -Ada.Numerics.Pi / 2.0);
         end if;
      end if;

      --  Draw the start arrow head

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then

         if X1 /= X2 then
            Draw_Arrow_Head (Canvas, GC, X1, Y1,
                             Arctan (Float (Y2 - Y1), Float (X2 - X1)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head (Canvas, GC, X1, Y1, -Ada.Numerics.Pi / 2.0);
         else
            Draw_Arrow_Head (Canvas, GC, X1, Y1, +Ada.Numerics.Pi / 2.0);
         end if;
      end if;

      --  Draw the text if any

      if Link.Descr /= null then
         Draw_Annotation (Canvas, GC,
                          (X1 + X2) / 2,
                          (Y1 + Y2) / 2,
                          Link.Descr);
      end if;

   end Draw_Straight_Link;

   -------------------
   -- Draw_Arc_Link --
   -------------------

   procedure Draw_Arc_Link (Canvas : access Interactive_Canvas_Record'Class;
                            GC     : in Gdk.GC.Gdk_GC;
                            Link   : in Link_Access)
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

      procedure Center (Center_X, Center_Y : out Gint)
      is
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

      use type Gdk.GC.Gdk_GC;
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
   begin

      --  Special case for self-referencing links

      if Link.Src = Link.Dest then
         Xc := Link.Src.Coord.X + Gint (Link.Src.Coord.Width);
         Yc := Link.Src.Coord.Y;
         Radius := Gint (Canvas.Arc_Link_Offset) / 2 * Link.Offset;
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

         X1 := Link.Src.Coord.X  + Gint (Link.Src.Coord.Width) / 2;
         Y1 := Link.Src.Coord.Y  + Gint (Link.Src.Coord.Height) / 2;
         X3 := Link.Dest.Coord.X + Gint (Link.Dest.Coord.Width) / 2;
         Y3 := Link.Dest.Coord.Y + Gint (Link.Dest.Coord.Height) / 2;

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
            Offset_X := Gint (Canvas.Arc_Link_Offset
                              * Cos (Angle - Right_Angle));
            Offset_Y := Gint (Canvas.Arc_Link_Offset
                              * Sin (Angle - Right_Angle));
         else
            Offset_X := Gint (Canvas.Arc_Link_Offset
                              * Cos (Angle + Right_Angle));
            Offset_Y := Gint (Canvas.Arc_Link_Offset
                              * Sin (Angle + Right_Angle));
         end if;

         X2 := (X1 + X3) / 2 + Offset_X * Link.Offset;
         Y2 := (Y1 + Y3) / 2 + Offset_Y * Link.Offset;

         --  Clip to the border of the boxes

         Clip_Line (Link.Src,  X2, Y2, X_Out => X1, Y_Out => Y1);
         Clip_Line (Link.Dest, X2, Y2, X_Out => X3, Y_Out => Y3);

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
            Angle_To   :=
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

      Draw_Arc (Get_Window (Canvas.Drawing_Area),
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

         Draw_Arrow_Head (Canvas, GC, X3, Y3, Angle);
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

         Draw_Arrow_Head (Canvas, GC, X1, Y1, Angle);
      end if;

      --  Draw the text if any

      if Link.Descr /= null then
         Draw_Annotation (Canvas, GC, X2, Y2, Link.Descr);
      end if;

   end Draw_Arc_Link;

   ------------------
   -- Update_Links --
   ------------------

   procedure Update_Links
     (Canvas : access Interactive_Canvas_Record'Class;
      GC     : in Gdk.GC.Gdk_GC;
      Item   : in Canvas_Item := null)
   is
      Current : Link_Access := Canvas.Links;
   begin
      while Current /= null loop
         if Item = null
           or else Current.Src = Canvas_Item (Item)
           or else Current.Dest = Canvas_Item (Item)
         then
            if Current.Side = Straight then
               Draw_Straight_Link (Canvas, GC, Current);
            else
               Draw_Arc_Link (Canvas, GC, Current);
            end if;
         end if;
         Current := Current.Next;
      end loop;
   end Update_Links;

   ------------
   -- Expose --
   ------------

   function Expose (Canvas : access Interactive_Canvas_Record'Class;
                    Event         : Gdk.Event.Gdk_Event)
                   return Boolean
   is
      pragma Warnings (Off, Event);
      Tmp : Canvas_Item_List := Canvas.Children;
      X : Guint := Canvas.Grid_Size;
      Y : Guint := Canvas.Grid_Size;
   begin

      --  If there are some expose events following this one, we don't have
      --  anything to do yet.

      if Get_Count (Event) /= 0 then
         return False;
      end if;

      --  Draw the background dots.

      while Y < Get_Allocation_Height (Canvas.Drawing_Area) loop
         X := Canvas.Grid_Size;
         while X < Get_Allocation_Width (Canvas.Drawing_Area) loop
            Draw_Point (Get_Window (Canvas.Drawing_Area),
                        GC       => Canvas.Black_GC,
                        X        => Gint (X),
                        Y        => Gint (Y));
            X := X + Canvas.Grid_Size;
         end loop;
         Y := Y + Canvas.Grid_Size;
      end loop;

      --  Draw the links first, so that they appear to be below the items.

      Update_Links (Canvas, Canvas.Black_GC);

      --  Draw each of the items.

      while Tmp /= null loop
         Draw_Pixmap (Get_Window (Canvas.Drawing_Area),
                      GC      => Canvas.Black_GC,
                      Src     => Pixmap (Tmp.Item),
                      Xsrc    => 0,
                      Ysrc    => 0,
                      Xdest   => Tmp.Item.Coord.X,
                      Ydest   => Tmp.Item.Coord.Y);
         Tmp := Tmp.Next;
      end loop;
      return False;
   end Expose;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item   : access Canvas_Item_Record'Class;
                         Win    : Gdk_Window;
                         Width  : Gint;
                         Height : Gint)
   is
      use type Gdk.Pixmap.Gdk_Pixmap;
   begin
      Item.Coord.Width  := Guint (Width);
      Item.Coord.Height := Guint (Height);

      if Item.Pixmap /= null then
         Gdk.Pixmap.Unref (Item.Pixmap);
      end if;

      --  Create the pixmap
      Gdk_New (Item.Pixmap, Win, Width, Height);
   end Initialize;

   -------------------
   -- Redraw_Canvas --
   -------------------

   procedure Redraw_Canvas (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      --  Adjust the scrollable area, and resize the canvas if needed

      Update_Adjustments (Canvas);

      --  Clean everything in the canvas.

      Clear_Area_E (Get_Window (Canvas.Drawing_Area), 0, 0,
                    Gint (Get_Allocation_Width (Canvas)) - 1,
                    Gint (Get_Allocation_Height (Canvas)) - 1);

   end Redraw_Canvas;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed (Canvas : access Interactive_Canvas_Record'Class;
                            Event : Gdk_Event)
                           return Boolean
   is
      Tmp : Canvas_Item_List := Canvas.Children;
      X : Gint := Gint (Get_X (Event));
      Y : Gint := Gint (Get_Y (Event));
      Item    : Canvas_Item;
   begin

      Canvas.Selected_Child := null;

      --  Find the selected item.

      while Tmp /= null loop
         if X >= Tmp.Item.Coord.X
           and then X <= Tmp.Item.Coord.X + Gint (Tmp.Item.Coord.Width)
           and then Y >= Tmp.Item.Coord.Y
           and then Y <= Tmp.Item.Coord.Y + Gint (Tmp.Item.Coord.Height)
         then
            Canvas.Selected_Child := Tmp.Item;
            exit;
         end if;
         Tmp := Tmp.Next;
      end loop;

      --  Double-click events are transmitted directly to the item, and are
      --  not used to move an item.

      if Get_Event_Type (Event) = Gdk_2button_Press then
         Set_X (Event,
                Get_X (Event) - Gdouble (Canvas.Selected_Child.Coord.X));
         Set_Y (Event,
                Get_Y (Event) - Gdouble (Canvas.Selected_Child.Coord.Y));
         On_Button_Click (Canvas.Selected_Child, Event);
      end if;

      --  Only left mouse button clicks can select an item

      if Get_Button (Event) /= 1 then
         return False;
      end if;

      --  If there was none, nothing to do...

      if Canvas.Selected_Child = null then
         return False;
      end if;

      Canvas.Last_X_Event := Gint (Get_X_Root (Event));
      Canvas.Last_Y_Event := Gint (Get_Y_Root (Event));
      Canvas.Mouse_Has_Moved := False;

      --  Highlight the item and links that will be moved.

      Item := Canvas.Selected_Child;

      if Canvas.Align_On_Grid then
         X := Item.Coord.X;
         Y := Item.Coord.Y;
         Item.Coord.X := Item.Coord.X
           - Item.Coord.X mod Gint (Canvas.Grid_Size);
         Item.Coord.Y := Item.Coord.Y
           - Item.Coord.Y mod Gint (Canvas.Grid_Size);
      end if;

      Draw_Rectangle (Get_Window (Canvas.Drawing_Area),
                      GC     => Canvas.Anim_GC,
                      Filled => False,
                      X      => Item.Coord.X,
                      Y      => Item.Coord.Y,
                      Width  => Gint (Item.Coord.Width) - 1,
                      Height => Gint (Item.Coord.Height) - 1);
      Update_Links (Canvas, Canvas.Anim_GC, Item);

      if Canvas.Align_On_Grid then
         Item.Coord.X := X;
         Item.Coord.Y := Y;
      end if;

      --  Make sure that no other widget steal the events while we are
      --  moving an item.

      Grab_Add (Canvas.Drawing_Area);

      return False;
   end Button_Pressed;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release (Canvas : access Interactive_Canvas_Record'Class;
                            Event : Gdk_Event)
                           return Boolean
   is
      Item    : Canvas_Item renames Canvas.Selected_Child;
      Tmp : Canvas_Item_List := Canvas.Children;
      Delta_X, Delta_Y : Gint := 0;
      Min_X, Min_Y : Gint := Gint'Last;
   begin
      if Canvas.Selected_Child = null then
         return False;
      end if;

      --  Move the items if the new one was moved out of the canvas.

      if Item.Coord.X < Gint (Canvas.Grid_Size) then
         Delta_X := -Item.Coord.X + Gint (Canvas.Grid_Size);
      end if;

      if Item.Coord.Y < Gint (Canvas.Grid_Size) then
         Delta_Y := -Item.Coord.Y + Gint (Canvas.Grid_Size);
      end if;

      if Delta_X = 0 and then Delta_Y = 0 then
         Tmp := Canvas.Children;
         while Tmp /= null loop
            if Tmp.Item.Coord.X < Min_X then
               Min_X := Tmp.Item.Coord.X;
            end if;
            if Tmp.Item.Coord.Y < Min_Y then
               Min_Y := Tmp.Item.Coord.Y;
            end if;
            Tmp := Tmp.Next;
         end loop;
         Delta_X := Gint (Canvas.Grid_Size) - Min_X;
         Delta_Y := Gint (Canvas.Grid_Size) - Min_Y;
      end if;

      if Delta_X /= 0 or else Delta_Y /= 0 then
         Tmp := Canvas.Children;
         while Tmp /= null loop
            Tmp.Item.Coord.X := Tmp.Item.Coord.X + Delta_X;
            Tmp.Item.Coord.Y := Tmp.Item.Coord.Y + Delta_Y;
            Tmp := Tmp.Next;
         end loop;
      end if;

      --  Align the items on the grid.

      if Canvas.Align_On_Grid then
         Item.Coord.X := Item.Coord.X
           - Item.Coord.X mod Gint (Canvas.Grid_Size);
         Item.Coord.Y := Item.Coord.Y
           - Item.Coord.Y mod Gint (Canvas.Grid_Size);
      end if;

      --  If the user did not move the mouse while it was pressed, this is
      --  because he only wanted to select the item.

      if not Canvas.Mouse_Has_Moved then
         Set_X (Event,
                Get_X (Event) - Gdouble (Canvas.Selected_Child.Coord.X));
         Set_Y (Event,
                Get_Y (Event) - Gdouble (Canvas.Selected_Child.Coord.Y));
         On_Button_Click (Canvas.Selected_Child, Event);
      end if;

      Redraw_Canvas (Canvas);

      --  Other widgets can now receive events as well.

      Grab_Remove (Canvas.Drawing_Area);

      return False;
   end Button_Release;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion (Canvas : access Interactive_Canvas_Record'Class;
                           Event : Gdk_Event)
                          return Boolean
   is
      use type Gdk.Window.Gdk_Window;
      Delta_X : Gint;
      Delta_Y : Gint;
      Item    : constant Canvas_Item := Canvas.Selected_Child;
      X, Y    : Gint;

   begin
      if Item = null then
         return False;
      end if;

      Delta_X := Gint (Get_X_Root (Event)) - Canvas.Last_X_Event;
      Delta_Y := Gint (Get_Y_Root (Event)) - Canvas.Last_Y_Event;

      --  Should this be considered as a mouse motion, or simply an item
      --  selection ?

      if not Canvas.Mouse_Has_Moved
        and then Delta_X < Canvas.Motion_Threshold
        and then Delta_Y < Canvas.Motion_Threshold
      then
         Canvas.Mouse_Has_Moved := True;
         return False;
      end if;

      --  Delete the currently dashed lines

      if Canvas.Align_On_Grid then
         X := Item.Coord.X;
         Y := Item.Coord.Y;
         Item.Coord.X := Item.Coord.X
           - Item.Coord.X mod Gint (Canvas.Grid_Size);
         Item.Coord.Y := Item.Coord.Y
           - Item.Coord.Y mod Gint (Canvas.Grid_Size);
      end if;

      Draw_Rectangle (Get_Window (Canvas.Drawing_Area),
                      GC     => Canvas.Anim_GC,
                      Filled => False,
                      X      => Item.Coord.X,
                      Y      => Item.Coord.Y,
                      Width  => Gint (Item.Coord.Width) - 1,
                      Height => Gint (Item.Coord.Height) - 1);
      Update_Links (Canvas, Canvas.Anim_GC, Item);

      if Canvas.Align_On_Grid then
         Item.Coord.X := X;
         Item.Coord.Y := Y;
      end if;

      --  Move everything

      Canvas.Last_X_Event := Gint (Get_X_Root (Event));
      Canvas.Last_Y_Event := Gint (Get_Y_Root (Event));
      Item.Coord.X := Item.Coord.X + Delta_X;
      Item.Coord.Y := Item.Coord.Y + Delta_Y;

      --  Redraw the dashed lines in a new position
      --  Note that in the case of Align_On_Grid, we do not want to constrain
      --  the item location now, since this wouldn't work.

      if Canvas.Align_On_Grid then
         X := Item.Coord.X;
         Y := Item.Coord.Y;
         Item.Coord.X := Item.Coord.X
           - Item.Coord.X mod Gint (Canvas.Grid_Size);
         Item.Coord.Y := Item.Coord.Y
           - Item.Coord.Y mod Gint (Canvas.Grid_Size);
      end if;

      Draw_Rectangle (Get_Window (Canvas.Drawing_Area),
                      GC     => Canvas.Anim_GC,
                      Filled => False,
                      X      => Item.Coord.X,
                      Y      => Item.Coord.Y,
                      Width  => Gint (Item.Coord.Width) - 1,
                      Height => Gint (Item.Coord.Height) - 1);
      Update_Links (Canvas, Canvas.Anim_GC, Item);

      if Canvas.Align_On_Grid then
         Item.Coord.X := X;
         Item.Coord.Y := Y;
      end if;

      return False;
   end Button_Motion;

   ------------
   -- Pixmap --
   ------------

   function Pixmap (Item : access Canvas_Item_Record'Class)
                   return Gdk.Pixmap.Gdk_Pixmap
   is
   begin
      return Item.Pixmap;
   end Pixmap;

   ------------------
   -- Item_Updated --
   ------------------

   procedure Item_Updated (Canvas : access Interactive_Canvas_Record;
                           Item   : access Canvas_Item_Record'Class)
   is
   begin
      Redraw_Canvas (Canvas);
   end Item_Updated;

   ------------------
   -- Item_Resized --
   ------------------

   procedure Item_Resized (Canvas : access Interactive_Canvas_Record;
                           Item   : access Canvas_Item_Record'Class)
   is
   begin
      Redraw_Canvas (Canvas);
   end Item_Resized;

   ------------
   -- Remove --
   ------------

   procedure Remove (Canvas : access Interactive_Canvas_Record;
                     Item   : access Canvas_Item_Record'Class)
   is
      procedure Free is new Unchecked_Deallocation (Link, Link_Access);
      procedure Free is new Unchecked_Deallocation
        (Canvas_Item_List_Record, Canvas_Item_List);
      Previous     : Canvas_Item_List := null;
      Current      : Canvas_Item_List := Canvas.Children;
      Previous_Link : Link_Access;
      Current_Link : Link_Access;
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
               if Current_Link.Src = Canvas_Item (Item)
                 or else Current_Link.Dest = Canvas_Item (Item)
               then

                  if Previous_Link = null then
                     Canvas.Links := Current_Link.Next;
                  else
                     Previous_Link.Next := Current_Link.Next;
                  end if;

                  Free (Current_Link.Descr);
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

            return;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click (Item   : access Canvas_Item_Record;
                              Event  : Gdk.Event.Gdk_Event_Button)
   is
   begin
      null;
   end On_Button_Click;

   ---------------
   -- Get_Coord --
   ---------------

   function Get_Coord (Item : access Canvas_Item_Record)
                      return Gdk.Rectangle.Gdk_Rectangle
   is
   begin
      return Item.Coord;
   end Get_Coord;

end Gtkada.Canvas;
