------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Tags;          use Ada.Tags;
with Cairo;             use Cairo;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Glib;              use Glib;
with Glib.Main;         use Glib.Main;
with Glib.Object;       use Glib.Object;
with Gtk.Accel_Group;   use Gtk.Accel_Group;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Widget;        use Gtk.Widget;
with System;            use System;

package body Gtkada.Canvas_View.Views is
   use Gdouble_Elementary_Functions;

   Min_Animation_Interval : constant Duration := 2.0 * 0.017;
   --  (in seconds). This is computed for a refresh rate of 60Hs,
   --  with 1s / 60.
   --  The animation loop will not do anything if called more often than that.

   Animation_Interval : constant Guint := 30;
   --  (in milliseconds). How long we give the application to perform one
   --  iteration of the animation loop.

   procedure On_Monitored_Destroyed
     (Minimap : System.Address; Monitored : System.Address);
   pragma Convention (C, On_Monitored_Destroyed);
   --  Called when the view monitored by a minimap is being destroyed.

   procedure On_Destroy_Minimap (Minimap : access Gtk_Widget_Record'Class);
   --  Called when the minimap is being destroyed

   procedure On_Monitored_Viewport_Changed
     (Minimap : not null access GObject_Record'Class);
   --  Called when the viewport has changed in the monitored view

   function On_Minimap_Item_Event
     (View  : not null access GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean;
   --  React to events in the minimap

   procedure Start_Continuous_Scrolling
     (Self    : not null access Canvas_View_Record'Class;
      Dx, Dy  : Model_Coordinate := 0.0);
   function Do_Continuous_Scroll (Self : Canvas_View) return Boolean;
   --  Start or cancel any continuous scrolling that might exist.

   package View_Sources is new Glib.Main.Generic_Sources (Canvas_View);

   procedure Move_Dragged_Items
     (Self           : not null access Canvas_View_Record'Class;
      Dx, Dy         : Model_Coordinate;
      From_Initial   : Boolean);
   --  Move all selected items (part of the current drag operation) by the
   --  specified amount (from their initial position if From_Initial is true,
   --  or from their current position otherwise)).

   package Animator_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Animator_Access);
   use Animator_Lists;

   type Animation_Data is new Base_Animation_Data with record
      Last_Run : Ada.Calendar.Time := No_Time;
      Queue    : Animator_Lists.List;
      Current  : Animator_Lists.Cursor := Animator_Lists.No_Element;
   end record;
   type Animation_Data_Access is access all Animation_Data;

   procedure On_Animate_Destroy (View : in out Canvas_View);
   --  Called when the animation queue terminates

   function On_Animate (View : Canvas_View) return Boolean;
   --  Perform one step of animation

   procedure Animation_Iteration
     (Self     : not null access Canvas_View_Record'Class;
      Current  : Ada.Calendar.Time;
      Max_Time : Duration);
   --  Animate as many items from the queue as possible, spending at most
   --  Max_Time seconds (or at least not starting a new animator if the
   --  previous one ended more Max_Time seconds after the start).
   --  Current is the current time. It can be set to No_Time to indicate that
   --  all animations should be completed.

   type Position_Animator is new Animator with record
      Pos_X  : Animation_Value;
      Pos_Y  : Animation_Value;
   end record;
   overriding function Execute
     (Self     : not null access Position_Animator;
      Progress : Animation_Progress) return Animation_Status;

   type Scale_Animator is new Animator with record
      Scale    : Animation_Value;
      Preserve : Model_Point := No_Point;
   end record;
   overriding function Execute
     (Self     : not null access Scale_Animator;
      Progress : Animation_Progress) return Animation_Status;

   type Scroll_Animator is new Animator with record
      Topleft_X : Animation_Value;
      Topleft_Y : Animation_Value;
   end record;
   overriding function Execute
     (Self     : not null access Scroll_Animator;
      Progress : Animation_Progress) return Animation_Status;

   ------------------
   -- Snap_To_Grid --
   ------------------

   function Snap_To_Grid
     (Self        : not null access Canvas_View_Record'Class;
      Pos         : Model_Coordinate;
      Size        : Model_Coordinate) return Model_Coordinate
   is
      GX : Model_Coordinate;
   begin
      if Self.Snap.Margin = 0.0 then
         return Pos;
      end if;

      --  Find the closest grid position to the left of Pos
      GX := Gdouble (Gint (Pos / Self.Grid_Size)) * Self.Grid_Size;
      if abs (Pos - GX) < Self.Snap.Margin then
         return GX;
      end if;

      --  Find the closest grid position to the right of Pos
      GX := GX + Self.Grid_Size;
      if abs (Pos - GX) < Self.Snap.Margin then
         return GX;
      end if;

      --  Find the closest grid position to the left of Pos + size
      GX := Gdouble (Gint ((Pos + Size) / Self.Grid_Size)) * Self.Grid_Size;
      if abs (Pos + Size - GX) < Self.Snap.Margin then
         return GX - Size;
      end if;

      --  Find the closest grid position to the right of Pos + size
      GX := GX + Self.Grid_Size;
      if abs (Pos + Size - GX) < Self.Snap.Margin then
         return GX - Size;
      end if;

      return Pos;
   end Snap_To_Grid;

   ---------------------
   -- Draw_Grid_Lines --
   ---------------------

   procedure Draw_Grid_Lines
     (Self    : not null access Canvas_View_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
      Tmp  : Gdouble;
   begin
      if Style.Get_Fill /= Null_Pattern then
         Set_Source (Context.Cr, Style.Get_Fill);
         Paint (Context.Cr);
      end if;

      if Self.Grid_Size /= 0.0 then
         New_Path (Context.Cr);

         Tmp := Gdouble (Gint (Area.X / Self.Grid_Size)) * Self.Grid_Size;
         while Tmp < Area.X + Area.Width loop
            Move_To (Context.Cr, Tmp, Area.Y);
            Rel_Line_To (Context.Cr, 0.0, Area.Height);
            Tmp := Tmp + Self.Grid_Size;
         end loop;

         Tmp := Gdouble (Gint (Area.Y / Self.Grid_Size)) * Self.Grid_Size;
         while Tmp < Area.Y + Area.Height loop
            Move_To (Context.Cr, Area.X, Tmp);
            Rel_Line_To (Context.Cr, Area.Width, 0.0);
            Tmp := Tmp + Self.Grid_Size;
         end loop;

         Style.Finish_Path (Context.Cr);
      end if;
   end Draw_Grid_Lines;

   --------------------
   -- Draw_Grid_Dots --
   --------------------

   procedure Draw_Grid_Dots
     (Self    : not null access Canvas_View_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
      TmpX, TmpY  : Gdouble;
   begin
      if Style.Get_Fill /= Null_Pattern then
         Set_Source (Context.Cr, Style.Get_Fill);
         Paint (Context.Cr);
      end if;

      if Self.Grid_Size /= 0.0 then
         New_Path (Context.Cr);

         TmpX := Gdouble (Gint (Area.X / Self.Grid_Size)) * Self.Grid_Size;
         while TmpX < Area.X + Area.Width loop
            TmpY := Gdouble (Gint (Area.Y / Self.Grid_Size)) * Self.Grid_Size;
            while TmpY < Area.Y + Area.Height loop
               Rectangle (Context.Cr, TmpX - 0.5, TmpY - 0.5, 1.0, 1.0);
               TmpY := TmpY + Self.Grid_Size;
            end loop;

            TmpX := TmpX + Self.Grid_Size;
         end loop;

         Style.Finish_Path (Context.Cr);
      end if;
   end Draw_Grid_Dots;

   -------------------------------------
   -- On_Item_Event_Scroll_Background --
   -------------------------------------

   function On_Item_Event_Scroll_Background
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Area, B : Model_Rectangle;
      X, Y    : Model_Coordinate;
   begin
      if Self.Model /= null
        and then Event.Toplevel_Item = null
      then
         if Event.Event_Type = Button_Press
           and then Event.Button = 1
         then
            --  Enable scrolling by dragging the background. However, there is
            --  no point showing an area where there is no item, so we limit
            --  the scrolling.
            Event.Allowed_Drag_Area :=
              Self.Model.Bounding_Box (Margin => View_Margin / Self.Get_Scale);
            --  Event.Dragging_Scrolls := True;
            return True;

         elsif Event.Event_Type = In_Drag then
            --  Compute the area that we are allowed to make visible. This
            --  is the combination of the allowed area and the currently
            --  visible one.

            X := Self.Topleft_At_Drag_Start.X
              - (Event.Root_Point.X
                 - Self.Last_Button_Press.Root_Point.X) / Self.Scale;

            Y := Self.Topleft_At_Drag_Start.Y
              - (Event.Root_Point.Y
                 - Self.Last_Button_Press.Root_Point.Y) / Self.Scale;

            if Self.Last_Button_Press.Allowed_Drag_Area /=
              Drag_Anywhere
            then
               Area := Self.Get_Visible_Area;
               B    := Self.Last_Button_Press.Allowed_Drag_Area;
               Union (B, Area);

               X := Model_Coordinate'Max (X, B.X);
               X := Model_Coordinate'Min (X, B.X + B.Width - Area.Width);
               Y := Model_Coordinate'Max (Y, B.Y);
               Y := Model_Coordinate'Min (Y, B.Y + B.Height - Area.Height);
            end if;

            Self.Center_On ((X, Y), X_Pos => 0.0, Y_Pos => 0.0);
         end if;
      end if;
      return False;
   end On_Item_Event_Scroll_Background;

   ---------------------------
   -- Cancel_Inline_Editing --
   ---------------------------

   procedure Cancel_Inline_Editing
     (Self    : not null access Canvas_View_Record'Class) is
   begin
      if Self.Inline_Edit.Item /= null then
         Self.Inline_Editing_Finished (Self.Inline_Edit.Item);
         Self.Inline_Edit.Item := null;
         Self.Remove (Self.Get_Child);
      end if;
   end Cancel_Inline_Editing;

   --------------------------------
   -- Inline_Editing_In_Progress --
   --------------------------------

   function Inline_Editing_In_Progress
     (Self : not null access Canvas_View_Record'Class)
     return Boolean is
   begin
      return Self.Inline_Edit.Item /= null;
   end Inline_Editing_In_Progress;

   --------------------------
   -- Start_Inline_Editing --
   --------------------------

   procedure Start_Inline_Editing
     (Self : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class)
   is
      W     : Gtk_Widget;
   begin
      Cancel_Inline_Editing (Self);
      W := Item.Edit_Widget (Self);
      if W /= null then
         Self.Inline_Edit.Item := Abstract_Item (Item);
         Self.Add (W);  --  also queues a resize, so calls On_Size_Allocate
         W.Show_All;
         W.Grab_Focus;
         Self.Inline_Editing_Started (Item);
      end if;
   end Start_Inline_Editing;

   ------------------------
   -- On_Item_Event_Edit --
   ------------------------

   function On_Item_Event_Edit
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self  : constant Canvas_View := Canvas_View (View);
   begin
      if Event.Item /= null then
         if Event.Event_Type = Double_Click
           or else (Event.Event_Type = Key_Press
                    and then Event.Key = GDK_Return)
         then
            Start_Inline_Editing (Self, Event.Item);
            return True;
         end if;
      end if;

      return False;
   end On_Item_Event_Edit;

   -----------------------------
   -- On_Item_Event_Move_Item --
   -----------------------------

   function On_Item_Event_Move_Item
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self  : constant Canvas_View := Canvas_View (View);
      Alloc : Gtk_Allocation;
      P     : View_Point;
   begin
      if Event.Event_Type = In_Drag
        and then not Self.Dragged_Items.Is_Empty
      then
         --  Disable snapping when shift is pressed.
         Event.Allow_Snapping := (Event.State and Shift_Mask) = 0;
         Self.Last_Button_Press.Allow_Snapping := Event.Allow_Snapping;

         --  The use of Topleft is to take into account the continuous
         --  scrolling

         Move_Dragged_Items
           (Self,
            Dx => (Self.Topleft.X - Self.Topleft_At_Drag_Start.X)
            + (Event.Root_Point.X
              - Self.Last_Button_Press.Root_Point.X) / Self.Scale,
            Dy => (Self.Topleft.Y - Self.Topleft_At_Drag_Start.Y)
            + (Event.Root_Point.Y
              - Self.Last_Button_Press.Root_Point.Y) / Self.Scale,
            From_Initial   => True);

         --  Should we do automatic scrolling of the view ?

         Self.Get_Allocation (Alloc);
         P := Self.Model_To_View (Event.M_Point);

         if P.X < View_Coordinate (Alloc.X)
           + Self.Continuous_Scroll.Margin
         then
            Start_Continuous_Scrolling
              (Self, Dx => -Self.Scale * Self.Continuous_Scroll.Speed);

         elsif P.X >
           View_Coordinate (Alloc.X + Alloc.Width)
           - Self.Continuous_Scroll.Margin
         then
            Start_Continuous_Scrolling
              (Self, Dx => Self.Scale * Self.Continuous_Scroll.Speed);

         elsif P.Y < View_Coordinate (Alloc.Y)
           + Self.Continuous_Scroll.Margin
         then
            Start_Continuous_Scrolling
              (Self, Dy => -Self.Scale * Self.Continuous_Scroll.Speed);

         elsif P.Y >
           View_Coordinate (Alloc.Y + Alloc.Height)
           - Self.Continuous_Scroll.Margin
         then
            Start_Continuous_Scrolling
              (Self, Dy => Self.Scale * Self.Continuous_Scroll.Speed);

         else
            Cancel_Continuous_Scrolling (Self);
         end if;

         Self.Queue_Draw;

      elsif Event.Event_Type = Start_Drag then
         --  Remove the waypoints for all the links that will be impacted.

         declare
            S : Item_Sets.Set;

            procedure Reset_Wp
              (Link : not null access Abstract_Item_Record'Class);
            procedure Add_Item
              (Item : not null access Abstract_Item_Record'Class);

            procedure Add_Item
              (Item : not null access Abstract_Item_Record'Class)
            is
            begin
               S.Include (Abstract_Item (Item));
            end Add_Item;

            procedure Reset_Wp
              (Link : not null access Abstract_Item_Record'Class)
            is
            begin
               Canvas_Link (Link).Set_Waypoints ((1 .. 0 => <>));
            end Reset_Wp;

         begin
            --  Self.Dragged_Items is not available yet
            Self.Model.For_Each_Item (Add_Item'Access, Selected_Only => True);
            Self.Model.For_Each_Link (Reset_Wp'Access, From_Or_To => S);
         end;

      elsif Event.Event_Type = End_Drag
        and then Self.Avoid_Overlap
      then
         declare
            use Item_Drag_Infos;
            C : Item_Drag_Infos.Cursor := Self.Dragged_Items.First;
            Do_Not_Move : Item_Sets.Set;
         begin
            while Has_Element (C) loop
               Do_Not_Move.Include (Element (C).Item);
               Next (C);
            end loop;

            C := Self.Dragged_Items.First;
            while Has_Element (C) loop
               Reserve_Space
                 (Self,
                  Rect        => Element (C).Item.Model_Bounding_Box,
                  Do_Not_Move => Do_Not_Move,
                  Duration    => Self.Avoid_Overlap_Duration);
               Next (C);
            end loop;
         end;

      elsif Event.Event_Type = Button_Press
        and then Event.Toplevel_Item /= null
        and then Event.Button = 1
      then
         --  Enable moving the item anywhere
         Event.Allowed_Drag_Area := Drag_Anywhere;
         Self.Model.Raise_Item (Event.Toplevel_Item);
         return True;
      end if;
      return False;
   end On_Item_Event_Move_Item;

   --------------------------------
   -- On_Item_Event_Zoom_Generic --
   --------------------------------

   function On_Item_Event_Zoom_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean is
   begin
      return On_Item_Event_Zoom
        (View, Event, Modifier, Factor,  Duration, Easing);
   end On_Item_Event_Zoom_Generic;

   ------------------------
   -- On_Item_Event_Zoom --
   ------------------------

   function On_Item_Event_Zoom
     (View     : not null access Glib.Object.GObject_Record'Class;
      Event    : Event_Details_Access;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
      Factor   : Gdouble;
      Duration : Standard.Duration;
      Easing   : Easing_Function)
      return Boolean
   is
      Self : constant Canvas_View := Canvas_View (View);
      S    : Gdouble;
   begin
      if Self.Model /= null and then Event.Event_Type = Scroll then
         if Event.State = Modifier then
            if Event.Button = 5 then
               S := Self.Get_Scale * Factor;
            else
               S := Self.Get_Scale / Factor;
            end if;

            if Duration > 0.0 then
               Start
                 (Animate_Scale
                    (Self, S, Preserve => Event.M_Point,
                     Duration => Duration, Easing => Easing),
                  Self);
            else
               Self.Set_Scale (S, Preserve => Event.M_Point);
            end if;

            Cancel_Inline_Editing (Self);
            return True;
         end if;
      end if;
      return False;
   end On_Item_Event_Zoom;

   --------------------------
   -- On_Item_Event_Select --
   --------------------------

   function On_Item_Event_Select
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self : constant Canvas_View := Canvas_View (View);
   begin
      if Self.Model /= null then
         if Event.Item = null
           or else not Self.Model.Is_Selectable (Event.Toplevel_Item)
         then
            case Event.Event_Type is
               when Button_Press =>
                  if (Event.State and Get_Default_Mod_Mask) = 0 then
                     Self.Model.Clear_Selection;
                  end if;

               when others =>
                  null;
            end case;

         else
            Self.Model.Raise_Item (Event.Toplevel_Item);

            case Event.Event_Type is
               when Button_Press =>
                  if Self.Model.Is_Selected (Event.Toplevel_Item) then
                     if (Event.State and Get_Default_Mod_Mask) = 0 then
                        null;   --  do nothing, preserve current selection

                     else
                        Self.Model.Remove_From_Selection (Event.Item);
                        Self.Model.Remove_From_Selection
                          (Event.Item.Get_Toplevel_Item);
                     end if;

                  else
                     if (Event.State and Get_Default_Mod_Mask) = 0 then
                        Self.Model.Clear_Selection;
                     end if;

                     Self.Model.Add_To_Selection
                       (Event.Item.Get_Toplevel_Item);
                  end if;

               when others =>
                  null;
            end case;
         end if;
      end if;
      return False;  --  always pass the event through
   end On_Item_Event_Select;

   ------------------
   -- Move_To_Item --
   ------------------

   function Move_To_Item
     (Self         : not null access Canvas_View_Record'Class;
      Item         : not null access Abstract_Item_Record'Class;
      Dir          : Gtk.Enums.Gtk_Direction_Type;
      Ignore_Links : Boolean := True)
      return Abstract_Item
   is
      Box : constant Model_Rectangle := Item.Model_Bounding_Box;
      Result : Abstract_Item := null;
      Best   : Model_Coordinate := Model_Coordinate'Last;
      Best_Center : Model_Coordinate := Model_Coordinate'Last;
      Best_Is_Centered : Boolean := False;

      procedure Do_Item (It : not null access Abstract_Item_Record'Class);
      procedure Do_Item (It : not null access Abstract_Item_Record'Class) is
         B        : constant Model_Rectangle := It.Model_Bounding_Box;
         D, C     : Model_Coordinate := Gdouble'Last;
         Centered : Boolean;
      begin
         if Ignore_Links and then It.Is_Link then
            return;
         end if;

         case Dir is
            when Dir_Tab_Forward | Dir_Right =>
               if B.X > Box.X + Box.Width then
                  --  Whether It is on the same "row" as Item (this is defined
                  --  computed so that any item in the same row gets precedence
                  --  other items farther away).
                  Centered := not
                    (B.Y + B.Height < Box.Y or else B.Y > Box.Y + Box.Height);

                  --  D is the vertical distance
                  D := Gdouble'Min (abs (B.X - Box.X - Box.Width), D);

                  --  C is the vertical distance between the centers
                  C := Gdouble'Min
                    (abs (Box.Y - B.Y + (Box.Height - B.Height) / 2.0), C);
               end if;

            when Dir_Tab_Backward | Dir_Left =>
               if B.X + B.Width < Box.X then
                  Centered := not
                    (B.Y + B.Height < Box.Y or else B.Y > Box.Y + Box.Height);
                  D := Gdouble'Min (abs (B.X + B.Width - Box.X), D);
                  C := Gdouble'Min
                    (abs (Box.Y - B.Y + (Box.Height - B.Height) / 2.0), C);
               end if;

            when Dir_Up =>
               if B.Y + B.Height < Box.Y then
                  Centered := not
                    (B.X + B.Width < Box.X or else B.X > Box.X + Box.Width);
                  D := Gdouble'Min (abs (B.Y + B.Height - Box.Y), D);
                  C := Gdouble'Min
                    (abs (Box.X - B.X + (Box.Width - B.Width) / 2.0), C);
               end if;

            when Dir_Down =>
               if B.Y > Box.Y + Box.Height then
                  Centered := not
                    (B.X + B.Width < Box.X or else B.X > Box.X + Box.Width);
                  D := Gdouble'Min (abs (B.Y - Box.Y - Box.Height), D);
                  C := Gdouble'Min
                    (abs (Box.X - B.X + (Box.Width - B.Width) / 2.0), C);
               end if;
         end case;

         if Best_Is_Centered then
            if Centered and then D < Best then
               Best := D;
               Result := Abstract_Item (It);
            end if;
         else
            if C < Best_Center then
               Best_Is_Centered := Centered;
               Best_Center := C;
               Best := D;
               Result := Abstract_Item (It);
            end if;
         end if;
      end Do_Item;

   begin
      Self.Model.For_Each_Item (Do_Item'Access);

      if Result = null then
         return Abstract_Item (Item);
      else
         return Result;
      end if;
   end Move_To_Item;

   ----------------------------------------
   -- On_Item_Event_Key_Navigate_Generic --
   ----------------------------------------

   function On_Item_Event_Key_Navigate_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self : constant Canvas_View := Canvas_View (View);
      Item : Abstract_Item;
   begin
      if Self.Model /= null
        and then Event.State = Modifier
        and then Event.Item /= null
      then
         case Event.Key is
            when GDK_Up    | GDK_KP_Up    =>
               Item := Move_To_Item
                 (Self, Event.Item, Dir_Up, Ignore_Links);
            when GDK_Down  | GDK_KP_Down  =>
               Item := Move_To_Item
                 (Self, Event.Item, Dir_Down, Ignore_Links);
            when GDK_Left  | GDK_KP_Left  =>
               Item := Move_To_Item
                 (Self, Event.Item, Dir_Left, Ignore_Links);
            when GDK_Right | GDK_KP_Right =>
               Item := Move_To_Item
                 (Self, Event.Item, Dir_Right, Ignore_Links);
            when others =>
               return False;
         end case;

         if Item /= null then
            Cancel_Inline_Editing (Self);

            Self.Model.Clear_Selection;
            Self.Model.Add_To_Selection (Item);
            Self.Scroll_Into_View (Item);
         end if;
         return True;
      end if;
      return False;
   end On_Item_Event_Key_Navigate_Generic;

   ---------------------------------------
   -- On_Item_Event_Key_Scrolls_Generic --
   ---------------------------------------

   function On_Item_Event_Key_Scrolls_Generic
     (View   : not null access Glib.Object.GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self : constant Canvas_View := Canvas_View (View);
      Box  : Model_Rectangle;
      Dx, Dy : Model_Coordinate := 0.0;
   begin
      if Self.Model /= null
        and then Event.State = Modifier
      then
         case Event.Key is
            when GDK_Up    | GDK_KP_Up    => Dy := -5.0;
            when GDK_Down  | GDK_KP_Down  => Dy := 5.0;
            when GDK_Left  | GDK_KP_Left  => Dx := -5.0;
            when GDK_Right | GDK_KP_Right => Dx := 5.0;
            when others => return False;
         end case;

         Cancel_Inline_Editing (Self);

         if Event.Item = null then
            Box := Self.Get_Visible_Area;
            Self.Center_On
              ((Box.X + Dx, Box.Y + Dy), X_Pos => 0.0, Y_Pos => 0.0);

         else
            Copy_Selected_To_Dragged_Items
              (Self, Force => Event.Toplevel_Item);
            Move_Dragged_Items
              (Self, Dx => Dx, Dy => Dy, From_Initial => True);
            Self.Dragged_Items.Clear;
            Self.Queue_Draw;
         end if;

         return True;
      end if;

      return False;
   end On_Item_Event_Key_Scrolls_Generic;

   --------------------------
   -- Prepare_Smart_Guides --
   --------------------------

   procedure Prepare_Smart_Guides
     (Self : not null access Canvas_View_Record'Class)
   is
      Screen : constant Model_Rectangle := Self.Get_Visible_Area;

      procedure Add_Guides
        (Guides              : in out Smart_Guide_Lists.List;
         Start, Middle, Last : Model_Coordinate;
         Min, Max            : Model_Coordinate);

      procedure Process_Item
        (Item : not null access Abstract_Item_Record'Class);
      --  Add smart guides for a specific item

      ----------------
      -- Add_Guides --
      ----------------

      procedure Add_Guides
        (Guides              : in out Smart_Guide_Lists.List;
         Start, Middle, Last : Model_Coordinate;
         Min, Max            : Model_Coordinate)
      is
         procedure Update_Guide (Guide : in out Smart_Guide);
         procedure Update_Guide (Guide : in out Smart_Guide) is
         begin
            Guide.Min := Model_Coordinate'Min (Guide.Min, Min);
            Guide.Max := Model_Coordinate'Max (Guide.Max, Max);
         end Update_Guide;

         use Smart_Guide_Lists;
         Found_Start, Found_Middle, Found_End : Boolean := False;
         C : Smart_Guide_Lists.Cursor := Guides.First;
         Pos : Model_Coordinate;

      begin
         while Has_Element (C) loop
            Pos := Element (C).Pos;
            if Pos = Start then
               Found_Start := True;
               Guides.Update_Element (C, Update_Guide'Access);
            elsif Pos = Middle then
               Found_Middle := True;
               Guides.Update_Element (C, Update_Guide'Access);
            elsif Pos = Last then
               Found_End := True;
               Guides.Update_Element (C, Update_Guide'Access);
            end if;

            Next (C);
         end loop;

         if not Found_Start then
            Guides.Append
              ((Pos        => Start,
                Min        => Min,
                Max        => Max,
                Visible    => False));
         end if;

         if not Found_Middle then
            Guides.Append
              ((Pos        => Middle,
                Min        => Min,
                Max        => Max,
                Visible    => False));
         end if;

         if not Found_End then
            Guides.Append
              ((Pos        => Last,
                Min        => Min,
                Max        => Max,
                Visible    => False));
         end if;
      end Add_Guides;

      ------------------
      -- Process_Item --
      ------------------

      procedure Process_Item
        (Item : not null access Abstract_Item_Record'Class)
      is
         Box   : Model_Rectangle;
      begin
         --  If this is not one of the items we are dragging
         if not Item.Is_Link
           and then not Self.Dragged_Items.Contains (Abstract_Item (Item))
         then
            Box := Item.Model_Bounding_Box;
            Add_Guides
              (Self.Snap.Hguides,
               Start  => Box.Y,
               Middle => Box.Y + Box.Height / 2.0,
               Last   => Box.Y + Box.Height,
               Min    => Box.X,
               Max    => Box.X + Box.Width);

            Add_Guides
              (Self.Snap.Vguides,
               Start  => Box.X,
               Middle => Box.X + Box.Width / 2.0,
               Last   => Box.X + Box.Width,
               Min    => Box.Y,
               Max    => Box.Y + Box.Height);
         end if;
      end Process_Item;

   begin
      Self.Snap.Hguides.Clear;
      Self.Snap.Vguides.Clear;

      if Self.Model = null then
         return;
      end if;

      Self.Model.For_Each_Item (Process_Item'Access, In_Area => Screen);
   end Prepare_Smart_Guides;

   -----------------------
   -- Free_Smart_Guides --
   -----------------------

   procedure Free_Smart_Guides
     (Self : not null access Canvas_View_Record'Class) is
   begin
      Self.Snap.Hguides.Clear;
      Self.Snap.Vguides.Clear;
   end Free_Smart_Guides;

   -------------------------
   -- Snap_To_Smart_Guide --
   -------------------------

   function Snap_To_Smart_Guides
     (Self       : not null access Canvas_View_Record'Class;
      Pos        : Model_Coordinate;
      Size       : Model_Coordinate;
      Horizontal : Boolean) return Model_Coordinate
   is
      use Smart_Guide_Lists;
      C      : Smart_Guide_Lists.Cursor;
      Result : Model_Coordinate := Pos;

      procedure Update_Guide (Guide : in out Smart_Guide);
      procedure Update_Guide (Guide : in out Smart_Guide) is
      begin
         if abs (Guide.Pos - Pos) < Self.Snap.Margin then
            Guide.Visible := True;
            Result := Guide.Pos;
         elsif abs (Guide.Pos - Pos - Size / 2.0) < Self.Snap.Margin then
            Guide.Visible := True;
            Result := Guide.Pos - Size / 2.0;
         elsif abs (Guide.Pos - Pos - Size) < Self.Snap.Margin then
            Guide.Visible := True;
            Result := Guide.Pos - Size;
         else
            Guide.Visible := False;
         end if;
      end Update_Guide;

   begin
      if Horizontal then
         C := Self.Snap.Hguides.First;
         while Has_Element (C) loop
            Self.Snap.Hguides.Update_Element (C, Update_Guide'Access);
            Next (C);
         end loop;
      else
         C := Self.Snap.Vguides.First;
         while Has_Element (C) loop
            Self.Snap.Vguides.Update_Element (C, Update_Guide'Access);
            Next (C);
         end loop;
      end if;

      return Result;
   end Snap_To_Smart_Guides;

   -------------------------------
   -- Draw_Visible_Smart_Guides --
   -------------------------------

   procedure Draw_Visible_Smart_Guides
     (Self     : not null access Canvas_View_Record'Class;
      Context  : Draw_Context;
      For_Item : not null access Abstract_Item_Record'Class)
   is
      use Smart_Guide_Lists;
      Box    : constant Model_Rectangle := For_Item.Model_Bounding_Box;
      C      : Smart_Guide_Lists.Cursor;
      Guide  : Smart_Guide;
      From, To : Model_Coordinate;
   begin
      C := Self.Snap.Hguides.First;
      while Has_Element (C) loop
         Guide := Element (C);
         if Guide.Visible then
            From := Model_Coordinate'Min (Guide.Min, Box.X);
            To   := Model_Coordinate'Max (Guide.Max, Box.X + Box.Width);
            Self.Snap.Style.Draw_Polyline
              (Cr     => Context.Cr,
               Points => ((From, Guide.Pos), (To, Guide.Pos)));
         end if;

         Next (C);
      end loop;

      C := Self.Snap.Vguides.First;
      while Has_Element (C) loop
         Guide := Element (C);
         if Guide.Visible then
            From := Model_Coordinate'Min (Guide.Min, Box.Y);
            To   := Model_Coordinate'Max (Guide.Max, Box.Y + Box.Height);
            Self.Snap.Style.Draw_Polyline
              (Cr     => Context.Cr,
               Points => ((Guide.Pos, From), (Guide.Pos, To)));
         end if;

         Next (C);
      end loop;
   end Draw_Visible_Smart_Guides;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self  : out Minimap_View;
      Style : Gtkada.Style.Drawing_Style := Default_Current_Region_Style)
   is
   begin
      Self := new Minimap_View_Record;
      Gtkada.Canvas_View.Views.Initialize (Self, Style);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access Minimap_View_Record'Class;
      Style : Gtkada.Style.Drawing_Style := Default_Current_Region_Style)
   is
   begin
      Gtkada.Canvas_View.Initialize (Self, Model => null);
      Self.Area_Style := Style;
      Self.On_Destroy (On_Destroy_Minimap'Access);
      Self.On_Item_Event (On_Minimap_Item_Event'Access);
   end Initialize;

   -------------
   -- Monitor --
   -------------

   procedure Monitor
     (Self : not null access Minimap_View_Record;
      View : access Canvas_View_Record'Class := null)
   is
   begin
      if Self.Monitored /= null then
         Weak_Unref (Self.Monitored, On_Monitored_Destroyed'Access,
                     Get_Object (Self));
         Self.Set_Model (null);
         Disconnect (Self.Monitored, Self.Viewport_Changed_Id);
      end if;

      Self.Monitored := Canvas_View (View);
      if View /= null then
         Weak_Ref (Self.Monitored, On_Monitored_Destroyed'Access,
                   Get_Object (Self));

         Self.Viewport_Changed_Id := View.On_Viewport_Changed
             (On_Monitored_Viewport_Changed'Access, Self);
      end if;

      Self.Queue_Draw;
   end Monitor;

   -----------------------------------
   -- On_Monitored_Viewport_Changed --
   -----------------------------------

   procedure On_Monitored_Viewport_Changed
     (Minimap : not null access GObject_Record'Class)
   is
      Self : constant Minimap_View := Minimap_View (Minimap);
   begin
      Self.Set_Model (Self.Monitored.Model);
      Self.Scale_To_Fit (Max_Scale => Gdouble'Last);
   end On_Monitored_Viewport_Changed;

   ------------------------
   -- On_Destroy_Minimap --
   ------------------------

   procedure On_Destroy_Minimap (Minimap : access Gtk_Widget_Record'Class) is
   begin
      Minimap_View (Minimap).Monitor (null);
   end On_Destroy_Minimap;

   ----------------------------
   -- On_Monitored_Destroyed --
   ----------------------------

   procedure On_Monitored_Destroyed
     (Minimap : System.Address; Monitored : System.Address)
   is
      pragma Unreferenced (Monitored);
      Self : constant Minimap_View :=
        Minimap_View (Get_User_Data_Or_Null (Minimap));
   begin
      Self.Monitored := null;
      Self.Queue_Draw;
   end On_Monitored_Destroyed;

   -------------------
   -- Draw_Internal --
   -------------------

   overriding procedure Draw_Internal
     (Self    : not null access Minimap_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
      Box : Model_Rectangle;
   begin
      Canvas_View_Record (Self.all).Draw_Internal (Context, Area); --  inherit

      if Self.Monitored /= null then
         Box := Self.Monitored.Get_Visible_Area;
         Self.Area_Style.Draw_Rect
           (Context.Cr,
            Topleft => (Box.X, Box.Y),
            Width   => Box.Width,
            Height  => Box.Height);
      end if;
   end Draw_Internal;

   ---------------------------
   -- On_Minimap_Item_Event --
   ---------------------------

   function On_Minimap_Item_Event
     (View  : not null access GObject_Record'Class;
      Event : Event_Details_Access)
      return Boolean
   is
      Self   : constant Minimap_View := Minimap_View (View);
      Screen : Model_Rectangle;
      S      : Gdouble;
   begin
      if Self.Monitored /= null then
         case Event.Event_Type is
            when Button_Press =>
               Event.Toplevel_Item := null;
               Event.Item := null;
               Event.Allowed_Drag_Area := Drag_Anywhere;

               Screen := Self.Monitored.Get_Visible_Area;

               if Point_In_Rect (Screen, Event.M_Point) then
                  Self.Drag_Pos_X :=
                    (Event.M_Point.X - Self.Monitored.Topleft.X)
                    / Screen.Width;
                  Self.Drag_Pos_Y :=
                    (Event.M_Point.Y - Self.Monitored.Topleft.Y)
                    / Screen.Height;
               else
                  Self.Monitored.Center_On (Event.M_Point);
                  Self.Drag_Pos_X := 0.5;
                  Self.Drag_Pos_Y := 0.5;
               end if;

               return True;

            when Button_Release =>
               Self.Monitored.Center_On (Event.M_Point);
               return True;

            when In_Drag =>
               Self.Monitored.Center_On
                 (Event.M_Point, Self.Drag_Pos_X, Self.Drag_Pos_Y);
               return True;

            when Scroll =>
               if Event.Button = 5 then
                  S := Self.Monitored.Get_Scale * 1.1;
               else
                  S := Self.Monitored.Get_Scale / 1.1;
               end if;

               Self.Monitored.Set_Scale (S, Preserve => Event.M_Point);
               return True;

            when others =>
               null;
         end case;
      end if;
      return False;
   end On_Minimap_Item_Event;

   --------------------------------
   -- Start_Continuous_Scrolling --
   --------------------------------

   procedure Start_Continuous_Scrolling
     (Self   : not null access Canvas_View_Record'Class;
      Dx, Dy : Model_Coordinate := 0.0)
   is
   begin
      Self.Continuous_Scroll.Dx := Dx;
      Self.Continuous_Scroll.Dy := Dy;

      if Self.Continuous_Scroll.Id = No_Source_Id then
         Self.Continuous_Scroll.Id := View_Sources.Timeout_Add
           (Interval   => Self.Continuous_Scroll.Timeout,
            Func       => Do_Continuous_Scroll'Access,
            Data       => Canvas_View (Self));
      end if;
   end Start_Continuous_Scrolling;

   ---------------------------------
   -- Cancel_Continuous_Scrolling --
   ---------------------------------

   procedure Cancel_Continuous_Scrolling
     (Self    : not null access Canvas_View_Record'Class)
   is
   begin
      if Self.Continuous_Scroll.Id /= No_Source_Id then
         Remove (Self.Continuous_Scroll.Id);
         Self.Continuous_Scroll.Id := No_Source_Id;
      end if;
   end Cancel_Continuous_Scrolling;

   --------------------------
   -- Do_Continuous_Scroll --
   --------------------------

   function Do_Continuous_Scroll (Self : Canvas_View) return Boolean is
   begin
      Self.Topleft :=
        (Self.Topleft.X + Self.Continuous_Scroll.Dx,
         Self.Topleft.Y + Self.Continuous_Scroll.Dy);

      Move_Dragged_Items
        (Self,
         Dx             => Self.Continuous_Scroll.Dx,
         Dy             => Self.Continuous_Scroll.Dy,
         From_Initial   => False);

      Self.Viewport_Changed;
      Queue_Draw (Self);
      return True;  --  keep repeating the timeout
   end Do_Continuous_Scroll;

   ------------------------
   -- Move_Dragged_Items --
   ------------------------

   procedure Move_Dragged_Items
     (Self           : not null access Canvas_View_Record'Class;
      Dx, Dy         : Model_Coordinate;
      From_Initial   : Boolean)
   is
      use Item_Drag_Infos;
      C    : Item_Drag_Infos.Cursor := Self.Dragged_Items.First;
      It   : Item_Drag_Info;
      B    : Model_Rectangle;
      BB   : Item_Rectangle;
      X, Y : Model_Coordinate;
      Pos  : Gtkada.Style.Point;
   begin
      while Has_Element (C) loop
         It := Element (C);

         if From_Initial then
            X := It.Pos.X + Dx;
            Y := It.Pos.Y + Dy;
         else
            Pos := It.Item.Position;
            X := Pos.X + Dx;
            Y := Pos.Y + Dy;
         end if;

         BB := It.Item.Bounding_Box;

         --  Constraint the move to a specific area

         if Self.Last_Button_Press.Allowed_Drag_Area /= Drag_Anywhere
           and then Self.Last_Button_Press.Allowed_Drag_Area /= No_Drag_Allowed
         then
            B  := Self.Last_Button_Press.Allowed_Drag_Area;

            X := Model_Coordinate'Max (X, B.X);
            X := Model_Coordinate'Min (X, B.X + B.Width - BB.Width);
            Y := Model_Coordinate'Max (Y, B.Y);
            Y := Model_Coordinate'Min (Y, B.Y + B.Height - BB.Height);
         end if;

         --  Snap to grid or smart guides

         if Self.Last_Button_Press.Allow_Snapping then
            if Self.Snap.Grid then
               X := Snap_To_Grid (Self, X, BB.Width);
               Y := Snap_To_Grid (Self, Y, BB.Height);
            end if;

            if Self.Snap.Smart_Guides then
               X := Snap_To_Smart_Guides (Self, X, BB.Width, False);
               Y := Snap_To_Smart_Guides (Self, Y, BB.Height, True);
            end if;
         end if;

         It.Item.Set_Position ((X => X, Y => Y));
         Next (C);
      end loop;

      Refresh_Link_Layout (Self.Model, Self.Dragged_Items);
   end Move_Dragged_Items;

   -------------------
   -- Easing_Linear --
   -------------------

   function Easing_Linear
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble
   is
   begin
      return Value.Start + (Value.Finish - Value.Start) * Gdouble (Progress);
   end Easing_Linear;

   -------------------------
   -- Easing_In_Out_Cubic --
   -------------------------

   function Easing_In_Out_Cubic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble
   is
      P : Gdouble := 2.0 * Gdouble (Progress);
   begin
      if Progress < 0.5 then
         return (Value.Finish - Value.Start) / 2.0 * P * P * P + Value.Start;
      else
         P := P - 2.0;
         return (Value.Finish - Value.Start) / 2.0 * (P * P * P + 2.0)
           + Value.Start;
      end if;
   end Easing_In_Out_Cubic;

   ---------------------
   -- Easing_In_Cubic --
   ---------------------

   function Easing_In_Cubic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble
   is
      P : constant Gdouble := Gdouble (Progress);
   begin
      return (Value.Finish - Value.Start) * P * P * P + Value.Start;
   end Easing_In_Cubic;

   ----------------------
   -- Easing_Out_Cubic --
   ----------------------

   function Easing_Out_Cubic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble
   is
      P : constant Gdouble := Gdouble (Progress) - 1.0;
   begin
      return (Value.Finish - Value.Start) * (P * P * P + 1.0) + Value.Start;
   end Easing_Out_Cubic;

   ------------------------
   -- Easing_Out_Elastic --
   ------------------------

   function Easing_Out_Elastic
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble
   is
      P : Gdouble;
   begin
      if Progress <= 0.0 then
         return Value.Start;
      end if;

      if Progress >= 1.0 then
         return Value.Finish;
      end if;

      P := Gdouble (Value.Duration) * 0.3;
      return (Value.Finish - Value.Start)
        * (2.0 ** (-10.0 * Gdouble (Progress)))
        * Sin
          ((Gdouble (Progress) * Gdouble (Value.Duration) - P / 4.0)
           * (2.0 * Pi / P))
        + Value.Finish;
   end Easing_Out_Elastic;

   -----------------------
   -- Easing_Out_Bounce --
   -----------------------

   function Easing_Out_Bounce
     (Value : Animation_Value; Progress : Animation_Progress) return Gdouble
   is
      Magic         : constant Gdouble := 7.5625;
      First_Bounce  : constant Gdouble := 1.0 / 2.75;
      Second_Bounce : constant Gdouble := 2.0 / 2.75;
      Third_Bounce  : constant Gdouble := 2.5 / 2.75;
      P             : Gdouble := Gdouble (Progress);
      C             : constant Gdouble := Value.Finish - Value.Start;
   begin
      if P < First_Bounce then
         return C * Magic * P * P + Value.Start;
      elsif P < Second_Bounce then
         P := P - (1.5 / 2.75);
         return C * (Magic * P * P + 0.75) + Value.Start;
      elsif P < Third_Bounce then
         P := P - (2.25 / 2.75);
         return C * (Magic * P * P + 0.9375) + Value.Start;
      else
         P := P - (2.625 / 2.75);
         return C * (Magic * P * P + 0.984375) + Value.Start;
      end if;
   end Easing_Out_Bounce;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Self     : in out Animator;
      Duration : Standard.Duration;
      Easing   : not null Easing_Function := Easing_In_Out_Cubic'Access;
      View     : access Canvas_View_Record'Class := null;
      Item     : access Abstract_Item_Record'Class := null)
   is
   begin
      Self.Easing   := Easing;
      Self.Duration := Duration;
      Self.View     := View;
      Self.Item     := Item;
   end Setup;

   ----------------------
   -- Animate_Position --
   ----------------------

   function Animate_Position
     (Item           : not null access Abstract_Item_Record'Class;
      Final_Position : Gtkada.Style.Point;
      Duration       : Standard.Duration := 0.4;
      Easing         : Easing_Function := Easing_In_Out_Cubic'Access)
      return Animator_Access
   is
      Pos : constant Gtkada.Style.Point := Item.Position;
   begin
      if Pos = Final_Position then
         return null;
      end if;

      return new Position_Animator'
        (Easing   => Easing,
         Duration => Duration,
         Item     => Item,
         Pos_X    => (Start    => Pos.X,
                      Finish   => Final_Position.X,
                      Duration => Duration),
         Pos_Y    => (Start    => Pos.Y,
                      Finish   => Final_Position.Y,
                      Duration => Duration),
         others   => <>);
   end Animate_Position;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self     : not null access Position_Animator;
      Progress : Animation_Progress)
      return Animation_Status is
   begin
      Self.Item.Set_Position
        ((X => Self.Easing (Self.Pos_X, Progress),
          Y => Self.Easing (Self.Pos_Y, Progress)));
      return Needs_Refresh_Links_From_Item;
   end Execute;

   -------------------
   -- Animate_Scale --
   -------------------

   function Animate_Scale
     (View           : not null access Canvas_View_Record'Class;
      Final_Scale    : Gdouble;
      Preserve       : Model_Point := No_Point;
      Duration       : Standard.Duration := 0.4;
      Easing         : Easing_Function := Easing_In_Out_Cubic'Access)
      return Animator_Access
   is
   begin
      if View.Scale = Final_Scale then
         return null;
      end if;

      return new Scale_Animator'
        (Easing   => Easing,
         Duration => Duration,
         View     => View,
         Preserve => Preserve,
         Scale    => (Start    => View.Scale,
                      Finish   => Final_Scale,
                      Duration => Duration),
         others   => <>);
   end Animate_Scale;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self     : not null access Scale_Animator;
      Progress : Animation_Progress) return Animation_Status is
   begin
      Self.View.Set_Scale
        (Scale    => Self.Easing (Self.Scale, Progress),
         Preserve => Self.Preserve);
      return 0;
   end Execute;

   --------------------
   -- Animate_Scroll --
   --------------------

   function Animate_Scroll
     (View           : not null access Canvas_View_Record'Class;
      Final_Topleft  : Model_Point;
      Duration       : Standard.Duration := 0.8;
      Easing         : Easing_Function := Easing_In_Out_Cubic'Access)
      return Animator_Access
   is
   begin
      if View.Topleft = Final_Topleft then
         return null;
      end if;

      return new Scroll_Animator'
        (Easing    => Easing,
         Duration  => Duration,
         View      => View,
         Topleft_X => (Start    => View.Topleft.X,
                       Finish   => Final_Topleft.X,
                       Duration => Duration),
         Topleft_Y => (Start    => View.Topleft.Y,
                       Finish   => Final_Topleft.Y,
                       Duration => Duration),
         others   => <>);
   end Animate_Scroll;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self     : not null access Scroll_Animator;
      Progress : Animation_Progress) return Animation_Status
   is
   begin
      Self.View.Topleft :=
        (X => Self.Easing (Self.Topleft_X, Progress),
         Y => Self.Easing (Self.Topleft_Y, Progress));
      Self.View.Set_Adjustment_Values;
      return 0;
   end Execute;

   ------------------------
   -- On_Animate_Destroy --
   ------------------------

   procedure On_Animate_Destroy (View : in out Canvas_View) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Base_Animation_Data'Class, Base_Animation_Data_Access);
   begin
      View.Id_Animation := No_Source_Id;
      Unchecked_Free (View.Animation_Data);
   end On_Animate_Destroy;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self : access Animator'Class;
      View : not null access Canvas_View_Record'Class)
   is
      Data : Animation_Data_Access;
      C    : Animator_Lists.Cursor;
   begin
      if Self = null then
         return;
      end if;

      if View.Animation_Data = null then
         View.Animation_Data := new Animation_Data;
      end if;

      if View.Id_Animation = No_Source_Id then
         --  Use an idle rather than a timeout: a timeout would execute after
         --  n milliseconds, then after (n + execution time for On_Animate)
         --  milliseconds, and so on, so the quality of the animation degrades.

         View.Id_Animation := View_Sources.Idle_Add
           (On_Animate'Access,
            Canvas_View (View),
            Notify => On_Animate_Destroy'Access);
      end if;

      Data := Animation_Data_Access (View.Animation_Data);

      --  Do we need to remove any of the previous animators ?

      if Self.Is_Unique_For_Item then
         C := Data.Queue.First;
         while Has_Element (C) loop
            if Element (C)'Tag = Self'Tag
              and then Element (C).Item = Self.Item
              and then Element (C).View = Self.View
            then
               if Data.Current = C then
                  Next (Data.Current);
               end if;

               --  No need to search for others, it is unique.
               Data.Queue.Delete (C);
               exit;
            end if;

            Next (C);
         end loop;
      end if;

      Data.Queue.Append (Animator_Access (Self));
   end Start;

   -------------------------
   -- Animation_Iteration --
   -------------------------

   procedure Animation_Iteration
     (Self     : not null access Canvas_View_Record'Class;
      Current  : Ada.Calendar.Time;
      Max_Time : Duration)
   is
      Data     : constant Animation_Data_Access := Animation_Data_Access
        (Self.Animation_Data);
      Previous : Animator_Lists.Cursor;
      Anim     : Animator_Access;
      Status   : Animation_Status := 0;
      Local_Status : Animation_Status;
      Progress : Duration;
      Items    : Item_Drag_Infos.Map;

      Max_Count : constant Integer := Integer (Data.Queue.Length);
      Count     : Natural := 0;
      --  These two variables re used to make sure we animate each item only
      --  once per iteration.

   begin
      if not Has_Element (Data.Current) then
         Data.Current := Data.Queue.First;
      end if;

      while Count < Max_Count
        and then Has_Element (Data.Current)
        and then (Current = No_Time or else Clock - Current < Max_Time)
      loop
         Anim := Element (Data.Current);

         --  Move to the next element, in case the current animator is removed
         Previous := Data.Current;
         Next (Data.Current);
         if not Has_Element (Data.Current) then
            Data.Current := Data.Queue.First;
         end if;

         --  Initialize first time animators;

         if Anim.Start = No_Time then
            Anim.Start := Current;
         end if;

         --  Perform the animation

         if Current = No_Time then
            Progress := 1.0;
         else
            Progress := (Current - Anim.Start) / Anim.Duration;
            if Progress > 1.0 then
               Progress := 1.0;
            end if;
         end if;

         Local_Status := Anim.Execute (Animation_Progress (Progress));
         Status := Local_Status or Status;

         if Local_Status = Needs_Refresh_Links_From_Item
           and then (Status and Needs_Refresh_All_Links) = 0
           and then (Status and Needs_Refresh_Layout) = 0
         then
            Items.Include
              (Abstract_Item (Anim.Item),
               Item_Drag_Info'(Item => Abstract_Item (Anim.Item), Pos => <>));
         end if;

         --  Destroy terminated animators
         if Progress >= 1.0 then
            Anim.Destroy;
            Data.Queue.Delete (Previous);
            if Data.Queue.Is_Empty then
               Data.Current := No_Element;
            end if;
         end if;

         Count := Count + 1;
      end loop;

      --  No need to refresh if the canvas is being destroyed
      if Current /= No_Time then
         if (Status and Needs_Refresh_Layout) /= 0 then
            Self.Model.Refresh_Layout;
         elsif (Status and Needs_Refresh_All_Links) /= 0 then
            Self.Model.Refresh_Link_Layout;
            Self.Model.Layout_Changed;
         elsif (Status and Needs_Refresh_Links_From_Item) /= 0 then
            Self.Model.Refresh_Link_Layout (Items);
            Self.Model.Layout_Changed;
         end if;
      end if;
   end Animation_Iteration;

   ----------------
   -- On_Animate --
   ----------------

   function On_Animate (View : Canvas_View) return Boolean is
      Now : constant Ada.Calendar.Time := Clock;
      Data : constant Animation_Data_Access :=
        Animation_Data_Access (View.Animation_Data);
   begin
      --  No need to do anywork if we are called too often
      if Data.Last_Run = No_Time
        or else (Now - Data.Last_Run > Min_Animation_Interval)
      then
         Data.Last_Run := Now;
         Animation_Iteration
           (View,
            Current  => Now,
            Max_Time => Duration (Animation_Interval) / 1000.0);
      end if;

      return not Data.Queue.Is_Empty;
   end On_Animate;

   -------------------------
   -- Terminate_Animation --
   -------------------------

   procedure Terminate_Animation
     (Self : not null access Canvas_View_Record'Class) is
   begin
      if Self.Id_Animation /= No_Source_Id then
         Animation_Iteration
           (Self,
            Current  => GNAT.Calendar.No_Time,
            Max_Time => Duration'Last);
         Remove (Self.Id_Animation);
      end if;
   end Terminate_Animation;

   ----------------------------------
   -- Terminate_Animation_For_Item --
   ----------------------------------

   procedure Terminate_Animation_For_Item
     (Self : not null access Canvas_View_Record'Class;
      Item : access Abstract_Item_Record'Class := null)
   is
      Data  : constant Animation_Data_Access :=
        Animation_Data_Access (Self.Animation_Data);
      C, C2 : Animator_Lists.Cursor;
   begin
      if Data /= null then
         C := Data.Queue.First;
         while Has_Element (C) loop
            C2 := C;
            Next (C);

            if (Item /= null and then Element (C2).Item = Item)
              or else (Item = null and then Element (C2).View = Self)
            then
               if Data.Current = C2 then
                  Next (Data.Current);
               end if;

               Data.Queue.Delete (C2);
            end if;
         end loop;

         if Data.Queue.Is_Empty then
            Remove (Self.Id_Animation);
         end if;
      end if;
   end Terminate_Animation_For_Item;

   ------------------------
   -- Is_Unique_For_Item --
   ------------------------

   function Is_Unique_For_Item
     (Self : not null access Animator) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Unique_For_Item;

   -------------------
   -- Reserve_Space --
   -------------------

   procedure Reserve_Space
     (Self        : not null access Canvas_View_Record'Class;
      Rect        : Model_Rectangle;
      Direction   : Move_Direction := Any;
      Do_Not_Move : Item_Sets.Set := Item_Sets.Empty_Set;
      Duration    : Standard.Duration := 0.0;
      Easing      : Easing_Function := Easing_In_Out_Cubic'Access)
   is
      use Item_Sets;

      Overlap_Margin : constant Gdouble := 5.0;
      --  Minimum distance between items

      Moved : Item_Sets.Set := Do_Not_Move;
      --  The items that have already been moved. An item is never moved twice.

      procedure Process
        (Item      : not null access Abstract_Item_Record'Class;
         Box       : Model_Rectangle;
         Direction : Move_Direction);
      --  Moves item in the given direction so that it no longer overlaps
      --  Box.

      procedure Move_Items_In_Box
        (Box       : Model_Rectangle;
         Direction : Move_Direction);
      --  Move all items in the given area

      -------------
      -- Process --
      -------------

      procedure Process
        (Item      : not null access Abstract_Item_Record'Class;
         Box       : Model_Rectangle;
         Direction : Move_Direction)
      is
         BBox : constant Model_Rectangle := Item.Model_Bounding_Box;
         Pos  : Point := (BBox.X, BBox.Y);
         D1, D2, D3, D4 : Gdouble;
         Dir  : Move_Direction := Direction;
      begin
         if Moved.Contains (Abstract_Item (Item)) then
            return;
         end if;

         case Direction is
            when Left =>
               Pos.X := Box.X - BBox.Width - Overlap_Margin;
            when Right =>
               Pos.X := Box.X + Box.Width + Overlap_Margin;
            when Up =>
               Pos.Y := Box.Y - BBox.Height - Overlap_Margin;
            when Down =>
               Pos.Y := Box.Y + Box.Height + Overlap_Margin;
            when Horizontal =>
               D1 := Box.X - BBox.Width - Overlap_Margin - BBox.X;
               D2 := Box.X + Box.Width + Overlap_Margin - BBox.X;
               if abs (D1) < abs (D2) then
                  Dir := Left;
                  Pos.X := D1 + BBox.X;
               else
                  Dir := Right;
                  Pos.X := D2 + BBox.X;
               end if;

            when Vertical =>
               D3 := Box.Y - BBox.Height - Overlap_Margin - BBox.Y;
               D4 := Box.Y + Box.Height + Overlap_Margin - BBox.Y;
               if abs (D3) < abs (D4) then
                  Dir := Up;
                  Pos.Y := D3 + BBox.Y;
               else
                  Dir := Down;
                  Pos.Y := D4 + BBox.Y;
               end if;

            when Any =>
               D1 := Box.X - BBox.Width - Overlap_Margin - BBox.X;
               D2 := Box.X + Box.Width + Overlap_Margin - BBox.X;
               if abs (D1) > abs (D2) then
                  D1 := D2;
               end if;

               D3 := Box.Y - BBox.Height - Overlap_Margin - BBox.Y;
               D4 := Box.Y + Box.Height + Overlap_Margin - BBox.Y;
               if abs (D3) > abs (D4) then
                  D3 := D4;
               end if;

               if abs (D1) < abs (D3) then
                  --  move horizontally
                  Pos.X := BBox.X + D1;
                  if D1 = D2 then
                     Dir := Right;
                  else
                     Dir := Left;
                  end if;

               else
                  Pos.Y := BBox.Y + D3;
                  if D3 = D4 then
                     Dir := Down;
                  else
                     Dir := Up;
                  end if;
               end if;
         end case;

         Moved.Include (Abstract_Item (Item));

         --  The new box is slightly less than the overlap margin, since
         --  otherwise we have the following scenario: move A on top of B.
         --  Assume B is moved downward (at a distance Overlap_Margin from A).
         --  If we then move C on top of the left-hand side of A, then A is
         --  correctly moved to the right, but B is also moved.

         Move_Items_In_Box
           ((Pos.X - (Overlap_Margin - 1.0),
             Pos.Y - (Overlap_Margin - 1.0),
             BBox.Width + 2.0 * Overlap_Margin - 2.0,
             BBox.Height + 2.0 * Overlap_Margin - 2.0),
            Direction => Dir);

         if Duration = 0.0 then
            Item.Set_Position (Pos);
         else
            Start (Animate_Position (Item, Pos, Duration, Easing), Self);
         end if;
      end Process;

      -----------------------
      -- Move_Items_In_Box --
      -----------------------

      procedure Move_Items_In_Box
        (Box       : Model_Rectangle;
         Direction : Move_Direction)
      is
         To_Move : Item_Sets.Set;
         C       : Item_Sets.Cursor;

         procedure On_Item (Item : not null access Abstract_Item_Record'Class);
         --  Process one of the items that was in the original area

         -------------
         -- On_Item --
         -------------

         procedure On_Item
           (Item : not null access Abstract_Item_Record'Class) is
         begin
            --  Store in a temporary structure, since modifying the model might
            --  break the iteration with For_Each_Item.
            To_Move.Include (Abstract_Item (Item));
         end On_Item;

      begin
         Self.Model.For_Each_Item
           (On_Item'Access, Filter => Kind_Item, In_Area => Box);

         while not To_Move.Is_Empty loop
            C := To_Move.First;
            Process (Element (C), Box, Direction => Direction);
            To_Move.Delete (C);
         end loop;
      end Move_Items_In_Box;

   begin
      Move_Items_In_Box (Rect, Direction);
   end Reserve_Space;

   -----------------------------
   -- Insert_And_Layout_Items --
   -----------------------------

   procedure Insert_And_Layout_Items
     (Self                 : not null access Canvas_View_Record'Class;
      Ref                  : not null access Abstract_Item_Record'Class;
      Items                : Items_Lists.List;
      Direction            : Specific_Direction;
      Space_Between_Items  : Gdouble := 10.0;
      Space_Between_Layers : Gdouble := 20.0;
      Duration             : Standard.Duration := 0.0)
   is
      use Items_Lists;
      Context : constant Draw_Context := Self.Build_Context;
      Horizontal : constant Boolean :=
        Direction = Left or else Direction = Right;
      C       : Items_Lists.Cursor;
      Total   : Gdouble := 0.0;   --  total space for children
      Max     : Gdouble := 0.0;
      Box     : Item_Rectangle;   --  size of the parent box
      CBox    : Item_Rectangle;   --  size of the current child box
      MBox    : Model_Rectangle;
      Pos     : Gtkada.Style.Point;
      Child   : Gtkada.Style.Point; --  coordinates of first child
      Coord   : Gtkada.Style.Point; --  coordinates of current child
      It      : Abstract_Item;
   begin
      --  Compute the size we need for the children

      C := Items.First;
      while Has_Element (C) loop
         Element (C).Refresh_Layout (Context); --  Compute its size
         CBox := Element (C).Bounding_Box;
         if Horizontal then
            Total := Total + CBox.Height;
            Max := Gdouble'Max (Max, CBox.Width);
         else
            Total := Total + CBox.Width;
            Max := Gdouble'Max (Max, CBox.Height);
         end if;
         Next (C);
      end loop;

      Total := Total + Space_Between_Items * Gdouble (Items.Length);

      --  Now insert (if needed) the parent item, below or to the right of
      --  the existing model elements, and centered with regards to its
      --  future children

      Pos := Ref.Position;

      if Pos = No_Position then
         Ref.Refresh_Layout (Context);  --  Compute its size
         Box := Ref.Bounding_Box;
         MBox := Self.Model.Bounding_Box;

         if Horizontal then
            Child.Y := MBox.Y + MBox.Height;
            Pos.Y := Child.Y + (Total - MBox.Height) / 2.0;
            if Direction = Right then
               Pos.X   := MBox.X;
               Child.X := Pos.X + Box.Width + Space_Between_Layers;
            else
               Child.X := MBox.X;
               Pos.X   := Child.X + Max + Space_Between_Layers;
            end if;

         else
            Child.X := MBox.X + MBox.Width;
            Pos.X := Child.X + (Total - MBox.Width) / 2.0;
            if Direction = Down then
               Pos.Y   := MBox.Y;
               Child.Y := Pos.Y + Box.Height + Space_Between_Layers;
            else
               Child.Y := MBox.Y;
               Pos.Y   := Child.Y + Max + Space_Between_Layers;
            end if;
         end if;

         Ref.Set_Position (Pos);  --  No animation

      else
         Box := Ref.Bounding_Box;
         if Horizontal then
            Child.Y := Pos.Y + (Box.Height - Total) / 2.0;
            if Direction = Right then
               Child.X := Pos.X + Box.Width + Space_Between_Layers;
            else
               Child.X := Pos.X - Max - Space_Between_Layers;
            end if;
         else
            Child.X := Pos.X + (Box.Width - Total) / 2.0;
            if Direction = Down then
               Child.Y := Pos.Y + Box.Height + Space_Between_Layers;
            else
               Child.Y := Pos.Y - Max - Space_Between_Layers;
            end if;
         end if;
      end if;

      --  Reserve space for the children by moving existing items aside.

      if Horizontal then
         Reserve_Space
           (Self,
            (Child.X, Child.Y, Max, Total),
            Direction => Gtkada.Canvas_View.Views.Horizontal,
            Duration  => Duration);
      else
         Reserve_Space
           (Self,
            (Child.X, Child.Y, Total, Max),
            Direction => Gtkada.Canvas_View.Views.Vertical,
            Duration  => Duration);
      end if;

      --  Now move the children into place

      C := Items.First;
      Coord := Child;
      while Has_Element (C) loop
         It := Abstract_Item (Element (C));

         if Duration = 0.0 then
            It.Set_Position (Coord);
         else
            It.Set_Position (Pos);  --  position of the parent initially
            Start
              (Animate_Position (It, Coord, Duration => Duration), Self);
         end if;

         CBox := It.Bounding_Box;
         if Horizontal then
            Coord.Y := Coord.Y + CBox.Height + Space_Between_Items;
         else
            Coord.X := Coord.X + CBox.Width + Space_Between_Items;
         end if;

         Next (C);
      end loop;

      Self.Model.Refresh_Layout;  --  for links

      if Horizontal then
         Self.Scroll_Into_View
           ((Gdouble'Min (Child.X, Pos.X), Gdouble'Min (Child.Y, Pos.Y),
            Max + Space_Between_Layers + Box.Width,
            Gdouble'Max (Total, Box.Height)),
            Duration  => Duration);
      else
         Self.Scroll_Into_View
           ((Gdouble'Min (Child.X, Pos.X), Gdouble'Min (Child.Y, Pos.Y),
            Gdouble'Max (Total, Box.Width),
            Max + Space_Between_Layers + Box.Height),
            Duration => Duration);
      end if;

      Self.Queue_Draw;
   end Insert_And_Layout_Items;

end Gtkada.Canvas_View.Views;
