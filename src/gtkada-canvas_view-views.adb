------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

with Cairo;        use Cairo;
with Glib;         use Glib;
with Glib.Main;    use Glib.Main;
with Glib.Object;  use Glib.Object;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget;   use Gtk.Widget;
with System;       use System;

package body Gtkada.Canvas_View.Views is

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

   procedure Move_Selected_Items
     (Self           : not null access Canvas_View_Record'Class;
      Dx, Dy         : Model_Coordinate;
      From_Initial   : Boolean);
   --  Move all selected items (part of the current drag operation) by the
   --  specified amount (from their initial position if From_Initial is true,
   --  or from their current position otherwise)).

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
        and then Event.Button = 1
        and then Event.Toplevel_Item = null
      then

         if Event.Event_Type = Button_Press then
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

         Move_Selected_Items
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

         --  Should redo the layout for links, but this might be
         --  expensive.
         Self.Queue_Draw;

      elsif Event.Event_Type = Button_Press
        and then Event.Toplevel_Item /= null
        and then Event.Button = 1
      then
         --  Enable moving the item anywhere
         Event.Allowed_Drag_Area := Drag_Anywhere;
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

            Self.Set_Scale (S, Preserve => Event.M_Point);
            return True;
         end if;
      end if;
      return False;
   end On_Item_Event_Zoom_Generic;

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
      C      : Cursor;
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
      C      : Cursor;
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

      Move_Selected_Items
        (Self,
         Dx             => Self.Continuous_Scroll.Dx,
         Dy             => Self.Continuous_Scroll.Dy,
         From_Initial   => False);

      Self.Viewport_Changed;
      Queue_Draw (Self);
      return True;  --  keep repeating the timeout
   end Do_Continuous_Scroll;

   -------------------------
   -- Move_Selected_Items --
   -------------------------

   procedure Move_Selected_Items
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

         if Self.Last_Button_Press.Allowed_Drag_Area /=
           Drag_Anywhere
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
   end Move_Selected_Items;

end Gtkada.Canvas_View.Views;
