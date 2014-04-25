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

with Cairo;   use Cairo;
with Glib;    use Glib;

package body Gtkada.Canvas_View.Views is

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
      Self : constant Canvas_View := Canvas_View (View);
   begin
      if Self.Model /= null
        and then Event.Button = 1
        and then Event.Event_Type = Button_Press
        and then Event.Toplevel_Item = null
      then
         --  Enable scrolling by dragging the background. However, there is
         --  no point showing an area where there is no item, so we limit
         --  the scrolling.
         Event.Allowed_Drag_Area :=
           Self.Model.Bounding_Box (Margin => View_Margin / Self.Get_Scale);
         return True;
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
      Self : constant Canvas_View := Canvas_View (View);
   begin
      if Event.Event_Type = In_Drag then
         --  Disable snapping when shift is pressed.
         Event.Allow_Snapping := (Event.State and Shift_Mask) = 0;
         Self.Last_Button_Press.Allow_Snapping := Event.Allow_Snapping;
      end if;

      if Event.Event_Type = Button_Press
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

            Self.Set_Scale (S, Center_On => Event.M_Point);
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

end Gtkada.Canvas_View.Views;
