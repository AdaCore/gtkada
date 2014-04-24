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
   -- Do_Snap_Grid --
   ------------------

   function Do_Snap_Grid
     (Self        : not null access Canvas_View_Record'Class;
      Snap_Margin : Model_Coordinate;
      Pos         : Model_Coordinate;
      Size        : Model_Coordinate) return Model_Coordinate
   is
      GX : Model_Coordinate;
   begin
      if Snap_Margin = 0.0 then
         return Pos;
      end if;

      --  Find the closest grid position to the left of Pos
      GX := Gdouble (Gint (Pos / Self.Grid_Size)) * Self.Grid_Size;
      if abs (Pos - GX) < Snap_Margin then
         return GX;
      end if;

      --  Find the closest grid position to the right of Pos
      GX := GX + Self.Grid_Size;
      if abs (Pos - GX) < Snap_Margin then
         return GX;
      end if;

      --  Find the closest grid position to the left of Pos + size
      GX := Gdouble (Gint ((Pos + Size) / Self.Grid_Size)) * Self.Grid_Size;
      if abs (Pos + Size - GX) < Snap_Margin then
         return GX - Size;
      end if;

      --  Find the closest grid position to the right of Pos + size
      GX := GX + Self.Grid_Size;
      if abs (Pos + Size - GX) < Snap_Margin then
         return GX - Size;
      end if;

      return Pos;
   end Do_Snap_Grid;

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

end Gtkada.Canvas_View.Views;
