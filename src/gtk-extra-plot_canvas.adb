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

with Gtk.Extra.Plot;
with System;
with Gdk.Color; use Gdk.Color;
with Gtk.Extra.Plot; use Gtk.Extra.Plot;

package body Gtk.Extra.Plot_Canvas is

   --------------
   -- Add_Plot --
   --------------

   procedure Add_Plot
      (Plot_Canvas : access Gtk_Plot_Canvas_Record;
       Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       X           : in Gdouble;
       Y           : in Gdouble)
   is
      procedure Internal
         (Plot_Canvas : in System.Address;
          Plot        : in System.Address;
          X           : in Gdouble;
          Y           : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_add_plot");
   begin
      Internal (Get_Object (Plot_Canvas),
                Get_Object (Plot),
                X,
                Y);
   end Add_Plot;

   -------------------
   -- Cancel_Action --
   -------------------

   procedure Cancel_Action (Plot_Canvas : access Gtk_Plot_Canvas_Record)
   is
      procedure Internal (Plot_Canvas : in System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_cancel_action");
   begin
      Internal (Get_Object (Plot_Canvas));
   end Cancel_Action;

   ------------------------
   -- Get_Active_Data --
   ------------------------

   function Get_Active_Data (Canvas : access Gtk_Plot_Canvas_Record)
                            return      Gtk.Extra.Plot.Gtk_Plot_Data
   is
      function Internal (Canvas : in System.Address)
                         return      Gtk.Extra.Plot.Gtk_Plot_Data;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_data");
   begin
      return Internal (Get_Object (Canvas));
   end Get_Active_Data;

   ---------------------
   -- Get_Active_Plot --
   ---------------------

   function Get_Active_Plot (Canvas : access Gtk_Plot_Canvas_Record)
                             return      Gtk.Extra.Plot.Gtk_Plot
   is
      function Internal (Canvas : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_plot");
      Stub : Gtk.Extra.Plot.Gtk_Plot_Record;
   begin
      return Gtk.Extra.Plot.Gtk_Plot
        (Get_User_Data (Internal (Get_Object (Canvas)), Stub));
   end Get_Active_Plot;

   ----------------------
   -- Get_Active_Point --
   ----------------------

   procedure Get_Active_Point (Canvas : access Gtk_Plot_Canvas_Record;
                               X      : out Gdouble;
                               Y      : out Gdouble)
   is
      procedure Internal (Canvas : in System.Address;
                          X      : out Gdouble;
                          Y      : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_point");
   begin
      Internal (Get_Object (Canvas), X, Y);
   end Get_Active_Point;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget        : out Gtk_Plot_Canvas;
      Width         : in Gint;
      Height        : in Gint;
      Magnification : in Gdouble := 1.0)
   is
   begin
      Widget := new Gtk_Plot_Canvas_Record;
      Initialize (Widget, Width, Height, Magnification);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget        : access Gtk_Plot_Canvas_Record'Class;
      Width         : in Gint;
      Height        : in Gint;
      Magnification : in Gdouble := 1.0)
   is
      function Internal (Width         : in Gint;
                         Height        : in Gint;
                         Magnification : in Gdouble)
                        return      System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_new");
   begin
      Set_Object (Widget, Internal (Width, Height, Magnification));
      Initialize_User_Data (Widget);
   end Initialize;

   ---------------------
   -- Set_Active_Plot --
   ---------------------

   procedure Set_Active_Plot
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class)
   is
      procedure Internal (Plot_Canvas : in System.Address;
                          Plot        : in System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_set_active_plot");
   begin
      Internal (Get_Object (Plot_Canvas), Get_Object (Plot));
   end Set_Active_Plot;

   -----------------------------
   -- Plot_Canvas_Flag_Is_Set --
   -----------------------------

   function Plot_Canvas_Flag_Is_Set
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Flag        : in Guint16)
     return Boolean
   is
      function Internal (Canvas : System.Address;
                         Flag   : Guint16)
                        return Guint16;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_flag_is_set");
   begin
      return Internal (Get_Object (Plot_Canvas), Flag) /= 0;
   end Plot_Canvas_Flag_Is_Set;

   ---------------------------
   -- Set_Plot_Canvas_Flags --
   ---------------------------

   procedure Plot_Canvas_Set_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : in Guint16)
   is
      procedure Internal (Canvas : System.Address;
                          Flags  : Guint16);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_set_flags");
   begin
      Internal (Get_Object (Plot_Canvas), Flags);
   end Plot_Canvas_Set_Flags;

   -----------------------------
   -- Plot_Canvas_Unset_Flags --
   -----------------------------

   procedure Plot_Canvas_Unset_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : in Guint16)
   is
      procedure Internal (Canvas : System.Address;
                          Flags  : Guint16);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_unset_flags");
   begin
      Internal (Get_Object (Plot_Canvas), Flags);
   end Plot_Canvas_Unset_Flags;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Canvas  : access Gtk_Plot_Canvas_Record;
                       Width   : in Gint;
                       Height  : in Gint)
   is
      procedure Internal (Canvas : System.Address;
                          Width  : Gint;
                          Height : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_set_size");
   begin
      Internal (Get_Object (Canvas), Width, Height);
   end Set_Size;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_refresh");
   begin
      Internal (Get_Object (Canvas));
   end Refresh;

   --------------
   -- Unselect --
   --------------

   procedure Unselect (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_unselect");
   begin
      Internal (Get_Object (Canvas));
   end Unselect;

   ---------------------
   -- Get_Active_Item --
   ---------------------

   function Get_Active_Item (Canvas  : access Gtk_Plot_Canvas_Record)
                            return Gtk_Plot_Canvas_Child
   is
      function Internal (Canvas : System.Address) return Gtk_Plot_Canvas_Child;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_item");
   begin
      return Internal (Get_Object (Canvas));
   end Get_Active_Item;

   ----------------------
   -- Grid_Set_Visible --
   ----------------------

   procedure Grid_Set_Visible (Canvas  : access Gtk_Plot_Canvas_Record;
                               Visible : in Boolean)
   is
      procedure Internal (Canvas  : System.Address;
                          Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_grid_set_visible");
   begin
      Internal (Get_Object (Canvas), Boolean'Pos (Visible));
   end Grid_Set_Visible;

   -------------------
   -- Grid_Set_Step --
   -------------------

   procedure Grid_Set_Step (Canvas : access Gtk_Plot_Canvas_Record;
                            Step   : in Gint)
   is
      procedure Internal (Canvas : System.Address;
                          Step   : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_grid_set_step");
   begin
      Internal (Get_Object (Canvas), Step);
   end Grid_Set_Step;

   -------------------------
   -- Grid_Set_Attributes --
   -------------------------

   procedure Grid_Set_Attributes
     (Canvas : access Gtk_Plot_Canvas_Record;
      Style  : in Gtk.Extra.Plot.Plot_Line_Style;
      Width  : in Gint;
      Color  : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Canvas : System.Address;
                          Style  : Gint;
                          Width  : Gint;
                          Color  : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_grid_set_attributes");
      Col : aliased Gdk.Color.Gdk_Color := Color;
      Cola : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Cola := System.Null_Address;
      end if;
      Internal (Get_Object (Canvas), Plot_Line_Style'Pos (Style),
                Width, Cola);
   end Grid_Set_Attributes;

   -----------------------
   -- Set_Magnification --
   -----------------------

   procedure Set_Magnification
     (Canvas        : access Gtk_Plot_Canvas_Record;
      Magnification : Gdouble := 1.0)
   is
      procedure Internal (Canvas : System.Address;
                          Magnification : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_set_magnification");
   begin
      Internal (Get_Object (Canvas), Magnification);
   end Set_Magnification;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Canvas     : access Gtk_Plot_Canvas_Record;
      Background : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Canvas : System.Address; Color  : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_set_background");
      Col : aliased Gdk.Color.Gdk_Color := Background;
      Cola : System.Address := Col'Address;
   begin
      if Background = Gdk.Color.Null_Color then
         Cola := System.Null_Address;
      end if;
      Internal (Get_Object (Canvas), Cola);
   end Set_Background;

   ---------------
   -- Get_Pixel --
   ---------------

   procedure Get_Pixel
     (Canvas : access Gtk_Plot_Canvas_Record;
      Px     : in Gdouble;
      Py     : in Gdouble;
      X      : out Gint;
      Y      : out Gint)
   is
      procedure Internal (Canvas : in System.Address;
                          Px     : in Gdouble;
                          Py     : in Gdouble;
                          X      : out Gint;
                          Y      : out Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_get_pixel");
   begin
      Internal (Get_Object (Canvas), Px, Py, X, Y);
   end Get_Pixel;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
      (Canvas : access Gtk_Plot_Canvas_Record;
       X      : in Gint;
       Y      : in Gint;
       Px     : out Gdouble;
       Py     : out Gdouble)
   is
      procedure Internal
         (Canvas : in System.Address;
          X      : in Gint;
          Y      : in Gint;
          Px     : out Gdouble;
          Py     : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_get_position");
   begin
      Internal (Get_Object (Canvas), X, Y, Px, Py);
   end Get_Position;

   --------------
   -- Put_Text --
   --------------

   function Put_Text
     (Canvas        : access Gtk_Plot_Canvas_Record;
      X             : in Gdouble;
      Y             : in Gdouble;
      Angle         : in Gint;
      Ps_Font       : in String;
      Height        : in Gint;
      Fg            : in Gdk.Color.Gdk_Color;
      Bg            : in Gdk.Color.Gdk_Color;
      Transparent   : in Boolean;
      Justification : in Gtk.Enums.Gtk_Justification;
      Text          : in String)
     return Gtk_Plot_Canvas_Child
   is
      function Internal (Canvas        : in System.Address;
                         X             : in Gdouble;
                         Y             : in Gdouble;
                         Angle         : in Gint;
                         Font          : in String;
                         Height        : in Gint;
                         Fg            : in System.Address;
                         Bg            : in System.Address;
                         Transparent   : in Gint;
                         Justification : in Gint;
                         Text          : in String)
                        return Gtk_Plot_Canvas_Child;
      pragma Import (C, Internal, "gtk_plot_canvas_put_text");
      use type Gdk.Color.Gdk_Color;

      Fg_C : aliased Gdk.Color.Gdk_Color := Fg;
      F : System.Address := Fg_C'Address;
      Bg_C : aliased Gdk.Color.Gdk_Color := Bg;
      B : System.Address := Bg_C'Address;

   begin
      if Fg = Gdk.Color.Null_Color then
         F := System.Null_Address;
      end if;

      if Bg = Gdk.Color.Null_Color then
         B := System.Null_Address;
      end if;

      return Internal
        (Get_Object (Canvas), X, Y, Angle, Ps_Font & ASCII.NUL, Height,
         F, B, Boolean'Pos (Transparent),
         Gtk.Enums.Gtk_Justification'Pos (Justification),
         Text & ASCII.NUL);
   end Put_Text;

   --------------
   -- Put_Line --
   --------------

   function Put_Line
     (Canvas     : access Gtk_Plot_Canvas_Record;
      X1         : Gdouble;
      Y1         : Gdouble;
      X2         : Gdouble;
      Y2         : Gdouble;
      Style      : Gtk.Extra.Plot.Plot_Line_Style;
      Width      : Gint;
      Color      : Gdk.Color.Gdk_Color;
      Arrow_Mask : Plot_Canvas_Arrow)
     return Gtk_Plot_Canvas_Child
   is
      function Internal
        (Canvas     : System.Address;
         X1         : Gdouble;
         Y1         : Gdouble;
         X2         : Gdouble;
         Y2         : Gdouble;
         Style      : Gint;
         Width      : Gint;
         Color      : System.Address;
         Arrow_Mask : Gint)
        return Gtk_Plot_Canvas_Child;
      pragma Import (C, Internal, "gtk_plot_canvas_put_line");
      Col  : aliased Gdk.Color.Gdk_Color := Color;
      Cola : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Cola := System.Null_Address;
      end if;
      return Internal
        (Get_Object (Canvas), X1, Y1, X2, Y2, Plot_Line_Style'Pos (Style),
         Width, Cola, Plot_Canvas_Arrow'Pos (Arrow_Mask));
   end Put_Line;

   -------------------
   -- Put_Rectangle --
   -------------------

   function Put_Rectangle
     (Canvas     : access Gtk_Plot_Canvas_Record;
      X1         : Gdouble;
      Y1         : Gdouble;
      X2         : Gdouble;
      Y2         : Gdouble;
      Style      : Gtk.Extra.Plot.Plot_Line_Style;
      Width      : Gint;
      Fg         : Gdk.Color.Gdk_Color;
      Bg         : Gdk.Color.Gdk_Color;
      Border     : Gtk.Extra.Plot.Plot_Border_Style;
      Fill       : Boolean := False)
     return Gtk_Plot_Canvas_Child
   is
      function Internal
        (Canvas     : System.Address;
         X1         : Gdouble;
         Y1         : Gdouble;
         X2         : Gdouble;
         Y2         : Gdouble;
         Style      : Gint;
         Width      : Gint;
         Fg         : System.Address;
         Bg         : System.Address;
         Border     : Gint;
         Fill       : Gint)
        return Gtk_Plot_Canvas_Child;
      pragma Import (C, Internal, "gtk_plot_canvas_put_rectangle");
      Fore : aliased Gdk.Color.Gdk_Color := Fg;
      Fga  : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Bg;
      Bga  : System.Address := Back'Address;
   begin
      if Fg = Gdk.Color.Null_Color then
         Fga := System.Null_Address;
      end if;
      if Bg = Gdk.Color.Null_Color then
         Bga := System.Null_Address;
      end if;
      return Internal
        (Get_Object (Canvas), X1, Y1, X2, Y2, Plot_Line_Style'Pos (Style),
         Width, Fga, Bga, Plot_Border_Style'Pos (Border), Boolean'Pos (Fill));
   end Put_Rectangle;

   -----------------
   -- Put_Ellipse --
   -----------------

   function Put_Ellipse
     (Canvas     : access Gtk_Plot_Canvas_Record;
      X1         : Gdouble;
      Y1         : Gdouble;
      X2         : Gdouble;
      Y2         : Gdouble;
      Style      : Gtk.Extra.Plot.Plot_Line_Style;
      Width      : Gint;
      Fg         : Gdk.Color.Gdk_Color;
      Bg         : Gdk.Color.Gdk_Color;
      Fill       : Boolean := False)
     return Gtk_Plot_Canvas_Child
   is
      function Internal
        (Canvas     : System.Address;
         X1         : Gdouble;
         Y1         : Gdouble;
         X2         : Gdouble;
         Y2         : Gdouble;
         Style      : Gint;
         Width      : Gint;
         Fg         : System.Address;
         Bg         : System.Address;
         Fill       : Gint)
        return Gtk_Plot_Canvas_Child;
      pragma Import (C, Internal, "gtk_plot_canvas_put_ellipse");
      Fore : aliased Gdk.Color.Gdk_Color := Fg;
      Fga  : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Bg;
      Bga  : System.Address := Back'Address;
   begin
      if Fg = Gdk.Color.Null_Color then
         Fga := System.Null_Address;
      end if;
      if Bg = Gdk.Color.Null_Color then
         Bga := System.Null_Address;
      end if;
      return Internal
        (Get_Object (Canvas), X1, Y1, X2, Y2, Plot_Line_Style'Pos (Style),
         Width, Fga, Bga,  Boolean'Pos (Fill));
   end Put_Ellipse;

   -------------------------
   -- Line_Set_Attributes --
   -------------------------

   procedure Line_Set_Attributes
     (Child : Gtk_Plot_Canvas_Child;
      Style : Gtk.Extra.Plot.Plot_Line_Style;
      Width : Gint;
      Color : Gdk.Color.Gdk_Color;
      Mask  : Plot_Canvas_Arrow)
   is
      procedure Internal
        (Child      : Gtk_Plot_Canvas_Child;
         Style      : Gint;
         Width      : Gint;
         Color      : System.Address;
         Arrow_Mask : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_line_set_attributes");
      Col  : aliased Gdk.Color.Gdk_Color := Color;
      Cola : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Cola := System.Null_Address;
      end if;
      Internal
        (Child, Plot_Line_Style'Pos (Style), Width, Cola,
         Plot_Canvas_Arrow'Pos (Mask));
   end Line_Set_Attributes;

   ------------------------------
   -- Rectangle_Set_Attributes --
   ------------------------------

   procedure Rectangle_Set_Attributes
     (Child  : Gtk_Plot_Canvas_Child;
      Style  : Gtk.Extra.Plot.Plot_Line_Style;
      Width  : Gint;
      Fg     : Gdk.Color.Gdk_Color;
      Bg     : Gdk.Color.Gdk_Color;
      Border : Gtk.Extra.Plot.Plot_Border_Style;
      Fill   : Boolean := False)
   is
      procedure Internal
        (Child      : Gtk_Plot_Canvas_Child;
         Style      : Gint;
         Width      : Gint;
         Fg         : System.Address;
         Bg         : System.Address;
         Border     : Gint;
         Fill       : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_rectangle_set_attributes");
      Fore : aliased Gdk.Color.Gdk_Color := Fg;
      Fga  : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Bg;
      Bga  : System.Address := Back'Address;
   begin
      if Fg = Gdk.Color.Null_Color then
         Fga := System.Null_Address;
      end if;
      if Bg = Gdk.Color.Null_Color then
         Bga := System.Null_Address;
      end if;
      Internal
        (Child, Plot_Line_Style'Pos (Style), Width, Fga, Bga,
         Plot_Border_Style'Pos (Border), Boolean'Pos (Fill));
   end Rectangle_Set_Attributes;

   ---------------------------
   -- Ellipse_Set_Attributes --
   ---------------------------

   procedure Ellipse_Set_Attributes
     (Child  : Gtk_Plot_Canvas_Child;
      Style  : Gtk.Extra.Plot.Plot_Line_Style;
      Width  : Gint;
      Fg     : Gdk.Color.Gdk_Color;
      Bg     : Gdk.Color.Gdk_Color;
      Fill   : Boolean := False)
   is
      procedure Internal
        (Child      : Gtk_Plot_Canvas_Child;
         Style      : Gint;
         Width      : Gint;
         Fg         : System.Address;
         Bg         : System.Address;
         Fill       : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_ellipse_set_attributes");
      Fore : aliased Gdk.Color.Gdk_Color := Fg;
      Fga  : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Bg;
      Bga  : System.Address := Back'Address;
   begin
      if Fg = Gdk.Color.Null_Color then
         Fga := System.Null_Address;
      end if;
      if Bg = Gdk.Color.Null_Color then
         Bga := System.Null_Address;
      end if;
      Internal
        (Child, Plot_Line_Style'Pos (Style), Width, Fga, Bga,
         Boolean'Pos (Fill));
   end Ellipse_Set_Attributes;

   ---------------
   -- Put_Child --
   ---------------

   procedure Put_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble;
      Y2     : Gdouble)
   is
      procedure Internal
        (Canvas : System.Address;
         Child  : Gtk_Plot_Canvas_Child;
         X1, Y1, X2, Y2 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_put_child");
   begin
      Internal (Get_Object (Canvas), Child, X1, Y1, X2, Y2);
   end Put_Child;

   ----------------
   -- Child_Move --
   ----------------

   procedure Child_Move
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child;
      X1     : Gdouble;
      Y1     : Gdouble)
   is
      procedure Internal
        (Canvas : System.Address;
         Child  : Gtk_Plot_Canvas_Child;
         X1, Y1 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_child_move");
   begin
      Internal (Get_Object (Canvas), Child, X1, Y1);
   end Child_Move;

   -----------------------
   -- Child_Move_Resize --
   -----------------------

   procedure Child_Move_Resize
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble;
      Y2     : Gdouble)
   is
      procedure Internal
        (Canvas : System.Address;
         Child  : Gtk_Plot_Canvas_Child;
         X1, Y1, X2, Y2 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_child_move_resize");
   begin
      Internal (Get_Object (Canvas), Child, X1, Y1, X2, Y2);
   end Child_Move_Resize;

   ------------------
   -- Remove_Child --
   ------------------

   function Remove_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : Gtk_Plot_Canvas_Child)
     return Boolean
   is
      function Internal
        (Canvas : System.Address;
         Child  : Gtk_Plot_Canvas_Child)
        return Gint;
      pragma Import (C, Internal, "gtk_plot_canvas_remove_child");
   begin
      return Boolean'Val (Internal (Get_Object (Canvas), Child));
   end Remove_Child;

   -------------
   -- Convert --
   -------------

   function Convert (Canvas : System.Address) return Gtk_Plot_Canvas is
      Stub : Gtk_Plot_Canvas_Record;
   begin
      return Gtk_Plot_Canvas (Get_User_Data (Canvas, Stub));
   end Convert;

   ----------------
   -- Get_Pixmap --
   ----------------

   function Get_Pixmap (Canvas : access Gtk_Plot_Canvas_Record)
                       return Gdk.Pixmap.Gdk_Pixmap
   is
      function Internal (Canvas : System.Address) return Gdk.Pixmap.Gdk_Pixmap;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_get_pixmap");
   begin
      return Internal (Get_Object (Canvas));
   end Get_Pixmap;

end Gtk.Extra.Plot_Canvas;
