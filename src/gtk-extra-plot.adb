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

with Gdk.Color;
with Gdk.Drawable;
with Gdk.GC;
with Gdk.Rectangle;
with Gtk.Enums;
with Gtk.Widget;
with System;
with Unchecked_Conversion;
with Interfaces.C.Strings;

package body Gtk.Extra.Plot is

   function To_Double_Array is new Unchecked_Conversion
     (System.Address, No_Range_Gdouble_Array_Access);

   ----------------------
   -- Plot_Flag_Is_Set --
   ----------------------

   function Plot_Flag_Is_Set (Plot : access Gtk_Plot_Record;
                              Flag : Guint8)
                             return Boolean
   is
      function Internal (Plot : System.Address; Flag : Guint8)
                        return Gint;
      pragma Import (C, Internal, "ada_gtk_extra_plot_flag_is_set");
   begin
      return Internal (Get_Object (Plot), Flag) /= 0;
   end Plot_Flag_Is_Set;

   --------------------
   -- Plot_Set_Flags --
   --------------------

   procedure Plot_Set_Flags  (Plot  : access Gtk_Plot_Record;
                              Flags : Guint8)
   is
      procedure Internal (Plot : System.Address; Flags : Guint8);
      pragma Import (C, Internal, "ada_gtk_extra_plot_set_flags");
   begin
      Internal (Get_Object (Plot), Flags);
   end Plot_Set_Flags;

   ----------------------
   -- Plot_Unset_Flags --
   ----------------------

   procedure Plot_Unset_Flags  (Plot  : access Gtk_Plot_Record;
                                Flags : Guint8)
   is
      procedure Internal (Plot : System.Address; Flags : Guint8);
      pragma Import (C, Internal, "ada_gtk_extra_plot_unset_flags");
   begin
      Internal (Get_Object (Plot), Flags);
   end Plot_Unset_Flags;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Plot     : out Gtk_Plot;
                      Drawable : in Gdk.Drawable.Gdk_Drawable
                        :=  Gdk.Drawable.Null_Drawable)
   is
   begin
      Plot := new Gtk_Plot_Record;
      Initialize (Plot, Drawable);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Plot     : access Gtk_Plot_Record'Class;
                         Drawable : in Gdk.Drawable.Gdk_Drawable)
   is
      function Internal (Drawable : in Gdk.Drawable.Gdk_Drawable)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_plot_new");
   begin
      Set_Object (Plot, Internal (Drawable));
      Initialize_User_Data (Plot);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Plot     : out Gtk_Plot;
                      Width    : in Gdouble;
                      Height   : in Gdouble;
                      Drawable : in Gdk.Drawable.Gdk_Drawable
                        :=  Gdk.Drawable.Null_Drawable)
   is
   begin
      Plot := new Gtk_Plot_Record;
      Initialize (Plot, Drawable, Width, Height);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Plot     : access Gtk_Plot_Record'Class;
                         Drawable : in Gdk.Drawable.Gdk_Drawable;
                         Width    : in Gdouble;
                         Height   : in Gdouble)
   is
      function Internal (Drawable : in Gdk.Drawable.Gdk_Drawable;
                         Width    : in Gdouble;
                         Height   : in Gdouble)
                        return        System.Address;
      pragma Import (C, Internal, "gtk_plot_new_with_size");
   begin
      Set_Object (Plot, Internal (Drawable, Width, Height));
      Initialize_User_Data (Plot);
   end Initialize;

   ------------------
   -- Set_Drawable --
   ------------------

   procedure Set_Drawable (Plot     : access Gtk_Plot_Record;
                           Drawable : in     Gdk.Drawable.Gdk_Drawable)
   is
      procedure Internal (Plot     : in System.Address;
                          Drawable : in Gdk.Drawable.Gdk_Drawable);
      pragma Import (C, Internal, "gtk_plot_set_drawable");
   begin
      Internal (Get_Object (Plot), Drawable);
   end Set_Drawable;

   ------------------
   -- Get_Drawable --
   ------------------

   function Get_Drawable (Plot   : access Gtk_Plot_Record)
                          return      Gdk.Drawable.Gdk_Drawable
   is
      function Internal (Plot : in System.Address)
                        return Gdk.Drawable.Gdk_Drawable;
      pragma Import (C, Internal, "gtk_plot_get_drawable");
   begin
      return Internal (Get_Object (Plot));
   end Get_Drawable;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position (Plot : access Gtk_Plot_Record;
                           X    : out Gdouble;
                           Y    : out Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          X    : out Gdouble;
                          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_position");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Get_Position;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size (Plot   : access Gtk_Plot_Record;
                       Width  : out Gdouble;
                       Height : out Gdouble)
   is
      procedure Internal (Plot   : in  System.Address;
                          Width  : out Gdouble;
                          Height : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_size");
   begin
      Internal (Get_Object (Plot), Width, Height);
   end Get_Size;

   -----------------------------
   -- Get_Internal_Allocation --
   -----------------------------

   function Get_Internal_Allocation (Plot   : access Gtk_Plot_Record)
                                     return      Gtk.Widget.Gtk_Allocation
   is
      function Internal (Plot   : in System.Address)
                         return      Gtk.Widget.Gtk_Allocation;
      pragma Import (C, Internal, "gtk_plot_get_internal_allocation");
   begin
      return Internal (Get_Object (Plot));
   end Get_Internal_Allocation;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Plot       : access Gtk_Plot_Record;
                             Background : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot       : in System.Address;
                          Background : in System.Address);
      pragma Import (C, Internal, "gtk_plot_set_background");
      use type Gdk.Color.Gdk_Color;
      Back : aliased Gdk.Color.Gdk_Color := Background;
      Backa : System.Address := Back'Address;
   begin
      if Background = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;
      Internal (Get_Object (Plot), Backa);
   end Set_Background;

   -----------
   -- Paint --
   -----------

   procedure Paint
      (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       Area   : in Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (Widget : in System.Address;
                          Area   : in System.Address);
      pragma Import (C, Internal, "gtk_plot_paint");
      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
   begin
      Internal (Get_Object (Widget), Rec'Address);
   end Paint;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
      (Plot : access Gtk_Plot_Record;
       Area : in Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (Plot : in System.Address;
                          Area : in System.Address);
      pragma Import (C, Internal, "gtk_plot_refresh");
      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
   begin
      Internal (Get_Object (Plot), Rec'Address);
   end Refresh;

   ----------
   -- Move --
   ----------

   procedure Move (Plot : access Gtk_Plot_Record;
                   X    : in Gdouble;
                   Y    : in Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          X    : in Gdouble;
                          Y    : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_move");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Move;

   ------------
   -- Resize --
   ------------

   procedure Resize (Plot   : access Gtk_Plot_Record;
                     Width  : in Gdouble;
                     Height : in Gdouble)
   is
      procedure Internal (Plot   : in System.Address;
                          Width  : in Gdouble;
                          Height : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_resize");
   begin
      Internal (Get_Object (Plot), Width, Height);
   end Resize;

   -----------------
   -- Move_Resize --
   -----------------

   procedure Move_Resize (Plot   : access Gtk_Plot_Record;
                          X      : in Gdouble;
                          Y      : in Gdouble;
                          Width  : in Gdouble;
                          Height : in Gdouble)
   is
      procedure Internal (Plot   : in System.Address;
                          X      : in Gdouble;
                          Y      : in Gdouble;
                          Width  : in Gdouble;
                          Height : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_move_resize");
   begin
      Internal (Get_Object (Plot), X, Y, Width, Height);
   end Move_Resize;

   ---------------
   -- Get_Pixel --
   ---------------

   procedure Get_Pixel (Plot : access Gtk_Plot_Record;
                        Xx   : in Gdouble;
                        Yy   : in Gdouble;
                        X    : out Gint;
                        Y    : out Gint)
   is
      procedure Internal (Plot : in System.Address;
                          Xx   : in Gdouble;
                          Yy   : in Gdouble;
                          X    : out Gint;
                          Y    : out Gint);
      pragma Import (C, Internal, "gtk_plot_get_pixel");
   begin
      Internal (Get_Object (Plot), Xx, Yy, X, Y);
   end Get_Pixel;

   ---------------
   -- Get_Point --
   ---------------

   procedure Get_Point (Plot : access Gtk_Plot_Record;
                        X    : in Gint;
                        Y    : in Gint;
                        Xx   : out Gdouble;
                        Yy   : out Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          X    : in Gint;
                          Y    : in Gint;
                          Xx   : out Gdouble;
                          Yy   : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_point");
   begin
      Internal (Get_Object (Plot), X, Y, Xx, Yy);
   end Get_Point;

   ----------------
   -- Set_Xrange --
   ----------------

   procedure Set_Xrange (Plot : access Gtk_Plot_Record;
                         Xmin : in Gdouble := 0.0;
                         Xmax : in Gdouble := 1.0)
   is
      procedure Internal (Plot : in System.Address;
                          Xmin : in Gdouble;
                          Xmax : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_xrange");
   begin
      Internal (Get_Object (Plot), Xmin, Xmax);
   end Set_Xrange;

   ----------------
   -- Set_Yrange --
   ----------------

   procedure Set_Yrange (Plot : access Gtk_Plot_Record;
                         Ymin : in Gdouble := 0.0;
                         Ymax : in Gdouble := 1.0)
   is
      procedure Internal (Plot : in System.Address;
                          Ymin : in Gdouble;
                          Ymax : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_yrange");
   begin
      Internal (Get_Object (Plot), Ymin, Ymax);
   end Set_Yrange;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range (Plot : access Gtk_Plot_Record;
                        Xmin : in Gdouble := 0.0;
                        Xmax : in Gdouble := 1.0;
                        Ymin : in Gdouble := 0.0;
                        Ymax : in Gdouble := 1.0)
   is
      procedure Internal (Plot : in System.Address;
                          Xmin : in Gdouble;
                          Xmax : in Gdouble;
                          Ymin : in Gdouble;
                          Ymax : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_range");
   begin
      Internal (Get_Object (Plot), Xmin, Xmax, Ymin, Ymax);
   end Set_Range;

   ---------------
   -- Autoscale --
   ---------------

   procedure Autoscale (Plot : access Gtk_Plot_Record) is
      procedure Internal (Plot : in System.Address);
      pragma Import (C, Internal, "gtk_plot_autoscale");
   begin
      Internal (Get_Object (Plot));
   end Autoscale;

   ----------------
   -- Get_Xrange --
   ----------------

   procedure Get_Xrange (Plot : access Gtk_Plot_Record;
                         Xmin : out Gdouble;
                         Xmax : out Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          Xmin : out Gdouble;
                          Xmax : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_xrange");
   begin
      Internal (Get_Object (Plot), Xmin, Xmax);
   end Get_Xrange;

   ----------------
   -- Get_Yrange --
   ----------------

   procedure Get_Yrange (Plot : access Gtk_Plot_Record;
                         Ymin : out Gdouble;
                         Ymax : out Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          Ymin : out Gdouble;
                          Ymax : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_yrange");
   begin
      Internal (Get_Object (Plot), Ymin, Ymax);
   end Get_Yrange;

   ----------------
   -- Set_Xscale --
   ----------------

   procedure Set_Xscale (Plot       : access Gtk_Plot_Record;
                         Scale_Type : in Plot_Scale)
   is
      procedure Internal (Plot       : in System.Address;
                          Scale_Type : in Gint);
      pragma Import (C, Internal, "gtk_plot_set_xscale");
   begin
      Internal (Get_Object (Plot),
                Plot_Scale'Pos (Scale_Type));
   end Set_Xscale;

   ----------------
   -- Set_Yscale --
   ----------------

   procedure Set_Yscale (Plot       : access Gtk_Plot_Record;
                         Scale_Type : in Plot_Scale)
   is
      procedure Internal (Plot       : in System.Address;
                          Scale_Type : in Gint);
      pragma Import (C, Internal, "gtk_plot_set_yscale");
   begin
      Internal (Get_Object (Plot),
                Plot_Scale'Pos (Scale_Type));
   end Set_Yscale;

   ----------------
   -- Get_Xscale --
   ----------------

   function Get_Xscale (Plot   : access Gtk_Plot_Record)
                        return      Plot_Scale
   is
      function Internal (Plot   : in System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_plot_get_xscale");
   begin
      return Plot_Scale'Val (Internal (Get_Object (Plot)));
   end Get_Xscale;

   ----------------
   -- Get_Yscale --
   ----------------

   function Get_Yscale (Plot   : access Gtk_Plot_Record)
                        return      Plot_Scale
   is
      function Internal (Plot   : in System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_plot_get_yscale");
   begin
      return Plot_Scale'Val (Internal (Get_Object (Plot)));
   end Get_Yscale;

   --------------
   -- Put_Text --
   --------------

   procedure Put_Text
      (Plot          : access Gtk_Plot_Record;
       X             : in Gdouble;
       Y             : in Gdouble;
       Angle         : in Plot_Angle;
       Ps_Font       : in String := "";
       Font_Height   : in Gint := 10;
       Foreground    : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
       Background    : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
       Justification : in Gtk.Enums.Gtk_Justification
         := Gtk.Enums.Justify_Center;
       Text          : in String := "")
   is
      procedure Internal (Plot          : in System.Address;
                          X             : in Gdouble;
                          Y             : in Gdouble;
                          Angle         : in Gint;
                          Font          : in System.Address;
                          Height        : in Gint;
                          Foreground    : in System.Address;
                          Background    : in System.Address;
                          Justification : in Gint;
                          Text          : in String);
      pragma Import (C, Internal, "gtk_plot_put_text");
      use type Gdk.Color.Gdk_Color;
      Back : aliased Gdk.Color.Gdk_Color := Background;
      Fore : aliased Gdk.Color.Gdk_Color := Foreground;
      Backa : System.Address := Back'Address;
      Forea : System.Address := Fore'Address;
      F    : aliased String := Ps_Font & ASCII.NUL;
   begin
      if Foreground = Gdk.Color.Null_Color then
         Forea := System.Null_Address;
      end if;
      if Background = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;

      if Ps_Font = "" then
         Internal (Get_Object (Plot), X, Y,
                   Plot_Angle'Pos (Angle),
                   System.Null_Address, Font_Height, Forea, Backa,
                   Gtk.Enums.Gtk_Justification'Pos (Justification),
                   Text & ASCII.Nul);
      else
         Internal (Get_Object (Plot), X, Y,
                   Plot_Angle'Pos (Angle),
                   F'Address, Font_Height, Forea, Backa,
                   Gtk.Enums.Gtk_Justification'Pos (Justification),
                   Text & ASCII.Nul);
      end if;
   end Put_Text;

   ----------------------
   -- Axis_Set_Visible --
   ----------------------

   procedure Axis_Set_Visible (Plot    : access Gtk_Plot_Record;
                               Axis    : in Plot_Axis_Pos;
                               Visible : in Boolean)
   is
      procedure Internal (Plot    : in System.Address;
                          Axis    : in Gint;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_visible");
   begin
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Boolean'Pos (Visible));
   end Axis_Set_Visible;

   ----------------------
   -- Axis_Get_Visible --
   ----------------------

   function Axis_Get_Visible (Plot   : access Gtk_Plot_Record;
                              Axis   : in Plot_Axis_Pos)
                             return      Boolean
   is
      function Internal (Plot   : in System.Address;
                         Axis   : in Gint)
                        return      Gint;
      pragma Import (C, Internal, "gtk_plot_axis_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Plot),
                                    Plot_Axis_Pos'Pos (Axis)));
   end Axis_Get_Visible;

   --------------------
   -- Axis_Set_Title --
   --------------------

   procedure Axis_Set_Title (Plot  : access Gtk_Plot_Record;
                             Axis  : in Plot_Axis_Pos;
                             Title : in String)
   is
      procedure Internal (Plot  : in System.Address;
                          Axis  : in Gint;
                          Title : in String);
      pragma Import (C, Internal, "gtk_plot_axis_set_title");
   begin
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Title & ASCII.Nul);
   end Axis_Set_Title;

   ---------------------
   -- Axis_Show_Title --
   ---------------------

   procedure Axis_Show_Title (Plot : access Gtk_Plot_Record;
                              Axis : in Plot_Axis_Pos)
   is
      procedure Internal (Plot : in System.Address;
                          Axis : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_show_title");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis));
   end Axis_Show_Title;

   ---------------------
   -- Axis_Hide_Title --
   ---------------------

   procedure Axis_Hide_Title (Plot : access Gtk_Plot_Record;
                              Axis : in Plot_Axis_Pos)
   is
      procedure Internal (Plot : in System.Address;
                          Axis : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_hide_title");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis));
   end Axis_Hide_Title;

   ---------------------
   -- Axis_Move_Title --
   ---------------------

   procedure Axis_Move_Title (Plot  : access Gtk_Plot_Record;
                              Axis  : in Plot_Axis_Pos;
                              Angle : in Plot_Angle;
                              X     : in Gdouble;
                              Y     : in Gdouble)
   is
      procedure Internal (Plot  : in System.Address;
                          Axis  : in Gint;
                          Angle : in Gint;
                          X     : in Gdouble;
                          Y     : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_move_title");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis),
                Plot_Angle'Pos (Angle), X, Y);
   end Axis_Move_Title;

   ------------------------
   -- Axis_Justify_Title --
   ------------------------

   procedure Axis_Justify_Title
     (Plot          : access Gtk_Plot_Record;
      Axis          : in Plot_Axis_Pos;
      Justification : in Gtk.Enums.Gtk_Justification)
   is
      procedure Internal (Plot          : in System.Address;
                          Axis          : in Gint;
                          Justification : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_justify_title");
   begin
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Gtk.Enums.Gtk_Justification'Pos (Justification));
   end Axis_Justify_Title;

   -------------------------
   -- Axis_Set_Attributes --
   -------------------------

   procedure Axis_Set_Attributes (Plot       : access Gtk_Plot_Record;
                                  Axis       : in     Plot_Axis_Pos;
                                  Line_Width : in     Gint;
                                  Color      : in     Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : in System.Address;
                          Axis  : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C  : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis), Line_Width, Ca);
   end Axis_Set_Attributes;

   -------------------------
   -- Axis_Get_Attributes --
   -------------------------

   procedure Axis_Get_Attributes (Plot       : access Gtk_Plot_Record;
                                  Axis       : in     Plot_Axis_Pos;
                                  Line_Width : out    Gint;
                                  Color      : out    Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : System.Address;
                          Axis  : Gint;
                          Width : out Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_get_attributes");
      Col : aliased Gdk.Color.Gdk_Color;
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis),
                Line_Width, Col'Address);
      Color := Col;
   end Axis_Get_Attributes;

   --------------------
   -- Axis_Set_Ticks --
   --------------------

   procedure Axis_Set_Ticks (Plot        : access Gtk_Plot_Record;
                             Orientation : in Gtk.Enums.Gtk_Orientation;
                             Major_Step  : in Gdouble;
                             Num_Minor   : in Gint)
   is
      procedure Internal (Plot        : in System.Address;
                          Orientation : in Gint;
                          Major_Step  : in Gdouble;
                          Num_Minor   : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks");
   begin
      Internal (Get_Object (Plot),
                Gtk.Enums.Gtk_Orientation'Pos (Orientation),
                Major_Step, Num_Minor);
   end Axis_Set_Ticks;

   --------------------------
   -- Axis_Set_Major_Ticks --
   --------------------------

   procedure Axis_Set_Major_Ticks (Plot        : access Gtk_Plot_Record;
                                   Orientation : in Gtk.Enums.Gtk_Orientation;
                                   Major_Step  : in Gdouble)
   is
      procedure Internal (Plot        : in System.Address;
                          Orientation : in Gint;
                          Major_Step  : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_set_major_ticks");
   begin
      Internal (Get_Object (Plot),
                Gtk.Enums.Gtk_Orientation'Pos (Orientation),
                Major_Step);
   end Axis_Set_Major_Ticks;

   --------------------------
   -- Axis_Set_Minor_Ticks --
   --------------------------

   procedure Axis_Set_Minor_Ticks (Plot        : access Gtk_Plot_Record;
                                   Orientation : in Gtk.Enums.Gtk_Orientation;
                                   Num_Minor   : in Gint)
   is
      procedure Internal (Plot        : in System.Address;
                          Orientation : in Gint;
                          Num_Minor   : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_minor_ticks");
   begin
      Internal (Get_Object (Plot),
                Gtk.Enums.Gtk_Orientation'Pos (Orientation),
                Num_Minor);
   end Axis_Set_Minor_Ticks;

   ---------------------------
   -- Axis_Set_Ticks_Length --
   ---------------------------

   procedure Axis_Set_Ticks_Length (Plot   : access Gtk_Plot_Record;
                                    Axis   : in Plot_Axis_Pos;
                                    Length : in Gint)
   is
      procedure Internal (Plot   : in System.Address;
                          Axis   : in Gint;
                          Length : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks_length");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis), Length);
   end Axis_Set_Ticks_Length;

   --------------------------
   -- Axis_Set_Ticks_Width --
   --------------------------

   procedure Axis_Set_Ticks_Width (Plot  : access Gtk_Plot_Record;
                                   Axis  : in Plot_Axis_Pos;
                                   Width : in Gint)
   is
      procedure Internal (Plot  : in System.Address;
                          Axis  : in Gint;
                          Width : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks_width");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis), Width);
   end Axis_Set_Ticks_Width;

   ---------------------
   -- Axis_Show_Ticks --
   ---------------------

   procedure Axis_Show_Ticks (Plot       : access Gtk_Plot_Record;
                              Axis       : in Plot_Axis_Pos;
                              Major_Mask : in Plot_Ticks_Pos;
                              Minor_Mask : in Plot_Ticks_Pos)
   is
      procedure Internal
         (Plot       : in System.Address;
          Axis       : in Gint;
          Major_Mask : in Gint;
          Minor_Mask : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_show_ticks");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis),
                Plot_Ticks_Pos'Pos (Major_Mask),
                Plot_Ticks_Pos'Pos (Minor_Mask));
   end Axis_Show_Ticks;

   ---------------------------
   -- Axis_Set_Ticks_Limits --
   ---------------------------

   procedure Axis_Set_Ticks_Limits (Plot        : access Gtk_Plot_Record;
                                    Orientation : in Gtk.Enums.Gtk_Orientation;
                                    Ticks_Beg   : in Gdouble;
                                    Ticks_End   : in Gdouble)
   is
      procedure Internal (Plot        : in System.Address;
                          Orientation : in Gint;
                          Beg         : in Gdouble;
                          The_End     : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks_limits");
   begin
      Internal (Get_Object (Plot),
                Gtk.Enums.Gtk_Orientation'Pos (Orientation),
                Ticks_Beg, Ticks_End);
   end Axis_Set_Ticks_Limits;

   -----------------------------
   -- Axis_Unset_Ticks_Limits --
   -----------------------------

   procedure Axis_Unset_Ticks_Limits
     (Plot        : access Gtk_Plot_Record;
      Orientation : in Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Plot        : in System.Address;
                          Orientation : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_unset_ticks_limits");
   begin
      Internal (Get_Object (Plot),
                Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Axis_Unset_Ticks_Limits;

   ----------------------
   -- Axis_Show_Labels --
   ----------------------

   procedure Axis_Show_Labels (Plot        : access Gtk_Plot_Record;
                               Axis        : in Plot_Axis_Pos;
                               Labels_Mask : in Plot_Label_Pos)
   is
      procedure Internal (Plot        : in System.Address;
                          Axis        : in Gint;
                          Labels_Mask : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_show_labels");
   begin
      Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis),
                Plot_Label_Pos'Pos (Labels_Mask));
   end Axis_Show_Labels;

   -------------------------------
   -- Axis_Title_Set_Attributes --
   -------------------------------

   procedure Axis_Title_Set_Attributes (Plot       : access Gtk_Plot_Record;
                                        Axis       : in Plot_Axis_Pos;
                                        Ps_Font    : in String;
                                        Height     : in Gint;
                                        Angle      : in Plot_Angle;
                                        Foreground : in Gdk.Color.Gdk_Color;
                                        Background : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot       : in System.Address;
                          Axis       : in Gint;
                          Font       : in String;
                          Height     : in Gint;
                          Angle      : in Gint;
                          Foreground : in System.Address;
                          Background : in System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_title_set_attributes");
      use type Gdk.Color.Gdk_Color;
      Fore : aliased Gdk.Color.Gdk_Color := Foreground;
      Fa   : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Background;
      Ba   : System.Address := Back'Address;
   begin
      if Fore = Gdk.Color.Null_Color then
         Fa := System.Null_Address;
      end if;
      if Back = Gdk.Color.Null_Color then
         Ba := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Ps_Font & ASCII.Nul,
                Height,
                Plot_Angle'Pos (Angle),
                Fa,
                Ba);
   end Axis_Title_Set_Attributes;

   --------------------------------
   -- Axis_Labels_Set_Attributes --
   --------------------------------

   procedure Axis_Labels_Set_Attributes (Plot       : access Gtk_Plot_Record;
                                         Axis       : in Plot_Axis_Pos;
                                         Ps_Font    : in String;
                                         Height     : in Gint;
                                         Angle      : in Plot_Angle;
                                         Foreground : in Gdk.Color.Gdk_Color;
                                         Background : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot       : in System.Address;
                          Axis       : in Gint;
                          Font       : in String;
                          Height     : in Gint;
                          Angle      : in Gint;
                          Foreground : in System.Address;
                          Background : in System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_labels_set_attributes");
      use type Gdk.Color.Gdk_Color;
      Fore : aliased Gdk.Color.Gdk_Color := Foreground;
      Fa   : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Background;
      Ba   : System.Address := Back'Address;
   begin
      if Fore = Gdk.Color.Null_Color then
         Fa := System.Null_Address;
      end if;
      if Back = Gdk.Color.Null_Color then
         Ba := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Ps_Font & ASCII.Nul,
                Height,
                Plot_Angle'Pos (Angle),
                Fa,
                Ba);
   end Axis_Labels_Set_Attributes;

   -----------------------------
   -- Axis_Labels_Set_Numbers --
   -----------------------------

   procedure Axis_Labels_Set_Numbers (Plot      : access Gtk_Plot_Record;
                                      Axis      : in Plot_Axis_Pos;
                                      Style     : in Plot_Label_Style;
                                      Precision : in Gint)
   is
      procedure Internal (Plot      : in System.Address;
                          Axis      : in Gint;
                          Style     : in Gint;
                          Precision : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_labels_set_numbers");
   begin
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Plot_Label_Style'Pos (Style),
                Precision);
   end Axis_Labels_Set_Numbers;

   --------------------------
   -- Axis_Set_Tick_Labels --
   --------------------------

   procedure Axis_Set_Tick_Labels (Plot   : access Gtk_Plot_Record;
                                   Axis   : in Plot_Axis_Pos;
                                   Labels : in Gtk.Enums.String_List.Glist)
   is
      procedure Internal (Plot   : in System.Address;
                          Axis   : in Gint;
                          Labels : in System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_set_tick_labels");
   begin
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Gtk.Enums.String_List.Get_Object (Labels));
   end Axis_Set_Tick_Labels;

   ---------------------------------
   -- Axis_Use_Custom_Tick_Labels --
   ---------------------------------

   procedure Axis_Use_Custom_Tick_Labels (Plot   : access Gtk_Plot_Record;
                                          Axis   : in Plot_Axis_Pos;
                                          Custom : in Boolean := True)
   is
      procedure Internal (Plot   : in System.Address;
                          Axis   : in Gint;
                          Custom : in Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_tick_labels");
   begin
      Internal (Get_Object (Plot),
                Plot_Axis_Pos'Pos (Axis),
                Boolean'Pos (Custom));
   end Axis_Use_Custom_Tick_Labels;

   --------------------
   -- X0_Get_Visible --
   --------------------

   function X0_Get_Visible (Plot   : access Gtk_Plot_Record)
                            return      Boolean
   is
      function Internal (Plot   : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_x0_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end X0_Get_Visible;

   --------------------
   -- X0_Set_Visible --
   --------------------

   procedure X0_Set_Visible (Plot    : access Gtk_Plot_Record;
                             Visible : in Boolean)
   is
      procedure Internal
         (Plot    : in System.Address;
          Visible : in Gint);
      pragma Import (C, Internal, "gtk_plot_x0_set_visible");
   begin
      Internal (Get_Object (Plot),
                Boolean'Pos (Visible));
   end X0_Set_Visible;

   --------------------
   -- Y0_Get_Visible --
   --------------------

   function Y0_Get_Visible (Plot   : access Gtk_Plot_Record)
                            return      Boolean
   is
      function Internal (Plot   : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_y0_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Y0_Get_Visible;

   --------------------
   -- Y0_Set_Visible --
   --------------------

   procedure Y0_Set_Visible (Plot    : access Gtk_Plot_Record;
                             Visible : in Boolean)
   is
      procedure Internal
         (Plot    : in System.Address;
          Visible : in Gint);
      pragma Import (C, Internal, "gtk_plot_y0_set_visible");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Visible));
   end Y0_Set_Visible;

   -----------------------
   -- Grids_Get_Visible --
   -----------------------

   procedure Grids_Get_Visible (Plot   : access Gtk_Plot_Record;
                                Vmajor : out Boolean;
                                Vminor : out Boolean;
                                Hmajor : out Boolean;
                                Hminor : out Boolean)
   is
      procedure Internal (Plot   : in System.Address;
                          Vmajor : out Boolean;
                          Vminor : out Boolean;
                          Hmajor : out Boolean;
                          Hminor : out Boolean);
      pragma Import (C, Internal, "gtk_plot_grids_get_visible");
   begin
      Internal (Get_Object (Plot), Vmajor, Vminor, Hmajor, Hminor);
   end Grids_Get_Visible;

   -----------------------
   -- Grids_Set_Visible --
   -----------------------

   procedure Grids_Set_Visible (Plot   : access Gtk_Plot_Record;
                                Vmajor : in Boolean;
                                Vminor : in Boolean;
                                Hmajor : in Boolean;
                                Hminor : in Boolean)
   is
      procedure Internal
         (Plot   : in System.Address;
          Vmajor : in Gint;
          Vminor : in Gint;
          Hmajor : in Gint;
          Hminor : in Gint);
      pragma Import (C, Internal, "gtk_plot_grids_set_visible");
   begin
      Internal (Get_Object (Plot),
                Boolean'Pos (Vmajor),
                Boolean'Pos (Vminor),
                Boolean'Pos (Hmajor),
                Boolean'Pos (Hminor));
   end Grids_Set_Visible;

   ---------------------------
   -- X0line_Set_Attributes --
   ---------------------------

   procedure X0line_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                    Style : in Plot_Line_Style;
                                    Width : in Gint;
                                    Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : in System.Address;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_x0line_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Line_Style'Pos (Style),
                Width,
                Ca);
   end X0line_Set_Attributes;

   ---------------------------
   -- Y0line_Set_Attributes --
   ---------------------------

   procedure Y0line_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                    Style : in Plot_Line_Style;
                                    Width : in Gint;
                                    Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Plot  : in System.Address;
          Style : in Gint;
          Width : in Gint;
          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_y0line_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Line_Style'Pos (Style),
                Width,
                Ca);
   end Y0line_Set_Attributes;

   --------------------------------
   -- Major_Hgrid_Set_Attributes --
   --------------------------------

   procedure Major_Hgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : in Plot_Line_Style;
                                         Width : in Gint;
                                         Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : in System.Address;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_major_hgrid_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Line_Style'Pos (Style),
                Width,
                Ca);
   end Major_Hgrid_Set_Attributes;

   --------------------------------
   -- Major_Vgrid_Set_Attributes --
   --------------------------------

   procedure Major_Vgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : in Plot_Line_Style;
                                         Width : in Gint;
                                         Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : in System.Address;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_major_vgrid_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Line_Style'Pos (Style),
                Width,
                Ca);
   end Major_Vgrid_Set_Attributes;

   --------------------------------
   -- Minor_Hgrid_Set_Attributes --
   --------------------------------

   procedure Minor_Hgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : in Plot_Line_Style;
                                         Width : in Gint;
                                         Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : in System.Address;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_minor_hgrid_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Line_Style'Pos (Style),
                Width,
                Ca);
   end Minor_Hgrid_Set_Attributes;

   --------------------------------
   -- Minor_Vgrid_Set_Attributes --
   --------------------------------

   procedure Minor_Vgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : in Plot_Line_Style;
                                         Width : in Gint;
                                         Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot  : in System.Address;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_minor_vgrid_set_attributes");
      use type Gdk.Color.Gdk_Color;
      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                Plot_Line_Style'Pos (Style),
                Width,
                Ca);
   end Minor_Vgrid_Set_Attributes;

   ------------------
   -- Show_Legends --
   ------------------

   procedure Show_Legends (Plot : access Gtk_Plot_Record)
   is
      procedure Internal (Plot : in System.Address);
      pragma Import (C, Internal, "gtk_plot_show_legends");
   begin
      Internal (Get_Object (Plot));
   end Show_Legends;

   ------------------
   -- Hide_Legends --
   ------------------

   procedure Hide_Legends (Plot : access Gtk_Plot_Record)
   is
      procedure Internal (Plot : in System.Address);
      pragma Import (C, Internal, "gtk_plot_hide_legends");
   begin
      Internal (Get_Object (Plot));
   end Hide_Legends;

   ------------------------
   -- Set_Legends_Border --
   ------------------------

   procedure Set_Legends_Border (Plot         : access Gtk_Plot_Record;
                                 Border       : Plot_Border_Style;
                                 Shadow_Width : Gint)
   is
      procedure Internal (Plot         : in System.Address;
                          Border       : Gint;
                          Shadow_Width : Gint);
      pragma Import (C, Internal, "gtk_plot_set_legends_border");
   begin
      Internal (Get_Object (Plot), Plot_Border_Style'Pos (Border),
                Shadow_Width);
   end Set_Legends_Border;

   ------------------
   -- Legends_Move --
   ------------------

   procedure Legends_Move (Plot : access Gtk_Plot_Record;
                           X    : in Gdouble;
                           Y    : in Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          X    : in Gdouble;
                          Y    : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_legends_move");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Legends_Move;

   --------------------------
   -- Legends_Get_Position --
   --------------------------

   procedure Legends_Get_Position (Plot : access Gtk_Plot_Record;
                                   X    : out Gdouble;
                                   Y    : out Gdouble)
   is
      procedure Internal (Plot : in System.Address;
                          X    : out Gdouble;
                          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_legends_get_position");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Legends_Get_Position;

   ----------------------------
   -- Legends_Get_Allocation --
   ----------------------------

   function Legends_Get_Allocation (Plot   : access Gtk_Plot_Record)
                                    return      Gtk.Widget.Gtk_Allocation
   is
      function Internal (Plot   : in System.Address)
                         return      Gtk.Widget.Gtk_Allocation;
      pragma Import (C, Internal, "gtk_plot_legends_get_allocation");
   begin
      return Internal (Get_Object (Plot));
   end Legends_Get_Allocation;

   ----------------------------
   -- Legends_Set_Attributes --
   ----------------------------

   procedure Legends_Set_Attributes (Plot       : access Gtk_Plot_Record;
                                     Ps_Font    : in String;
                                     Height     : in Gint;
                                     Foreground : in Gdk.Color.Gdk_Color;
                                     Background : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot       : in System.Address;
                          Font       : in System.Address;
                          Height     : in Gint;
                          Foreground : in System.Address;
                          Background : in System.Address);
      pragma Import (C, Internal, "gtk_plot_legends_set_attributes");
      use type Gdk.Color.Gdk_Color;
      Fore  : aliased Gdk.Color.Gdk_Color := Foreground;
      Forea : System.Address := Fore'Address;
      Back  : aliased Gdk.Color.Gdk_Color := Background;
      Backa : System.Address := Back'Address;
      Font  : String := Ps_Font & ASCII.NUL;
      F     : System.Address := Font'Address;
   begin
      if Foreground = Gdk.Color.Null_Color then
         Forea := System.Null_Address;
      end if;
      if Background = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;
      if Ps_Font = "" then
         F := System.Null_Address;
      end if;
      Internal (Get_Object (Plot),
                F,
                Height,
                Forea,
                Backa);
   end Legends_Set_Attributes;

   ----------
   -- Free --
   ----------

   procedure Free (Widget : in out Gtk_Plot_Data) is
      procedure Internal (S : Gtk_Plot_Data);
      pragma Import (C, Internal, "g_free");
   begin
      Internal (Widget);
      Widget := null;
   end Free;

   -----------------
   -- Add_Dataset --
   -----------------

   procedure Add_Dataset (Plot    : access Gtk_Plot_Record;
                          Dataset : Gtk_Plot_Data)
   is
      procedure Internal (Plot    : in System.Address;
                          Dataset : in Gtk_Plot_Data);
      pragma Import (C, Internal, "gtk_plot_add_dataset");
   begin
      Internal (Get_Object (Plot), Dataset);
   end Add_Dataset;

   ------------------
   -- Add_Function --
   ------------------

   function Add_Function (Plot   : access Gtk_Plot_Record;
                          Func   : in Plot_Function)
                         return      Gtk_Plot_Data
   is
      function Internal (Plot   : in System.Address;
                         Func   : in Plot_Function)
                        return      Gtk_Plot_Data;
      pragma Import (C, Internal, "gtk_plot_add_function");
   begin
      return Internal (Get_Object (Plot), Func);
   end Add_Function;

   ------------------
   -- Draw_Dataset --
   ------------------

   procedure Draw_Dataset (Plot : access Gtk_Plot_Record;
                           Gc   : in Gdk.GC.Gdk_GC;
                           Data : in Gtk_Plot_Data)
   is
      procedure Internal (Plot : in System.Address;
                          Gc   : in Gdk.GC.Gdk_GC;
                          Data : in Gtk_Plot_Data);
      pragma Import (C, Internal, "gtk_plot_draw_dataset");
   begin
      Internal (Get_Object (Plot), Gc, Data);
   end Draw_Dataset;

   ------------------------
   -- Dataset_Set_Points --
   ------------------------

   procedure Dataset_Set_Points (Data       : in Gtk_Plot_Data;
                                 X          : in Gdouble_Array_Access;
                                 Y          : in Gdouble_Array_Access;
                                 Dx         : in Gdouble_Array_Access;
                                 Dy         : in Gdouble_Array_Access)
   is
      procedure Internal (Data       : in Gtk_Plot_Data;
                          X          : in System.Address;
                          Y          : in System.Address;
                          Dx         : in System.Address;
                          Dy         : in System.Address;
                          Num_Points : in Gint);
      pragma Import (C, Internal, "gtk_plot_dataset_set_points");
   begin
      Internal (Data,
                X (X'First)'Address, Y (Y'First)'Address,
                Dx (Dx'First)'Address, Dy (Dy'First)'Address,
                X'Length);
   end Dataset_Set_Points;

   ------------------------
   -- Dataset_Get_Points --
   ------------------------

   procedure Dataset_Get_Points (Data       : in Gtk_Plot_Data;
                                 X          : out Points_Array;
                                 Y          : out Points_Array;
                                 Dx         : out Points_Array;
                                 Dy         : out Points_Array)
   is
      procedure Internal (Data       : in Gtk_Plot_Data;
                          X          : out System.Address;
                          Y          : out System.Address;
                          Dx         : out System.Address;
                          Dy         : out System.Address;
                          Num_Points : out Gint);
      pragma Import (C, Internal, "gtk_plot_dataset_get_points");
      Num_Points : Gint;
      X1, Y1, Dx1, Dy1 : System.Address;
   begin
      Internal (Data, X1, Y1, Dx1, Dy1, Num_Points);
      X  := (Points => To_Double_Array (X1),  Num_Points => Num_Points);
      Y  := (Points => To_Double_Array (Y1),  Num_Points => Num_Points);
      Dx := (Points => To_Double_Array (Dx1), Num_Points => Num_Points);
      Dy := (Points => To_Double_Array (Dy1), Num_Points => Num_Points);
   end Dataset_Get_Points;

   -------------------
   -- Dataset_Set_X --
   -------------------

   procedure Dataset_Set_X (Data : in Gtk_Plot_Data;
                            X    : in Gdouble_Array_Access)
   is
      procedure Internal (Data : in Gtk_Plot_Data;
                          X    : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_x");
   begin
      pragma Assert (Dataset_Get_Numpoints (Data) = X'Length);
      Internal (Data, X (X'First)'Address);
   end Dataset_Set_X;

   -------------------
   -- Dataset_Set_Y --
   -------------------

   procedure Dataset_Set_Y (Data : in Gtk_Plot_Data;
                            Y    : in Gdouble_Array_Access)
   is
      procedure Internal (Data : in Gtk_Plot_Data;
                          Y    : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_y");
   begin
      pragma Assert (Dataset_Get_Numpoints (Data) = Y'Length);
      Internal (Data, Y (Y'First)'Address);
   end Dataset_Set_Y;

   --------------------
   -- Dataset_Set_Dx --
   --------------------

   procedure Dataset_Set_Dx (Data : in Gtk_Plot_Data;
                             Dx   : in Gdouble_Array_Access)
   is
      procedure Internal (Data : in Gtk_Plot_Data;
                          Dx   : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_dx");
   begin
      pragma Assert (Dataset_Get_Numpoints (Data) = Dx'Length);
      Internal (Data, Dx (Dx'First)'Address);
   end Dataset_Set_Dx;

   --------------------
   -- Dataset_Set_Dy --
   --------------------

   procedure Dataset_Set_Dy (Data : in Gtk_Plot_Data;
                             Dy   : in Gdouble_Array_Access)
   is
      procedure Internal (Data : in Gtk_Plot_Data;
                          Dy   : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_dy");
   begin
      pragma Assert (Dataset_Get_Numpoints (Data) = Dy'Length);
      Internal (Data, Dy (Dy'First)'Address);
   end Dataset_Set_Dy;

   -------------------
   -- Dataset_Get_X --
   -------------------

   function Dataset_Get_X (Data       : in Gtk_Plot_Data)
                          return Points_Array
   is
      function Internal (Data       : in Gtk_Plot_Data;
                         Num_Points : in System.Address)
                        return      System.Address;
      pragma Import (C, Internal, "gtk_plot_dataset_get_x");
      Num_Points : aliased Gint;
      S : System.Address := Internal (Data, Num_Points'Address);
   begin
      return (Points => To_Double_Array (S), Num_Points => Num_Points);
   end Dataset_Get_X;

   -------------------
   -- Dataset_Get_Y --
   -------------------

   function Dataset_Get_Y (Data       : in Gtk_Plot_Data)
                          return    Points_Array
   is
      function Internal (Data       : in Gtk_Plot_Data;
                         Num_Points : in System.Address)
                        return        System.Address;
      pragma Import (C, Internal, "gtk_plot_dataset_get_y");
      Num_Points : aliased Gint;
      S : System.Address := Internal (Data, Num_Points'Address);
   begin
      return (Points => To_Double_Array (S), Num_Points => Num_Points);
   end Dataset_Get_Y;

   --------------------
   -- Dataset_Get_Dx --
   --------------------

   function Dataset_Get_Dx (Data     : in Gtk_Plot_Data)
                           return    Points_Array
   is
      function Internal (Data       : in Gtk_Plot_Data;
                         Num_Points : in System.Address)
                        return          System.Address;
      pragma Import (C, Internal, "gtk_plot_dataset_get_dx");
      Num_Points : aliased Gint;
      S : System.Address := Internal (Data, Num_Points'Address);
   begin
      return (Points => To_Double_Array (S), Num_Points => Num_Points);
   end Dataset_Get_Dx;

   --------------------
   -- Dataset_Get_Dy --
   --------------------

   function Dataset_Get_Dy (Data       : in Gtk_Plot_Data)
                           return      Points_Array
   is
      function Internal (Data       : in Gtk_Plot_Data;
                         Num_Points : in System.Address)
                        return          System.Address;
      pragma Import (C, Internal, "gtk_plot_dataset_get_dy");
      Num_Points : aliased Gint;
      S : System.Address := Internal (Data, Num_Points'Address);
   begin
      return (Points => To_Double_Array (S), Num_Points => Num_Points);
   end Dataset_Get_Dy;

   ------------------------
   -- Dataset_Set_Symbol --
   ------------------------

   procedure Dataset_Set_Symbol (Data       : in Gtk_Plot_Data;
                                 The_Type   : in Plot_Symbol_Type;
                                 Style      : in Plot_Symbol_Style;
                                 Size       : in Gint;
                                 Line_Width : in Gint;
                                 Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data       : in Gtk_Plot_Data;
                          The_Type   : in Gint;
                          Style      : in Gint;
                          Size       : in Gint;
                          Line_Width : in Gint;
                          Color      : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_symbol");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      C   : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         C := System.Null_Address;
      end if;
      Internal (Data,
                Plot_Symbol_Type'Pos (The_Type),
                Plot_Symbol_Style'Pos (Style),
                Size,
                Line_Width,
                C);
   end Dataset_Set_Symbol;

   ------------------------
   -- Dataset_Get_Symbol --
   ------------------------

   procedure Dataset_Get_Symbol (Data       : in Gtk_Plot_Data;
                                 The_Type   : out Plot_Symbol_Type;
                                 Style      : out Plot_Symbol_Style;
                                 Size       : out Gint;
                                 Line_Width : out Gint;
                                 Color      : out Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data       : in Gtk_Plot_Data;
                          The_Type   : out Gint;
                          Style      : out Gint;
                          Size       : out Gint;
                          Line_Width : out Gint;
                          Color      : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_get_symbol");
      C : aliased Gdk.Color.Gdk_Color;
      T, S : Gint;
   begin
      Internal (Data, T, S, Size, Line_Width, C'Address);
      The_Type := Plot_Symbol_Type'Val (T);
      Style    := Plot_Symbol_Style'Val (S);
      Color    := C;
   end Dataset_Get_Symbol;

   ---------------------------------
   -- Dataset_Set_Line_Attributes --
   ---------------------------------

   procedure Dataset_Set_Line_Attributes (Data  : in Gtk_Plot_Data;
                                          Style : in Plot_Line_Style;
                                          Width : in Gint;
                                          Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data  : in Gtk_Plot_Data;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_line_attributes");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      C   : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         C := System.Null_Address;
      end if;
      Internal (Data,
                Plot_Line_Style'Pos (Style),
                Width,
                C);
   end Dataset_Set_Line_Attributes;

   ---------------------------------
   -- Dataset_Get_Line_Attributes --
   ---------------------------------

   procedure Dataset_Get_Line_Attributes (Data  : in Gtk_Plot_Data;
                                          Style : out Plot_Line_Style;
                                          Width : out Gint;
                                          Color : out Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data  : in Gtk_Plot_Data;
                          Style : out Gint;
                          Width : out Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_get_line_attributes");
      C : aliased Gdk.Color.Gdk_Color;
      S : Gint;
   begin
      Internal (Data, S, Width, C'Address);
      Color := C;
      Style := Plot_Line_Style'Val (S);
   end Dataset_Get_Line_Attributes;

   ------------------------------
   -- Dataset_Set_X_Attributes --
   ------------------------------

   procedure Dataset_Set_X_Attributes (Data  : in Gtk_Plot_Data;
                                       Style : in Plot_Line_Style;
                                       Width : in Gint;
                                       Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data  : in Gtk_Plot_Data;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_x_attributes");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      C : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         C := System.Null_Address;
      end if;
      Internal (Data,
                Plot_Line_Style'Pos (Style),
                Width,
                C);
   end Dataset_Set_X_Attributes;

   ------------------------------
   -- Dataset_Set_Y_Attributes --
   ------------------------------

   procedure Dataset_Set_Y_Attributes (Data  : in Gtk_Plot_Data;
                                       Style : in Plot_Line_Style;
                                       Width : in Gint;
                                       Color : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Data  : in Gtk_Plot_Data;
                          Style : in Gint;
                          Width : in Gint;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_plot_dataset_set_y_attributes");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      C : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         C := System.Null_Address;
      end if;
      Internal (Data,
                Plot_Line_Style'Pos (Style),
                Width,
                C);
   end Dataset_Set_Y_Attributes;

   ------------------------
   -- Dataset_Set_Legend --
   ------------------------

   procedure Dataset_Set_Legend (Data   : in Gtk_Plot_Data;
                                 Legend : in String)
   is
      procedure Internal (Data   : in Gtk_Plot_Data;
                          Legend : in String);
      pragma Import (C, Internal, "gtk_plot_dataset_set_legend");
   begin
      Internal (Data, Legend & ASCII.NUL);
   end Dataset_Set_Legend;

   ----------------------
   -- Dataset_Set_Name --
   ----------------------

   procedure Dataset_Set_Name (Data : in Gtk_Plot_Data;
                               Name : in String)
   is
      procedure Internal (Data : in Gtk_Plot_Data;
                          Name : in String);
      pragma Import (C, Internal, "gtk_plot_dataset_set_name");
   begin
      Internal (Data, Name & ASCII.Nul);
   end Dataset_Set_Name;

   ----------------------
   -- Dataset_Get_Name --
   ----------------------

   function Dataset_Get_Name (Data : in Gtk_Plot_Data)
                             return String
   is
      function Internal (Data : Gtk_Plot_Data)
                        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_dataset_get_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Data));
   end Dataset_Get_Name;


   --------------------
   -- Remove_Dataset --
   --------------------

   function Remove_Dataset (Plot    : access Gtk_Plot_Record;
                            Dataset : in Gtk_Plot_Data)
                           return       Boolean
   is
      function Internal (Plot    : in System.Address;
                         Dataset : in Gtk_Plot_Data)
                        return       Gint;
      pragma Import (C, Internal, "gtk_plot_remove_dataset");
   begin
      return Boolean'Val (Internal (Get_Object (Plot), Dataset));
   end Remove_Dataset;

   --------------
   -- Get_Axis --
   --------------

   function Get_Axis (Plot   : access Gtk_Plot_Record;
                      Axis   : in Plot_Axis_Pos)
                     return     Gtk_Plot_Axis
   is
      function Internal (Plot   : in System.Address;
                         Axis   : in Gint)
                        return      Gtk_Plot_Axis;
      pragma Import (C, Internal, "gtk_plot_get_axis");
   begin
      return Internal (Get_Object (Plot), Plot_Axis_Pos'Pos (Axis));
   end Get_Axis;

   ---------------------------
   -- Generic_Plot_Function --
   ---------------------------

   function Generic_Plot_Function (Plot  : System.Address;
                                   Set   : Gtk_Plot_Data;
                                   X     : Gdouble;
                                   Error : access Gboolean)
                                  return Gdouble
   is
      Stub : Gtk_Plot_Record;
      B    : aliased Boolean;
      Y    : Gdouble;
   begin
      Y :=  Func (Gtk_Plot (Get_User_Data (Plot, Stub)),
                  Set,
                  X,
                  B'Access);
      Error.all := Boolean'Pos (B);
      return Y;
   end Generic_Plot_Function;

   ------------------
   -- Get_Datasets --
   ------------------

   function Get_Datasets (Plot : access Gtk_Plot_Record)
                         return Datasets_List.Glist
   is
      function Internal (Plot : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_plot_get_datasets");
      List : Datasets_List.Glist;
   begin
      Datasets_List.Set_Object (List, Internal (Get_Object (Plot)));
      return List;
   end Get_Datasets;

   ---------------
   -- Get_Texts --
   ---------------

   function Get_Texts (Plot : access Gtk_Plot_Record)
                      return Texts_List.Glist
   is
      function Internal (Plot : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_plot_get_texts");
      List : Texts_List.Glist;
   begin
      Texts_List.Set_Object (List, Internal (Get_Object (Plot)));
      return List;
   end Get_Texts;

   ---------------------
   -- Get_Text_String --
   ---------------------

   function Get_Text_String (Text : in Gtk_Plot_Text) return String is
      function Internal (Text : Gtk_Plot_Text)
                        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_plot_get_text_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Text));
   end Get_Text_String;

   -----------------
   -- Remove_Text --
   -----------------

   procedure Remove_Text (Plot : access Gtk_Plot_Record;
                          Text : in Gtk_Plot_Text)
   is
      procedure Internal (Plot : System.Address;
                          Text : Gtk_Plot_Text);
      pragma Import (C, Internal, "gtk_plot_remove_text");
   begin
      Internal (Get_Object (Plot), Text);
   end Remove_Text;

end Gtk.Extra.Plot;
