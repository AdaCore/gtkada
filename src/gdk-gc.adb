-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

package body Gdk.GC is

   --------------
   -- Gdk_New  --
   --------------

   procedure Gdk_New (GC     :    out Gdk_GC;
                      Window : in     Gdk.Window.Gdk_Window)
   is
      function Internal (Window : in Gdk.Window.Gdk_Window) return Gdk_GC;
      pragma Import (C, Internal, "gdk_gc_new");
   begin
      GC := Internal (Window);
   end Gdk_New;

   --------------
   -- Gdk_New  --
   --------------

   procedure Gdk_New (GC          :    out Gdk_GC;
                      Window      : in     Gdk.Window.Gdk_Window;
                      Values      : in     Gdk_GC_Values;
                      Values_Mask : in     Types.Gdk_GC_Values_Mask) is
      function Internal (Window      : in Gdk.Window.Gdk_Window;
                         Values      : in Gdk_GC_Values;
                         Values_Mask : in Types.Gdk_GC_Values_Mask)
                         return Gdk_GC;
      pragma Import (C, Internal, "gdk_gc_new_with_values");
   begin
      GC := Internal (Window, Values, Values_Mask);
   end Gdk_New;

   -------------
   -- Gdk_New --
   -------------

   function Gdk_New return Gdk_GC_Values is
      function Internal return Gdk_GC_Values;
      pragma Import (C, Internal, "ada_gdk_gc_new_values");
   begin
      return Internal;
   end Gdk_New;

   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (GC     : in Gdk_GC;
                             Color  : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Gc    : in Gdk_GC;
                          Color : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (GC, System.Null_Address);
      else
         Internal (GC, Col'Address);
      end if;
   end Set_Background;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Values : in Gdk_GC_Values;
                             Color  : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (GC    : in Gdk_GC_Values;
                          Color : in System.Address);
      pragma Import (C, Internal, "ada_gdk_gc_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (Values, System.Null_Address);
      else
         Internal (Values, Col'Address);
      end if;
   end Set_Background;

   --------------------------
   --  Set_Clip_Rectangle  --
   --------------------------

   procedure Set_Clip_Rectangle
     (GC        : in Gdk_GC;
      Rectangle : in Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (Gc        : in Gdk_GC;
                          Rectangle : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_rectangle");

      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Rectangle;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      Internal (GC, Rec'Address);
   end Set_Clip_Rectangle;

   -----------------------
   --  Set_Clip_Region  --
   -----------------------

   procedure Set_Clip_Region (GC     : in Gdk_GC;
                              Region : in Gdk.Region.Gdk_Region) is
      procedure Internal (Gc     : Gdk_GC;
                          Region : in Gdk.Region.Gdk_Region);
      pragma Import (C, Internal, "gdk_gc_set_clip_region");
   begin
      Internal (GC, Region);
   end Set_Clip_Region;

   ----------------
   -- Set_Dashes --
   ----------------

   procedure Set_Dashes (Gc          : in Gdk_GC;
                         Dash_Offset : in Gint;
                         Dash_List   : in Guchar_Array) is
      procedure Internal (Gc          : in Gdk_GC;
                          Dash_Offset : in Gint;
                          Dash_List   : in System.Address;
                          N           : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_dashes");
   begin
      Internal (Gc,
                Dash_Offset,
                Dash_List'Address,
                Dash_List'Length);
   end Set_Dashes;

   ---------------------
   --  Set_Exposures  --
   ---------------------

   procedure Set_Exposures (GC        : in Gdk_GC;
                            Exposures : in Boolean) is
      procedure Internal (GC : in Gdk_GC; Exposures : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_exposures");
   begin
      Internal (GC, Boolean'Pos (Exposures));
   end Set_Exposures;

   -------------------
   -- Set_Exposures --
   -------------------

   procedure Set_Exposures (Values    : in Gdk_GC_Values;
                            Exposures : in Boolean) is
      procedure Internal (GC : in Gdk_GC_Values; Exposures : in Gint);
      pragma Import (C, Internal, "ada_gdk_gc_set_exposures");
   begin
      Internal (Values, Boolean'Pos (Exposures));
   end Set_Exposures;

   ----------------
   --  Set_Font  --
   ----------------

   procedure Set_Font (GC   : in Gdk_GC;
                       Font : in Gdk.Font.Gdk_Font) is
      procedure Internal (Gc  : in Gdk_GC; Font : in Gdk.Font.Gdk_Font);
      pragma Import (C, Internal, "gdk_gc_set_font");
   begin
      Internal (GC, Font);
   end Set_Font;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (Values : in Gdk_GC_Values;
                       Font   : in Gdk.Font.Gdk_Font) is
      procedure Internal (Gc : in Gdk_GC_Values; Font : in Gdk.Font.Gdk_Font);
      pragma Import (C, Internal, "ada_gdk_gc_set_font");
   begin
      Internal (Values, Font);
   end Set_Font;

   ----------------------
   --  Set_Foreground  --
   ----------------------

   procedure Set_Foreground (GC    : in Gdk_GC;
                             Color : in Gdk.Color.Gdk_Color) is
      procedure Internal (Gc  : in Gdk_GC; Color : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_foreground");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (GC, System.Null_Address);
      else
         Internal (GC, Col'Address);
      end if;
   end Set_Foreground;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Values : in Gdk_GC_Values;
                             Color  : in Gdk.Color.Gdk_Color) is
      procedure Internal (Gc : in Gdk_GC_Values; Color : in System.Address);
      pragma Import (C, Internal, "ada_gdk_gc_set_foreground");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (Values, System.Null_Address);
      else
         Internal (Values, Col'Address);
      end if;
   end Set_Foreground;

   --------------------
   --  Set_Function  --
   --------------------

   procedure Set_Function (GC   : in Gdk_GC;
                           Func : in Types.Gdk_Function) is
      procedure Internal (GC   : in Gdk_GC;
                          Func : in Types.Gdk_Function);
      pragma Import (C, Internal, "gdk_gc_set_function");
   begin
      Internal (GC, Func);
   end Set_Function;

   ------------------
   -- Set_Function --
   ------------------

   procedure Set_Function (Values : in Gdk_GC_Values;
                           Func   : in Types.Gdk_Function) is
      procedure Internal (GC   : in Gdk_GC_Values;
                          Func : in Types.Gdk_Function);
      pragma Import (C, Internal, "ada_gdk_gc_set_function");
   begin
      Internal (Values, Func);
   end Set_Function;

   --------------
   -- Set_Fill --
   --------------

   procedure Set_Fill (Values : in Gdk_GC_Values;
                       Fill   : in Types.Gdk_Fill)
   is
      procedure Internal (Values : in Gdk_GC_Values; Fill : Types.Gdk_Fill);
      pragma Import (C, Internal, "ada_gdk_gc_set_fill");
   begin
      Internal (Values, Fill);
   end Set_Fill;

   --------------
   -- Set_Fill --
   --------------

   procedure Set_Fill (GC   : in Gdk_GC;
                       Fill : in Types.Gdk_Fill)
   is
      procedure Internal (GC : in Gdk_GC; Fill : Types.Gdk_Fill);
      pragma Import (C, Internal, "gdk_gc_set_fill");
   begin
      Internal (GC, Fill);
   end Set_Fill;

   ---------------------
   -- Set_Clip_Origin --
   ---------------------

   procedure Set_Clip_Origin (GC   : in Gdk_GC;
                              X, Y : in Gint)
   is
      procedure Internal (Gc : Gdk_GC; X, Y : Gint);
      pragma Import (C, Internal, "gdk_gc_set_clip_origin");
   begin
      Internal (GC, X, Y);
   end Set_Clip_Origin;

   ---------------------
   -- Set_Clip_Origin --
   ---------------------

   procedure Set_Clip_Origin (Values : in Gdk_GC_Values;
                              X, Y   : in Gint)
   is
      procedure Internal (Values : Gdk_GC_Values; X, Y : Gint);
      pragma Import (C, Internal, "gdk_gc_set_clip_origin");
   begin
      Internal (Values, X, Y);
   end Set_Clip_Origin;

   ---------------------------
   --  Set_Line_Attributes  --
   ---------------------------

   procedure Set_Line_Attributes (GC         : in Gdk_GC;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style) is
      procedure Internal (Gc         : in     Gdk_GC;
                          Line_Width : in     Gint;
                          Line_Style : in     Types.Gdk_Line_Style;
                          Cap_Style  : in     Types.Gdk_Cap_Style;
                          Join_Style : in     Types.Gdk_Join_Style);
      pragma Import (C, Internal, "gdk_gc_set_line_attributes");
   begin
      Internal (GC, Line_Width, Line_Style, Cap_Style, Join_Style);
   end Set_Line_Attributes;

   -------------------------
   -- Set_Line_Attributes --
   -------------------------

   procedure Set_Line_Attributes (Values     : in Gdk_GC_Values;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style) is
      procedure Internal (Gc         : in Gdk_GC_Values;
                          Line_Width : in Gint;
                          Line_Style : in Types.Gdk_Line_Style;
                          Cap_Style  : in Types.Gdk_Cap_Style;
                          Join_Style : in Types.Gdk_Join_Style);
      pragma Import (C, Internal, "ada_gdk_gc_set_line_attributes");
   begin
      Internal (Values, Line_Width, Line_Style, Cap_Style, Join_Style);
   end Set_Line_Attributes;

   ---------------------
   --  Set_Subwindow  --
   ---------------------

   procedure Set_Subwindow (GC   : in Gdk_GC;
                            Mode : in Types.Gdk_Subwindow_Mode) is
      procedure Internal (GC   : in Gdk_GC;
                          Mode : in Types.Gdk_Subwindow_Mode);
      pragma Import (C, Internal, "gdk_gc_set_subwindow");
   begin
      Internal (GC, Mode);
   end Set_Subwindow;

   -------------------
   -- Set_Subwindow --
   -------------------

   procedure Set_Subwindow (Values : in Gdk_GC_Values;
                            Mode   : in Types.Gdk_Subwindow_Mode) is
      procedure Internal (GC : in Gdk_GC_Values;
                          Mode : in Types.Gdk_Subwindow_Mode);
      pragma Import (C, Internal, "ada_gdk_gc_set_subwindow");
   begin
      Internal (Values, Mode);
   end Set_Subwindow;

   ---------------------
   --  Set_Ts_Origin  --
   ---------------------

   procedure Set_Ts_Origin (GC   : in Gdk_GC;
                            X, Y : in Gint) is
      procedure Internal (GC : in Gdk_GC; X, Y : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_ts_origin");
   begin
      Internal (GC, X, Y);
   end Set_Ts_Origin;

   -------------------
   -- Set_Ts_Origin --
   -------------------

   procedure Set_Ts_Origin (Values : in Gdk_GC_Values;
                            X, Y   : in Gint) is
      procedure Internal (GC : in Gdk_GC_Values; X, Y : in Gint);
      pragma Import (C, Internal, "ada_gdk_gc_set_ts_origin");
   begin
      Internal (Values, X, Y);
   end Set_Ts_Origin;

end Gdk.GC;
