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

   ------------
   --  Copy  --
   ------------

   procedure Copy (Dst_GC :    out Gdk_GC;
                   Src_GC : in     Gdk_GC) is
      procedure Internal (Dst_GC :    out System.Address;
                          Src_GC : in     System.Address);
      pragma Import (C, Internal, "gdk_gc_copy");
      Temp : System.Address := System.Null_Address;
   begin
      Internal (Temp, Get_Object (Src_GC));
      Set_Object (Dst_GC, Temp);
   end Copy;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (GC : in out Gdk_GC) is
      procedure Internal (GC : in System.Address);
      pragma Import (C, Internal, "gdk_gc_destroy");
   begin
      Internal (Get_Object (GC));
      Set_Object (GC, System.Null_Address);
   end Destroy;

   ------------------
   --  Get_Values  --
   ------------------

   procedure Get_Values (GC     : in     Gdk_GC'Class;
                         Values :    out Gdk_GC_Values) is
      procedure Internal (GC : in System.Address;
                          Values : out System.Address);
      pragma Import (C, Internal, "gdk_gc_get_values");
      Temp : System.Address := System.Null_Address;
   begin
      Internal (Get_Object (GC), Temp);
      Set_Object (Values, Temp);
   end Get_Values;

   --------------
   -- Gdk_New  --
   --------------

   procedure Gdk_New (GC     :    out Gdk_GC;
                      Window : in     Gdk.Window.Gdk_Window'Class)
   is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gc_new");
   begin
      Set_Object (GC, Internal (Get_Object (Window)));
   end Gdk_New;

   --------------
   -- Gdk_New  --
   --------------

   procedure Gdk_New (GC          :    out Gdk_GC;
                      Window      : in     Gdk.Window.Gdk_Window'Class;
                      Values      : in     Gdk_GC_Values'Class;
                      Values_Mask : in     Types.Gdk_GC_Values_Mask) is
      function Internal (Window : in System.Address;
                         Values : in System.Address;
                         Values_Mask : in Types.Gdk_GC_Values_Mask)
                         return System.Address;
      pragma Import (C, Internal, "gdk_gc_new_with_values");
   begin
      Set_Object (GC, Internal (Get_Object (Window), Get_Object (Values),
                                Values_Mask));
   end Gdk_New;

   -------------
   -- Gdk_New --
   -------------

   function Gdk_New return Gdk_GC_Values is
      function Internal return System.Address;
      pragma Import (C, Internal, "ada_gdk_gc_new_values");
      Values : Gdk_GC_Values;
   begin
      Set_Object (Values, Internal);
      return Values;
   end Gdk_New;

   ----------
   -- Free --
   ----------

   procedure Free (Values : in out Gdk_GC_Values) is
      procedure Internal (Values : System.Address);
      pragma Import (C, Internal, "ada_gdk_gc_free_values");
   begin
      Internal (Get_Object (Values));
   end Free;

   -----------
   --  Ref  --
   -----------

   procedure Ref (GC : in out Gdk_GC) is
      function Internal (GC : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gc_ref");
      S : System.Address;
   begin
      S := Internal (Get_Object (GC));
   end Ref;

   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (GC     : in Gdk_GC;
                             Color  : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (GC, Color : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (Get_Object (GC), System.Null_Address);
      else
         Internal (Get_Object (GC), Col'Address);
      end if;
   end Set_Background;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Values : in Gdk_GC_Values;
                             Color  : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (GC, Color : in System.Address);
      pragma Import (C, Internal, "ada_gdk_gc_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (Get_Object (Values), System.Null_Address);
      else
         Internal (Get_Object (Values), Col'Address);
      end if;
   end Set_Background;

   -----------------------
   --  Set_Clip_Origin  --
   -----------------------

   procedure Set_Clip_Origin (GC   : in Gdk_GC;
                              X, Y : in Gint)
   is
      procedure Internal (GC : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_clip_origin");
   begin
      Internal (Get_Object (GC), X, Y);
   end Set_Clip_Origin;

   ---------------------
   -- Set_Clip_Origin --
   ---------------------

   procedure Set_Clip_Origin (Values : in Gdk_GC_Values;
                              X, Y : in Gint)
   is
      procedure Internal (GC : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "ada_gdk_gc_set_clip_origin");
   begin
      Internal (Get_Object (Values), X, Y);
   end Set_Clip_Origin;

   --------------------------
   --  Set_Clip_Rectangle  --
   --------------------------

   procedure Set_Clip_Rectangle
     (GC        : in Gdk_GC;
      Rectangle : in Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (GC, Rectangle : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_rectangle");

      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Rectangle;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      Internal (Get_Object (GC), Rec'Address);
   end Set_Clip_Rectangle;

   -----------------------
   --  Set_Clip_Region  --
   -----------------------

   procedure Set_Clip_Region (GC     : in Gdk_GC;
                              Region : in Gdk.Region.Gdk_Region'Class) is
      procedure Internal (GC, Region : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_region");
   begin
      Internal (Get_Object (GC), Get_Object (Region));
   end Set_Clip_Region;

   ----------------
   -- Set_Dashes --
   ----------------

   procedure Set_Dashes (Gc          : in Gdk_GC;
                         Dash_Offset : in Gint;
                         Dash_List   : in Guchar_Array) is
      procedure Internal (Gc          : in System.Address;
                          Dash_Offset : in Gint;
                          Dash_List   : in System.Address;
                          N           : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_dashes");
   begin
      Internal (Get_Object (Gc),
                Dash_Offset,
                Dash_List'Address,
                Dash_List'Length);
   end Set_Dashes;

   ---------------------
   --  Set_Exposures  --
   ---------------------

   procedure Set_Exposures (GC        : in Gdk_GC;
                            Exposures : in Boolean) is
      procedure Internal (GC : in System.Address; Exposures : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_exposures");
   begin
      Internal (Get_Object (GC), Boolean'Pos (Exposures));
   end Set_Exposures;

   -------------------
   -- Set_Exposures --
   -------------------

   procedure Set_Exposures (Values    : in Gdk_GC_Values;
                            Exposures : in Boolean) is
      procedure Internal (GC : in System.Address; Exposures : in Gint);
      pragma Import (C, Internal, "ada_gdk_gc_set_exposures");
   begin
      Internal (Get_Object (Values), Boolean'Pos (Exposures));
   end Set_Exposures;

   ----------------
   --  Set_Fill  --
   ----------------

   procedure Set_Fill (GC   : in Gdk_GC;
                       Fill : in Types.Gdk_Fill) is
      procedure Internal (GC : in System.Address; Fill : in Types.Gdk_Fill);
      pragma Import (C, Internal, "gdk_gc_set_fill");
   begin
      Internal (Get_Object (GC), Fill);
   end Set_Fill;

   --------------
   -- Set_Fill --
   --------------

   procedure Set_Fill (Values : in Gdk_GC_Values;
                       Fill   : in Types.Gdk_Fill) is
      procedure Internal (GC : in System.Address; Fill : in Types.Gdk_Fill);
      pragma Import (C, Internal, "ada_gdk_gc_set_fill");
   begin
      Internal (Get_Object (Values), Fill);
   end Set_Fill;

   ----------------
   --  Set_Font  --
   ----------------

   procedure Set_Font (GC   : in Gdk_GC;
                       Font : in Gdk.Font.Gdk_Font'Class) is
      procedure Internal (GC, Font : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_font");
   begin
      Internal (Get_Object (GC), Get_Object (Font));
   end Set_Font;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (Values : in Gdk_GC_Values;
                       Font   : in Gdk.Font.Gdk_Font'Class) is
      procedure Internal (GC, Font : in System.Address);
      pragma Import (C, Internal, "ada_gdk_gc_set_font");
   begin
      Internal (Get_Object (Values), Get_Object (Font));
   end Set_Font;

   ----------------------
   --  Set_Foreground  --
   ----------------------

   procedure Set_Foreground (GC    : in Gdk_GC;
                             Color : in Gdk.Color.Gdk_Color) is
      procedure Internal (GC, Color : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_foreground");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (Get_Object (GC), System.Null_Address);
      else
         Internal (Get_Object (GC), Col'Address);
      end if;
   end Set_Foreground;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Values : in Gdk_GC_Values;
                             Color  : in Gdk.Color.Gdk_Color) is
      procedure Internal (GC, Color : in System.Address);
      pragma Import (C, Internal, "ada_gdk_gc_set_foreground");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      if Color = Gdk.Color.Null_Color then
         Internal (Get_Object (Values), System.Null_Address);
      else
         Internal (Get_Object (Values), Col'Address);
      end if;
   end Set_Foreground;

   --------------------
   --  Set_Function  --
   --------------------

   procedure Set_Function (GC   : in Gdk_GC;
                           Func : in Types.Gdk_Function) is
      procedure Internal (GC   : in System.Address;
                          Func : in Types.Gdk_Function);
      pragma Import (C, Internal, "gdk_gc_set_function");
   begin
      Internal (Get_Object (GC), Func);
   end Set_Function;

   ------------------
   -- Set_Function --
   ------------------

   procedure Set_Function (Values : in Gdk_GC_Values;
                           Func   : in Types.Gdk_Function) is
      procedure Internal (GC   : in System.Address;
                          Func : in Types.Gdk_Function);
      pragma Import (C, Internal, "ada_gdk_gc_set_function");
   begin
      Internal (Get_Object (Values), Func);
   end Set_Function;

   ---------------------------
   --  Set_Line_Attributes  --
   ---------------------------

   procedure Set_Line_Attributes (GC         : in Gdk_GC;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style) is
      procedure Internal (GC : in System.Address;
                          Line_Width : in     Gint;
                          Line_Style : in     Types.Gdk_Line_Style;
                          Cap_Style  : in     Types.Gdk_Cap_Style;
                          Join_Style : in     Types.Gdk_Join_Style);
      pragma Import (C, Internal, "gdk_gc_set_line_attributes");
   begin
      Internal (Get_Object (GC), Line_Width, Line_Style,
                Cap_Style, Join_Style);
   end Set_Line_Attributes;

   -------------------------
   -- Set_Line_Attributes --
   -------------------------

   procedure Set_Line_Attributes (Values     : in Gdk_GC_Values;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style) is
      procedure Internal (GC : in System.Address;
                          Line_Width : in     Gint;
                          Line_Style : in     Types.Gdk_Line_Style;
                          Cap_Style  : in     Types.Gdk_Cap_Style;
                          Join_Style : in     Types.Gdk_Join_Style);
      pragma Import (C, Internal, "ada_gdk_gc_set_line_attributes");
   begin
      Internal (Get_Object (Values), Line_Width, Line_Style,
                Cap_Style, Join_Style);
   end Set_Line_Attributes;

   ---------------------
   --  Set_Subwindow  --
   ---------------------

   procedure Set_Subwindow (GC   : in Gdk_GC;
                            Mode : in Types.Gdk_Subwindow_Mode) is
      procedure Internal (GC : in System.Address;
                          Mode : in Types.Gdk_Subwindow_Mode);
      pragma Import (C, Internal, "gdk_gc_set_subwindow");
   begin
      Internal (Get_Object (GC), Mode);
   end Set_Subwindow;

   -------------------
   -- Set_Subwindow --
   -------------------

   procedure Set_Subwindow (Values : in Gdk_GC_Values;
                            Mode   : in Types.Gdk_Subwindow_Mode) is
      procedure Internal (GC : in System.Address;
                          Mode : in Types.Gdk_Subwindow_Mode);
      pragma Import (C, Internal, "ada_gdk_gc_set_subwindow");
   begin
      Internal (Get_Object (Values), Mode);
   end Set_Subwindow;

   ---------------------
   --  Set_Ts_Origin  --
   ---------------------

   procedure Set_Ts_Origin (GC   : in Gdk_GC;
                            X, Y : in Gint) is
      procedure Internal (GC : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gdk_gc_set_ts_origin");
   begin
      Internal (Get_Object (GC), X, Y);
   end Set_Ts_Origin;

   -------------------
   -- Set_Ts_Origin --
   -------------------

   procedure Set_Ts_Origin (Values : in Gdk_GC_Values;
                            X, Y   : in Gint) is
      procedure Internal (GC : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "ada_gdk_gc_set_ts_origin");
   begin
      Internal (Get_Object (Values), X, Y);
   end Set_Ts_Origin;

   -------------
   --  Unref  --
   -------------

   procedure Unref (GC : in out Gdk_GC) is
      procedure Internal (GC : in System.Address);
      pragma Import (C, Internal, "gdk_gc_unref");
   begin
      Internal (Get_Object (GC));
      Set_Object (GC, System.Null_Address);
   end Unref;

end Gdk.GC;
