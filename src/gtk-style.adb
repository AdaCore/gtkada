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

with Gdk; use Gdk;
with Gtk.Enums; use Gtk.Enums;
with System;

package body Gtk.Style is

   --------------
   --  Attach  --
   --------------

   function Attach (Style  : access Gtk_Style_Record;
                    Window : in Gdk.Window.Gdk_Window) return Gtk_Style is
      function Internal (Style : in System.Address;
                         Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_attach");
      Result : Gtk_Style;
   begin
      Result := new Gtk_Style_Record;
      Set_Object (Result, Internal (Get_Object (Style),
                                    Get_Object (Window)));
      Initialize_User_Data (Result);
      return Result;
   end Attach;


   ------------
   --  Copy  --
   ------------

   function Copy (Source : access Gtk_Style_Record) return Gtk_Style is
      function Internal (Style : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_copy");
      Destination : Gtk_Style;
   begin
      Destination := new Gtk_Style_Record;
      Set_Object (Destination, Internal (Get_Object (Source)));
      Initialize_User_Data (Destination);
      return Destination;
   end Copy;


   --------------
   --  Detach  --
   --------------

   procedure Detach (Style : access Gtk_Style_Record) is
      procedure Internal (Style : in System.Address);
      pragma Import (C, Internal, "gtk_style_detach");
   begin
      Internal (Get_Object (Style));
   end Detach;


   ------------------
   --  Draw_Arrow  --
   ------------------

   procedure Draw_Arrow (Style       : access Gtk_Style_Record;
                         Window      : in Gdk.Window.Gdk_Window;
                         State_Type  : in Enums.Gtk_State_Type;
                         Shadow_Type : in Enums.Gtk_Shadow_Type;
                         Arrow_Type  : in Enums.Gtk_Arrow_Type;
                         Fill        : in Gint;
                         X, Y        : in Gint;
                         Width       : in Gint;
                         Height      : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          Arrow_Type  : in Enums.Gtk_Arrow_Type;
                          Fill        : in Gint;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_arrow");
   begin
      Internal (Get_Object (Style), Get_Object (Window), State_Type,
                Shadow_Type, Arrow_Type, Fill, X, Y, Width, Height);
   end Draw_Arrow;



   --------------------
   --  Draw_Diamond  --
   --------------------

   procedure Draw_Diamond (Style       : access Gtk_Style_Record;
                           Window      : in Gdk.Window.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           X, Y        : in Gint;
                           Width       : in Gint;
                           Height      : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_diamond");
   begin
      Internal (Get_Object (Style), Get_Object (Window), State_Type,
                Shadow_Type, X, Y, Width, Height);
   end Draw_Diamond;


   ------------------
   --  Draw_Hline  --
   ------------------

   procedure Draw_Hline (Style      : access Gtk_Style_Record;
                         Window     : in Gdk.Window.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         X1, X2     : in Gint;
                         Y          : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          X1, X2, Y     : in Gint);
      pragma Import (C, Internal, "gtk_draw_hline");
   begin
      Internal (Get_Object (Style), Get_Object (Window),
                State_Type, X1, X2, Y);
   end Draw_Hline;


   -----------------
   --  Draw_Oval  --
   -----------------

   procedure Draw_Oval (Style       : access Gtk_Style_Record;
                        Window      : in Gdk.Window.Gdk_Window;
                        State_Type  : in Enums.Gtk_State_Type;
                        Shadow_Type : in Enums.Gtk_Shadow_Type;
                        X, Y        : in Gint;
                        Width       : in Gint;
                        Height      : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_oval");
   begin
      Internal (Get_Object (Style), Get_Object (Window), State_Type,
                Shadow_Type, X, Y, Width, Height);
   end Draw_Oval;


   --------------------
   --  Draw_Polygon  --
   --------------------

   procedure Draw_Polygon (Style       : access Gtk_Style_Record;
                           Window      : in Gdk.Window.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           Points      : in Gdk.Types.Gdk_Points_Array;
                           Fill        : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          Shadow_Type   : in Enums.Gtk_Shadow_Type;
                          Points        : in Gdk.Types.Gdk_Points_Array;
                          Npoints       : in Gint;
                          Fill          : in Gint);
      pragma Import (C, Internal, "gtk_draw_polygon");
   begin
      Internal (Get_Object (Style), Get_Object (Window), State_Type,
                Shadow_Type, Points, Points'Length, Fill);
   end Draw_Polygon;


   -------------------
   --  Draw_Shadow  --
   -------------------

   procedure Draw_Shadow (Style       : access Gtk_Style_Record;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint) is
      procedure Internal (Style, Window       : in System.Address;
                          State_Type          : in Enums.Gtk_State_Type;
                          Shadow_Type         : in Enums.Gtk_Shadow_Type;
                          X, Y, Width, Height : Gint);
      pragma Import (C, Internal, "gtk_draw_shadow");
   begin
      Internal (Get_Object (Style), Get_Object (Window),
                State_Type, Shadow_Type, X, Y, Width, Height);
   end Draw_Shadow;


   -------------------
   --  Draw_String  --
   -------------------

   procedure Draw_String (Style       : access Gtk_Style_Record;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          X, Y        : in Gint;
                          Str         : in String) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          X, Y          : in Gint;
                          Str           : in String);
      pragma Import (C, Internal, "gtk_draw_string");
   begin
      Internal (Get_Object (Style), Get_Object (Window),
                State_Type, X,  Y, Str & ASCII.NUL);
   end Draw_String;


   ------------------
   --  Draw_Vline  --
   ------------------

   procedure Draw_Vline (Style      : access Gtk_Style_Record;
                         Window     : in Gdk.Window.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         Y1, Y2     : in Gint;
                         X          : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          Y1, Y2, X     : in Gint);
      pragma Import (C, Internal, "gtk_draw_vline");
   begin
      Internal (Get_Object (Style), Get_Object (Window),
                State_Type, Y1, Y2, X);
   end Draw_Vline;

   ------------
   -- Get_Bg --
   ------------

   function Get_Bg (Style      : access Gtk_Style_Record;
                    State_Type : in Enums.Gtk_State_Type)
                    return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in System.Address;
                         State_Type : in Gint)
                        return Gdk.Color.Gdk_Color;
      pragma Import (C, Internal, "ada_style_get_bg");
   begin
      return Internal (Get_Object (Style),
                       Gtk_State_Type'Pos (State_Type));
   end Get_Bg;

   ---------------
   -- Get_Black --
   ---------------

   function Get_Black (Style : access Gtk_Style_Record)
                       return Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in System.Address)
                        return Gdk.Color.Gdk_Color;
      pragma Import (C, Internal, "ada_style_get_black");
   begin
      return Internal (Get_Object (Style));
   end Get_Black;


   --------------------
   --  Get_Black_GC  --
   --------------------

   function Get_Black_GC (Style : access Gtk_Style_Record)
                          return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_style_get_black_gc");
      Result : Gdk.GC.Gdk_GC;
   begin
      Set_Object (Result, Internal (Get_Object (Style)));
      return Result;
   end Get_Black_GC;

   -----------------
   --  Get_Bg_GC  --
   -----------------

   function Get_Bg_GC (Style : access Gtk_Style_Record;
                       State : in Enums.Gtk_State_Type)
                       return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in System.Address; State : Gint)
                         return System.Address;
      pragma Import (C, Internal, "ada_gtk_style_get_bg_gc");
      Result : Gdk.GC.Gdk_GC;
   begin
      Set_Object (Result, Internal (Get_Object (Style),
                                    Enums.Gtk_State_Type'Pos (State)));
      return Result;
   end Get_Bg_GC;

   --------------------
   --  Get_White_GC  --
   --------------------

   function Get_White_GC (Style : access Gtk_Style_Record)
                          return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_style_get_white_gc");
      Result : Gdk.GC.Gdk_GC;
   begin
      Set_Object (Result, Internal (Get_Object (Style)));
      return Result;
   end Get_White_GC;

   ---------------
   -- Get_White --
   ---------------

   function Get_White (Style : access Gtk_Style_Record)
                       return Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in System.Address)
                        return Gdk.Color.Gdk_Color;
      pragma Import (C, Internal, "ada_style_get_white");
   begin
      return Internal (Get_Object (Style));
   end Get_White;

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Style : out Gtk_Style) is
   begin
      Style := new Gtk_Style_Record;
      Initialize (Style);
   end Gtk_New;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                      return       Gtk.Style.Gtk_Style
   is
      function Internal (Widget : System.Address)
                        return    System.Address;
      pragma Import (C, Internal, "ada_widget_get_style");
      Stub : Gtk.Style.Gtk_Style_Record;
   begin
      return Gtk.Style.Gtk_Style
        (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Style;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Style : access Gtk_Style_Record) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_style_new");
   begin
      Set_Object (Style, Internal);
      Initialize_User_Data (Style);
   end Initialize;

   ---------
   -- Ref --
   ---------

   procedure Ref (Object : access Gtk_Style_Record) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_style_ref");
      use type System.Address;
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Ref;

   --------------
   -- Set_Base --
   --------------

   procedure Set_Base (Style      : access Gtk_Style_Record;
                       State_Type : in     Enums.Gtk_State_Type;
                       Color      : in     Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address; State : Gint; Color : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "ada_style_set_base");
   begin
      Internal
        (Get_Object (Style), Enums.Gtk_State_Type'Pos (State_Type), Color);
   end Set_Base;

   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (Style      : access Gtk_Style_Record;
                             Window     : in     Gdk.Window.Gdk_Window;
                             State_Type : in     Enums.Gtk_State_Type) is
      procedure Internal (Style      : in System.Address;
                          Window     : in System.Address;
                          State_Type : in Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_style_set_background");
   begin
      Internal (Get_Object (Style), Get_Object (Window), State_Type);
   end Set_Background;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (Style : access Gtk_Style_Record;
                       Font  : Gdk.Font.Gdk_Font)
   is
      procedure Internal
        (Style : System.Address; Font : System.Address);
      pragma Import (C, Internal, "ada_style_set_font");
   begin
      Internal (Get_Object (Style), Get_Object (Font));
   end Set_Font;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Style      : access Gtk_Style_Record;
                             State_Type : in     Enums.Gtk_State_Type;
                             Color      : in     Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address; State : Gint; Color : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "ada_style_set_foreground");
   begin
      Internal
        (Get_Object (Style), Enums.Gtk_State_Type'Pos (State_Type), Color);
   end Set_Foreground;

   -----------
   -- Unref --
   -----------

   procedure Unref (Object : access Gtk_Style_Record) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_style_unref");
      use type System.Address;
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Unref;

end Gtk.Style;
