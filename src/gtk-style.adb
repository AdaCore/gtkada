-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Gtk.Enums; use Gtk.Enums;
with System;

package body Gtk.Style is

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Style : out Gtk_Style) is
      function Internal return Gtk_Style;
      pragma Import (C, Internal, "gtk_style_new");
   begin
      Style := Internal;
   end Gtk_New;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                      return       Gtk_Style
   is
      function Internal (Widget : System.Address)
                        return    Gtk_Style;
      pragma Import (C, Internal, "ada_widget_get_style");
      --  Note: a GtkStyle is not a descendant from GtkObject, and has no
      --  user data associated with it. We need to create a new instance
      --  everytime. We can't return a pointer, since we have no way to
      --  free the memory afterward.
   begin
      return Internal (Get_Object (Widget));
   end Get_Style;

   ---------------
   -- Set_Style --
   ---------------
   procedure Set_Style (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                        Style : in Gtk_Style)
   is
      procedure Internal (Widget : System.Address; Style : Gtk_Style);
      pragma Import (C, Internal, "gtk_widget_set_style");
   begin
      Internal (Get_Object (Widget), Style);
   end Set_Style;

   --------------------
   --  Draw_Polygon  --
   --------------------

   procedure Draw_Polygon (Style       : in Gtk_Style;
                           Window      : in Gdk.Window.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           Points      : in Gdk.Types.Gdk_Points_Array;
                           Fill        : in Gint) is
      procedure Internal (Style         : in Gtk_Style;
                          Window        : in Gdk.Window.Gdk_Window;
                          State_Type    : in Enums.Gtk_State_Type;
                          Shadow_Type   : in Enums.Gtk_Shadow_Type;
                          Points        : in Gdk.Types.Gdk_Points_Array;
                          Npoints       : in Gint;
                          Fill          : in Gint);
      pragma Import (C, Internal, "gtk_draw_polygon");
   begin
      Internal (Style, Window, State_Type,
                Shadow_Type, Points, Points'Length, Fill);
   end Draw_Polygon;

   -------------------
   --  Draw_String  --
   -------------------

   procedure Draw_String (Style       : in Gtk_Style;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          X, Y        : in Gint;
                          Str         : in String) is
      procedure Internal (Style         : in Gtk_Style;
                          Window        : in Gdk.Window.Gdk_Window;
                          State_Type    : in Enums.Gtk_State_Type;
                          X, Y          : in Gint;
                          Str           : in String);
      pragma Import (C, Internal, "gtk_draw_string");
   begin
      Internal (Style, Window,
                State_Type, X,  Y, Str & ASCII.NUL);
   end Draw_String;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
                           return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
                        return System.Address;
      pragma Import (C, Internal, "ada_style_get_fg");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Style      : in Gtk_Style;
                             Window     : in Gdk.Window.Gdk_Window;
                             State_Type : in Enums.Gtk_State_Type)
   is
      procedure Internal (Style      : Gtk_Style;
                          Window     : Gdk.Window.Gdk_Window;
                          State_Type : Gint);
      pragma Import (C, Internal, "gtk_style_set_background");
   begin
      Internal (Style, Window, Enums.Gtk_State_Type'Pos (State_Type));
   end Set_Background;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_fg");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal
        (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Foreground;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
     return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_bg");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Background;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_bg");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal
        (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Background;

   ---------------
   -- Get_Light --
   ---------------

   function Get_Light      (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
                           return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_light");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Light;

   ---------------
   -- Set_Light --
   ---------------

   procedure Set_Light      (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_light");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal
        (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Light;

   --------------
   -- Get_Dark --
   --------------

   function Get_Dark       (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
                           return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_dark");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Dark;

   --------------
   -- Set_Dark --
   --------------

   procedure Set_Dark       (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_dark");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Dark;

   ----------------
   -- Get_Middle --
   ----------------

   function Get_Middle     (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
                           return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_mid");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Middle;

   ----------------
   -- Set_Middle --
   ----------------

   procedure Set_Middle     (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_mid");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal
        (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Middle;

   --------------
   -- Get_Text --
   --------------

   function Get_Text       (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
                           return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_text");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text       (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_text");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal
        (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Text;

   --------------
   -- Get_Base --
   --------------

   function Get_Base       (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type)
                           return          Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style;
                         State_Type : in Gint)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_base");
      Add : System.Address := Internal (Style,
                                        Gtk_State_Type'Pos (State_Type));
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Base;

   --------------
   -- Set_Base --
   --------------

   procedure Set_Base       (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; State : Gint; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_base");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal
        (Style, Enums.Gtk_State_Type'Pos (State_Type), Color_A);
   end Set_Base;

   ---------------
   -- Get_Black --
   ---------------

   function Get_Black (Style : in Gtk_Style)
     return Gdk.Color.Gdk_Color
   is
      function Internal (Style      : in Gtk_Style)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_black");
      Add : System.Address := Internal (Style);
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_Black;

   ---------------
   -- Set_Black --
   ---------------

   procedure Set_Black (Style      : in Gtk_Style;
                        Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_black");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal (Style, Color_A);
   end Set_Black;

   ---------------
   -- Get_White --
   ---------------

   function Get_White (Style : in Gtk_Style)
                      return Gdk.Color.Gdk_Color
   is
      function Internal (Style : in Gtk_Style)
        return System.Address;
      pragma Import (C, Internal, "ada_style_get_white");
      Add : System.Address := Internal (Style);
      Col : Gdk.Color.Gdk_Color;
      for Col'Address use Add;
   begin
      return Col;
   end Get_White;

   ---------------
   -- Set_White --
   ---------------

   procedure Set_White (Style      : in Gtk_Style;
                        Color      : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : Gtk_Style; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_white");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;
      Internal (Style, Color_A);
   end Set_White;

   -------------------------
   --  Get_Background_GC  --
   -------------------------

   function Get_Background_GC (Style : in Gtk_Style;
                               State_Type : in Enums.Gtk_State_Type)
                              return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_bg_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Background_GC;

   -----------------------
   -- Set_Background_GC --
   -----------------------

   procedure Set_Background_GC (Style : in Gtk_Style;
                                State_Type : in Enums.Gtk_State_Type;
                                GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_bg_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Background_GC;

   -------------------------
   --  Get_Foreground_GC  --
   -------------------------

   function Get_Foreground_GC (Style : in Gtk_Style;
                               State_Type : in Enums.Gtk_State_Type)
                              return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_fg_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Foreground_GC;

   -----------------------
   -- Set_Foreground_GC --
   -----------------------

   procedure Set_Foreground_GC (Style : in Gtk_Style;
                                State_Type : in Enums.Gtk_State_Type;
                                GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_fg_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Foreground_GC;

   --------------------
   --  Get_Light_GC  --
   --------------------

   function Get_Light_GC (Style : in Gtk_Style;
                          State_Type : in Enums.Gtk_State_Type)
                         return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_light_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Light_GC;

   ------------------
   -- Set_Light_GC --
   ------------------

   procedure Set_Light_GC (Style : in Gtk_Style;
                           State_Type : in Enums.Gtk_State_Type;
                           GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_light_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Light_GC;

   -------------------
   --  Get_Dark_GC  --
   -------------------

   function Get_Dark_GC (Style : in Gtk_Style;
                         State_Type : in Enums.Gtk_State_Type)
                        return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_dark_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Dark_GC;

   -----------------
   -- Set_Dark_GC --
   -----------------

   procedure Set_Dark_GC (Style : in Gtk_Style;
                          State_Type : in Enums.Gtk_State_Type;
                          GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_dark_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Dark_GC;

   ---------------------
   --  Get_Middle_GC  --
   ---------------------

   function Get_Middle_GC (Style : in Gtk_Style;
                           State_Type : in Enums.Gtk_State_Type)
                          return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_mid_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Middle_GC;

   -------------------
   -- Set_Middle_GC --
   -------------------

   procedure Set_Middle_GC (Style : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type;
                            GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_mid_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Middle_GC;

   -------------------
   --  Get_Text_GC  --
   -------------------

   function Get_Text_GC (Style : in Gtk_Style;
                         State_Type : in Enums.Gtk_State_Type)
                        return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_text_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Text_GC;

   -----------------
   -- Set_Text_GC --
   -----------------

   procedure Set_Text_GC (Style : in Gtk_Style;
                          State_Type : in Enums.Gtk_State_Type;
                          GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_text_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Text_GC;

   -------------------
   --  Get_Base_GC  --
   -------------------

   function Get_Base_GC (Style : in Gtk_Style;
                         State_Type : in Enums.Gtk_State_Type)
                        return Gdk.GC.Gdk_GC
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_base_gc");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Base_GC;

   -----------------
   -- Set_Base_GC --
   -----------------

   procedure Set_Base_GC (Style : in Gtk_Style;
                          State_Type : in Enums.Gtk_State_Type;
                          GC    : in Gdk.GC.Gdk_GC)
   is
      procedure Internal (Style : in Gtk_Style;
                          State : in Gint;
                          GC    : in Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_base_gc");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), GC);
   end Set_Base_GC;

   -------------------
   -- Set_Bg_Pixmap --
   -------------------

   procedure Set_Bg_Pixmap (Style      : in Gtk_Style;
                            State_Type : in Enums.Gtk_State_Type;
                            Pixmap     : in Gdk.Pixmap.Gdk_Pixmap)
   is
      procedure Internal (Style  : in Gtk_Style;
                          State  : in Gint;
                          Pixmap : in Gdk.Pixmap.Gdk_Pixmap);
      pragma Import (C, Internal, "ada_style_set_bg_pixmap");
   begin
      Internal (Style, Enums.Gtk_State_Type'Pos (State_Type), Pixmap);
   end Set_Bg_Pixmap;

   -------------------
   -- Get_Bg_Pixmap --
   -------------------

   function Get_Bg_Pixmap (Style      : in Gtk_Style;
                           State_Type : in Enums.Gtk_State_Type)
                          return Gdk.Pixmap.Gdk_Pixmap
   is
      function Internal (Style : in Gtk_Style; State : Gint)
                        return Gdk.Pixmap.Gdk_Pixmap;
      pragma Import (C, Internal, "ada_style_get_bg_pixmap");
   begin
      return Internal (Style, Enums.Gtk_State_Type'Pos (State_Type));
   end Get_Bg_Pixmap;

end Gtk.Style;
