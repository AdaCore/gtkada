
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
with Gdk; use Gdk;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Extra.Plot;
with System;

package body Gtk.Extra.Plot_Layout is

   --------------
   -- Add_Plot --
   --------------

   procedure Add_Plot
     (Plot_Layout : access Gtk_Plot_Layout_Record;
      Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
      X           : in Gint;
      Y           : in Gint)
   is
      procedure Internal (Plot_Layout : in System.Address;
                          Plot        : in System.Address;
                          X           : in Gint;
                          Y           : in Gint);
      pragma Import (C, Internal, "gtk_plot_layout_add_plot");
   begin
      Internal (Get_Object (Plot_Layout), Get_Object (Plot), X, Y);
   end Add_Plot;

   ---------------
   -- Get_Pixel --
   ---------------

   procedure Get_Pixel (Plot_Layout : access Gtk_Plot_Layout_Record;
                        Px          : in Gdouble;
                        Py          : in Gdouble;
                        X           : out Gint;
                        Y           : out Gint)
   is
      procedure Internal (Plot_Layout : in System.Address;
                          Px          : in Gdouble;
                          Py          : in Gdouble;
                          X           : out Gint;
                          Y           : out Gint);
      pragma Import (C, Internal, "gtk_plot_layout_get_pixel");
   begin
      Internal (Get_Object (Plot_Layout), Px, Py, X, Y);
   end Get_Pixel;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
      (Plot_Layout : access Gtk_Plot_Layout_Record;
       X           : in Gint;
       Y           : in Gint;
       Px          : out Gdouble;
       Py          : out Gdouble)
   is
      procedure Internal
         (Plot_Layout : in System.Address;
          X           : in Gint;
          Y           : in Gint;
          Px          : out Gdouble;
          Py          : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_layout_get_position");
   begin
      Internal (Get_Object (Plot_Layout), X, Y, Px, Py);
   end Get_Position;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Plot_Layout;
                      Width  : in Gint;
                      Height : in Gint)
   is
   begin
      Widget := new Gtk_Plot_Layout_Record;
      Initialize (Widget, Width, Height);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Plot_Layout_Record;
                         Width  : in Gint;
                         Height : in Gint)
   is
      function Internal (Width  : in Gint;
                         Height : in Gint)
                        return      System.Address;
      pragma Import (C, Internal, "gtk_plot_layout_new");
   begin
      Set_Object (Widget, Internal (Width, Height));
      Initialize_User_Data (Widget);
   end Initialize;

   --------------
   -- Put_Text --
   --------------

   procedure Put_Text (Layout        : access Gtk_Plot_Layout_Record;
                       X             : in Gdouble;
                       Y             : in Gdouble;
                       Angle         : in Gint;
                       Ps_Font       : in String;
                       Height        : in Gint;
                       Fg            : in Gdk.Color.Gdk_Color;
                       Bg            : in Gdk.Color.Gdk_Color;
                       Justification : in Gtk.Enums.Gtk_Justification;
                       Text          : in String)
   is
      procedure Internal (Layout        : in System.Address;
                          X             : in Gdouble;
                          Y             : in Gdouble;
                          Angle         : in Gint;
                          Font          : in String;
                          Height        : in Gint;
                          Fg            : in System.Address;
                          Bg            : in System.Address;
                          Justification : in Gint;
                          Text          : in String);
      pragma Import (C, Internal, "gtk_plot_layout_put_text");
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
      Internal (Get_Object (Layout),
                X,
                Y,
                Angle,
                Ps_Font & Ascii.NUL,
                Height,
                F,
                B,
                Gtk.Enums.Gtk_Justification'Pos (Justification),
                Text & Ascii.NUL);
   end Put_Text;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Layout : access Gtk_Plot_Layout_Record)
   is
      procedure Internal (Layout : in System.Address);
      pragma Import (C, Internal, "gtk_plot_layout_refresh");
   begin
      Internal (Get_Object (Layout));
   end Refresh;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Plot_Layout : access Gtk_Plot_Layout_Record;
                             Color       : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot_Layout : in System.Address;
                          Color       : in System.Address);
      pragma Import (C, Internal, "gtk_plot_layout_set_background");
      use type Gdk.Color.Gdk_Color;
      Col : aliased Gdk.Color.Gdk_Color := Color;
      C : System.Address := Col'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         C := System.Null_Address;
      end if;
      Internal (Get_Object (Plot_Layout), C);
   end Set_Background;

end Gtk.Extra.Plot_Layout;
