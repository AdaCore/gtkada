------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

with Gtkada.Bindings;      use Gtkada.Bindings;
with Gdk.Color;            use Gdk.Color;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package body Gtk.Extra.Plot_Canvas.Text is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Text;
      Text     : String;
      Font     : String := "";
      Height   : Gint := 0;
      Angle    : Gtk.Extra.Plot_Data.Plot_Angle := Gtk.Extra.Plot_Data.Angle_0;
      Fg       : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean := True;
      Justification : Gtk.Enums.Gtk_Justification := Gtk.Enums.Justify_Center)
   is
      function Internal
        (Font : chars_ptr; Height : Gint; Angle : Plot_Angle;
         Fg, Bg : System.Address;
         Transparent : Gboolean;
         Justification : Gtk_Justification;
         Text : String) return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_text_new");

      F1   : aliased Gdk_Color := Fg;
      B1   : aliased Gdk_Color := Bg;
      F, B : System.Address := System.Null_Address;
      T    : chars_ptr := String_Or_Null (Font);

   begin
      if Fg /= Null_Color then
         F := F1'Address;
      end if;
      if Bg /= Null_Color then
         B := B1'Address;
      end if;

      Child := new Gtk_Plot_Canvas_Text_Record;
      Set_Object
        (Child, Internal
           (T, Height, Angle, F, B,
            Boolean'Pos (Transparent), Justification,
            Text & ASCII.NUL));
      Free (T);
   end Gtk_New;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Child         : access Gtk_Plot_Canvas_Text_Record;
      Font          : String := "";
      Height        : Gint;
      Angle         : Gtk.Extra.Plot_Data.Plot_Angle;
      Fg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification;
      Text          : String)
   is
      procedure Internal
        (Child         : System.Address;
         Font          : chars_ptr;
         Height        : Gint;
         Angle         : Plot_Angle;
         Fg, Bg        : System.Address;
         Transparent   : Gboolean;
         Justification : Gtk_Justification;
         Text          : String);
      pragma Import (C, Internal, "gtk_plot_canvas_text_set_attributes");
      F1   : aliased Gdk_Color := Fg;
      B1   : aliased Gdk_Color := Bg;
      F, B : System.Address := System.Null_Address;
      T    : chars_ptr := String_Or_Null (Font);
   begin
      if Fg /= Null_Color then
         F := F1'Address;
      end if;
      if Bg /= Null_Color then
         B := B1'Address;
      end if;
      Internal (Get_Object (Child), T, Height, Angle, F, B,
                Boolean'Pos (Transparent), Justification, Text & ASCII.NUL);
      Free (T);
   end Set_Attributes;

end Gtk.Extra.Plot_Canvas.Text;
