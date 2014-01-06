------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with Gdk.Color;           use Gdk.Color;
with Gtk.Extra.Plot;      use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data; use Gtk.Extra.Plot_Data;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Canvas.Rectangle is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Canvas_Rectangle_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child       : out Gtk_Plot_Canvas_Rectangle;
      Style       : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Line_Width  : Gfloat;
      Fg          : Gdk.Color.Gdk_Color;
      Bg          : Gdk.Color.Gdk_Color;
      Border      : Gtk.Extra.Plot.Plot_Border_Style;
      Fill        : Boolean)
   is
      function Internal
        (Style  : Plot_Line_Style;
         Width  : Gfloat;
         Fg     : Gdk_Color;
         Bg     : Gdk_Color;
         Border : Plot_Border_Style;
         Fill   : Gboolean)
         return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_rectangle_new");
   begin
      Child := new Gtk_Plot_Canvas_Rectangle_Record;
      Set_Object (Child, Internal (Style, Line_Width, Fg, Bg, Border,
                                   Boolean'Pos (Fill)));
   end Gtk_New;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Rectangle : access Gtk_Plot_Canvas_Rectangle_Record;
      Style     : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width     : Gfloat;
      Fg        : Gdk.Color.Gdk_Color;
      Bg        : Gdk.Color.Gdk_Color;
      Border    : Gtk.Extra.Plot.Plot_Border_Style;
      Fill      : Boolean)
   is
      procedure Internal
        (Rectangle : System.Address;
         Style     : Plot_Line_Style;
         Width     : Gfloat;
         Fg        : Gdk_Color;
         Bg        : Gdk_Color;
         Border    : Plot_Border_Style;
         Fill      : Gboolean);
      pragma Import (C, Internal, "gtk_plot_canvas_rectangle_set_attributes");
   begin
      Internal (Get_Object (Rectangle), Style, Width, Fg, Bg, Border,
                Boolean'Pos (Fill));
   end Set_Attributes;

end Gtk.Extra.Plot_Canvas.Rectangle;
