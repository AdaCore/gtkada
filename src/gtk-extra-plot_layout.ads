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

--  <description>
--  This widget provides a simple container for a Gtk_Plot widget.
--  This container can have multiple plots, displayed at various locations.
--  It can also contain any text.
--
--  It also provides a high-level interface to convert from relative
--  coordinates (a percentage between 0.0 and 1.0, that goes from left to
--  right or top to bottom) to absolute coordinates within the widget.
--
--  These relative coordinates makes it really easy to handle the resizing
--  of this container.
--  </description>
--  <c_version>gtk+extra 0.99</c_version>

with Gdk.Color;
with Gtk.Enums;
with Gtk.Layout;
with Gtk.Extra.Plot;

package Gtk.Extra.Plot_Layout is

   type Gtk_Plot_Layout_Record is new Gtk.Layout.Gtk_Layout_Record
     with private;
   type Gtk_Plot_Layout is access all Gtk_Plot_Layout_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Plot_Layout;
                      Width  : in Gint;
                      Height : in Gint);
   --  Create a new layout, with a known initial size.
   --  Initially, this widget is empty, and will simply modify its background
   --  color, depending on the choice made by Set_Background.

   procedure Initialize (Widget : access Gtk_Plot_Layout_Record;
                         Width  : in Gint;
                         Height : in Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Layout.

   procedure Add_Plot
     (Plot_Layout : access Gtk_Plot_Layout_Record;
      Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
      X           : in Gint;
      Y           : in Gint);
   --  Add a new plot to the layout.
   --  This plot is displayed at the absolute coordinates (X, Y) in the layout.

   procedure Set_Background (Plot_Layout : access Gtk_Plot_Layout_Record;
                             Color       : in Gdk.Color.Gdk_Color);
   --  Change the background color of the layout

   procedure Refresh (Layout : access Gtk_Plot_Layout_Record);
   --  Force a refresh of the layout and all its children.

   procedure Put_Text (Layout        : access Gtk_Plot_Layout_Record;
                       X             : in Gdouble;
                       Y             : in Gdouble;
                       Angle         : in Gint;
                       Ps_Font       : in String;
                       Height        : in Gint;
                       Fg            : in Gdk.Color.Gdk_Color;
                       Bg            : in Gdk.Color.Gdk_Color;
                       Justification : in Gtk.Enums.Gtk_Justification;
                       Text          : in String);
   --  Put an arbitrary text in the layout.
   --  Ps_Font should be the name of a postscript font.
   --  (X, Y) are the relative coordinates to which the text should be drawn.
   --  The only legal values for Angle are 0, 90, 180 and 270 degres.

   procedure Get_Pixel (Plot_Layout : access Gtk_Plot_Layout_Record;
                        Px          : in Gdouble;
                        Py          : in Gdouble;
                        X           : out Gint;
                        Y           : out Gint);
   --  Convert from relative coordinates to absolute ones.

   procedure Get_Position (Plot_Layout : access Gtk_Plot_Layout_Record;
                           X           : in Gint;
                           Y           : in Gint;
                           Px          : out Gdouble;
                           Py          : out Gdouble);
   --  Convert from absolute coordinates to relative ones.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Plot_Layout_Record is new Gtk.Layout.Gtk_Layout_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot_layout_get_type");
end Gtk.Extra.Plot_Layout;
