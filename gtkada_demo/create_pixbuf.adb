------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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

with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Gtk.Frame;
with Gtk.Grid;      use Gtk.Grid;
with Gtk.Label;     use Gtk.Label;
with Gtk.Picture;   use Gtk.Picture;

package body Create_Pixbuf is

   function Help return String is
   begin
      return "@bGdk.Pixbuf@B stores and transforms image pixels. This demo"
        & " builds an image in memory and shows copies made by scaling,"
        & " flipping and rotating it."
        & ASCII.LF
        & "@bGtk.Picture@B displays each pixbuf through a @bGdk.Texture@B."
        & " File loading and PNG saving are also available in Gdk.Pixbuf.";
   end Help;

   procedure Run (F : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Grid   : constant Gtk_Grid := Gtk_Grid_New;
      Source : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 160, 100);
      Tile   : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 80, 50);

      procedure Show
        (Title : String; Image : Gdk_Pixbuf; Column, Row : Glib.Gint)
      is
         Picture : constant Gtk_Picture := Gtk_Picture_New_For_Pixbuf (Image);
      begin
         Picture.Set_Can_Shrink (False);
         Grid.Attach (Gtk_Label_New (Title), Column, Row * 2);
         Grid.Attach (Picture, Column, Row * 2 + 1);
         --  Gtk.Picture keeps the texture alive after the pixbuf is released.
         Unref (Image);
      end Show;
   begin
      F.Set_Label ("Pixbuf transformations");
      F.Set_Child (Grid);
      Grid.Set_Row_Spacing (12);
      Grid.Set_Column_Spacing (24);
      Grid.Set_Margin_Top (12);
      Grid.Set_Margin_Bottom (12);
      Grid.Set_Margin_Start (12);
      Grid.Set_Margin_End (12);

      Source.Fill (16#355C9FFF#);
      Tile.Fill (16#F6C344FF#);
      Tile.Copy_Area (0, 0, 80, 50, Source, 0, 0);
      Tile.Fill (16#D85C65FF#);
      Tile.Copy_Area (0, 0, 80, 50, Source, 80, 50);
      Unref (Tile);

      Show ("Original", Source.Copy, 0, 0);
      Show ("Scaled", Source.Scale_Simple (240, 150, Gdk_Interp_Bilinear),
            1, 0);
      Show ("Flipped", Source.Flip (True), 0, 1);
      Show ("Rotated", Source.Rotate_Simple (Gdk_Pixbuf_Rotate_Clockwise),
            1, 1);
      Unref (Source);
   end Run;

end Create_Pixbuf;
