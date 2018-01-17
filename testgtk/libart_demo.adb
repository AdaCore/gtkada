------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Cairo;             use Cairo;
with Gdk.Cairo;         use Gdk.Cairo;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Label;         use Gtk.Label;
with Glib;              use Glib;
with Glib.Error;        use Glib.Error;
with Gtk.Box;           use Gtk.Box;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Widget;        use Gtk.Widget;

package body Libart_Demo is

   --------------------------
   --  The type below is a special drawing area that displays the
   --  associated image in it automatically, and destroys the image
   --  when the widget is destroyed.
   ---------------------------

   type Image_Drawing_Record is new Gtk.Box.Gtk_Box_Record with record
      Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Pix  : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;
   type Image_Drawing is access all Image_Drawing_Record'Class;
   --  A special type of drawing area that can be associated with
   --  an image.

   procedure Gtk_New
     (Draw   : out Image_Drawing;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Title  : String);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGdk_Pixbuf@B represents an image, normally in RGB or RGBA format."
        & " Pixbufs are normally used to load files from disk and perform"
        & " image scaling.";
   end Help;

   ------------------------
   -- Callbacks packages --
   ------------------------

   package Expose_Cb is new Gtk.Handlers.Return_Callback
     (Image_Drawing_Record, Boolean);

   package Destroy_Cb is new Gtk.Handlers.Callback (Image_Drawing_Record);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Draw : access Image_Drawing_Record'Class) is
   begin
      --  Destroy the associated image
      Unref (Draw.Pix);
   end Destroy;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
      (Draw : access Image_Drawing_Record'Class;
       Cr   : Cairo_Context) return Boolean is
   begin
      Set_Source_Pixbuf (Cr, Draw.Pix, 0.0, 0.0);
      Cairo.Paint (Cr);
      return False;
   end On_Draw;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Draw   : out Image_Drawing;
      Pixbuf : Gdk_Pixbuf;
      Title  : String)
   is
      Label : Gtk_Label;
   begin
      Draw := new Image_Drawing_Record;
      Initialize_Vbox (Draw, Homogeneous => False, Spacing => 0);

      Gtk_New (Label, Title);
      Pack_Start (Draw, Label, Expand => False, Fill => False);

      Draw.Pix := Pixbuf;
      Set_Size_Request
        (Draw,
         Get_Width (Draw.Pix),
         Get_Height (Draw.Pix) + Get_Allocated_Height (Label));

      Gtk_New (Draw.Area);
      Pack_Start (Draw, Draw.Area);

      Expose_Cb.Object_Connect
        (Draw.Area, Signal_Draw,
         Expose_Cb.To_Marshaller (On_Draw'Access),
         Slot_Object => Draw);
      Destroy_Cb.Connect
        (Draw, "destroy",
         Destroy_Cb.To_Marshaller (Destroy'Access));
   end Gtk_New;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      VBox            : Gtk_Box;
      Hbox            : Gtk_Box;
      Pix, Pix2, Pix3 : Gdk_Pixbuf;
      Draw            : Image_Drawing;
      Label           : Gtk_Label;
      Error           : Glib.Error.GError;

   begin
      Gtk_New_Vbox (VBox, Homogeneous => False, Spacing => 0);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 0);
      Pack_Start (VBox, Hbox);

      --  Creating the images.

      Gdk_New_From_File (Pix, "alps.png", Error);

      if Pix = Null_Pixbuf then
         Gtk_New (Label, "Pixmaps not found. Please run testgtk from the"
               & " testgtk/ directory itself.");
         Add (Frame, Label);
         Show_All (Frame);
         return;
      end if;

      Pix2 := Scale_Simple
        (Pix,
         Gint (550) - Get_Width (Pix),
         Get_Height (Pix) / 2);
      Gdk_New_From_File (Pix3, "lightning.png", Error);

      if Pix3 = Null_Pixbuf then
         Gtk_New (Label, "Pixmaps not found. Please run testgtk from the"
               & " testgtk/ directory itself.");
         Add (Frame, Label);
         Show_All (Frame);
         return;
      end if;

      Composite
        (Src           => Pix,
         Dest          => Pix3,
         Dest_X        => 0,
         Dest_Y        => 0,
         Scale_X       => 0.5,
         Scale_Y       => 0.5,
         Dest_Width    => Get_Width (Pix3),
         Dest_Height   => Get_Height (Pix3),
         Overall_Alpha => 128);

      --  Creating the canvases
      Add (Frame, VBox);

      Gtk_New (Draw, Pix, "Initial Image");
      Pack_Start (Hbox, Draw, Expand => False, Fill => True);

      Gtk_New (Draw, Pix2, "Scaled Image");
      Pack_Start (Hbox, Draw, Expand => False, Fill => True);

      Gtk_New (Draw, Pix3, "Composite Image with Opacity"
               & ASCII.LF
               & "This image is the addition of two simpler images");
      Pack_Start (VBox, Draw);

      Show_All (Frame);
   end Run;

end Libart_Demo;
