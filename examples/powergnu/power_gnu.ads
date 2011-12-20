------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Gdk.Pixbuf;
with Gtk.Drawing_Area;

package Power_GNU is

   --------------------------
   --  The type below is a special drawing area that displays the
   --  associated image in it automatically, and destroys the image
   --  when the widget is destroyed.
   ---------------------------

   type Image_Drawing_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with private;
   type Image_Drawing is access all Image_Drawing_Record'Class;
   --  A special type of drawing area that can be associated with
   --  an image.

   procedure Gtk_New
     (Draw : out Image_Drawing);
   --  Create a new Image

   procedure Initialize
     (Draw : access Image_Drawing_Record'Class);

   procedure Set_Image
     (Draw  : Image_Drawing;
      Image : String);
   --  Set the current image to display in Draw
   --  Image is the file name of the image
   --  The image will be resized to the current size of Draw

private
   type Image_Drawing_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Orig : Gdk.Pixbuf.Gdk_Pixbuf;  --  The image loaded from disk
      Pix  : Gdk.Pixbuf.Gdk_Pixbuf;  --  The currently displayed (scaled) image
   end record;
end Power_GNU;
