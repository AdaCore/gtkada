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

--  <description>
--  A special kind of child that can be put in a Gtk_Plot_Canvas.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>

with Gdk.Bitmap;           use Gdk.Bitmap;
with Gdk.Pixmap;           use Gdk.Pixmap;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Canvas.Pixmap is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Canvas_Pixmap_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Pixmap;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
        (Pixmap : Gdk.Pixmap.Gdk_Pixmap; Mask : Gdk.Bitmap.Gdk_Bitmap)
         return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_pixmap_new");
   begin
      Child := new Gtk_Plot_Canvas_Pixmap_Record;
      Set_Object (Child, Internal (Pixmap, Mask));
   end Gtk_New;

end Gtk.Extra.Plot_Canvas.Pixmap;
