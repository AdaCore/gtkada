------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Glib.Object; use Glib.Object;
with System; use System;

package body Gdk.Cairo is

   ------------
   -- Create --
   ------------

   function Create (Window : Gdk_Window) return Cairo_Context is
      function Internal (S : Gdk_Window) return Cairo_Context;
      pragma Import (C, Internal, "gdk_cairo_create");

      Cr : Cairo_Context;
   begin
      Cr := Internal (Window);
      return Cr;
   end Create;

   -----------------------
   -- Set_Source_Pixbuf --
   -----------------------

   procedure Set_Source_Pixbuf
     (Cr       : Cairo_Context;
      Pixbuf   : Gdk_Pixbuf;
      Pixbuf_X : Gdouble;
      Pixbuf_Y : Gdouble)
   is
      procedure Internal (Cr     : Cairo_Context;
                          Pixbuf : System.Address;
                          Pixbuf_X, Pixbuf_Y : Gdouble);
      pragma Import (C, Internal, "gdk_cairo_set_source_pixbuf");
   begin
      Internal (Cr, Get_Object (Pixbuf), Pixbuf_X, Pixbuf_Y);
   end Set_Source_Pixbuf;

   ------------------------
   -- Create_From_Pixbuf --
   ------------------------

   function Create_From_Pixbuf
      (Pixbuf  : Gdk_Pixbuf;
       Scale   : Gint;
       For_Window : Gdk.Gdk_Window := null) return Cairo_Surface
   is
      function Internal (Pixbuf  : System.Address;
                         Scale   : Gint;
                         Win     : Gdk_Window) return Cairo_Surface;
      pragma Import (C, Internal, "gdk_cairo_surface_create_from_pixbuf");
   begin
      return Internal (Get_Object (Pixbuf), Scale, For_Window);
   end Create_From_Pixbuf;

   ---------------------
   -- Set_Source_RGBA --
   ---------------------

   procedure Set_Source_RGBA
     (Cr       : Cairo_Context;
      Color    : Gdk.RGBA.Gdk_RGBA) is
   begin
      Set_Source_Rgba (Cr, Color.Red, Color.Green, Color.Blue, Color.Alpha);
   end Set_Source_RGBA;

end Gdk.Cairo;
