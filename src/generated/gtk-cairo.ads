------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

--  <group>Drawing</group>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;      use Cairo;
with Gdk;        use Gdk;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Cairo is
   ---------------
   -- Functions --
   ---------------

   procedure Transform_To_Window
      (Cr     : Cairo.Cairo_Context;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Window : Gdk.Gdk_Window);
   --  Transforms the given cairo context Cr that from Widget-relative
   --  coordinates to Window-relative coordinates. If the Widget's window is
   --  not an ancestor of Window, no modification will be applied.
   --  This is the inverse to the transformation GTK applies when preparing an
   --  expose event to be emitted with the Gtk.Widget.Gtk_Widget::draw signal.
   --  It is intended to help porting multiwindow widgets from GTK+ 2 to the
   --  rendering architecture of GTK+ 3.
   --  Since: gtk+ 3.0
   --  "cr": the cairo context to transform
   --  "widget": the widget the context is currently centered for
   --  "window": the window to transform the context to

   function Should_Draw_Window
      (Cr     : Cairo.Cairo_Context;
       Window : Gdk.Gdk_Window) return Boolean;
   --  This function is supposed to be called in Gtk.Widget.Gtk_Widget::draw
   --  implementations for widgets that support multiple windows. Cr must be
   --  untransformed from invoking of the draw function. This function will
   --  return True if the contents of the given Window are supposed to be drawn
   --  and False otherwise. Note that when the drawing was not initiated by the
   --  windowing system this function will return True for all windows, so you
   --  need to draw the bottommost window first. Also, do not use "else if"
   --  statements to check which window should be drawn.
   --  Since: gtk+ 3.0
   --  "cr": a cairo context
   --  "window": the window to check. Window may not be an input-only window.

end Gtk.Cairo;
