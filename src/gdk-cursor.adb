------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

with System;

package body Gdk.Cursor is

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Widget      : out Gdk_Cursor;
      Cursor_Type : Gdk_Cursor_Type)
   is
      function Internal (Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
      pragma Import (C, Internal, "ada_gdk_cursor_new");

   begin
      Widget := Internal (Cursor_Type);
   end Gdk_New;

   procedure Gdk_New
     (Widget : out Gdk_Cursor;
      Source : Gdk.Gdk_Pixmap;
      Mask   : Gdk.Gdk_Pixmap;
      Fg     : Gdk.Color.Gdk_Color;
      Bg     : Gdk.Color.Gdk_Color;
      X      : Glib.Gint;
      Y      : in Glib.Gint)
   is
      function Internal
        (Source : Gdk.Gdk_Pixmap;
         Mask   : Gdk.Gdk_Pixmap;
         Fg     : System.Address;
         Bg     : System.Address;
         X      : Glib.Gint;
         Y      : Glib.Gint) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_pixmap");

      Col_Fg : aliased Gdk.Color.Gdk_Color := Fg;
      Col_Bg : aliased Gdk.Color.Gdk_Color := Bg;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Widget := Internal
        (Source, Mask, Col_Fg'Address,  Col_Bg'Address, X, Y);
   end Gdk_New;

   procedure Gdk_New
     (Cursor  : out Gdk_Cursor;
      Name    : String)
   is
      function Internal (Name : String) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_name");
   begin
      Cursor := Internal (Name & ASCII.NUL);
   end Gdk_New;
end Gdk.Cursor;
