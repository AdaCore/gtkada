-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Gdk.Cursor is

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Widget      : out Gdk_Cursor;
                      Cursor_Type : in Gdk.Types.Gdk_Cursor_Type)
   is
      function Internal (Cursor_Type : in Gdk.Types.Gdk_Cursor_Type)
                         return Gdk_Cursor;
      pragma Import (C, Internal, "ada_gdk_cursor_new");
   begin
      Widget := Internal (Cursor_Type);
   end Gdk_New;

   procedure Gdk_New
      (Widget : out Gdk_Cursor;
       Source : in Gdk.Gdk_Pixmap;
       Mask   : in Gdk.Gdk_Pixmap;
       Fg     : in Gdk.Color.Gdk_Color;
       Bg     : in Gdk.Color.Gdk_Color;
       X      : in Glib.Gint;
       Y      : in Glib.Gint)
   is
      function Internal
         (Source : in Gdk.Gdk_Pixmap;
          Mask   : in Gdk.Gdk_Pixmap;
          Fg     : in System.Address;
          Bg     : in System.Address;
          X      : in Glib.Gint;
          Y      : in Glib.Gint)
          return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_pixmap");

      Col_Fg : aliased Gdk.Color.Gdk_Color := Fg;
      Col_Bg : aliased Gdk.Color.Gdk_Color := Bg;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Widget := Internal
        (Source, Mask, Col_Fg'Address,  Col_Bg'Address, X, Y);
   end Gdk_New;

end Gdk.Cursor;
