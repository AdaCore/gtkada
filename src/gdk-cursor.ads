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

with Glib;
with Gdk.Types;
with Gdk.Color;

package Gdk.Cursor is

   type Gdk_Cursor is new Gdk.C_Proxy;
   Null_Cursor : constant Gdk_Cursor;

   procedure Gdk_New (Widget      : out Gdk_Cursor;
                      Cursor_Type : in  Gdk.Types.Gdk_Cursor_Type);

   procedure Gdk_New
     (Widget : out Gdk_Cursor;
      Source : in Gdk.Gdk_Pixmap;
      Mask   : in Gdk.Gdk_Pixmap;
      Fg     : in Gdk.Color.Gdk_Color;
      Bg     : in Gdk.Color.Gdk_Color;
      X      : in Glib.Gint;
      Y      : in Glib.Gint);

   procedure Destroy (Cursor : in out Gdk_Cursor);

private
   Null_Cursor : constant Gdk_Cursor := null;
   pragma Import (C, Destroy, "gdk_cursor_destroy");
end Gdk.Cursor;
