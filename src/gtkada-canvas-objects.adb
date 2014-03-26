------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

with Cairo;       use Cairo;
with Glib;        use Glib;

package body Gtkada.Canvas.Objects is

   -----------------------
   -- Link_Anchor_Point --
   -----------------------

   function Link_Anchor_Point
     (Self   : not null access Canvas_Item_Record'Class;
      Anchor : Anchor_Attachment)
      return Point
   is
      Abs_Coord : constant Cairo_Rectangle := Self.Get_Coord;
   begin
      case Anchor.Side is
         when Auto =>
            return (Abs_Coord.X + Abs_Coord.Width * Anchor.X,
                    Abs_Coord.Y + Abs_Coord.Height * Anchor.Y);

         when Top =>
            return (Abs_Coord.X + Abs_Coord.Width * Anchor.X,
                    Abs_Coord.Y);

         when Right =>
            return (Abs_Coord.X + Abs_Coord.Width,
                    Abs_Coord.Y + Abs_Coord.Height * Anchor.Y);

         when Bottom =>
            return (Abs_Coord.X + Abs_Coord.Width * Anchor.X,
                    Abs_Coord.Y + Abs_Coord.Height);

         when Left =>
            return (Abs_Coord.X,
                    Abs_Coord.Y + Abs_Coord.Height * Anchor.Y);
      end case;
   end Link_Anchor_Point;

end Gtkada.Canvas.Objects;
