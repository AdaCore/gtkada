------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

with Glib;        use Glib;

package body Gtkada.Canvas_View.Objects is

   -----------------------
   -- Link_Anchor_Point --
   -----------------------

   function Link_Anchor_Point
     (Self   : not null access Abstract_Item_Record'Class;
      Anchor : Anchor_Attachment)
      return Item_Point
   is
      C : constant Item_Rectangle := Self.Bounding_Box;
   begin
      case Anchor.Toplevel_Side is
         when Auto | No_Clipping =>
            return (C.Width * abs (Anchor.X), C.Height * abs (Anchor.Y));

         when Top =>
            return (C.Width * abs (Anchor.X), 0.0);

         when Right =>
            return (C.Width, C.Height * abs (Anchor.Y));

         when Bottom =>
            return (C.Width * abs (Anchor.X), C.Height);

         when Left =>
            return (0.0, C.Height * abs (Anchor.Y));
      end case;
   end Link_Anchor_Point;

   --------------
   -- Toplevel --
   --------------

   function Toplevel
     (Self : not null access Abstract_Item_Record'Class)
      return Abstract_Item
   is
      Result : Abstract_Item := Abstract_Item (Self);
      P : Abstract_Item := Result.Parent;
   begin
      while P /= null loop
         Result := P;
         P := Result.Parent;
      end loop;
      return Result;
   end Toplevel;

end Gtkada.Canvas_View.Objects;
