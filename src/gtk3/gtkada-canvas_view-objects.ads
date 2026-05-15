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

--  Various support utilities for the drawing of objects in GtkAda.Canvas

package Gtkada.Canvas_View.Objects is

   function Toplevel
     (Self : not null access Abstract_Item_Record'Class)
      return Abstract_Item;
   --  Return the top-most container for Self, or null

   function Link_Anchor_Point
     (Self   : not null access Abstract_Item_Record'Class;
      Anchor : Anchor_Attachment)
      return Item_Point;
   --  Implementation helper for an object's Link_Anchor_Point primitive.
   --  This uses the bounding-box of the object to compute the middle point,
   --  which works fine on most cases, except for objects where link
   --  attachment is constrained to a small section of the object.

end Gtkada.Canvas_View.Objects;
