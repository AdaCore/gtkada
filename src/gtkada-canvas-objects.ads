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

--  Various support utilities for the drawing of objects in GtkAda.Canvas

with Gtkada.Style;       use Gtkada.Style;

package Gtkada.Canvas.Objects is

   function Link_Anchor_Point
     (Self   : not null access Canvas_Item_Record'Class;
      Anchor : Anchor_Attachment)
      return Point;
   --  Return the anchor point for links to or from this object. In general,
   --  this anchor point is in the middle of the object or depends on the
   --  Anchor parameter, and the link will automatically be clipped to one
   --  of the borders. The coordinates are absolute.
   --  This anchor point can be in the middle of an object, the link itself
   --  will be clipped to the border of the bounding box.

end Gtkada.Canvas.Objects;
