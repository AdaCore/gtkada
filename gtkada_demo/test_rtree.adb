------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Ada.Text_IO;               use Ada.Text_IO;
with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Rtrees; use Gtkada.Canvas_View.Rtrees;
with Gtkada.Style;              use Gtkada.Style;

procedure Test_Rtree is
   R     : Rtree (Min_Children => 2, Max_Children => 2);
   Item  : Rect_Item;
   Style : constant Drawing_Style := Gtk_New;
   Context : Draw_Context;
begin
   Put_Line ("Empty Rtree");
   Dump_Debug (R);

   Put_Line ("One child");
   Item := Gtk_New_Rect (Style, Width => 20.0, Height => 20.0);
   Item.Set_Position ((10.0, 10.0));
   Item.Size_Request (Context);
   R.Insert (Item);
   Dump_Debug (R);

   Put_Line ("Two children");
   Item := Gtk_New_Rect (Style, Width => 20.0, Height => 20.0);
   Item.Set_Position ((20.0, 20.0));
   Item.Size_Request (Context);
   R.Insert (Item);
   Dump_Debug (R);

   Put_Line ("Three children");
   Item := Gtk_New_Rect (Style, Width => 20.0, Height => 20.0);
   Item.Set_Position ((20.0, 10.0));
   Item.Size_Request (Context);
   R.Insert (Item);
   Dump_Debug (R);

   R.Clear;
   Dump_Debug (R);

end Test_Rtree;
