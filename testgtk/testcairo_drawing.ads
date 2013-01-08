------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2013, AdaCore                     --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Cairo; use Cairo;
with Gtk.Button; use Gtk.Button;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

package Testcairo_Drawing is

   type Test_Type is (Rectangles, Transparency, Operators, Matrix,
                      Transformations, Paths, Patterns, Clip_And_Paint,
                      Surface_And_Png, Toy_Text, Pango_Text, Image);
   --  The supported drawing types

   package Window_Cb is new Gtk.Handlers.Return_Callback
      (Gtk_Window_Record, Boolean);

   type Doc_Array is array (Test_Type) of Unbounded_String;
   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   package Button_Cb is new Gtk.Handlers.Callback
     (Gtk_Button_Record);

   package Selection_Cb is new Gtk.Handlers.Callback
     (Gtk_Tree_Selection_Record);

   procedure Draw_On_Context
     (Cr : Cairo_Context;
      Win  : Gtk_Widget;
      Test : Test_Type);
   --  Draw Test on Cr.
   --  Win is a widget used as base for creating Pango layouts.

end Testcairo_Drawing;
