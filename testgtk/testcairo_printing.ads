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

with Glib;  use Glib;

with Gtkada.Printing;        use Gtkada.Printing;
with Gtk.Print_Context;      use Gtk.Print_Context;

with Gtk.Widget;             use Gtk.Widget;

with Testcairo_Drawing;      use Testcairo_Drawing;

package Testcairo_Printing is

   type Testcairo_Print_Operation_Record is new Gtkada_Print_Operation_Record
   with record
      Test   : Test_Type;
      Win    : Gtk_Widget;
   end record;
   type Testcairo_Print_Operation is access all
     Testcairo_Print_Operation_Record'Class;

   procedure Draw_Page
     (Op          : access Testcairo_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint);
   --  Handler responsible for printing pages

end Testcairo_Printing;
