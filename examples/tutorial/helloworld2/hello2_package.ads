------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Gdk.Event; use Gdk.Event;
with Gtk.Widget, Gtk.Handlers; use Gtk.Widget, Gtk.Handlers;

package Hello2_Package is

   type String_Access is access all String;

   package Handlers is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type   => String_Access);

   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record,
      Return_Type => Boolean);

   procedure Hello_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Data   : String_Access);

   function Delete_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

end Hello2_Package;
