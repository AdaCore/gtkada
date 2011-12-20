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

with Gtk.Widget, Gtk.Box, Gtk.Toggle_Button, Gtk.Handlers;
with Glib;

package Tictactoe is

   type Button_Record is record
      Button : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Id : Gtk.Handlers.Handler_Id;
   end record;
   --  Hold a check item button and the handler id of a connection to the
   --  "toggled" signal.

   type Gtk_Tictactoe_Buttons is array
     (Glib.Guint range 1 .. 3, Glib.Guint range 1 .. 3) of
      Button_Record;

   type Gtk_Tictactoe_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Buttons : Gtk_Tictactoe_Buttons;
   end record;
   type Gtk_Tictactoe is access all Gtk_Tictactoe_Record'Class;

   --  Primitive functions

   procedure Gtk_New (Tictactoe : out Gtk_Tictactoe);
   procedure Initialize (Tictactoe : access Gtk_Tictactoe_Record);
   procedure Clear (Tictactoe : access Gtk_Tictactoe_Record);

   --  Callbacks

   procedure Win (Tictactoe : access Gtk_Tictactoe_Record'Class);
   procedure Quit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   --  Signal handling

   package Tictactoe_Cb is new Gtk.Handlers.Callback (Gtk_Tictactoe_Record);

   package Widget_Cb is new Gtk.Handlers.Callback
     (Gtk.Widget.Gtk_Widget_Record);
end Tictactoe;
