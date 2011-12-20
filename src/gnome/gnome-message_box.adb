------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk; use Gtk;
with System;

package body Gnome.Message_Box is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget          : out Gnome_Message_Box;
      Message         : String;
      Messagebox_Type : String;
      Buttons         : Chars_Ptr_Array)
   is
   begin
      Widget := new Gnome_Message_Box_Record;
      Initialize (Widget, Message, Messagebox_Type, Buttons);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget          : access Gnome_Message_Box_Record'Class;
      Message         : String;
      Messagebox_Type : String;
      Buttons         : Chars_Ptr_Array)
   is
      function Internal
        (Message         : String;
         Messagebox_Type : String;
         Buttons         : Chars_Ptr_Array)
         return System.Address;
      pragma Import (C, Internal, "gnome_message_box_newv");
   begin
      Set_Object (Widget, Internal (Message & ASCII.NUL,
                                    Messagebox_Type & ASCII.NUL,
                                    Buttons + Null_Ptr));
   end Initialize;

end Gnome.Message_Box;
