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

with Gnome.Dialog;
with Gtk;
with Gtkada.Types; use Gtkada.Types;

package Gnome.Message_Box is

   type Gnome_Message_Box_Record is new
     Gnome.Dialog.Gnome_Dialog_Record with private;
   type Gnome_Message_Box is access all Gnome_Message_Box_Record'Class;

   --  gnome_message_box_new not bound: variable number of arguments

   procedure Gnome_New
     (Widget          : out Gnome_Message_Box;
      Message         : String;
      Messagebox_Type : String;
      Buttons         : Chars_Ptr_Array);

   procedure Initialize
     (Widget          : access Gnome_Message_Box_Record'Class;
      Message         : String;
      Messagebox_Type : String;
      Buttons         : Chars_Ptr_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Message_Box_Record is new
     Gnome.Dialog.Gnome_Dialog_Record with null record;

   pragma Import (C, Get_Type, "gnome_message_box_get_type");
end Gnome.Message_Box;
