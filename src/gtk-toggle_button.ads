-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------


with Gtk.Button;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button is new Gtk.Button.Gtk_Button with private;

   function Is_Active (Widget : in Gtk_Toggle_Button'Class)
                        return      Boolean;
   procedure Gtk_New (Widget : out Gtk_Toggle_Button);
   procedure Gtk_New (Widget : out Gtk_Toggle_Button;
                      Label  : in String);
   procedure Set_Mode
      (Toggle_Button  : in Gtk_Toggle_Button'Class;
       Draw_Indicator : in Gint);
   procedure Set_State
      (Toggle_Button : in Gtk_Toggle_Button'Class;
       Active        : in Boolean);
   procedure Toggled (Toggle_Button : in Gtk_Toggle_Button'Class);

private
   type Gtk_Toggle_Button is new Gtk.Button.Gtk_Button with null record;

   --  mapping: Is_Active gtktogglebutton.h GtkToggleButton->active
   --  mapping: NOT_IMPLEMENTED gtktogglebutton.h gtk_toggle_button_get_type
   --  mapping: Gtk_New gtktogglebutton.h gtk_toggle_button_new
   --  mapping: Gtk_New gtktogglebutton.h gtk_toggle_button_new_with_label
   --  mapping: Set_Mode gtktogglebutton.h gtk_toggle_button_set_mode
   --  mapping: Set_State gtktogglebutton.h gtk_toggle_button_set_state
   --  mapping: Toggled gtktogglebutton.h gtk_toggle_button_toggled
end Gtk.Toggle_Button;
