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

with Gtk.Check_Button;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Radio_Button is

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with private;

   function Group (Button : in Gtk_Radio_Button) return Widget_SList.GSlist;
   --  mapping: Set_Group gtkradiobutton.h gtk_radio_button_group

   procedure Gtk_New (Button : out Gtk_Radio_Button;
                      Group  : in Widget_SList.GSlist);
   --  mapping: Gtk_New gtkradiobutton.h gtk_radio_button_new

   procedure Gtk_New (Button : out Gtk_Radio_Button;
                      Group  : in Widget_SList.GSlist;
                      Label  : in String);
   --  mapping: Gtk_New gtkradiobutton.h gtk_radio_button_new_with_label

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button);
   --  mapping: New_From_Widget gtkradiobutton.h \
   --  mapping:                 gtk_radio_button_new_from_widget

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button;
      Label  : in String);
   --  mapping: New_With_Label_From_Widget  gtkradiobutton.h \
   --  mapping: gtk_radio_button_new_with_label_from_widget

   procedure Set_Group (Button : in Gtk_Radio_Button;
                        Group  : in Widget_SList.GSlist);
   --  mapping: Set_Group gtkradiobutton.h gtk_radio_button_set_group

private

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with null record;

   --  mapping: NOT_IMPLEMENTED gtkradiobutton.h gtk_radio_button_get_type

end Gtk.Radio_Button;

