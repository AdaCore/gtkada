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

with Gtk.Container;

package Gtk.Button is

   type Gtk_Button is new Container.Gtk_Container with private;


   procedure Gtk_New (Widget : out Gtk_Button);
   --  mapping: Create_New gtkbutton.h gtk_button_new

   procedure Gtk_New (Widget      : out Gtk_Button;
                      Label  : in String);
   --  mapping: Create_New gtkbutton.h gtk_button_new_with_label

   ---------------
   --  Signals  --
   ---------------

   procedure Pressed (Widget : in Gtk_Button'Class);
   --  mapping: Pressed gtkbutton.h gtk_button_pressed

   procedure Released (Widget : in Gtk_Button'Class);
   --  mapping: Released gtkbutton.h gtk_button_released

   procedure Clicked (Widget : in Gtk_Button'Class);
   --  mapping: Clicked gtkbutton.h gtk_button_clicked

   procedure Enter (Widget : in Gtk_Button'Class);
   --  mapping: Enter gtkbutton.h gtk_button_enter

   procedure Leave (Widget : in Gtk_Button'Class);
   --  mapping: Leave gtkbutton.h gtk_button_leave

private

   type Gtk_Button is new Container.Gtk_Container with null record;

   --  services NOT mapped...
   --

   --  mapping: USE_OBJECT_ORIENTED gtkbutton.h gtk_button_get_type

end Gtk.Button;
