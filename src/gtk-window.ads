-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Bin;    use Gtk.Bin;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Window is

   type Gtk_Window is new Bin.Gtk_Bin with private;

   procedure Gtk_New (Window   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type);
   --  mapping: New gtkwindow.h gtk_window_new

   procedure Set_Title (Window : in out Gtk_Window'Class;
                        Title  : in String);
   --  mapping: Set_Title gtkwindow.h gtk_window_set_title

   procedure Set_Focus (Window : in out Gtk_Window'Class);
   --  mapping: Set_Focus gtkwindow.h gtk_window_set_focus

   procedure Set_Default (Window   : in out Gtk_Window'Class;
                          Defaultw : in     Widget.Gtk_Widget'Class);
   --  mapping: Set_Default gtkwindow.h gtk_window_set_default

   procedure Set_Policy (Window       : in out Gtk_Window'Class;
                         Allow_Shrink : in     Boolean;
                         Allow_Grow   : in     Boolean;
                         Auto_Shrink  : in     Boolean);
   --  mapping: Set_Policy gtkwindow.h gtk_window_set_policy

   procedure Position (Window   : in out Gtk_Window'Class;
                       Position : in     Enums.Gtk_Window_Position);
   --  mapping: Position gtkwindow.h gtk_window_position

   function Activate_Focus (Window : in Gtk_Window'Class) return Boolean;
   --  mapping: Activate_Focus gtkwindow.h gtk_window_activate_focus

   function Activate_Default (Window : in Gtk_Window'Class) return Boolean;
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_activate_default

private

   type Gtk_Window is new Bin.Gtk_Bin with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkwindow.h gtk_window_get_type
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_set_wmclass

   --  FIXME  --  need Gtk_Accelerator_Table
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_add_accelerator_table
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_remove_accelerator_table

end Gtk.Window;
