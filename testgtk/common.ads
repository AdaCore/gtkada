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
-- General Public License for more details.                          --
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

with Gtk; use Gtk;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Signal;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

Package Common is

    --  This package is created to avoid the instanciation of the
    --  generic packages for callbacks. This provides a much smaller
    --  executable

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget_Record);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget_Record, Gtk_Widget);
   package Widget3_Cb is new Signal.Void_Callback (Gtk_Widget_Record);
   package Label_Cb is new Signal.Object_Callback (Gtk_Label_Record);
   package Adj_Cb is new Signal.Void_Callback (Gtk_Adjustment_Record);
   package Check_Cb is new Signal.Void_Callback (Gtk_Check_Button_Record);


   type Gtk_Window_Access is access all Gtk_Window;
   package Destroy_Cb is new Signal.Callback (Gtk_Window_Record,
                                              Gtk_Window_Access);
   procedure Destroy_Window (Win : access Gtk.Window.Gtk_Window_Record;
                             Ptr : in Gtk_Window_Access);
end Common;

