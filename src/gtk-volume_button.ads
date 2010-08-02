-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

--  <description>
--  Gtk_Volume_Button is a subclass of Gtk_Scale_Button that has been tailored
--  for use as a volume control widget with suitable icons, tooltips and
--  accessible labels.
--  </description>
--  <c_version>2.16.6</c_version>

with Gtk.Scale_Button;

package Gtk.Volume_Button is

   type Gtk_Volume_Button_Record is
     new Gtk.Scale_Button.Gtk_Scale_Button_Record with private;
   type Gtk_Volume_Button is access all Gtk_Volume_Button_Record'Class;

   function Get_Type return GType;

   procedure Gtk_New (Widget : out Gtk_Volume_Button);
   procedure Initialize (Widget : access Gtk_Volume_Button_Record'Class);
   --  Create a new Gtk_Volume_Button widget.

private

   type Gtk_Volume_Button_Record is
     new Gtk.Scale_Button.Gtk_Scale_Button_Record with null record;

   pragma Import (C, Get_Type, "gtk_volume_button_get_type");

end Gtk.Volume_Button;
