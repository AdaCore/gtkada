-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--
--  A Gtk_Vbutton_Box is a specific Gtk_Button_Box that organizes its
--  children vertically.
--  The beginning of the box (when you add children with Gtk.Box.Pack_Start)
--  is on the top of the box. Its end (for Gtk.Box.Pack_End) is on the bottom.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Glib;
with Gtk.Button_Box;

package Gtk.Vbutton_Box is

   type Gtk_Vbutton_Box_Record is
     new Gtk.Button_Box.Gtk_Button_Box_Record with private;
   type Gtk_Vbutton_Box is access all Gtk_Vbutton_Box_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Vbutton_Box);
   --  Create a new vertical button box.

   procedure Initialize (Widget : access Gtk_Vbutton_Box_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Vbutton_Box.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Vbutton_Box_Record is
     new Gtk.Button_Box.Gtk_Button_Box_Record with null record;

   pragma Import (C, Get_Type, "gtk_vbutton_box_get_type");
end Gtk.Vbutton_Box;
