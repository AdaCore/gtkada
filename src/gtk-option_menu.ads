-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  A GtkOptionMenu is a widget that allows the user to choose from a list of
--  valid choices. The GtkOptionMenu displays the selected choice. When
--  activated, the GtkOptionMenu displays a popup GtkMenu which allows the
--  user to make a new choice.
--  </description>
--  <c_version>1.2.7</c_version>

with Gtk.Button;
with Gtk.Menu;
with Gtk.Widget;

package Gtk.Option_Menu is

   type Gtk_Option_Menu_Record is new Button.Gtk_Button_Record with private;
   type Gtk_Option_Menu is access all Gtk_Option_Menu_Record'Class;

   procedure Gtk_New (Option_Menu : out Gtk_Option_Menu);
   --  Create a new Gtk_Option_Menu.

   procedure Initialize (Option_Menu : access Gtk_Option_Menu_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Option_Menu.

   function Get_Menu (Option_Menu : access Gtk_Option_Menu_Record)
     return Gtk.Menu.Gtk_Menu;
   --  Return the Gtk_Menu associated with the Gtk_Option_Menu.

   procedure Set_Menu (Option_Menu : access Gtk_Option_Menu_Record;
                       Menu        : access Widget.Gtk_Widget_Record'Class);
   --  Provide the Gtk_Menu that is popped up to allow the user to choose a new
   --  value. You should provide a simple menu avoiding the use of tearoff menu
   --  items, submenus, and accelerators.

   procedure Remove_Menu (Option_Menu : access Gtk_Option_Menu_Record;
                          Menu        : access Widget.Gtk_Widget_Record'Class);
   --  Remove the menu from the option menu.

   procedure Set_History (Option_Menu : access Gtk_Option_Menu_Record;
                          Index       : in     Gint);
   --  Select the menu item specified by index making it the newly selected
   --  value for the option menu.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Option_Menu_Record is new Button.Gtk_Button_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_option_menu_get_type");
end Gtk.Option_Menu;
