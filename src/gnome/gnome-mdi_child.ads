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

with Glib.Object;
with Gnome.App_Helper;
with Gtk;
with Gtk.Widget;

package Gnome.MDI_Child is

   type Gnome_MDI_Child_Record is new
     Glib.Object.GObject_Record with private;
   type Gnome_MDI_Child is access all Gnome_MDI_Child_Record'Class;

   function Add_View
     (MDI_Child : access Gnome_MDI_Child_Record) return Gtk.Widget.Gtk_Widget;

   procedure Remove_View
     (MDI_Child : access Gnome_MDI_Child_Record;
      View      : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Menu_Template
     (MDI_Child : access Gnome_MDI_Child_Record;
      Menu_Tmpl : access Gnome.App_Helper.UI_Info_Array);

   procedure Set_Name
     (MDI_Child : access Gnome_MDI_Child_Record;
      Name      : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_MDI_Child_Record is new
     Glib.Object.GObject_Record with null record;

end Gnome.MDI_Child;
