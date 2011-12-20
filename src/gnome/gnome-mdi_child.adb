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

with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Gnome.MDI_Child is

   --------------
   -- Add_View --
   --------------

   function Add_View (MDI_Child : access Gnome_MDI_Child_Record)
                      return Gtk.Widget.Gtk_Widget
   is
      function Internal (MDI_Child : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gnome_mdi_child_add_view");
   begin
      return Widget.Convert (Internal (Get_Object (MDI_Child)));
   end Add_View;

   -----------------
   -- Remove_View --
   -----------------

   procedure Remove_View
     (MDI_Child : access Gnome_MDI_Child_Record;
      View      : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (MDI_Child : System.Address;
         View      : System.Address);
      pragma Import (C, Internal, "gnome_mdi_child_remove_view");
   begin
      Internal (Get_Object (MDI_Child),
                Get_Object (View));
   end Remove_View;

   -----------------------
   -- Set_Menu_Template --
   -----------------------

   procedure Set_Menu_Template
     (MDI_Child : access Gnome_MDI_Child_Record;
      Menu_Tmpl : access Gnome.App_Helper.UI_Info_Array)
   is
      procedure Internal
        (MDI_Child : System.Address;
         Menu_Tmpl : System.Address);
      pragma Import (C, Internal, "gnome_mdi_child_set_menu_template");
   begin
      Internal (Get_Object (MDI_Child), Menu_Tmpl.all'Address);
   end Set_Menu_Template;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (MDI_Child : access Gnome_MDI_Child_Record;
      Name      : String)
   is
      procedure Internal
        (MDI_Child : System.Address;
         Name      : String);
      pragma Import (C, Internal, "gnome_mdi_child_set_name");
   begin
      Internal (Get_Object (MDI_Child),
                Name & ASCII.NUL);
   end Set_Name;

end Gnome.MDI_Child;
