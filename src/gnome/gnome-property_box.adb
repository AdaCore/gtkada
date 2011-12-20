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

with Gtk; use Gtk;
with System;

package body Gnome.Property_Box is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Property_Box) is
   begin
      Widget := new Gnome_Property_Box_Record;
      Gnome.Property_Box.Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gnome_Property_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_property_box_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
     (Property_Box : access Gnome_Property_Box_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label    : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
        (Property_Box : System.Address;
         Child        : System.Address;
         Tab_Label    : System.Address)
         return Gint;
      pragma Import (C, Internal, "gnome_property_box_append_page");
   begin
      return Internal (Get_Object (Property_Box),
                       Get_Object (Child),
                       Get_Object (Tab_Label));
   end Append_Page;

   -------------
   -- Changed --
   -------------

   procedure Changed (Property_Box : access Gnome_Property_Box_Record) is
      procedure Internal (Property_Box : System.Address);
      pragma Import (C, Internal, "gnome_property_box_changed");
   begin
      Internal (Get_Object (Property_Box));
   end Changed;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
     (Property_Box : access Gnome_Property_Box_Record;
      State        : Boolean)
   is
      procedure Internal
        (Property_Box : System.Address;
         State        : Gint);
      pragma Import (C, Internal, "gnome_property_box_set_modified");
   begin
      Internal (Get_Object (Property_Box), Boolean'Pos (State));
   end Set_Modified;

end Gnome.Property_Box;
