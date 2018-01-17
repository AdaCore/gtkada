------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with System;

with Glib;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_Child is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Child_Anchor_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------
   -- Get_Deleted --
   -----------------

   function Get_Deleted
     (Anchor : access Gtk_Text_Child_Anchor_Record) return Boolean
   is
      function Internal (Anchor : System.Address) return Gboolean;
      pragma Import  (C, Internal, "gtk_text_child_anchor_get_deleted");

   begin
      return Internal (Get_Object (Anchor)) /= 0;
   end Get_Deleted;

   -----------------
   -- Get_Widgets --
   -----------------

   function Get_Widgets
     (Anchor : access Gtk_Text_Child_Anchor_Record)
      return Gtk.Widget.Widget_List.Glist
   is
      use Gtk.Widget.Widget_List;
      function Internal (Anchor : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_get_widgets");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Anchor)));
      return List;
   end Get_Widgets;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Text_Child_Anchor) is
   begin
      Widget := new Gtk_Text_Child_Anchor_Record;
      Gtk.Text_Child.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Text_Child_Anchor_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

end Gtk.Text_Child;
