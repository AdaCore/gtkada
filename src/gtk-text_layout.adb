-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with System;

package body Gtk.Text_Layout is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Layout : out Gtk_Text_Layout) is
   begin
      Layout := new Gtk_Text_Layout_Record;
      Initialize (Layout);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Layout : access Gtk_Text_Layout_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_text_layout_new");
   begin
      Set_Object (Layout, Internal);
   end Initialize;

   -----------------------
   -- Get_Default_Style --
   -----------------------

   function Get_Default_Style (Layout : access Gtk_Text_Layout_Record)
     return Gtk.Text_Attributes.Gtk_Text_Attributes
   is
      function Internal (Layout : System.Address)
        return Gtk.Text_Attributes.Gtk_Text_Attributes;
      pragma Import (C, Internal, "ada_text_layout_get_default_style");
   begin
      return Internal (Get_Object (Layout));
   end Get_Default_Style;

   -----------------------
   -- Set_Default_Style --
   -----------------------

   procedure Set_Default_Style
     (Layout : access Gtk_Text_Layout_Record;
      Values : Gtk.Text_Attributes.Gtk_Text_Attributes)
   is
      procedure Internal
        (Layout : System.Address;
         Values : Gtk.Text_Attributes.Gtk_Text_Attributes);
      pragma Import (C, Internal, "gtk_text_layout_set_default_style");
   begin
      Internal (Get_Object (Layout), Values);
   end Set_Default_Style;

   ---------------------------
   -- Default_Style_Changed --
   ---------------------------

   procedure Default_Style_Changed (Layout : access Gtk_Text_Layout_Record)
   is
      procedure Internal (Layout : System.Address);
      pragma Import (C, Internal, "gtk_text_layout_default_style_changed");
   begin
      Internal (Get_Object (Layout));
   end Default_Style_Changed;

end Gtk.Text_Layout;
