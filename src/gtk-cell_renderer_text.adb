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

with Glib.Type_Conversion_Hooks;

package body Gtk.Cell_Renderer_Text is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Text_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Text)
   is
   begin
      Widget := new Gtk_Cell_Renderer_Text_Record;
      Cell_Renderer_Text.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Cell_Renderer_Text_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_text_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   --------------------------------
   -- Set_Fixed_Height_From_Font --
   --------------------------------

   procedure Set_Fixed_Height_From_Font
     (Renderer       : access Gtk_Cell_Renderer_Text_Record;
      Number_Of_Rows : Gint)
   is
      procedure Internal
        (Renderer       : System.Address;
         Number_Of_Rows : Gint);
      pragma Import
        (C, Internal, "gtk_cell_renderer_text_set_fixed_height_from_font");
   begin
      Internal (Get_Object (Renderer),
                Number_Of_Rows);
   end Set_Fixed_Height_From_Font;

end Gtk.Cell_Renderer_Text;
