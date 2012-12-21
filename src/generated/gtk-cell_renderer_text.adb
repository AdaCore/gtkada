------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Cell_Renderer_Text is

   package Type_Conversion_Gtk_Cell_Renderer_Text is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Text_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer_Text);

   --------------------------------
   -- Gtk_Cell_Renderer_Text_New --
   --------------------------------

   function Gtk_Cell_Renderer_Text_New return Gtk_Cell_Renderer_Text is
      Self : constant Gtk_Cell_Renderer_Text := new Gtk_Cell_Renderer_Text_Record;
   begin
      Gtk.Cell_Renderer_Text.Initialize (Self);
      return Self;
   end Gtk_Cell_Renderer_Text_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Text) is
   begin
      Self := new Gtk_Cell_Renderer_Text_Record;
      Gtk.Cell_Renderer_Text.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Text_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_text_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   --------------------------------
   -- Set_Fixed_Height_From_Font --
   --------------------------------

   procedure Set_Fixed_Height_From_Font
      (Self           : not null access Gtk_Cell_Renderer_Text_Record;
       Number_Of_Rows : Gint)
   is
      procedure Internal (Self : System.Address; Number_Of_Rows : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_text_set_fixed_height_from_font");
   begin
      Internal (Get_Object (Self), Number_Of_Rows);
   end Set_Fixed_Height_From_Font;

   ---------------
   -- On_Edited --
   ---------------

   procedure On_Edited
      (Self : not null access Gtk_Cell_Renderer_Text_Record;
       Call : not null access procedure
         (Self     : access Gtk_Cell_Renderer_Text_Record'Class;
          Path     : UTF8_String;
          New_Text : UTF8_String))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Edited;

   ---------------
   -- On_Edited --
   ---------------

   procedure On_Edited
      (Self : not null access Gtk_Cell_Renderer_Text_Record;
       Call : not null access procedure
         (Self     : access Glib.Object.GObject_Record'Class;
          Path     : UTF8_String;
          New_Text : UTF8_String);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Edited;

end Gtk.Cell_Renderer_Text;
