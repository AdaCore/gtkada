------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

with Interfaces.C.Strings;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Font_Selection is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Selection_Dialog_Record);
   pragma Warnings (Off, Type_Conversion);

   procedure g_free (S : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, g_free, "g_free");
   --  External binding: g_free

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
     (Fsd : access Gtk_Font_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal (Fsd : System.Address) return System.Address;
      pragma Import (C, Internal,
                     "gtk_font_selection_dialog_get_cancel_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Fsd)), Stub));
   end Get_Cancel_Button;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_OK_Button
     (Fsd : access Gtk_Font_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal (Fsd : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_ok_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Fsd)), Stub));
   end Get_OK_Button;

   ----------------------
   -- Get_Apply_Button --
   ----------------------

   function Get_Apply_Button
     (Fsd : access Gtk_Font_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button
   is
      function Internal (Fsd : System.Address) return System.Address;
      pragma Import (C, Internal,
                     "gtk_font_selection_dialog_get_apply_button");

      Stub : Gtk.Button.Gtk_Button_Record;

   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Fsd)), Stub));
   end Get_Apply_Button;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
     (Fsd : access Gtk_Font_Selection_Dialog_Record) return Gdk.Font.Gdk_Font
   is
      function Internal (Fsd : System.Address) return Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font");

   begin
      return Internal (Get_Object (Fsd));
   end Get_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
     (Fsd : access Gtk_Font_Selection_Dialog_Record) return String
   is
      use type Interfaces.C.Strings.chars_ptr;

      function Internal
        (Fsd : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font_name");

      S : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Get_Object (Fsd));

   begin
      if S /= Interfaces.C.Strings.Null_Ptr then
         declare
            Val : constant String := Interfaces.C.Strings.Value (S);
         begin
            g_free (S);
            return Val;
         end;
      else
         return "";
      end if;
   end Get_Font_Name;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
     (Fsd : access Gtk_Font_Selection_Dialog_Record) return UTF8_String
   is
      function Internal
        (Fsd : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import
        (C, Internal, "gtk_font_selection_dialog_get_preview_text");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fsd)));
   end Get_Preview_Text;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
     (Fsd      : access Gtk_Font_Selection_Dialog_Record;
      Fontname : String) return Boolean
   is
      function Internal
        (Fsd      : System.Address;
         Fontname : String) return Gint;
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_font_name");

   begin
      return Boolean'Val (Internal (Get_Object (Fsd), Fontname & ASCII.NUL));
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
     (Fsd : access Gtk_Font_Selection_Dialog_Record; Text : UTF8_String)
   is
      procedure Internal (Fsd : System.Address; Text : UTF8_String);
      pragma Import
        (C, Internal, "gtk_font_selection_dialog_set_preview_text");

   begin
      Internal (Get_Object (Fsd), Text & ASCII.NUL);
   end Set_Preview_Text;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
     (Fontsel : access Gtk_Font_Selection_Record) return Gdk.Font.Gdk_Font
   is
      function Internal (Fontsel : System.Address) return Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_font_selection_get_font");

   begin
      return Internal (Get_Object (Fontsel));
   end Get_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
     (Fontsel : access Gtk_Font_Selection_Record) return String
   is
      function Internal
        (Fontsel : System.Address) return Interfaces.C.Strings.chars_ptr;

      pragma Import (C, Internal, "gtk_font_selection_get_font_name");

      use type Interfaces.C.Strings.chars_ptr;
      S : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Get_Object (Fontsel));

   begin
      if S /= Interfaces.C.Strings.Null_Ptr then
         declare
            Val : constant String := Interfaces.C.Strings.Value (S);
         begin
            g_free (S);
            return Val;
         end;
      else
         return "";
      end if;
   end Get_Font_Name;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
     (Fontsel : access Gtk_Font_Selection_Record) return UTF8_String
   is
      function Internal
        (Fontsel : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_preview_text");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fontsel)));
   end Get_Preview_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Font_Selection_Dialog; Title : UTF8_String) is
   begin
      Widget := new Gtk_Font_Selection_Dialog_Record;
      Initialize (Widget, Title);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Font_Selection) is
   begin
      Widget := new Gtk_Font_Selection_Record;
      Gtk.Font_Selection.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Font_Selection_Dialog_Record'Class;
      Title  : UTF8_String)
   is
      function Internal (Title : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_new");

   begin
      Set_Object (Widget, Internal (Title & ASCII.NUL));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Font_Selection_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
     (Fontsel  : access Gtk_Font_Selection_Record;
      Fontname : String) return Boolean
   is
      function Internal
        (Fontsel : System.Address; Fontname : String) return Gint;
      pragma Import (C, Internal, "gtk_font_selection_set_font_name");

   begin
      return Boolean'Val
        (Internal (Get_Object (Fontsel), Fontname & ASCII.NUL));
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
     (Fontsel : access Gtk_Font_Selection_Record; Text : UTF8_String)
   is
      procedure Internal (Fontsel : System.Address; Text : UTF8_String);
      pragma Import (C, Internal, "gtk_font_selection_set_preview_text");

   begin
      Internal (Get_Object (Fontsel), Text & ASCII.NUL);
   end Set_Preview_Text;

end Gtk.Font_Selection;
