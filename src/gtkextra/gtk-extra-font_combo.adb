------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2013, AdaCore                     --
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
with Pango.Font; use Pango.Font;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Font_Combo is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Combo_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------------
   -- Font_Combo_Select --
   -----------------------

   procedure Font_Combo_Select
     (Font_Combo : access Gtk_Font_Combo_Record;
      Family     : String;
      Bold       : Boolean;
      Italic     : Boolean;
      Height     : Gint)
   is
      procedure Internal
        (Font_Combo : System.Address;
         Family     : String;
         Bold       : Gint;
         Italic     : Gint;
         Height     : Gint);
      pragma Import (C, Internal, "gtk_font_combo_select");

   begin
      Internal
        (Get_Object (Font_Combo),
         Family & ASCII.NUL,
         Boolean'Pos (Bold),
         Boolean'Pos (Italic),
         Height);
   end Font_Combo_Select;

   ---------------------------
   -- Font_Combo_Select_Nth --
   ---------------------------

   procedure Font_Combo_Select_Nth
     (Font_Combo : access Gtk_Font_Combo_Record;
      N          : Gint;
      Bold       : Boolean;
      Italic     : Boolean;
      Height     : Gint)
   is
      procedure Internal
        (Font_Combo : System.Address;
         N          : Gint;
         Bold       : Gint;
         Italic     : Gint;
         Height     : Gint);
      pragma Import (C, Internal, "gtk_font_combo_select_nth");

   begin
      Internal
        (Get_Object (Font_Combo),
         N,
         Boolean'Pos (Bold),
         Boolean'Pos (Italic),
         Height);
   end Font_Combo_Select_Nth;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Font_Combo) is
   begin
      Widget := new Gtk_Font_Combo_Record;
      Gtk.Extra.Font_Combo.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Font_Combo_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_combo_new");

      procedure Psfont_Init;
      pragma Import (C, Psfont_Init, "gtk_psfont_init");

   begin
      --  gtk_psfont needs to be initialized. Since we no longer provide
      --  a binding for it (it is replaced by pango), this needs to be done
      --  automatically. Subsequent calls to Psfont_Init are ignored.

      Psfont_Init;
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------
   -- Get_GdkFont --
   -----------------

   function Get_GdkFont
     (Font_Combo : access Gtk_Font_Combo_Record) return Gdk.Font.Gdk_Font
   is
      function Internal (Font_Combo : System.Address) return Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_font_combo_get_gdkfont");
   begin
      return Internal (Get_Object (Font_Combo));
   end Get_GdkFont;

   ---------------------
   -- Get_Font_Height --
   ---------------------

   function Get_Font_Height
     (Font_Combo : access Gtk_Font_Combo_Record)  return Glib.Gint
   is
      function Internal (Combo : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_font_combo_get_font_height");
   begin
      return Internal (Get_Object (Font_Combo));
   end Get_Font_Height;

   --------------------------
   -- Get_Font_Description --
   --------------------------

   function Get_Font_Description
     (Font_Combo : access Gtk_Font_Combo_Record)
      return Pango.Font.Pango_Font_Description
   is
      function Internal (Combo : System.Address) return Pango_Font_Description;
      pragma Import (C, Internal, "gtk_font_combo_get_font_description");
   begin
      return Internal (Get_Object (Font_Combo));
   end Get_Font_Description;

end Gtk.Extra.Font_Combo;
