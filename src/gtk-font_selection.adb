-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gdk.Font;
with Gdk; use Gdk;
with Interfaces.C.Strings;
with System;

package body Gtk.Font_Selection is

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button (Fsd : access Gtk_Font_Selection_Dialog_Record)
                               return Gtk.Button.Gtk_Button is
      function Internal (Fsd : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_selection_dialog_get_cancel");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Fsd)), Stub));
   end Get_Cancel_Button;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button (Fsd : access Gtk_Font_Selection_Dialog_Record)
                               return Gtk.Button.Gtk_Button is
      function Internal (Fsd : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_selection_dialog_get_ok");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Fsd)), Stub));
   end Get_Ok_Button;

   ----------------------
   -- Get_Apply_Button --
   ----------------------

   function Get_Apply_Button (Fsd : access Gtk_Font_Selection_Dialog_Record)
                              return Gtk.Button.Gtk_Button is
      function Internal (Fsd : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_selection_dialog_get_apply");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Fsd)), Stub));
   end Get_Apply_Button;

   --------------
   -- Get_Font --
   --------------

   function Get_Font (Fsd    : access Gtk_Font_Selection_Dialog_Record)
                      return      Gdk.Font.Gdk_Font
   is
      function Internal (Fsd    : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font");
      Tmp : Gdk.Font.Gdk_Font;
   begin
      Set_Object (Tmp, Internal (Get_Object (Fsd)));
      return Tmp;
   end Get_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name (Fsd    : access Gtk_Font_Selection_Dialog_Record)
                           return      String
   is
      use type Interfaces.C.Strings.chars_ptr;
      function Internal (Fsd    : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font_name");
      S : Interfaces.C.Strings.chars_ptr := Internal (Get_Object (Fsd));
   begin
      if S /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (Internal (Get_Object (Fsd)));
      else
         return "";
      end if;
   end Get_Font_Name;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text (Fsd    : access Gtk_Font_Selection_Dialog_Record)
                              return      String
   is
      function Internal (Fsd    : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal,
                     "gtk_font_selection_dialog_get_preview_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fsd)));
   end Get_Preview_Text;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
      (Fsd         : access Gtk_Font_Selection_Dialog_Record;
       Filter_Type : in Gtk_Font_Filter_Type;
       Font_Type   : in Gtk_Font_Type;
       Foundries   : in String;
       Weights     : in String;
       Slants      : in String;
       Setwidths   : in String;
       Spacings    : in String;
       Charsets    : in String)
   is
      procedure Internal
         (Fsd         : in System.Address;
          Filter_Type : in Gint;
          Font_Type   : in Gint;
          Foundries   : in String;
          Weights     : in String;
          Slants      : in String;
          Setwidths   : in String;
          Spacings    : in String;
          Charsets    : in String);
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_filter");
   begin
      Internal (Get_Object (Fsd),
                Gtk_Font_Filter_Type'Pos (Filter_Type),
                Gtk_Font_Type'Pos (Font_Type),
                Foundries & Ascii.NUL,
                Weights & Ascii.NUL,
                Slants & Ascii.NUL,
                Setwidths & Ascii.NUL,
                Spacings & Ascii.NUL,
                Charsets & Ascii.NUL);
   end Set_Filter;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Fsd      : access Gtk_Font_Selection_Dialog_Record;
       Fontname : in String)
       return        Boolean
   is
      function Internal
         (Fsd      : in System.Address;
          Fontname : in String)
          return        Gint;
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_font_name");
   begin
      return Boolean'Val (Internal (Get_Object (Fsd),
                                    Fontname & Ascii.NUL));
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Fsd  : access Gtk_Font_Selection_Dialog_Record;
       Text : in String)
   is
      procedure Internal
         (Fsd  : in System.Address;
          Text : in String);
      pragma Import (C, Internal,
                     "gtk_font_selection_dialog_set_preview_text");
   begin
      Internal (Get_Object (Fsd),
                Text & Ascii.NUL);
   end Set_Preview_Text;



   --------------
   -- Get_Font --
   --------------

   function Get_Font (Fontsel : access Gtk_Font_Selection_Record)
                      return       Gdk.Font.Gdk_Font
   is
      function Internal (Fontsel : in System.Address)
                         return       System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_font");
      Tmp : Gdk.Font.Gdk_Font;
   begin
      Set_Object (Tmp, Internal (Get_Object (Fontsel)));
      return Tmp;
   end Get_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name (Fontsel : access Gtk_Font_Selection_Record)
                           return       String
   is
      function Internal (Fontsel : in System.Address)
                         return       Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_font_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fontsel)));
   end Get_Font_Name;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text (Fontsel : access Gtk_Font_Selection_Record)
                              return       String
   is
      function Internal (Fontsel : in System.Address)
                         return       Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_preview_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fontsel)));
   end Get_Preview_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Font_Selection_Dialog;
                      Title : String) is
   begin
      Widget := new Gtk_Font_Selection_Dialog_Record;
      Initialize (Widget, Title);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Font_Selection)
   is
   begin
      Widget := new Gtk_Font_Selection_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Font_Selection_Dialog_Record;
                         Title : String) is
      function Internal (Title  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_new");
   begin
      Set_Object (Widget, Internal (Title & Ascii.NUL));
      Initialize_User_Data (Widget);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Font_Selection_Record)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
      (Fontsel     : access Gtk_Font_Selection_Record;
       Filter_Type : in Gtk_Font_Filter_Type;
       Font_Type   : in Gtk_Font_Type;
       Foundries   : in String;
       Weights     : in String;
       Slants      : in String;
       Setwidths   : in String;
       Spacings    : in String;
       Charsets    : in String)
   is
      procedure Internal
         (Fontsel     : in System.Address;
          Filter_Type : in Gint;
          Font_Type   : in Gint;
          Foundries   : in String;
          Weights     : in String;
          Slants      : in String;
          Setwidths   : in String;
          Spacings    : in String;
          Charsets    : in String);
      pragma Import (C, Internal, "gtk_font_selection_set_filter");
   begin
      Internal (Get_Object (Fontsel),
                Gtk_Font_Filter_Type'Pos (Filter_Type),
                Gtk_Font_Type'Pos (Font_Type),
                Foundries & Ascii.NUL,
                Weights & Ascii.NUL,
                Slants & Ascii.NUL,
                Setwidths & Ascii.NUL,
                Spacings & Ascii.NUL,
                Charsets & Ascii.NUL);
   end Set_Filter;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Fontsel  : access Gtk_Font_Selection_Record;
       Fontname : in String)
       return        Boolean
   is
      function Internal
         (Fontsel  : in System.Address;
          Fontname : in String)
          return        Gint;
      pragma Import (C, Internal, "gtk_font_selection_set_font_name");
   begin
      return Boolean'Val (Internal (Get_Object (Fontsel),
                                    Fontname & Ascii.NUL));
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Fontsel : access Gtk_Font_Selection_Record;
       Text    : in String)
   is
      procedure Internal
         (Fontsel : in System.Address;
          Text    : in String);
      pragma Import (C, Internal, "gtk_font_selection_set_preview_text");
   begin
      Internal (Get_Object (Fontsel),
                Text & Ascii.NUL);
   end Set_Preview_Text;

end Gtk.Font_Selection;
