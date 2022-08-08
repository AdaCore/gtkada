------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;

package body Gtk.Symbolic_Color is

   function From_Object_Free
     (B : access Gtk_Symbolic_Color'Class) return Gtk_Symbolic_Color
   is
      Result : constant Gtk_Symbolic_Color := Gtk_Symbolic_Color (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Symbolic_Color is
      S : Gtk_Symbolic_Color;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------------
   -- Gtk_New_Alpha --
   -------------------

   procedure Gtk_New_Alpha
      (Self   : out Gtk_Symbolic_Color;
       Color  : Gtk_Symbolic_Color;
       Factor : Gdouble)
   is
      function Internal
         (Color  : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_alpha");
   begin
      Self.Set_Object (Internal (Get_Object (Color), Factor));
   end Gtk_New_Alpha;

   ---------------------
   -- Gtk_New_Literal --
   ---------------------

   procedure Gtk_New_Literal
      (Self  : out Gtk_Symbolic_Color;
       Color : Gdk.RGBA.Gdk_RGBA)
   is
      function Internal (Color : Gdk.RGBA.Gdk_RGBA) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_literal");
   begin
      Self.Set_Object (Internal (Color));
   end Gtk_New_Literal;

   -----------------
   -- Gtk_New_Mix --
   -----------------

   procedure Gtk_New_Mix
      (Self   : out Gtk_Symbolic_Color;
       Color1 : Gtk_Symbolic_Color;
       Color2 : Gtk_Symbolic_Color;
       Factor : Gdouble)
   is
      function Internal
         (Color1 : System.Address;
          Color2 : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_mix");
   begin
      Self.Set_Object (Internal (Get_Object (Color1), Get_Object (Color2), Factor));
   end Gtk_New_Mix;

   ------------------
   -- Gtk_New_Name --
   ------------------

   procedure Gtk_New_Name
      (Self : out Gtk_Symbolic_Color;
       Name : UTF8_String)
   is
      function Internal
         (Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_name");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Name);
      Free (Tmp_Name);
      Self.Set_Object (Tmp_Return);
   end Gtk_New_Name;

   -------------------
   -- Gtk_New_Shade --
   -------------------

   procedure Gtk_New_Shade
      (Self   : out Gtk_Symbolic_Color;
       Color  : Gtk_Symbolic_Color;
       Factor : Gdouble)
   is
      function Internal
         (Color  : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_shade");
   begin
      Self.Set_Object (Internal (Get_Object (Color), Factor));
   end Gtk_New_Shade;

   -------------------
   -- Gtk_New_Win32 --
   -------------------

   procedure Gtk_New_Win32
      (Self        : out Gtk_Symbolic_Color;
       Theme_Class : UTF8_String;
       Id          : Glib.Gint)
   is
      function Internal
         (Theme_Class : Gtkada.Types.Chars_Ptr;
          Id          : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_win32");
      Tmp_Theme_Class : Gtkada.Types.Chars_Ptr := New_String (Theme_Class);
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Theme_Class, Id);
      Free (Tmp_Theme_Class);
      Self.Set_Object (Tmp_Return);
   end Gtk_New_Win32;

   ----------------------------------
   -- Gtk_Symbolic_Color_New_Alpha --
   ----------------------------------

   function Gtk_Symbolic_Color_New_Alpha
      (Color  : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color
   is
      function Internal
         (Color  : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_alpha");
      Self : Gtk_Symbolic_Color;
   begin
      Self.Set_Object (Internal (Get_Object (Color), Factor));
      return Self;
   end Gtk_Symbolic_Color_New_Alpha;

   ------------------------------------
   -- Gtk_Symbolic_Color_New_Literal --
   ------------------------------------

   function Gtk_Symbolic_Color_New_Literal
      (Color : Gdk.RGBA.Gdk_RGBA) return Gtk_Symbolic_Color
   is
      function Internal (Color : Gdk.RGBA.Gdk_RGBA) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_literal");
      Self : Gtk_Symbolic_Color;
   begin
      Self.Set_Object (Internal (Color));
      return Self;
   end Gtk_Symbolic_Color_New_Literal;

   --------------------------------
   -- Gtk_Symbolic_Color_New_Mix --
   --------------------------------

   function Gtk_Symbolic_Color_New_Mix
      (Color1 : Gtk_Symbolic_Color;
       Color2 : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color
   is
      function Internal
         (Color1 : System.Address;
          Color2 : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_mix");
      Self : Gtk_Symbolic_Color;
   begin
      Self.Set_Object (Internal (Get_Object (Color1), Get_Object (Color2), Factor));
      return Self;
   end Gtk_Symbolic_Color_New_Mix;

   ---------------------------------
   -- Gtk_Symbolic_Color_New_Name --
   ---------------------------------

   function Gtk_Symbolic_Color_New_Name
      (Name : UTF8_String) return Gtk_Symbolic_Color
   is
      function Internal
         (Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_name");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
      Self       : Gtk_Symbolic_Color;
   begin
      Tmp_Return := Internal (Tmp_Name);
      Free (Tmp_Name);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gtk_Symbolic_Color_New_Name;

   ----------------------------------
   -- Gtk_Symbolic_Color_New_Shade --
   ----------------------------------

   function Gtk_Symbolic_Color_New_Shade
      (Color  : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color
   is
      function Internal
         (Color  : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_shade");
      Self : Gtk_Symbolic_Color;
   begin
      Self.Set_Object (Internal (Get_Object (Color), Factor));
      return Self;
   end Gtk_Symbolic_Color_New_Shade;

   ----------------------------------
   -- Gtk_Symbolic_Color_New_Win32 --
   ----------------------------------

   function Gtk_Symbolic_Color_New_Win32
      (Theme_Class : UTF8_String;
       Id          : Glib.Gint) return Gtk_Symbolic_Color
   is
      function Internal
         (Theme_Class : Gtkada.Types.Chars_Ptr;
          Id          : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_win32");
      Tmp_Theme_Class : Gtkada.Types.Chars_Ptr := New_String (Theme_Class);
      Tmp_Return      : System.Address;
      Self            : Gtk_Symbolic_Color;
   begin
      Tmp_Return := Internal (Tmp_Theme_Class, Id);
      Free (Tmp_Theme_Class);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gtk_Symbolic_Color_New_Win32;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Symbolic_Color) return Gtk_Symbolic_Color is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gtk_Symbolic_Color) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_symbolic_color_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Symbolic_Color) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_symbolic_color_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Symbolic_Color;
