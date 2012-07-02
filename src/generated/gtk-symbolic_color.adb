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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Gtkada.Bindings;      use Gtkada.Bindings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.Symbolic_Color is

   function From_Object (Object : System.Address) return Gtk_Symbolic_Color is
      S : Gtk_Symbolic_Color;
   begin
      S.Ptr := Object;
      return S;
   end From_Object;

   function Get_Object
     (Object : Gtk_Symbolic_Color'Class) return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

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
      Self.Ptr := Internal (Color);
   end Gtk_New_Literal;

   ------------------
   -- Gtk_New_Name --
   ------------------

   procedure Gtk_New_Name
      (Self : out Gtk_Symbolic_Color;
       Name : UTF8_String)
   is
      function Internal
         (Name : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_name");
      Tmp_Name   : Interfaces.C.Strings.chars_ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Name);
      Free (Tmp_Name);
      Self.Ptr := Tmp_Return;
   end Gtk_New_Name;

   ---------------
   -- New_Alpha --
   ---------------

   function New_Alpha
      (Self   : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color
   is
      function Internal
         (Self   : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_alpha");
   begin
      return From_Object (Internal (Get_Object (Self), Factor));
   end New_Alpha;

   -------------
   -- New_Mix --
   -------------

   function New_Mix
      (Self   : Gtk_Symbolic_Color;
       Color2 : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color
   is
      function Internal
         (Self   : System.Address;
          Color2 : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_mix");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Color2), Factor));
   end New_Mix;

   ---------------
   -- New_Shade --
   ---------------

   function New_Shade
      (Self   : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color
   is
      function Internal
         (Self   : System.Address;
          Factor : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_symbolic_color_new_shade");
   begin
      return From_Object (Internal (Get_Object (Self), Factor));
   end New_Shade;

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
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
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
