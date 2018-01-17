------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

package body Cairo is

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
      (Value : Glib.Values.GValue) return Cairo_Context is
   begin
      return Cairo_Context (Glib.Values.Get_Address (Value));
   end Get_Context;

   --------------
   -- Set_Dash --
   --------------

   procedure Set_Dash
     (Cr         : Cairo_Context;
      Dashes     : Dash_Array;
      Offset     : Gdouble)
   is
      procedure C_Set_Dash
        (Cr         : Cairo_Context;
         Dashes     : System.Address;
         Num_Dashes : Gint;
         Offset     : Gdouble);
      pragma Import (C, C_Set_Dash, "cairo_set_dash");

      Len : constant Natural := Dashes'Length;
   begin
      if Len = 0 then
         C_Set_Dash (Cr, System.Null_Address, 0, Offset);
      else
         C_Set_Dash (Cr, Dashes (Dashes'First)'Address, Dashes'Length, Offset);
      end if;
   end Set_Dash;

   --------------
   -- Get_Dash --
   --------------

   procedure Get_Dash
     (Cr     : Cairo_Context;
      Dashes : out Dash_Array_Access;
      Offset : out Gdouble)
   is
      procedure C_Get_Dash
        (Cr     : Cairo_Context;
         Dashes : System.Address;
         Offset : access Gdouble);
      pragma Import (C, C_Get_Dash, "cairo_get_dash");

      Count : constant Integer := Integer (Get_Dash_Count (Cr));
      G     : aliased Gdouble;
   begin
      if Count = 0 then
         Offset := 0.0;
         Dashes := null;
         return;
      end if;

      Dashes := new Dash_Array (1 .. Count);

      C_Get_Dash (Cr, Dashes (Dashes'First)'Address, G'Access);
      Offset := G;
   end Get_Dash;

   ----------------------
   -- Select_Font_Face --
   ----------------------

   procedure Select_Font_Face
     (Cr     : Cairo_Context;
      Family : String;
      Slant  : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight)
   is
      procedure C_Select_Font_Face
        (Cr     : Cairo_Context;
         Family : System.Address;
         Slant  : Cairo_Font_Slant;
         Weight : Cairo_Font_Weight);

      pragma Import (C, C_Select_Font_Face, "cairo_select_font_face");

      Tmp : constant String := Family & ASCII.NUL;
   begin
      C_Select_Font_Face (Cr, Tmp'Address, Slant, Weight);
   end Select_Font_Face;

   ---------------
   -- Show_Text --
   ---------------

   procedure Show_Text
     (Cr   : Cairo_Context;
      Utf8 : String)
   is
      procedure C_Show_Text (Cr : Cairo_Context; Utf8 : System.Address);
      pragma Import (C, C_Show_Text, "cairo_show_text");
      Tmp : constant String := Utf8 & ASCII.NUL;
   begin
      C_Show_Text (Cr, Tmp'Address);
   end Show_Text;

   ---------------
   -- Text_Path --
   ---------------

   procedure Text_Path
     (Cr   : Cairo_Context;
      Utf8 : String)
   is
      procedure C_Text_Path (Cr : Cairo_Context; Utf8 : System.Address);
      pragma Import (C, C_Text_Path, "cairo_text_path");
      Tmp : constant String := Utf8 & ASCII.NUL;
   begin
      C_Text_Path (Cr, Tmp'Address);
   end Text_Path;

   ---------------
   -- In_Stroke --
   ---------------

   function In_Stroke
     (Cr   : Cairo_Context;
      X    : Gdouble;
      Y    : Gdouble)
      return Boolean
   is
      function Internal (C : Cairo_Context; X, Y : Gdouble) return Gboolean;
      pragma Import (C, Internal, "cairo_in_stroke");
   begin
      return Internal (Cr, X, Y) /= 0;
   end In_Stroke;

   -------------
   -- In_Fill --
   -------------

   function In_Fill
     (Cr   : Cairo_Context;
      X    : Gdouble;
      Y    : Gdouble)
      return Boolean
   is
      function Internal (C : Cairo_Context; X, Y : Gdouble) return Gboolean;
      pragma Import (C, Internal, "cairo_in_fill");
   begin
      return Internal (Cr, X, Y) /= 0;
   end In_Fill;

   -----------------------
   -- Has_Current_Point --
   -----------------------

   function Has_Current_Point (Cr : Cairo_Context) return Boolean is
      function Internal (Cr : Cairo_Context) return Gboolean;
      pragma Import (C, Internal, "cairo_has_current_point");
   begin
      return Internal (Cr) /= 0;
   end Has_Current_Point;

end Cairo;
