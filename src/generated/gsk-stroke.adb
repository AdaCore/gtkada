------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Ada.Unchecked_Conversion;

package body Gsk.Stroke is

   function From_Object_Free
     (B : access Gsk_Stroke'Class) return Gsk_Stroke
   is
      Result : constant Gsk_Stroke := Gsk_Stroke (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsk_Stroke is
      S : Gsk_Stroke;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   procedure Set_Dash (Self : Gsk_Stroke; Dash : Dash_Array) is
      procedure Internal
        (Self : System.Address; Dash : System.Address; N : Gsize);
      pragma Import (C, Internal, "gsk_stroke_set_dash");
   begin
      Internal (Get_Object (Self), Dash'Address, Gsize (Dash'Length));
   end Set_Dash;

   function Get_Dash (Self : Gsk_Stroke) return Dash_Array is
      function Internal (Self : System.Address; N : out Gsize)
      return System.Address;
      pragma Import (C, Internal, "gsk_stroke_get_dash");
      N : Gsize;
      Data : constant System.Address := Internal (Get_Object (Self), N);
   begin
      if N = 0 then
         return (1 .. 0 => 0.0);
      end if;
      declare
         subtype Buffer is Dash_Array (1 .. Natural (N));
         type Buffer_Access is access all Buffer;
         function Convert is new Ada.Unchecked_Conversion
           (System.Address, Buffer_Access);
      begin
         return Convert (Data).all;
      end;
   end Get_Dash;

   --------------------
   -- Gsk_Stroke_New --
   --------------------

   function Gsk_Stroke_New
      (Line_Width : Interfaces.C.C_float) return Gsk_Stroke
   is
      function Internal
         (Line_Width : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_new");
      Self : Gsk_Stroke;
   begin
      Self.Set_Object (Internal (Line_Width));
      return Self;
   end Gsk_Stroke_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gsk_Stroke;
       Line_Width : Interfaces.C.C_float)
   is
      function Internal
         (Line_Width : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_new");
   begin
      Self.Set_Object (Internal (Line_Width));
   end Gtk_New;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Gsk_Stroke) return Gsk_Stroke is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_stroke_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Gsk_Stroke) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_stroke_free");
   begin
      Internal (Get_Object (Self));
   end Free;

   ---------------------
   -- Get_Dash_Offset --
   ---------------------

   function Get_Dash_Offset (Self : Gsk_Stroke) return Interfaces.C.C_float is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_stroke_get_dash_offset");
   begin
      return Internal (Get_Object (Self));
   end Get_Dash_Offset;

   ------------------
   -- Get_Line_Cap --
   ------------------

   function Get_Line_Cap (Self : Gsk_Stroke) return Gsk.Enums.Gsk_Line_Cap is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Line_Cap;
      pragma Import (C, Internal, "gsk_stroke_get_line_cap");
   begin
      return Internal (Get_Object (Self));
   end Get_Line_Cap;

   -------------------
   -- Get_Line_Join --
   -------------------

   function Get_Line_Join (Self : Gsk_Stroke) return Gsk.Enums.Gsk_Line_Join is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Line_Join;
      pragma Import (C, Internal, "gsk_stroke_get_line_join");
   begin
      return Internal (Get_Object (Self));
   end Get_Line_Join;

   --------------------
   -- Get_Line_Width --
   --------------------

   function Get_Line_Width (Self : Gsk_Stroke) return Interfaces.C.C_float is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_stroke_get_line_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Line_Width;

   ---------------------
   -- Get_Miter_Limit --
   ---------------------

   function Get_Miter_Limit (Self : Gsk_Stroke) return Interfaces.C.C_float is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_stroke_get_miter_limit");
   begin
      return Internal (Get_Object (Self));
   end Get_Miter_Limit;

   ---------------------
   -- Set_Dash_Offset --
   ---------------------

   procedure Set_Dash_Offset
      (Self   : Gsk_Stroke;
       Offset : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Offset : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_stroke_set_dash_offset");
   begin
      Internal (Get_Object (Self), Offset);
   end Set_Dash_Offset;

   ------------------
   -- Set_Line_Cap --
   ------------------

   procedure Set_Line_Cap
      (Self     : Gsk_Stroke;
       Line_Cap : Gsk.Enums.Gsk_Line_Cap)
   is
      procedure Internal
         (Self     : System.Address;
          Line_Cap : Gsk.Enums.Gsk_Line_Cap);
      pragma Import (C, Internal, "gsk_stroke_set_line_cap");
   begin
      Internal (Get_Object (Self), Line_Cap);
   end Set_Line_Cap;

   -------------------
   -- Set_Line_Join --
   -------------------

   procedure Set_Line_Join
      (Self      : Gsk_Stroke;
       Line_Join : Gsk.Enums.Gsk_Line_Join)
   is
      procedure Internal
         (Self      : System.Address;
          Line_Join : Gsk.Enums.Gsk_Line_Join);
      pragma Import (C, Internal, "gsk_stroke_set_line_join");
   begin
      Internal (Get_Object (Self), Line_Join);
   end Set_Line_Join;

   --------------------
   -- Set_Line_Width --
   --------------------

   procedure Set_Line_Width
      (Self       : Gsk_Stroke;
       Line_Width : Interfaces.C.C_float)
   is
      procedure Internal
         (Self       : System.Address;
          Line_Width : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_stroke_set_line_width");
   begin
      Internal (Get_Object (Self), Line_Width);
   end Set_Line_Width;

   ---------------------
   -- Set_Miter_Limit --
   ---------------------

   procedure Set_Miter_Limit
      (Self  : Gsk_Stroke;
       Limit : Interfaces.C.C_float)
   is
      procedure Internal
         (Self  : System.Address;
          Limit : Interfaces.C.C_float);
      pragma Import (C, Internal, "gsk_stroke_set_miter_limit");
   begin
      Internal (Get_Object (Self), Limit);
   end Set_Miter_Limit;

   --------------
   -- To_Cairo --
   --------------

   procedure To_Cairo (Self : Gsk_Stroke; Cr : Cairo.Cairo_Context) is
      procedure Internal (Self : System.Address; Cr : Cairo.Cairo_Context);
      pragma Import (C, Internal, "gsk_stroke_to_cairo");
   begin
      Internal (Get_Object (Self), Cr);
   end To_Cairo;

   -----------
   -- Equal --
   -----------

   function Equal
      (Stroke1 : Gsk_Stroke;
       Stroke2 : Gsk_Stroke) return Boolean
   is
      function Internal
         (Stroke1 : System.Address;
          Stroke2 : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_stroke_equal");
   begin
      return Internal (Get_Object (Stroke1), Get_Object (Stroke2)) /= 0;
   end Equal;

end Gsk.Stroke;
