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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Pango.Font_Face is

   package Type_Conversion_Pango_Font_Face is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Font_Face_Record);
   pragma Unreferenced (Type_Conversion_Pango_Font_Face);

   --------------
   -- Describe --
   --------------

   function Describe
      (Self : not null access Pango_Font_Face_Record)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self : System.Address) return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_face_describe");
   begin
      return Internal (Get_Object (Self));
   end Describe;

   -------------------
   -- Get_Face_Name --
   -------------------

   function Get_Face_Name
      (Self : not null access Pango_Font_Face_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_font_face_get_face_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Face_Name;

   --------------------
   -- Is_Synthesized --
   --------------------

   function Is_Synthesized
      (Self : not null access Pango_Font_Face_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_face_is_synthesized");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Synthesized;

   ----------------
   -- List_Sizes --
   ----------------

   function List_Sizes
      (Self : not null access Pango_Font_Face_Record) return Gint_Array
   is
      type gint_array_bounded is array (Natural) of Gint;

      procedure Internal
         (Self        : System.Address;
          Sizes       : System.Address;
          Acc_N_Sizes : access Gint);
      pragma Import (C, Internal, "pango_font_face_list_sizes");

      procedure g_free (s : access gint_array_bounded);
      pragma Import (C, g_free, "g_free");

      Sizes : aliased access gint_array_bounded;
      Size : aliased Gint;
   begin
      Internal (Self.Get_Object, Sizes'Address, Size'Access);

      declare
         Result : Gint_Array (1 .. Integer (Size));
      begin
         for R in 0 .. Integer (Size) - 1 loop
             Result (R + 1) := Sizes (R);
         end loop;
         g_free (Sizes);

         return Result;
      end;
   end List_Sizes;

end Pango.Font_Face;
