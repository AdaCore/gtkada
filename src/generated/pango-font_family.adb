------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2021, AdaCore                     --
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

package body Pango.Font_Family is

   package Type_Conversion_Pango_Font_Family is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Font_Family_Record);
   pragma Unreferenced (Type_Conversion_Pango_Font_Family);

   --------------
   -- Get_Face --
   --------------

   function Get_Face
      (Self : not null access Pango_Font_Family_Record;
       Name : UTF8_String := "") return Pango.Font_Face.Pango_Font_Face
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "pango_font_family_get_face");
      Tmp_Name             : Gtkada.Types.Chars_Ptr;
      Stub_Pango_Font_Face : Pango.Font_Face.Pango_Font_Face_Record;
      Tmp_Return           : System.Address;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Pango.Font_Face.Pango_Font_Face (Get_User_Data (Tmp_Return, Stub_Pango_Font_Face));
   end Get_Face;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Pango_Font_Family_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_font_family_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ------------------
   -- Is_Monospace --
   ------------------

   function Is_Monospace
      (Self : not null access Pango_Font_Family_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_family_is_monospace");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Monospace;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable
      (Self : not null access Pango_Font_Family_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_font_family_is_variable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Variable;

   ----------------
   -- List_Faces --
   ----------------

   function List_Faces
      (Self : not null access Pango_Font_Family_Record)
       return Pango_Font_Face_Array
   is
      type font_face_array_bounded is array (Natural) of System.Address;

      procedure Internal
         (Self        : System.Address;
          Faces       : System.Address;
          Acc_N_Faces : access Gint);
      pragma Import (C, Internal, "pango_font_family_list_faces");

      procedure g_free (s : access font_face_array_bounded);
      pragma Import (C, g_free, "g_free");

      Faces : aliased access font_face_array_bounded;
      Size : aliased Gint;
      Tmplt : Pango_Font_Face_Record;
   begin
      Internal (Self.Get_Object, Faces'Address, Size'Access);

      declare
         Result : Pango_Font_Face_Array (1 .. Integer (Size));
      begin
         for R in 0 .. Integer (Size) - 1 loop
             Result (R + 1) := Pango_Font_Face
                (Get_User_Data (Faces (R), Tmplt));
         end loop;
         g_free (Faces);

         return Result;
      end;
   end List_Faces;

end Pango.Font_Family;
