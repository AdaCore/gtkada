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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Pango.Language is

   function From_Object_Free
     (B : access Pango_Language'Class) return Pango_Language
   is
      Result : constant Pango_Language := Pango_Language (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Pango_Language is
      S : Pango_Language;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------------------
   -- Get_Sample_String --
   -----------------------

   function Get_Sample_String (Self : Pango_Language) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_language_get_sample_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Sample_String;

   -----------------
   -- Get_Scripts --
   -----------------

   function Get_Scripts (Self : Pango_Language) return Pango_Script_Array is
      type script_array_bounded is array (Natural) of Pango_Script;
      type script_array_access is access script_array_bounded;

      function Internal
         (Self     : System.Address;
          Size     : access Gint) return script_array_access;
      pragma Import (C, Internal, "pango_language_get_scripts");

      Scripts : script_array_access;
      Size : aliased Gint;
   begin
      Scripts := Internal (Self.Get_Object, Size'Access);

      declare
         Result : Pango_Script_Array (1 .. Integer (Size));
      begin
         for R in 0 .. Integer (Size) - 1 loop
             Result (R + 1) := Scripts (R);
         end loop;
         return Result;
      end;
   end Get_Scripts;

   ---------------------
   -- Includes_Script --
   ---------------------

   function Includes_Script
      (Self   : Pango_Language;
       Script : Pango_Script) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Script : Pango_Script) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_language_includes_script");
   begin
      return Internal (Get_Object (Self), Script) /= 0;
   end Includes_Script;

   -------------
   -- Matches --
   -------------

   function Matches
      (Self       : Pango_Language;
       Range_List : UTF8_String) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Range_List : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_language_matches");
      Tmp_Range_List : Gtkada.Types.Chars_Ptr := New_String (Range_List);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Range_List);
      Free (Tmp_Range_List);
      return Tmp_Return /= 0;
   end Matches;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Pango_Language) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_language_to_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Language : UTF8_String := "") return Pango_Language is
      function Internal
         (Language : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "pango_language_from_string");
      Tmp_Language : Gtkada.Types.Chars_Ptr;
      Tmp_Return   : System.Address;
   begin
      if Language = "" then
         Tmp_Language := Gtkada.Types.Null_Ptr;
      else
         Tmp_Language := New_String (Language);
      end if;
      Tmp_Return := Internal (Tmp_Language);
      Free (Tmp_Language);
      return From_Object (Tmp_Return);
   end From_String;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Pango_Language is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_language_get_default");
   begin
      return From_Object (Internal);
   end Get_Default;

end Pango.Language;
