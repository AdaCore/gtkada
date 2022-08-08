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

package body Pango.Font_Map is

   package Type_Conversion_Pango_Font_Map is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Font_Map_Record);
   pragma Unreferenced (Type_Conversion_Pango_Font_Map);

   -------------
   -- Changed --
   -------------

   procedure Changed (Self : not null access Pango_Font_Map_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_font_map_changed");
   begin
      Internal (Get_Object (Self));
   end Changed;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
      (Self : not null access Pango_Font_Map_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_font_map_create_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Context));
   end Create_Context;

   ----------------
   -- Get_Serial --
   ----------------

   function Get_Serial
      (Self : not null access Pango_Font_Map_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "pango_font_map_get_serial");
   begin
      return Internal (Get_Object (Self));
   end Get_Serial;

   -------------------
   -- List_Families --
   -------------------

   function List_Families
      (Self : not null access Pango_Font_Map_Record)
       return Pango_Font_Family_Array
   is
      type font_family_array_bounded is array (Natural) of System.Address;

      procedure Internal
         (Self     : System.Address;
          Families : System.Address;
          Size     : access Gint);
      pragma Import (C, Internal, "pango_font_map_list_families");

      procedure g_free (s : access font_family_array_bounded);
      pragma Import (C, g_free, "g_free");

      Families : aliased access font_family_array_bounded;
      Size : aliased Gint;
      Tmplt : Pango_Font_Family_Record;
   begin
      Internal (Self.Get_Object, Families'Address, Size'Access);

      declare
         Result : Pango_Font_Family_Array (1 .. Integer (Size));
      begin
         for R in 0 .. Integer (Size) - 1 loop
             Result (R + 1) := Pango_Font_Family
                (Get_User_Data (Families (R), Tmplt));
         end loop;
         g_free (Families);

         return Result;
      end;
   end List_Families;

   ---------------
   -- Load_Font --
   ---------------

   function Load_Font
      (Self    : not null access Pango_Font_Map_Record;
       Context : not null access Pango.Context.Pango_Context_Record'Class;
       Desc    : Pango.Font.Pango_Font_Description)
       return Pango.Font.Pango_Font
   is
      function Internal
         (Self    : System.Address;
          Context : System.Address;
          Desc    : Pango.Font.Pango_Font_Description) return System.Address;
      pragma Import (C, Internal, "pango_font_map_load_font");
      Stub_Pango_Font : Pango.Font.Pango_Font_Record;
   begin
      return Pango.Font.Pango_Font (Get_User_Data (Internal (Get_Object (Self), Get_Object (Context), Desc), Stub_Pango_Font));
   end Load_Font;

   ------------------
   -- Load_Fontset --
   ------------------

   function Load_Fontset
      (Self     : not null access Pango_Font_Map_Record;
       Context  : not null access Pango.Context.Pango_Context_Record'Class;
       Desc     : Pango.Font.Pango_Font_Description;
       Language : Pango.Language.Pango_Language)
       return Pango.Fontset.Pango_Fontset
   is
      function Internal
         (Self     : System.Address;
          Context  : System.Address;
          Desc     : Pango.Font.Pango_Font_Description;
          Language : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_font_map_load_fontset");
      Stub_Pango_Fontset : Pango.Fontset.Pango_Fontset_Record;
   begin
      return Pango.Fontset.Pango_Fontset (Get_User_Data (Internal (Get_Object (Self), Get_Object (Context), Desc, Get_Object (Language)), Stub_Pango_Fontset));
   end Load_Fontset;

end Pango.Font_Map;
