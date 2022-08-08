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

package body Pango.Context is

   package Type_Conversion_Pango_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Context_Record);
   pragma Unreferenced (Type_Conversion_Pango_Context);

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Pango_Context) is
   begin
      Self := new Pango_Context_Record;
      Pango.Context.Initialize (Self);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Pango_Context_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_context_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Pango_Context_New --
   -----------------------

   function Pango_Context_New return Pango_Context is
      Self : constant Pango_Context := new Pango_Context_Record;
   begin
      Pango.Context.Initialize (Self);
      return Self;
   end Pango_Context_New;

   -------------
   -- Changed --
   -------------

   procedure Changed (Self : not null access Pango_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_context_changed");
   begin
      Internal (Get_Object (Self));
   end Changed;

   ------------------
   -- Get_Base_Dir --
   ------------------

   function Get_Base_Dir
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.Direction
   is
      function Internal (Self : System.Address) return Pango.Enums.Direction;
      pragma Import (C, Internal, "pango_context_get_base_dir");
   begin
      return Internal (Get_Object (Self));
   end Get_Base_Dir;

   ----------------------
   -- Get_Base_Gravity --
   ----------------------

   function Get_Base_Gravity
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.Gravity
   is
      function Internal (Self : System.Address) return Pango.Enums.Gravity;
      pragma Import (C, Internal, "pango_context_get_base_gravity");
   begin
      return Internal (Get_Object (Self));
   end Get_Base_Gravity;

   --------------------------
   -- Get_Font_Description --
   --------------------------

   function Get_Font_Description
      (Self : not null access Pango_Context_Record)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self : System.Address) return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "pango_context_get_font_description");
   begin
      return Internal (Get_Object (Self));
   end Get_Font_Description;

   -----------------
   -- Get_Gravity --
   -----------------

   function Get_Gravity
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.Gravity
   is
      function Internal (Self : System.Address) return Pango.Enums.Gravity;
      pragma Import (C, Internal, "pango_context_get_gravity");
   begin
      return Internal (Get_Object (Self));
   end Get_Gravity;

   ----------------------
   -- Get_Gravity_Hint --
   ----------------------

   function Get_Gravity_Hint
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.GravityHint
   is
      function Internal
         (Self : System.Address) return Pango.Enums.GravityHint;
      pragma Import (C, Internal, "pango_context_get_gravity_hint");
   begin
      return Internal (Get_Object (Self));
   end Get_Gravity_Hint;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
      (Self : not null access Pango_Context_Record)
       return Pango.Language.Pango_Language
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_context_get_language");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Language;

   ----------------
   -- Get_Matrix --
   ----------------

   function Get_Matrix
      (Self : not null access Pango_Context_Record)
       return Pango.Matrix.Pango_Matrix
   is
      function Internal
         (Self : System.Address) return access Pango.Matrix.Pango_Matrix;
      pragma Import (C, Internal, "pango_context_get_matrix");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Matrix;

   -----------------
   -- Get_Metrics --
   -----------------

   function Get_Metrics
      (Self     : not null access Pango_Context_Record;
       Desc     : Pango.Font.Pango_Font_Description;
       Language : Pango.Language.Pango_Language)
       return Pango.Font_Metrics.Pango_Font_Metrics
   is
      function Internal
         (Self     : System.Address;
          Desc     : Pango.Font.Pango_Font_Description;
          Language : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_context_get_metrics");
   begin
      return From_Object (Internal (Get_Object (Self), Desc, Get_Object (Language)));
   end Get_Metrics;

   -------------------------------
   -- Get_Round_Glyph_Positions --
   -------------------------------

   function Get_Round_Glyph_Positions
      (Self : not null access Pango_Context_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_context_get_round_glyph_positions");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Round_Glyph_Positions;

   ----------------
   -- Get_Serial --
   ----------------

   function Get_Serial
      (Self : not null access Pango_Context_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "pango_context_get_serial");
   begin
      return Internal (Get_Object (Self));
   end Get_Serial;

   -------------------
   -- List_Families --
   -------------------

   function List_Families
      (Self : not null access Pango_Context_Record)
       return Pango_Font_Family_Array
   is
      type font_family_array_bounded is array (Natural) of System.Address;

      procedure Internal
         (Self     : System.Address;
          Families : System.Address;
          Size     : access Gint);
      pragma Import (C, Internal, "pango_context_list_families");

      procedure g_free (s : access font_family_array_bounded);
      pragma Import (C, g_free, "g_free");

      Families : aliased access font_family_array_bounded;
      Size : aliased Gint;
      Tmplt : Pango_Font_Family_Record;
   begin
      Internal (Get_Object (Self), Families'Address, Size'Access);

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
      (Self : not null access Pango_Context_Record;
       Desc : Pango.Font.Pango_Font_Description)
       return Pango.Font.Pango_Font
   is
      function Internal
         (Self : System.Address;
          Desc : Pango.Font.Pango_Font_Description) return System.Address;
      pragma Import (C, Internal, "pango_context_load_font");
      Stub_Pango_Font : Pango.Font.Pango_Font_Record;
   begin
      return Pango.Font.Pango_Font (Get_User_Data (Internal (Get_Object (Self), Desc), Stub_Pango_Font));
   end Load_Font;

   ------------------
   -- Load_Fontset --
   ------------------

   function Load_Fontset
      (Self     : not null access Pango_Context_Record;
       Desc     : Pango.Font.Pango_Font_Description;
       Language : Pango.Language.Pango_Language)
       return Pango.Fontset.Pango_Fontset
   is
      function Internal
         (Self     : System.Address;
          Desc     : Pango.Font.Pango_Font_Description;
          Language : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_context_load_fontset");
      Stub_Pango_Fontset : Pango.Fontset.Pango_Fontset_Record;
   begin
      return Pango.Fontset.Pango_Fontset (Get_User_Data (Internal (Get_Object (Self), Desc, Get_Object (Language)), Stub_Pango_Fontset));
   end Load_Fontset;

   ------------------
   -- Set_Base_Dir --
   ------------------

   procedure Set_Base_Dir
      (Self      : not null access Pango_Context_Record;
       Direction : Pango.Enums.Direction)
   is
      procedure Internal
         (Self      : System.Address;
          Direction : Pango.Enums.Direction);
      pragma Import (C, Internal, "pango_context_set_base_dir");
   begin
      Internal (Get_Object (Self), Direction);
   end Set_Base_Dir;

   ----------------------
   -- Set_Base_Gravity --
   ----------------------

   procedure Set_Base_Gravity
      (Self    : not null access Pango_Context_Record;
       Gravity : Pango.Enums.Gravity)
   is
      procedure Internal
         (Self    : System.Address;
          Gravity : Pango.Enums.Gravity);
      pragma Import (C, Internal, "pango_context_set_base_gravity");
   begin
      Internal (Get_Object (Self), Gravity);
   end Set_Base_Gravity;

   --------------------------
   -- Set_Font_Description --
   --------------------------

   procedure Set_Font_Description
      (Self : not null access Pango_Context_Record;
       Desc : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
         (Self : System.Address;
          Desc : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "pango_context_set_font_description");
   begin
      Internal (Get_Object (Self), Desc);
   end Set_Font_Description;

   ----------------------
   -- Set_Gravity_Hint --
   ----------------------

   procedure Set_Gravity_Hint
      (Self : not null access Pango_Context_Record;
       Hint : Pango.Enums.GravityHint)
   is
      procedure Internal
         (Self : System.Address;
          Hint : Pango.Enums.GravityHint);
      pragma Import (C, Internal, "pango_context_set_gravity_hint");
   begin
      Internal (Get_Object (Self), Hint);
   end Set_Gravity_Hint;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
      (Self     : not null access Pango_Context_Record;
       Language : Pango.Language.Pango_Language)
   is
      procedure Internal (Self : System.Address; Language : System.Address);
      pragma Import (C, Internal, "pango_context_set_language");
   begin
      Internal (Get_Object (Self), Get_Object (Language));
   end Set_Language;

   ----------------
   -- Set_Matrix --
   ----------------

   procedure Set_Matrix
      (Self   : not null access Pango_Context_Record;
       Matrix : Pango.Matrix.Pango_Matrix)
   is
      procedure Internal
         (Self   : System.Address;
          Matrix : Pango.Matrix.Pango_Matrix);
      pragma Import (C, Internal, "pango_context_set_matrix");
   begin
      Internal (Get_Object (Self), Matrix);
   end Set_Matrix;

   -------------------------------
   -- Set_Round_Glyph_Positions --
   -------------------------------

   procedure Set_Round_Glyph_Positions
      (Self            : not null access Pango_Context_Record;
       Round_Positions : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Round_Positions : Glib.Gboolean);
      pragma Import (C, Internal, "pango_context_set_round_glyph_positions");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Round_Positions));
   end Set_Round_Glyph_Positions;

end Pango.Context;
