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
with Ada.Unchecked_Conversion;
with Glib.Object;

package body Glib.Option is

   function From_Object_Free
     (B : access Goption_Context'Class) return Goption_Context
   is
      Result : constant Goption_Context := Goption_Context (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Goption_Context is
      S : Goption_Context;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   function From_Object_Free (B : access GOption_Group) return GOption_Group is
      Result : constant GOption_Group := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access GOption_Entry) return GOption_Entry is
      Result : constant GOption_Entry := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self         : Goption_Context;
      Command_Line : not null access Glib.Application.Gapplication_Command_Line_Record'Class;
      Filter       : Parse_Filter := null;
      Success      : out Boolean;
      Error        : out Glib.Error.GError)
   is
      function Get_Args
        (Self : System.Address;
         Argc : access Glib.Gint) return chars_ptr_array_access;
      pragma Import (C, Get_Args, "g_application_command_line_get_arguments");

      function Internal
        (Self  : System.Address;
         Argc  : access Glib.Gint;
         Argv  : System.Address;
         Error : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_option_context_parse");

      pragma Warnings (Off, "possible aliasing problem for*");
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, chars_ptr_array_access);
      pragma Warnings (On, "possible aliasing problem for*");

      Argc : aliased Glib.Gint;
      Argv : chars_ptr_array_access;
      Err  : Glib.Error.GError;
      Ret  : Glib.Gboolean;
   begin
      Argv := Get_Args (Command_Line.Get_Object, Argc'Access);

      if Filter = null then
         Ret := Internal (Get_Object (Self), Argc'Access, Argv'Address, Err'Address);
      else
         declare
            Args : aliased Gtkada.Types.Chars_Ptr_Array := To_Chars_Ptr (Argv);
            Idx  : aliased Glib.Gint := 1;
         begin
            --  Copy command name argument
            Args (0) := Argv (0);
            Idx := 1;

            for J in 1 .. Argc - 1 loop
               if Filter
                 (Gtkada.Types.Value (Argv (Interfaces.C.size_t (J))))
               then
                  Args (Interfaces.C.size_t (Idx)) :=
                  Argv (Interfaces.C.size_t (J));
                  Idx := Idx + 1;
               end if;
            end loop;

            Argv := Convert (Args'Address);
            Ret := Internal
              (Get_Object (Self), Idx'Access, Argv'Address, Err'Address);
         end;
      end if;

      Error := Err;
      Success := Ret /= 0;
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self         : Goption_Context;
      Argv         : access chars_ptr_array_access;--  Null-terminated
      Success      : out Boolean;
      Error        : out Glib.Error.GError)
   is
      use Interfaces.C;
      function Internal
        (Self  : System.Address;
         Argc  : access Glib.Gint;
         Argv  : access chars_ptr_array_access;
         Error : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_option_context_parse");

      Err  : Glib.Error.GError;
      Ret  : Glib.Gboolean;
      Argc : aliased Glib.Gint := 0;

   begin
      while Argv.all (size_t (Argc)) /= Gtkada.Types.Null_Ptr loop
         Argc := Argc + 1;
      end loop;

      Ret := Internal (Get_Object (Self), Argc'Access, Argv, Err'Address);
      Argv.all (size_t (Argc)) := Gtkada.Types.Null_Ptr;
      Error := Err;
      Success := Ret /= 0;
   end Parse;

   procedure C_G_Option_Context_Set_Translate_Func
      (Self           : System.Address;
       Func           : System.Address;
       Data           : System.Address;
       Destroy_Notify : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_G_Option_Context_Set_Translate_Func, "g_option_context_set_translate_func");
   --  Sets the function which is used to translate the contexts user-visible
   --  strings, for `--help` output. If Func is null, strings are not
   --  translated.
   --  Note that option groups have their own translation functions, this
   --  function only affects the Parameter_String (see Glib.Option.G_New), the
   --  summary (see Glib.Option.Set_Summary) and the description (see
   --  Glib.Option.Set_Description).
   --  If you are using gettext, you only need to set the translation domain,
   --  see Glib.Option.Set_Translation_Domain.
   --  Since: gtk+ 2.12
   --  "func": the Gtranslate_Func, or null
   --  "data": user data to pass to Func, or null
   --  "destroy_notify": a function which gets called to free Data, or null

   function To_Gtranslate_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtranslate_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtranslate_Func, System.Address);

   function Internal_Gtranslate_Func
      (Str  : Gtkada.Types.Chars_Ptr;
       Data : System.Address) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Internal_Gtranslate_Func);
   --  "str": the untranslated string
   --  "data": user data specified when installing the function, e.g. in
   --  g_option_group_set_translate_func

   ------------------------------
   -- Internal_Gtranslate_Func --
   ------------------------------

   function Internal_Gtranslate_Func
      (Str  : Gtkada.Types.Chars_Ptr;
       Data : System.Address) return Gtkada.Types.Chars_Ptr
   is
      Func : constant Gtranslate_Func := To_Gtranslate_Func (Data);
   begin
      return New_String (Func (Gtkada.Bindings.Value_Allowing_Null (Str)));
   end Internal_Gtranslate_Func;

   ---------------
   -- Add_Group --
   ---------------

   procedure Add_Group (Self : Goption_Context; Group : GOption_Group) is
      procedure Internal (Self : System.Address; Group : GOption_Group);
      pragma Import (C, Internal, "g_option_context_add_group");
   begin
      Internal (Get_Object (Self), Group);
   end Add_Group;

   ----------------------
   -- Add_Main_Entries --
   ----------------------

   procedure Add_Main_Entries
      (Self               : Goption_Context;
       Entries            : GOption_Entry_Array;
       Translation_Domain : UTF8_String := "")
   is
      procedure Internal
         (Self               : System.Address;
          Entries            : GOption_Entry_Array;
          Translation_Domain : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_option_context_add_main_entries");
      Tmp_Translation_Domain : Gtkada.Types.Chars_Ptr;
   begin
      if Translation_Domain = "" then
         Tmp_Translation_Domain := Gtkada.Types.Null_Ptr;
      else
         Tmp_Translation_Domain := New_String (Translation_Domain);
      end if;
      Internal (Get_Object (Self), Entries, Tmp_Translation_Domain);
      Free (Tmp_Translation_Domain);
   end Add_Main_Entries;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Goption_Context) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_option_context_free");
   begin
      Internal (Get_Object (Self));
   end Free;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Self : Goption_Context) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_option_context_get_description");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Description;

   --------------
   -- Get_Help --
   --------------

   function Get_Help
      (Self      : Goption_Context;
       Main_Help : Boolean;
       Group     : GOption_Group) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Main_Help : Glib.Gboolean;
          Group     : GOption_Group) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_option_context_get_help");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Boolean'Pos (Main_Help), Group));
   end Get_Help;

   ----------------------
   -- Get_Help_Enabled --
   ----------------------

   function Get_Help_Enabled (Self : Goption_Context) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_option_context_get_help_enabled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Help_Enabled;

   --------------------------------
   -- Get_Ignore_Unknown_Options --
   --------------------------------

   function Get_Ignore_Unknown_Options
      (Self : Goption_Context) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_option_context_get_ignore_unknown_options");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Ignore_Unknown_Options;

   --------------------
   -- Get_Main_Group --
   --------------------

   function Get_Main_Group (Self : Goption_Context) return GOption_Group is
      function Internal (Self : System.Address) return access GOption_Group;
      pragma Import (C, Internal, "g_option_context_get_main_group");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Main_Group;

   -----------------
   -- Get_Summary --
   -----------------

   function Get_Summary (Self : Goption_Context) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_option_context_get_summary");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Summary;

   ---------------------
   -- Set_Description --
   ---------------------

   procedure Set_Description
      (Self        : Goption_Context;
       Description : UTF8_String := "")
   is
      procedure Internal
         (Self        : System.Address;
          Description : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_option_context_set_description");
      Tmp_Description : Gtkada.Types.Chars_Ptr;
   begin
      if Description = "" then
         Tmp_Description := Gtkada.Types.Null_Ptr;
      else
         Tmp_Description := New_String (Description);
      end if;
      Internal (Get_Object (Self), Tmp_Description);
      Free (Tmp_Description);
   end Set_Description;

   ----------------------
   -- Set_Help_Enabled --
   ----------------------

   procedure Set_Help_Enabled
      (Self         : Goption_Context;
       Help_Enabled : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Help_Enabled : Glib.Gboolean);
      pragma Import (C, Internal, "g_option_context_set_help_enabled");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Help_Enabled));
   end Set_Help_Enabled;

   --------------------------------
   -- Set_Ignore_Unknown_Options --
   --------------------------------

   procedure Set_Ignore_Unknown_Options
      (Self           : Goption_Context;
       Ignore_Unknown : Boolean)
   is
      procedure Internal
         (Self           : System.Address;
          Ignore_Unknown : Glib.Gboolean);
      pragma Import (C, Internal, "g_option_context_set_ignore_unknown_options");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Ignore_Unknown));
   end Set_Ignore_Unknown_Options;

   --------------------
   -- Set_Main_Group --
   --------------------

   procedure Set_Main_Group (Self : Goption_Context; Group : GOption_Group) is
      procedure Internal (Self : System.Address; Group : GOption_Group);
      pragma Import (C, Internal, "g_option_context_set_main_group");
   begin
      Internal (Get_Object (Self), Group);
   end Set_Main_Group;

   -----------------
   -- Set_Summary --
   -----------------

   procedure Set_Summary
      (Self    : Goption_Context;
       Summary : UTF8_String := "")
   is
      procedure Internal
         (Self    : System.Address;
          Summary : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_option_context_set_summary");
      Tmp_Summary : Gtkada.Types.Chars_Ptr;
   begin
      if Summary = "" then
         Tmp_Summary := Gtkada.Types.Null_Ptr;
      else
         Tmp_Summary := New_String (Summary);
      end if;
      Internal (Get_Object (Self), Tmp_Summary);
      Free (Tmp_Summary);
   end Set_Summary;

   ------------------------
   -- Set_Translate_Func --
   ------------------------

   procedure Set_Translate_Func
      (Self           : Goption_Context;
       Func           : Gtranslate_Func;
       Destroy_Notify : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Func = null then
         C_G_Option_Context_Set_Translate_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Destroy_Notify);
      else
         C_G_Option_Context_Set_Translate_Func (Get_Object (Self), Internal_Gtranslate_Func'Address, To_Address (Func), Destroy_Notify);
      end if;
   end Set_Translate_Func;

   package body Set_Translate_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtranslate_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtranslate_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtranslate_Func, System.Address);

      function Internal_Cb
         (Str  : Gtkada.Types.Chars_Ptr;
          Data : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Convention (C, Internal_Cb);
      --  The type of functions which are used to translate user-visible
      --  strings, for <option>--help</option> output.
      --  "str": the untranslated string
      --  "data": user data specified when installing the function, e.g. in
      --  g_option_group_set_translate_func

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Str  : Gtkada.Types.Chars_Ptr;
          Data : System.Address) return Gtkada.Types.Chars_Ptr
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return New_String (To_Gtranslate_Func (D.Func) (Gtkada.Bindings.Value_Allowing_Null (Str), D.Data.all));
      end Internal_Cb;

      ------------------------
      -- Set_Translate_Func --
      ------------------------

      procedure Set_Translate_Func
         (Self           : Glib.Option.Goption_Context;
          Func           : Gtranslate_Func;
          Data           : User_Data_Type;
          Destroy_Notify : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Func = null then
            C_G_Option_Context_Set_Translate_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Destroy_Notify);
         else
            D := Users.Build (To_Address (Func), Data);
            C_G_Option_Context_Set_Translate_Func (Get_Object (Self), Internal_Cb'Address, D, Destroy_Notify);
         end if;
      end Set_Translate_Func;

   end Set_Translate_Func_User_Data;

   ----------------------------
   -- Set_Translation_Domain --
   ----------------------------

   procedure Set_Translation_Domain
      (Self   : Goption_Context;
       Domain : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          Domain : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_option_context_set_translation_domain");
      Tmp_Domain : Gtkada.Types.Chars_Ptr := New_String (Domain);
   begin
      Internal (Get_Object (Self), Tmp_Domain);
      Free (Tmp_Domain);
   end Set_Translation_Domain;

   -----------
   -- G_New --
   -----------

   function G_New
      (Parameter_String : UTF8_String := "") return Goption_Context
   is
      function Internal
         (Parameter_String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_option_context_new");
      Tmp_Parameter_String : Gtkada.Types.Chars_Ptr;
      Tmp_Return           : System.Address;
   begin
      if Parameter_String = "" then
         Tmp_Parameter_String := Gtkada.Types.Null_Ptr;
      else
         Tmp_Parameter_String := New_String (Parameter_String);
      end if;
      Tmp_Return := Internal (Tmp_Parameter_String);
      Free (Tmp_Parameter_String);
      return From_Object (Tmp_Return);
   end G_New;

end Glib.Option;
