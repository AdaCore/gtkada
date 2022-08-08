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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Print_Settings is

   procedure C_Gtk_Print_Settings_Foreach
      (Self      : System.Address;
       Func      : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Print_Settings_Foreach, "gtk_print_settings_foreach");
   --  Calls Func for each key-value pair of Settings.
   --  Since: gtk+ 2.10
   --  "func": the function to call
   --  "user_data": user data for Func

   function To_Gtk_Print_Settings_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Print_Settings_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Print_Settings_Func, System.Address);

   procedure Internal_Gtk_Print_Settings_Func
      (Key       : Gtkada.Types.Chars_Ptr;
       Value     : Gtkada.Types.Chars_Ptr;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Print_Settings_Func);

   --------------------------------------
   -- Internal_Gtk_Print_Settings_Func --
   --------------------------------------

   procedure Internal_Gtk_Print_Settings_Func
      (Key       : Gtkada.Types.Chars_Ptr;
       Value     : Gtkada.Types.Chars_Ptr;
       User_Data : System.Address)
   is
      Func : constant Gtk_Print_Settings_Func := To_Gtk_Print_Settings_Func (User_Data);
   begin
      Func (Gtkada.Bindings.Value_Allowing_Null (Key), Gtkada.Bindings.Value_Allowing_Null (Value));
   end Internal_Gtk_Print_Settings_Func;

   package Type_Conversion_Gtk_Print_Settings is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Print_Settings_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Print_Settings);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Print_Settings) is
   begin
      Self := new Gtk_Print_Settings_Record;
      Gtk.Print_Settings.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
      (Self      : out Gtk_Print_Settings;
       File_Name : UTF8_String)
   is
   begin
      Self := new Gtk_Print_Settings_Record;
      Gtk.Print_Settings.Initialize_From_File (Self, File_Name);
   end Gtk_New_From_File;

   ---------------------------
   -- Gtk_New_From_Gvariant --
   ---------------------------

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_Print_Settings;
       Variant : Glib.Variant.Gvariant)
   is
   begin
      Self := new Gtk_Print_Settings_Record;
      Gtk.Print_Settings.Initialize_From_Gvariant (Self, Variant);
   end Gtk_New_From_Gvariant;

   ---------------------------
   -- Gtk_New_From_Key_File --
   ---------------------------

   procedure Gtk_New_From_Key_File
      (Self       : out Gtk_Print_Settings;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
   begin
      Self := new Gtk_Print_Settings_Record;
      Gtk.Print_Settings.Initialize_From_Key_File (Self, Key_File, Group_Name);
   end Gtk_New_From_Key_File;

   ----------------------------
   -- Gtk_Print_Settings_New --
   ----------------------------

   function Gtk_Print_Settings_New return Gtk_Print_Settings is
      Self : constant Gtk_Print_Settings := new Gtk_Print_Settings_Record;
   begin
      Gtk.Print_Settings.Initialize (Self);
      return Self;
   end Gtk_Print_Settings_New;

   --------------------------------------
   -- Gtk_Print_Settings_New_From_File --
   --------------------------------------

   function Gtk_Print_Settings_New_From_File
      (File_Name : UTF8_String) return Gtk_Print_Settings
   is
      Self : constant Gtk_Print_Settings := new Gtk_Print_Settings_Record;
   begin
      Gtk.Print_Settings.Initialize_From_File (Self, File_Name);
      return Self;
   end Gtk_Print_Settings_New_From_File;

   ------------------------------------------
   -- Gtk_Print_Settings_New_From_Gvariant --
   ------------------------------------------

   function Gtk_Print_Settings_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Print_Settings
   is
      Self : constant Gtk_Print_Settings := new Gtk_Print_Settings_Record;
   begin
      Gtk.Print_Settings.Initialize_From_Gvariant (Self, Variant);
      return Self;
   end Gtk_Print_Settings_New_From_Gvariant;

   ------------------------------------------
   -- Gtk_Print_Settings_New_From_Key_File --
   ------------------------------------------

   function Gtk_Print_Settings_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Gtk_Print_Settings
   is
      Self : constant Gtk_Print_Settings := new Gtk_Print_Settings_Record;
   begin
      Gtk.Print_Settings.Initialize_From_Key_File (Self, Key_File, Group_Name);
      return Self;
   end Gtk_Print_Settings_New_From_Key_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Print_Settings_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Self      : not null access Gtk_Print_Settings_Record'Class;
       File_Name : UTF8_String)
   is
      function Internal
         (File_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new_from_file");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_File_Name);
         Free (Tmp_File_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_File;

   ------------------------------
   -- Initialize_From_Gvariant --
   ------------------------------

   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_Print_Settings_Record'Class;
       Variant : Glib.Variant.Gvariant)
   is
      function Internal (Variant : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new_from_gvariant");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Variant)));
      end if;
   end Initialize_From_Gvariant;

   ------------------------------
   -- Initialize_From_Key_File --
   ------------------------------

   procedure Initialize_From_Key_File
      (Self       : not null access Gtk_Print_Settings_Record'Class;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
      function Internal
         (Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new_from_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      if not Self.Is_Created then
         if Group_Name = "" then
            Tmp_Group_Name := Gtkada.Types.Null_Ptr;
         else
            Tmp_Group_Name := New_String (Group_Name);
         end if;
         Tmp_Return := Internal (Key_File, Tmp_Group_Name);
         Free (Tmp_Group_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Key_File;

   ----------
   -- Copy --
   ----------

   function Copy
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk_Print_Settings
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_copy");
      Stub_Gtk_Print_Settings : Gtk_Print_Settings_Record;
   begin
      return Gtk.Print_Settings.Gtk_Print_Settings (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Print_Settings));
   end Copy;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Self : not null access Gtk_Print_Settings_Record;
       Func : Gtk_Print_Settings_Func)
   is
   begin
      if Func = null then
         C_Gtk_Print_Settings_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Print_Settings_Foreach (Get_Object (Self), Internal_Gtk_Print_Settings_Func'Address, To_Address (Func));
      end if;
   end Foreach;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Print_Settings_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Print_Settings_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Print_Settings_Func, System.Address);

      procedure Internal_Cb
         (Key       : Gtkada.Types.Chars_Ptr;
          Value     : Gtkada.Types.Chars_Ptr;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Self      : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class;
          Func      : Gtk_Print_Settings_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Print_Settings_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), User_Data);
            C_Gtk_Print_Settings_Foreach (Get_Object (Self), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Key       : Gtkada.Types.Chars_Ptr;
          Value     : Gtkada.Types.Chars_Ptr;
          User_Data : System.Address)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         To_Gtk_Print_Settings_Func (D.Func) (Gtkada.Bindings.Value_Allowing_Null (Key), Gtkada.Bindings.Value_Allowing_Null (Value), D.Data.all);
      end Internal_Cb;

   end Foreach_User_Data;

   ---------
   -- Get --
   ---------

   function Get
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get;

   --------------
   -- Get_Bool --
   --------------

   function Get_Bool
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_bool");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Get_Bool;

   -----------------
   -- Get_Collate --
   -----------------

   function Get_Collate
      (Self : not null access Gtk_Print_Settings_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_collate");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Collate;

   ------------------------
   -- Get_Default_Source --
   ------------------------

   function Get_Default_Source
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_default_source");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Default_Source;

   ----------------
   -- Get_Dither --
   ----------------

   function Get_Dither
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_dither");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Dither;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_double");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gdouble;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Double;

   -----------------------------
   -- Get_Double_With_Default --
   -----------------------------

   function Get_Double_With_Default
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Def  : Gdouble) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr;
          Def  : Gdouble) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_double_with_default");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gdouble;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Def);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Double_With_Default;

   ----------------
   -- Get_Duplex --
   ----------------

   function Get_Duplex
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Duplex
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Print_Duplex;
      pragma Import (C, Internal, "gtk_print_settings_get_duplex");
   begin
      return Internal (Get_Object (Self));
   end Get_Duplex;

   --------------------
   -- Get_Finishings --
   --------------------

   function Get_Finishings
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_finishings");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Finishings;

   -------------
   -- Get_Int --
   -------------

   function Get_Int
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_int");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Int;

   --------------------------
   -- Get_Int_With_Default --
   --------------------------

   function Get_Int_With_Default
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Def  : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr;
          Def  : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_int_with_default");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Def);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Int_With_Default;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_length");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gdouble;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Unit);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Length;

   --------------------
   -- Get_Media_Type --
   --------------------

   function Get_Media_Type
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_media_type");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Media_Type;

   ------------------
   -- Get_N_Copies --
   ------------------

   function Get_N_Copies
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_n_copies");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Copies;

   -------------------
   -- Get_Number_Up --
   -------------------

   function Get_Number_Up
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_number_up");
   begin
      return Internal (Get_Object (Self));
   end Get_Number_Up;

   --------------------------
   -- Get_Number_Up_Layout --
   --------------------------

   function Get_Number_Up_Layout
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Number_Up_Layout
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Number_Up_Layout;
      pragma Import (C, Internal, "gtk_print_settings_get_number_up_layout");
   begin
      return Internal (Get_Object (Self));
   end Get_Number_Up_Layout;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Page_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Page_Orientation;
      pragma Import (C, Internal, "gtk_print_settings_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   --------------------
   -- Get_Output_Bin --
   --------------------

   function Get_Output_Bin
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_output_bin");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Output_Bin;

   ---------------------
   -- Get_Page_Ranges --
   ---------------------

   function Get_Page_Ranges
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk_Page_Range_Array
   is
      type Internal_Page_Range_Array is
        array (Natural) of Gtk_Page_Range_Record;
      pragma Convention (C, Internal_Page_Range_Array);

      type Internal_Page_Range_Array_Access is
        access Internal_Page_Range_Array;
      pragma Convention (C, Internal_Page_Range_Array_Access);

      procedure Free (Item : in out Internal_Page_Range_Array_Access);
      pragma Import (C, Free, "g_free");

      function Internal
        (Settings   : System.Address;
         Num_Ranges : access Gint)
         return Internal_Page_Range_Array_Access;
      pragma Import (C, Internal, "gtk_print_settings_get_page_ranges");

      Len    : aliased Gint;
      Result : Internal_Page_Range_Array_Access;
   begin
      Result := Internal (Get_Object (Self), Len'Access);
      declare
         Ranges : constant Gtk_Page_Range_Array :=
           Gtk_Page_Range_Array (Result (0 .. Natural (Len)));
      begin
         Free (Result);
         return Ranges;
      end;
   end Get_Page_Ranges;

   ------------------
   -- Get_Page_Set --
   ------------------

   function Get_Page_Set
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Page_Set
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Page_Set;
      pragma Import (C, Internal, "gtk_print_settings_get_page_set");
   begin
      return Internal (Get_Object (Self));
   end Get_Page_Set;

   ----------------------
   -- Get_Paper_Height --
   ----------------------

   function Get_Paper_Height
      (Self : not null access Gtk_Print_Settings_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_paper_height");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Paper_Height;

   --------------------
   -- Get_Paper_Size --
   --------------------

   function Get_Paper_Size
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Paper_Size.Gtk_Paper_Size
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_get_paper_size");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Paper_Size;

   ---------------------
   -- Get_Paper_Width --
   ---------------------

   function Get_Paper_Width
      (Self : not null access Gtk_Print_Settings_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_paper_width");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Paper_Width;

   ---------------------
   -- Get_Print_Pages --
   ---------------------

   function Get_Print_Pages
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Pages
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Print_Pages;
      pragma Import (C, Internal, "gtk_print_settings_get_print_pages");
   begin
      return Internal (Get_Object (Self));
   end Get_Print_Pages;

   -----------------
   -- Get_Printer --
   -----------------

   function Get_Printer
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_printer");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Printer;

   ---------------------
   -- Get_Printer_Lpi --
   ---------------------

   function Get_Printer_Lpi
      (Self : not null access Gtk_Print_Settings_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_printer_lpi");
   begin
      return Internal (Get_Object (Self));
   end Get_Printer_Lpi;

   -----------------
   -- Get_Quality --
   -----------------

   function Get_Quality
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Quality
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Print_Quality;
      pragma Import (C, Internal, "gtk_print_settings_get_quality");
   begin
      return Internal (Get_Object (Self));
   end Get_Quality;

   --------------------
   -- Get_Resolution --
   --------------------

   function Get_Resolution
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_resolution");
   begin
      return Internal (Get_Object (Self));
   end Get_Resolution;

   ----------------------
   -- Get_Resolution_X --
   ----------------------

   function Get_Resolution_X
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_resolution_x");
   begin
      return Internal (Get_Object (Self));
   end Get_Resolution_X;

   ----------------------
   -- Get_Resolution_Y --
   ----------------------

   function Get_Resolution_Y
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_resolution_y");
   begin
      return Internal (Get_Object (Self));
   end Get_Resolution_Y;

   -----------------
   -- Get_Reverse --
   -----------------

   function Get_Reverse
      (Self : not null access Gtk_Print_Settings_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_reverse");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Reverse;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
      (Self : not null access Gtk_Print_Settings_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_scale");
   begin
      return Internal (Get_Object (Self));
   end Get_Scale;

   -------------------
   -- Get_Use_Color --
   -------------------

   function Get_Use_Color
      (Self : not null access Gtk_Print_Settings_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_use_color");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Color;

   -------------
   -- Has_Key --
   -------------

   function Has_Key
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_has_key");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Has_Key;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
      (Self      : not null access Gtk_Print_Settings_Record;
       File_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          File_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_load_file");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_File_Name);
      Free (Tmp_File_Name);
      return Tmp_Return /= 0;
   end Load_File;

   -------------------
   -- Load_Key_File --
   -------------------

   function Load_Key_File
      (Self       : not null access Gtk_Print_Settings_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Boolean
   is
      function Internal
         (Self       : System.Address;
          Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_load_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : Glib.Gboolean;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
      return Tmp_Return /= 0;
   end Load_Key_File;

   ---------
   -- Set --
   ---------

   procedure Set
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set");
      Tmp_Key   : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Value : Gtkada.Types.Chars_Ptr;
   begin
      if Value = "" then
         Tmp_Value := Gtkada.Types.Null_Ptr;
      else
         Tmp_Value := New_String (Value);
      end if;
      Internal (Get_Object (Self), Tmp_Key, Tmp_Value);
      Free (Tmp_Value);
      Free (Tmp_Key);
   end Set;

   --------------
   -- Set_Bool --
   --------------

   procedure Set_Bool
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Boolean)
   is
      procedure Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_bool");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key, Boolean'Pos (Value));
      Free (Tmp_Key);
   end Set_Bool;

   -----------------
   -- Set_Collate --
   -----------------

   procedure Set_Collate
      (Self    : not null access Gtk_Print_Settings_Record;
       Collate : Boolean)
   is
      procedure Internal (Self : System.Address; Collate : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_collate");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Collate));
   end Set_Collate;

   ------------------------
   -- Set_Default_Source --
   ------------------------

   procedure Set_Default_Source
      (Self           : not null access Gtk_Print_Settings_Record;
       Default_Source : UTF8_String)
   is
      procedure Internal
         (Self           : System.Address;
          Default_Source : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set_default_source");
      Tmp_Default_Source : Gtkada.Types.Chars_Ptr := New_String (Default_Source);
   begin
      Internal (Get_Object (Self), Tmp_Default_Source);
      Free (Tmp_Default_Source);
   end Set_Default_Source;

   ----------------
   -- Set_Dither --
   ----------------

   procedure Set_Dither
      (Self   : not null access Gtk_Print_Settings_Record;
       Dither : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          Dither : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set_dither");
      Tmp_Dither : Gtkada.Types.Chars_Ptr := New_String (Dither);
   begin
      Internal (Get_Object (Self), Tmp_Dither);
      Free (Tmp_Dither);
   end Set_Dither;

   ----------------
   -- Set_Double --
   ----------------

   procedure Set_Double
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Gdouble)
   is
      procedure Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gdouble);
      pragma Import (C, Internal, "gtk_print_settings_set_double");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
   end Set_Double;

   ----------------
   -- Set_Duplex --
   ----------------

   procedure Set_Duplex
      (Self   : not null access Gtk_Print_Settings_Record;
       Duplex : Gtk.Enums.Gtk_Print_Duplex)
   is
      procedure Internal
         (Self   : System.Address;
          Duplex : Gtk.Enums.Gtk_Print_Duplex);
      pragma Import (C, Internal, "gtk_print_settings_set_duplex");
   begin
      Internal (Get_Object (Self), Duplex);
   end Set_Duplex;

   --------------------
   -- Set_Finishings --
   --------------------

   procedure Set_Finishings
      (Self       : not null access Gtk_Print_Settings_Record;
       Finishings : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Finishings : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set_finishings");
      Tmp_Finishings : Gtkada.Types.Chars_Ptr := New_String (Finishings);
   begin
      Internal (Get_Object (Self), Tmp_Finishings);
      Free (Tmp_Finishings);
   end Set_Finishings;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_int");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
   end Set_Int;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Gdouble;
       Unit  : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gdouble;
          Unit  : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_settings_set_length");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key, Value, Unit);
      Free (Tmp_Key);
   end Set_Length;

   --------------------
   -- Set_Media_Type --
   --------------------

   procedure Set_Media_Type
      (Self       : not null access Gtk_Print_Settings_Record;
       Media_Type : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Media_Type : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set_media_type");
      Tmp_Media_Type : Gtkada.Types.Chars_Ptr := New_String (Media_Type);
   begin
      Internal (Get_Object (Self), Tmp_Media_Type);
      Free (Tmp_Media_Type);
   end Set_Media_Type;

   ------------------
   -- Set_N_Copies --
   ------------------

   procedure Set_N_Copies
      (Self       : not null access Gtk_Print_Settings_Record;
       Num_Copies : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Num_Copies : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_n_copies");
   begin
      Internal (Get_Object (Self), Num_Copies);
   end Set_N_Copies;

   -------------------
   -- Set_Number_Up --
   -------------------

   procedure Set_Number_Up
      (Self      : not null access Gtk_Print_Settings_Record;
       Number_Up : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Number_Up : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_number_up");
   begin
      Internal (Get_Object (Self), Number_Up);
   end Set_Number_Up;

   --------------------------
   -- Set_Number_Up_Layout --
   --------------------------

   procedure Set_Number_Up_Layout
      (Self             : not null access Gtk_Print_Settings_Record;
       Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout)
   is
      procedure Internal
         (Self             : System.Address;
          Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout);
      pragma Import (C, Internal, "gtk_print_settings_set_number_up_layout");
   begin
      Internal (Get_Object (Self), Number_Up_Layout);
   end Set_Number_Up_Layout;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Print_Settings_Record;
       Orientation : Gtk.Enums.Gtk_Page_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Page_Orientation);
      pragma Import (C, Internal, "gtk_print_settings_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   --------------------
   -- Set_Output_Bin --
   --------------------

   procedure Set_Output_Bin
      (Self       : not null access Gtk_Print_Settings_Record;
       Output_Bin : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Output_Bin : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set_output_bin");
      Tmp_Output_Bin : Gtkada.Types.Chars_Ptr := New_String (Output_Bin);
   begin
      Internal (Get_Object (Self), Tmp_Output_Bin);
      Free (Tmp_Output_Bin);
   end Set_Output_Bin;

   ---------------------
   -- Set_Page_Ranges --
   ---------------------

   procedure Set_Page_Ranges
      (Self        : not null access Gtk_Print_Settings_Record;
       Page_Ranges : Gtk_Page_Range_Array)
   is
      procedure Internal
        (Settings    : System.Address;
         Page_Ranges : System.Address;
         Num_Ranges  : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_page_ranges");
   begin
      Internal
        (Get_Object (Self),
         Page_Ranges (Page_Ranges'First)'Address,
         Page_Ranges'Length);
   end Set_Page_Ranges;

   ------------------
   -- Set_Page_Set --
   ------------------

   procedure Set_Page_Set
      (Self     : not null access Gtk_Print_Settings_Record;
       Page_Set : Gtk.Enums.Gtk_Page_Set)
   is
      procedure Internal
         (Self     : System.Address;
          Page_Set : Gtk.Enums.Gtk_Page_Set);
      pragma Import (C, Internal, "gtk_print_settings_set_page_set");
   begin
      Internal (Get_Object (Self), Page_Set);
   end Set_Page_Set;

   ----------------------
   -- Set_Paper_Height --
   ----------------------

   procedure Set_Paper_Height
      (Self   : not null access Gtk_Print_Settings_Record;
       Height : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self   : System.Address;
          Height : Gdouble;
          Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_settings_set_paper_height");
   begin
      Internal (Get_Object (Self), Height, Unit);
   end Set_Paper_Height;

   --------------------
   -- Set_Paper_Size --
   --------------------

   procedure Set_Paper_Size
      (Self       : not null access Gtk_Print_Settings_Record;
       Paper_Size : Gtk.Paper_Size.Gtk_Paper_Size)
   is
      procedure Internal
         (Self       : System.Address;
          Paper_Size : System.Address);
      pragma Import (C, Internal, "gtk_print_settings_set_paper_size");
   begin
      Internal (Get_Object (Self), Get_Object (Paper_Size));
   end Set_Paper_Size;

   ---------------------
   -- Set_Paper_Width --
   ---------------------

   procedure Set_Paper_Width
      (Self  : not null access Gtk_Print_Settings_Record;
       Width : Gdouble;
       Unit  : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self  : System.Address;
          Width : Gdouble;
          Unit  : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_settings_set_paper_width");
   begin
      Internal (Get_Object (Self), Width, Unit);
   end Set_Paper_Width;

   ---------------------
   -- Set_Print_Pages --
   ---------------------

   procedure Set_Print_Pages
      (Self  : not null access Gtk_Print_Settings_Record;
       Pages : Gtk.Enums.Gtk_Print_Pages)
   is
      procedure Internal
         (Self  : System.Address;
          Pages : Gtk.Enums.Gtk_Print_Pages);
      pragma Import (C, Internal, "gtk_print_settings_set_print_pages");
   begin
      Internal (Get_Object (Self), Pages);
   end Set_Print_Pages;

   -----------------
   -- Set_Printer --
   -----------------

   procedure Set_Printer
      (Self    : not null access Gtk_Print_Settings_Record;
       Printer : UTF8_String)
   is
      procedure Internal
         (Self    : System.Address;
          Printer : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_set_printer");
      Tmp_Printer : Gtkada.Types.Chars_Ptr := New_String (Printer);
   begin
      Internal (Get_Object (Self), Tmp_Printer);
      Free (Tmp_Printer);
   end Set_Printer;

   ---------------------
   -- Set_Printer_Lpi --
   ---------------------

   procedure Set_Printer_Lpi
      (Self : not null access Gtk_Print_Settings_Record;
       Lpi  : Gdouble)
   is
      procedure Internal (Self : System.Address; Lpi : Gdouble);
      pragma Import (C, Internal, "gtk_print_settings_set_printer_lpi");
   begin
      Internal (Get_Object (Self), Lpi);
   end Set_Printer_Lpi;

   -----------------
   -- Set_Quality --
   -----------------

   procedure Set_Quality
      (Self    : not null access Gtk_Print_Settings_Record;
       Quality : Gtk.Enums.Gtk_Print_Quality)
   is
      procedure Internal
         (Self    : System.Address;
          Quality : Gtk.Enums.Gtk_Print_Quality);
      pragma Import (C, Internal, "gtk_print_settings_set_quality");
   begin
      Internal (Get_Object (Self), Quality);
   end Set_Quality;

   --------------------
   -- Set_Resolution --
   --------------------

   procedure Set_Resolution
      (Self       : not null access Gtk_Print_Settings_Record;
       Resolution : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Resolution : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_resolution");
   begin
      Internal (Get_Object (Self), Resolution);
   end Set_Resolution;

   -----------------------
   -- Set_Resolution_Xy --
   -----------------------

   procedure Set_Resolution_Xy
      (Self         : not null access Gtk_Print_Settings_Record;
       Resolution_X : Glib.Gint;
       Resolution_Y : Glib.Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Resolution_X : Glib.Gint;
          Resolution_Y : Glib.Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_resolution_xy");
   begin
      Internal (Get_Object (Self), Resolution_X, Resolution_Y);
   end Set_Resolution_Xy;

   -----------------
   -- Set_Reverse --
   -----------------

   procedure Set_Reverse
      (Self        : not null access Gtk_Print_Settings_Record;
       Gtk_Reverse : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Gtk_Reverse : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_reverse");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Gtk_Reverse));
   end Set_Reverse;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
      (Self  : not null access Gtk_Print_Settings_Record;
       Scale : Gdouble)
   is
      procedure Internal (Self : System.Address; Scale : Gdouble);
      pragma Import (C, Internal, "gtk_print_settings_set_scale");
   begin
      Internal (Get_Object (Self), Scale);
   end Set_Scale;

   -------------------
   -- Set_Use_Color --
   -------------------

   procedure Set_Use_Color
      (Self      : not null access Gtk_Print_Settings_Record;
       Use_Color : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Color : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_use_color");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Color));
   end Set_Use_Color;

   -------------
   -- To_File --
   -------------

   function To_File
      (Self      : not null access Gtk_Print_Settings_Record;
       File_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          File_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_to_file");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_File_Name);
      Free (Tmp_File_Name);
      return Tmp_Return /= 0;
   end To_File;

   -----------------
   -- To_Gvariant --
   -----------------

   function To_Gvariant
      (Self : not null access Gtk_Print_Settings_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_to_gvariant");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Gvariant;

   -----------------
   -- To_Key_File --
   -----------------

   procedure To_Key_File
      (Self       : not null access Gtk_Print_Settings_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
      procedure Internal
         (Self       : System.Address;
          Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_to_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Internal (Get_Object (Self), Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
   end To_Key_File;

   -----------
   -- Unset --
   -----------

   procedure Unset
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_settings_unset");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
   end Unset;

end Gtk.Print_Settings;
