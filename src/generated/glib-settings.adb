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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.Settings is

   package Type_Conversion_Gsettings is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gsettings_Record);
   pragma Unreferenced (Type_Conversion_Gsettings);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gsettings; Schema_Id : UTF8_String) is
   begin
      Self := new Gsettings_Record;
      Glib.Settings.Initialize (Self, Schema_Id);
   end G_New;

   ----------------
   -- G_New_Full --
   ----------------

   procedure G_New_Full
      (Self    : out Gsettings;
       Schema  : Glib.Settings_Schema.Gsettings_Schema;
       Backend : access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path    : UTF8_String := "")
   is
   begin
      Self := new Gsettings_Record;
      Glib.Settings.Initialize_Full (Self, Schema, Backend, Path);
   end G_New_Full;

   ------------------------
   -- G_New_With_Backend --
   ------------------------

   procedure G_New_With_Backend
      (Self      : out Gsettings;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class)
   is
   begin
      Self := new Gsettings_Record;
      Glib.Settings.Initialize_With_Backend (Self, Schema_Id, Backend);
   end G_New_With_Backend;

   ---------------------------------
   -- G_New_With_Backend_And_Path --
   ---------------------------------

   procedure G_New_With_Backend_And_Path
      (Self      : out Gsettings;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path      : UTF8_String)
   is
   begin
      Self := new Gsettings_Record;
      Glib.Settings.Initialize_With_Backend_And_Path (Self, Schema_Id, Backend, Path);
   end G_New_With_Backend_And_Path;

   ---------------------
   -- G_New_With_Path --
   ---------------------

   procedure G_New_With_Path
      (Self      : out Gsettings;
       Schema_Id : UTF8_String;
       Path      : UTF8_String)
   is
   begin
      Self := new Gsettings_Record;
      Glib.Settings.Initialize_With_Path (Self, Schema_Id, Path);
   end G_New_With_Path;

   -------------------
   -- Gsettings_New --
   -------------------

   function Gsettings_New (Schema_Id : UTF8_String) return Gsettings is
      Self : constant Gsettings := new Gsettings_Record;
   begin
      Glib.Settings.Initialize (Self, Schema_Id);
      return Self;
   end Gsettings_New;

   ------------------------
   -- Gsettings_New_Full --
   ------------------------

   function Gsettings_New_Full
      (Schema  : Glib.Settings_Schema.Gsettings_Schema;
       Backend : access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path    : UTF8_String := "") return Gsettings
   is
      Self : constant Gsettings := new Gsettings_Record;
   begin
      Glib.Settings.Initialize_Full (Self, Schema, Backend, Path);
      return Self;
   end Gsettings_New_Full;

   --------------------------------
   -- Gsettings_New_With_Backend --
   --------------------------------

   function Gsettings_New_With_Backend
      (Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class)
       return Gsettings
   is
      Self : constant Gsettings := new Gsettings_Record;
   begin
      Glib.Settings.Initialize_With_Backend (Self, Schema_Id, Backend);
      return Self;
   end Gsettings_New_With_Backend;

   -----------------------------------------
   -- Gsettings_New_With_Backend_And_Path --
   -----------------------------------------

   function Gsettings_New_With_Backend_And_Path
      (Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path      : UTF8_String) return Gsettings
   is
      Self : constant Gsettings := new Gsettings_Record;
   begin
      Glib.Settings.Initialize_With_Backend_And_Path (Self, Schema_Id, Backend, Path);
      return Self;
   end Gsettings_New_With_Backend_And_Path;

   -----------------------------
   -- Gsettings_New_With_Path --
   -----------------------------

   function Gsettings_New_With_Path
      (Schema_Id : UTF8_String;
       Path      : UTF8_String) return Gsettings
   is
      Self : constant Gsettings := new Gsettings_Record;
   begin
      Glib.Settings.Initialize_With_Path (Self, Schema_Id, Path);
      return Self;
   end Gsettings_New_With_Path;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String)
   is
      function Internal
         (Schema_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_new");
      Tmp_Schema_Id : Gtkada.Types.Chars_Ptr := New_String (Schema_Id);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Schema_Id);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Schema_Id);
   end Initialize;

   ---------------------
   -- Initialize_Full --
   ---------------------

   procedure Initialize_Full
      (Self    : not null access Gsettings_Record'Class;
       Schema  : Glib.Settings_Schema.Gsettings_Schema;
       Backend : access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path    : UTF8_String := "")
   is
      function Internal
         (Schema  : System.Address;
          Backend : System.Address;
          Path    : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_new_full");
      Tmp_Path   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Path :=
           (if Path = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Path));
         Tmp_Return := Internal (Get_Object (Schema), Get_Object_Or_Null (GObject (Backend)), Tmp_Path);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Path);
   end Initialize_Full;

   -----------------------------
   -- Initialize_With_Backend --
   -----------------------------

   procedure Initialize_With_Backend
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class)
   is
      function Internal
         (Schema_Id : Gtkada.Types.Chars_Ptr;
          Backend   : System.Address) return System.Address;
      pragma Import (C, Internal, "g_settings_new_with_backend");
      Tmp_Schema_Id : Gtkada.Types.Chars_Ptr := New_String (Schema_Id);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Schema_Id, Get_Object (Backend));
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Schema_Id);
   end Initialize_With_Backend;

   --------------------------------------
   -- Initialize_With_Backend_And_Path --
   --------------------------------------

   procedure Initialize_With_Backend_And_Path
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String;
       Backend   : not null access Glib.Settings_Backend.Gsettings_Backend_Record'Class;
       Path      : UTF8_String)
   is
      function Internal
         (Schema_Id : Gtkada.Types.Chars_Ptr;
          Backend   : System.Address;
          Path      : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_new_with_backend_and_path");
      Tmp_Schema_Id : Gtkada.Types.Chars_Ptr := New_String (Schema_Id);
      Tmp_Path      : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Schema_Id, Get_Object (Backend), Tmp_Path);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Path);
      Free (Tmp_Schema_Id);
   end Initialize_With_Backend_And_Path;

   --------------------------
   -- Initialize_With_Path --
   --------------------------

   procedure Initialize_With_Path
      (Self      : not null access Gsettings_Record'Class;
       Schema_Id : UTF8_String;
       Path      : UTF8_String)
   is
      function Internal
         (Schema_Id : Gtkada.Types.Chars_Ptr;
          Path      : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_new_with_path");
      Tmp_Schema_Id : Gtkada.Types.Chars_Ptr := New_String (Schema_Id);
      Tmp_Path      : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Schema_Id, Tmp_Path);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Path);
      Free (Tmp_Schema_Id);
   end Initialize_With_Path;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : not null access Gsettings_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_settings_apply");
   begin
      Internal (Get_Object (Self));
   end Apply;

   ----------
   -- Bind --
   ----------

   procedure Bind
      (Self     : not null access Gsettings_Record;
       Key      : UTF8_String;
       Object   : not null access Glib.Object.GObject_Record'Class;
       Property : UTF8_String;
       Flags    : GSettings_Bind_Flags)
   is
      procedure Internal
         (Self     : System.Address;
          Key      : Gtkada.Types.Chars_Ptr;
          Object   : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          Flags    : GSettings_Bind_Flags);
      pragma Import (C, Internal, "g_settings_bind");
      Tmp_Key      : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Self), Tmp_Key, Get_Object (Object), Tmp_Property, Flags);
      Free (Tmp_Property);
      Free (Tmp_Key);
   end Bind;

   -------------------
   -- Bind_Writable --
   -------------------

   procedure Bind_Writable
      (Self     : not null access Gsettings_Record;
       Key      : UTF8_String;
       Object   : not null access Glib.Object.GObject_Record'Class;
       Property : UTF8_String;
       Inverted : Boolean)
   is
      procedure Internal
         (Self     : System.Address;
          Key      : Gtkada.Types.Chars_Ptr;
          Object   : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          Inverted : Glib.Gboolean);
      pragma Import (C, Internal, "g_settings_bind_writable");
      Tmp_Key      : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Self), Tmp_Key, Get_Object (Object), Tmp_Property, Boolean'Pos (Inverted));
      Free (Tmp_Property);
      Free (Tmp_Key);
   end Bind_Writable;

   -------------------
   -- Create_Action --
   -------------------

   function Create_Action
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Action.Gaction
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Action.Gaction;
      pragma Import (C, Internal, "g_settings_create_action");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Action.Gaction;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Create_Action;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_get_boolean");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Get_Boolean;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gsettings_Record;
       Name : UTF8_String) return Gsettings
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_get_child");
      Tmp_Name       : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_Gsettings : Gsettings_Record;
      Tmp_Return     : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Glib.Settings.Gsettings (Get_User_Data (Tmp_Return, Stub_Gsettings));
   end Get_Child;

   -----------------------
   -- Get_Default_Value --
   -----------------------

   function Get_Default_Value
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_get_default_value");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return From_Object (Tmp_Return);
   end Get_Default_Value;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gdouble;
      pragma Import (C, Internal, "g_settings_get_double");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gdouble;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Double;

   --------------
   -- Get_Enum --
   --------------

   function Get_Enum
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gint;
      pragma Import (C, Internal, "g_settings_get_enum");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Enum;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Guint
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Guint;
      pragma Import (C, Internal, "g_settings_get_flags");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Flags;

   -----------------------
   -- Get_Has_Unapplied --
   -----------------------

   function Get_Has_Unapplied
      (Self : not null access Gsettings_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_get_has_unapplied");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Unapplied;

   -------------
   -- Get_Int --
   -------------

   function Get_Int
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gint;
      pragma Import (C, Internal, "g_settings_get_int");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Int;

   ---------------
   -- Get_Int64 --
   ---------------

   function Get_Int64
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Gint64
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gint64;
      pragma Import (C, Internal, "g_settings_get_int64");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gint64;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Int64;

   ---------------
   -- Get_Range --
   ---------------

   function Get_Range
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_get_range");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return From_Object (Tmp_Return);
   end Get_Range;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_settings_get_string");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Get_String;

   --------------
   -- Get_Strv --
   --------------

   function Get_Strv
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_get_strv");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return To_String_List_And_Free (Tmp_Return);
   end Get_Strv;

   --------------
   -- Get_Uint --
   --------------

   function Get_Uint
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Guint
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Guint;
      pragma Import (C, Internal, "g_settings_get_uint");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Uint;

   ----------------
   -- Get_Uint64 --
   ----------------

   function Get_Uint64
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Guint64
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Guint64;
      pragma Import (C, Internal, "g_settings_get_uint64");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Guint64;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return;
   end Get_Uint64;

   --------------------
   -- Get_User_Value --
   --------------------

   function Get_User_Value
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_get_user_value");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return From_Object (Tmp_Return);
   end Get_User_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_settings_get_value");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return From_Object (Tmp_Return);
   end Get_Value;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
      (Self : not null access Gsettings_Record;
       Name : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_is_writable");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Tmp_Return /= 0;
   end Is_Writable;

   -------------------
   -- List_Children --
   -------------------

   function List_Children
      (Self : not null access Gsettings_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_list_children");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end List_Children;

   ---------------
   -- List_Keys --
   ---------------

   function List_Keys
      (Self : not null access Gsettings_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_list_keys");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end List_Keys;

   -----------------
   -- Range_Check --
   -----------------

   function Range_Check
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_range_check");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Get_Object (Value));
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Range_Check;

   -----------
   -- Reset --
   -----------

   procedure Reset
      (Self : not null access Gsettings_Record;
       Key  : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_settings_reset");
      Tmp_Key : Gtkada.Types.Chars_Ptr := New_String (Key);
   begin
      Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
   end Reset;

   ------------
   -- Revert --
   ------------

   procedure Revert (Self : not null access Gsettings_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_settings_revert");
   begin
      Internal (Get_Object (Self));
   end Revert;

   -----------------
   -- Set_Boolean --
   -----------------

   function Set_Boolean
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Boolean) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_boolean");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Boolean'Pos (Value));
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Boolean;

   ----------------
   -- Set_Double --
   ----------------

   function Set_Double
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Gdouble) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_double");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Double;

   --------------
   -- Set_Enum --
   --------------

   function Set_Enum
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_enum");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Enum;

   ---------------
   -- Set_Flags --
   ---------------

   function Set_Flags
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Guint) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_flags");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Flags;

   -------------
   -- Set_Int --
   -------------

   function Set_Int
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_int");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Int;

   ---------------
   -- Set_Int64 --
   ---------------

   function Set_Int64
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Gint64) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gint64) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_int64");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Int64;

   ----------------
   -- Set_String --
   ----------------

   function Set_String
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : UTF8_String) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_string");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Value  : Gtkada.Types.Chars_Ptr := New_String (Value);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Tmp_Value);
      Free (Tmp_Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_String;

   --------------
   -- Set_Strv --
   --------------

   function Set_Strv
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : GNAT.Strings.String_List) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gtkada.Types.chars_ptr_array) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_strv");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Value  : Gtkada.Types.chars_ptr_array := From_String_List (Value);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Tmp_Value);
      Gtkada.Types.Free (Tmp_Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Strv;

   --------------
   -- Set_Uint --
   --------------

   function Set_Uint
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Guint) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_uint");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Uint;

   ----------------
   -- Set_Uint64 --
   ----------------

   function Set_Uint64
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Guint64) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Guint64) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_uint64");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Uint64;

   ---------------
   -- Set_Value --
   ---------------

   function Set_Value
      (Self  : not null access Gsettings_Record;
       Key   : UTF8_String;
       Value : Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_settings_set_value");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Get_Object (Value));
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Value;

   ---------------
   -- The_Delay --
   ---------------

   procedure The_Delay (Self : not null access Gsettings_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_settings_delay");
   begin
      Internal (Get_Object (Self));
   end The_Delay;

   ------------------------------
   -- List_Relocatable_Schemas --
   ------------------------------

   function List_Relocatable_Schemas return GNAT.Strings.String_List is
      function Internal return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_list_relocatable_schemas");
   begin
      return To_String_List (Internal.all);
   end List_Relocatable_Schemas;

   ------------------
   -- List_Schemas --
   ------------------

   function List_Schemas return GNAT.Strings.String_List is
      function Internal return chars_ptr_array_access;
      pragma Import (C, Internal, "g_settings_list_schemas");
   begin
      return To_String_List (Internal.all);
   end List_Schemas;

   ----------
   -- Sync --
   ----------

   procedure Sync is
      procedure Internal;
      pragma Import (C, Internal, "g_settings_sync");
   begin
      Internal;
   end Sync;

   ------------
   -- Unbind --
   ------------

   procedure Unbind
      (Object   : not null access Glib.Object.GObject_Record'Class;
       Property : UTF8_String)
   is
      procedure Internal
         (Object   : System.Address;
          Property : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_settings_unbind");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Object), Tmp_Property);
      Free (Tmp_Property);
   end Unbind;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gsettings_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gsettings_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   procedure Connect
      (Object  : access Gsettings_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gsettings_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gsettings_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gsettings_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gsettings_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gsettings_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gsettings_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gsettings_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gsettings_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

   --------------------------------------
   -- Marsh_Gsettings_UTF8_String_Void --
   --------------------------------------

   procedure Marsh_Gsettings_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gsettings_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gsettings := Gsettings (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gsettings_UTF8_String_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_Gsettings_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   -------------------------
   -- On_Writable_Changed --
   -------------------------

   procedure On_Writable_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_Gsettings_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "writable-changed" & ASCII.NUL, Call, After);
   end On_Writable_Changed;

   -------------------------
   -- On_Writable_Changed --
   -------------------------

   procedure On_Writable_Changed
      (Self  : not null access Gsettings_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "writable-changed" & ASCII.NUL, Call, After, Slot);
   end On_Writable_Changed;

end Glib.Settings;
