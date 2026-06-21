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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Builder is

   package Type_Conversion_Gtk_Builder is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Builder_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Builder);

   ---------------------
   -- Gtk_Builder_New --
   ---------------------

   function Gtk_Builder_New return Gtk_Builder is
      Self : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize (Self);
      return Self;
   end Gtk_Builder_New;

   -------------------------------
   -- Gtk_Builder_New_From_File --
   -------------------------------

   function Gtk_Builder_New_From_File
      (Filename : UTF8_String) return Gtk_Builder
   is
      Self : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize_From_File (Self, Filename);
      return Self;
   end Gtk_Builder_New_From_File;

   -----------------------------------
   -- Gtk_Builder_New_From_Resource --
   -----------------------------------

   function Gtk_Builder_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Builder
   is
      Self : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize_From_Resource (Self, Resource_Path);
      return Self;
   end Gtk_Builder_New_From_Resource;

   ---------------------------------
   -- Gtk_Builder_New_From_String --
   ---------------------------------

   function Gtk_Builder_New_From_String
      (String : UTF8_String;
       Length : Gssize) return Gtk_Builder
   is
      Self : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize_From_String (Self, String, Length);
      return Self;
   end Gtk_Builder_New_From_String;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Builder) is
   begin
      Self := new Gtk_Builder_Record;
      Gtk.Builder.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
      (Self     : out Gtk_Builder;
       Filename : UTF8_String)
   is
   begin
      Self := new Gtk_Builder_Record;
      Gtk.Builder.Initialize_From_File (Self, Filename);
   end Gtk_New_From_File;

   ---------------------------
   -- Gtk_New_From_Resource --
   ---------------------------

   procedure Gtk_New_From_Resource
      (Self          : out Gtk_Builder;
       Resource_Path : UTF8_String)
   is
   begin
      Self := new Gtk_Builder_Record;
      Gtk.Builder.Initialize_From_Resource (Self, Resource_Path);
   end Gtk_New_From_Resource;

   -------------------------
   -- Gtk_New_From_String --
   -------------------------

   procedure Gtk_New_From_String
      (Self   : out Gtk_Builder;
       String : UTF8_String;
       Length : Gssize)
   is
   begin
      Self := new Gtk_Builder_Record;
      Gtk.Builder.Initialize_From_String (Self, String, Length);
   end Gtk_New_From_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Builder_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_builder_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Self     : not null access Gtk_Builder_Record'Class;
       Filename : UTF8_String)
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_new_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Filename);
         Free (Tmp_Filename);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_File;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Self          : not null access Gtk_Builder_Record'Class;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path);
         Free (Tmp_Resource_Path);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Resource;

   ----------------------------
   -- Initialize_From_String --
   ----------------------------

   procedure Initialize_From_String
      (Self   : not null access Gtk_Builder_Record'Class;
       String : UTF8_String;
       Length : Gssize)
   is
      function Internal
         (String : Gtkada.Types.Chars_Ptr;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "gtk_builder_new_from_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_String, Length);
         Free (Tmp_String);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_String;

   -------------------
   -- Add_From_File --
   -------------------

   function Add_From_File
      (Self     : not null access Gtk_Builder_Record;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_add_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Add_From_File;

   -----------------------
   -- Add_From_Resource --
   -----------------------

   function Add_From_Resource
      (Self          : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_add_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
      return Tmp_Return /= 0;
   end Add_From_Resource;

   ---------------------
   -- Add_From_String --
   ---------------------

   function Add_From_String
      (Self   : not null access Gtk_Builder_Record;
       Buffer : UTF8_String;
       Length : Gssize) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Buffer : Gtkada.Types.Chars_Ptr;
          Length : Gssize) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_add_from_string");
      Tmp_Buffer : Gtkada.Types.Chars_Ptr := New_String (Buffer);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Buffer, Length);
      Free (Tmp_Buffer);
      return Tmp_Return /= 0;
   end Add_From_String;

   ---------------------------
   -- Add_Objects_From_File --
   ---------------------------

   function Add_Objects_From_File
      (Self       : not null access Gtk_Builder_Record;
       Filename   : UTF8_String;
       Object_Ids : GNAT.Strings.String_List) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Filename   : Gtkada.Types.Chars_Ptr;
          Object_Ids : Gtkada.Types.chars_ptr_array) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_add_objects_from_file");
      Tmp_Filename   : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Object_Ids : Gtkada.Types.chars_ptr_array := From_String_List (Object_Ids);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename, Tmp_Object_Ids);
      Gtkada.Types.Free (Tmp_Object_Ids);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Add_Objects_From_File;

   -------------------------------
   -- Add_Objects_From_Resource --
   -------------------------------

   function Add_Objects_From_Resource
      (Self          : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String;
       Object_Ids    : GNAT.Strings.String_List) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr;
          Object_Ids    : Gtkada.Types.chars_ptr_array) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_add_objects_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Object_Ids    : Gtkada.Types.chars_ptr_array := From_String_List (Object_Ids);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Resource_Path, Tmp_Object_Ids);
      Gtkada.Types.Free (Tmp_Object_Ids);
      Free (Tmp_Resource_Path);
      return Tmp_Return /= 0;
   end Add_Objects_From_Resource;

   -----------------------------
   -- Add_Objects_From_String --
   -----------------------------

   function Add_Objects_From_String
      (Self       : not null access Gtk_Builder_Record;
       Buffer     : UTF8_String;
       Length     : Gssize;
       Object_Ids : GNAT.Strings.String_List) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Buffer     : Gtkada.Types.Chars_Ptr;
          Length     : Gssize;
          Object_Ids : Gtkada.Types.chars_ptr_array) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_add_objects_from_string");
      Tmp_Buffer     : Gtkada.Types.Chars_Ptr := New_String (Buffer);
      Tmp_Object_Ids : Gtkada.Types.chars_ptr_array := From_String_List (Object_Ids);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Buffer, Length, Tmp_Object_Ids);
      Gtkada.Types.Free (Tmp_Object_Ids);
      Free (Tmp_Buffer);
      return Tmp_Return /= 0;
   end Add_Objects_From_String;

   --------------------
   -- Create_Closure --
   --------------------

   function Create_Closure
      (Self          : not null access Gtk_Builder_Record;
       Function_Name : UTF8_String;
       Flags         : Gtk.Enums.Gtk_Builder_Closure_Flags;
       Object        : access Glib.Object.GObject_Record'Class)
       return System.Address
   is
      function Internal
         (Self          : System.Address;
          Function_Name : Gtkada.Types.Chars_Ptr;
          Flags         : Gtk.Enums.Gtk_Builder_Closure_Flags;
          Object        : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_builder_create_closure");
      Tmp_Function_Name : Gtkada.Types.Chars_Ptr := New_String (Function_Name);
      Tmp_Return        : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Function_Name, Flags, Get_Object_Or_Null (GObject (Object)));
      Free (Tmp_Function_Name);
      return Tmp_Return;
   end Create_Closure;

   -------------------
   -- Expose_Object --
   -------------------

   procedure Expose_Object
      (Self   : not null access Gtk_Builder_Record;
       Name   : UTF8_String;
       Object : not null access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
         (Self   : System.Address;
          Name   : Gtkada.Types.Chars_Ptr;
          Object : System.Address);
      pragma Import (C, Internal, "gtk_builder_expose_object");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name, Get_Object (Object));
      Free (Tmp_Name);
   end Expose_Object;

   --------------------------
   -- Extend_With_Template --
   --------------------------

   function Extend_With_Template
      (Self          : not null access Gtk_Builder_Record;
       Object        : not null access Glib.Object.GObject_Record'Class;
       Template_Type : GType;
       Buffer        : UTF8_String;
       Length        : Gssize) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Object        : System.Address;
          Template_Type : GType;
          Buffer        : Gtkada.Types.Chars_Ptr;
          Length        : Gssize) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_extend_with_template");
      Tmp_Buffer : Gtkada.Types.Chars_Ptr := New_String (Buffer);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Object), Template_Type, Tmp_Buffer, Length);
      Free (Tmp_Buffer);
      return Tmp_Return /= 0;
   end Extend_With_Template;

   ------------------------
   -- Get_Current_Object --
   ------------------------

   function Get_Current_Object
      (Self : not null access Gtk_Builder_Record) return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_current_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Current_Object;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Self : not null access Gtk_Builder_Record;
       Name : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_object");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_GObject : Glib.Object.GObject_Record;
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Get_User_Data (Tmp_Return, Stub_GObject);
   end Get_Object;

   -----------------
   -- Get_Objects --
   -----------------

   function Get_Objects
      (Self : not null access Gtk_Builder_Record)
       return Glib.Object.Object_Simple_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_objects");
      Tmp_Return : Glib.Object.Object_Simple_List.Glist;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Objects;

   ---------------
   -- Get_Scope --
   ---------------

   function Get_Scope
      (Self : not null access Gtk_Builder_Record)
       return Gtk.Builder_Scope.Gtk_Builder_Scope
   is
      function Internal
         (Self : System.Address) return Gtk.Builder_Scope.Gtk_Builder_Scope;
      pragma Import (C, Internal, "gtk_builder_get_scope");
   begin
      return Internal (Get_Object (Self));
   end Get_Scope;

   ----------------------------
   -- Get_Translation_Domain --
   ----------------------------

   function Get_Translation_Domain
      (Self : not null access Gtk_Builder_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_builder_get_translation_domain");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Translation_Domain;

   ------------------------
   -- Get_Type_From_Name --
   ------------------------

   function Get_Type_From_Name
      (Self      : not null access Gtk_Builder_Record;
       Type_Name : UTF8_String) return GType
   is
      function Internal
         (Self      : System.Address;
          Type_Name : Gtkada.Types.Chars_Ptr) return GType;
      pragma Import (C, Internal, "gtk_builder_get_type_from_name");
      Tmp_Type_Name : Gtkada.Types.Chars_Ptr := New_String (Type_Name);
      Tmp_Return    : GType;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Type_Name);
      Free (Tmp_Type_Name);
      return Tmp_Return;
   end Get_Type_From_Name;

   ------------------------
   -- Set_Current_Object --
   ------------------------

   procedure Set_Current_Object
      (Self           : not null access Gtk_Builder_Record;
       Current_Object : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
         (Self           : System.Address;
          Current_Object : System.Address);
      pragma Import (C, Internal, "gtk_builder_set_current_object");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Current_Object)));
   end Set_Current_Object;

   ---------------
   -- Set_Scope --
   ---------------

   procedure Set_Scope
      (Self  : not null access Gtk_Builder_Record;
       Scope : Gtk.Builder_Scope.Gtk_Builder_Scope)
   is
      procedure Internal
         (Self  : System.Address;
          Scope : Gtk.Builder_Scope.Gtk_Builder_Scope);
      pragma Import (C, Internal, "gtk_builder_set_scope");
   begin
      Internal (Get_Object (Self), Scope);
   end Set_Scope;

   ----------------------------
   -- Set_Translation_Domain --
   ----------------------------

   procedure Set_Translation_Domain
      (Self   : not null access Gtk_Builder_Record;
       Domain : UTF8_String := "")
   is
      procedure Internal
         (Self   : System.Address;
          Domain : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_builder_set_translation_domain");
      Tmp_Domain : Gtkada.Types.Chars_Ptr;
   begin
      if Domain = "" then
         Tmp_Domain := Gtkada.Types.Null_Ptr;
      else
         Tmp_Domain := New_String (Domain);
      end if;
      Internal (Get_Object (Self), Tmp_Domain);
      Free (Tmp_Domain);
   end Set_Translation_Domain;

   -----------------------
   -- Value_From_String --
   -----------------------

   function Value_From_String
      (Self   : not null access Gtk_Builder_Record;
       Pspec  : in out Glib.Param_Spec;
       String : UTF8_String;
       Value  : access Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Pspec : access Glib.Param_Spec;
          String    : Gtkada.Types.Chars_Ptr;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_value_from_string");
      Acc_Pspec  : aliased Glib.Param_Spec := Pspec;
      Acc_Value  : aliased Glib.Values.GValue;
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Pspec'Access, Tmp_String, Acc_Value'Access);
      Free (Tmp_String);
      Pspec := Acc_Pspec;
      Value.all := Acc_Value;
      return Tmp_Return /= 0;
   end Value_From_String;

   ----------------------------
   -- Value_From_String_Type --
   ----------------------------

   function Value_From_String_Type
      (Self     : not null access Gtk_Builder_Record;
       The_Type : GType;
       String   : UTF8_String;
       Value    : access Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : System.Address;
          The_Type  : GType;
          String    : Gtkada.Types.Chars_Ptr;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_value_from_string_type");
      Acc_Value  : aliased Glib.Values.GValue;
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), The_Type, Tmp_String, Acc_Value'Access);
      Free (Tmp_String);
      Value.all := Acc_Value;
      return Tmp_Return /= 0;
   end Value_From_String_Type;

end Gtk.Builder;
