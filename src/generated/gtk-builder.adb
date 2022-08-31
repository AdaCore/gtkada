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

package body Gtk.Builder is

   procedure C_Gtk_Builder_Connect_Signals_Full
      (Builder   : System.Address;
       Func      : System.Address;
       User_Data : System.Address);
   pragma Import (C, C_Gtk_Builder_Connect_Signals_Full, "gtk_builder_connect_signals_full");
   --  This function can be thought of the interpreted language binding
   --  version of Gtk.Builder.Connect_Signals, except that it does not require
   --  GModule to function correctly.
   --  Since: gtk+ 2.12
   --  "func": the function used to connect the signals
   --  "user_data": arbitrary data that will be passed to the connection
   --  function

   function To_Gtk_Builder_Connect_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Builder_Connect_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Builder_Connect_Func, System.Address);

   procedure Internal_Gtk_Builder_Connect_Func
      (Builder        : System.Address;
       Object         : System.Address;
       Signal_Name    : Gtkada.Types.Chars_Ptr;
       Handler_Name   : Gtkada.Types.Chars_Ptr;
       Connect_Object : System.Address;
       Flags          : Glib.G_Connect_Flags;
       User_Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Builder_Connect_Func);
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "object": object to connect a signal to
   --  "signal_name": name of the signal
   --  "handler_name": name of the handler
   --  "connect_object": a Glib.Object.GObject, if non-null, use
   --  g_signal_connect_object
   --  "flags": Glib.G_Connect_Flags to use
   --  "user_data": user data

   ---------------------------------------
   -- Internal_Gtk_Builder_Connect_Func --
   ---------------------------------------

   procedure Internal_Gtk_Builder_Connect_Func
      (Builder        : System.Address;
       Object         : System.Address;
       Signal_Name    : Gtkada.Types.Chars_Ptr;
       Handler_Name   : Gtkada.Types.Chars_Ptr;
       Connect_Object : System.Address;
       Flags          : Glib.G_Connect_Flags;
       User_Data      : System.Address)
   is
      Func             : constant Gtk_Builder_Connect_Func := To_Gtk_Builder_Connect_Func (User_Data);
      Stub_Gtk_Builder : Gtk_Builder_Record;
      Stub_GObject     : Glib.Object.GObject_Record;
   begin
      Func (Gtk.Builder.Gtk_Builder (Get_User_Data (Builder, Stub_Gtk_Builder)), Get_User_Data (Object, Stub_GObject), Gtkada.Bindings.Value_Allowing_Null (Signal_Name), Gtkada.Bindings.Value_Allowing_Null (Handler_Name), Get_User_Data (Connect_Object, Stub_GObject), Flags);
   end Internal_Gtk_Builder_Connect_Func;

   package Type_Conversion_Gtk_Builder is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Builder_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Builder);

   ---------------------
   -- Gtk_Builder_New --
   ---------------------

   function Gtk_Builder_New return Gtk_Builder is
      Builder : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize (Builder);
      return Builder;
   end Gtk_Builder_New;

   -------------------------------
   -- Gtk_Builder_New_From_File --
   -------------------------------

   function Gtk_Builder_New_From_File
      (Filename : UTF8_String) return Gtk_Builder
   is
      Builder : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize_From_File (Builder, Filename);
      return Builder;
   end Gtk_Builder_New_From_File;

   -----------------------------------
   -- Gtk_Builder_New_From_Resource --
   -----------------------------------

   function Gtk_Builder_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Builder
   is
      Builder : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize_From_Resource (Builder, Resource_Path);
      return Builder;
   end Gtk_Builder_New_From_Resource;

   ---------------------------------
   -- Gtk_Builder_New_From_String --
   ---------------------------------

   function Gtk_Builder_New_From_String
      (String : UTF8_String;
       Length : Gssize) return Gtk_Builder
   is
      Builder : constant Gtk_Builder := new Gtk_Builder_Record;
   begin
      Gtk.Builder.Initialize_From_String (Builder, String, Length);
      return Builder;
   end Gtk_Builder_New_From_String;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Builder : out Gtk_Builder) is
   begin
      Builder := new Gtk_Builder_Record;
      Gtk.Builder.Initialize (Builder);
   end Gtk_New;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
      (Builder  : out Gtk_Builder;
       Filename : UTF8_String)
   is
   begin
      Builder := new Gtk_Builder_Record;
      Gtk.Builder.Initialize_From_File (Builder, Filename);
   end Gtk_New_From_File;

   ---------------------------
   -- Gtk_New_From_Resource --
   ---------------------------

   procedure Gtk_New_From_Resource
      (Builder       : out Gtk_Builder;
       Resource_Path : UTF8_String)
   is
   begin
      Builder := new Gtk_Builder_Record;
      Gtk.Builder.Initialize_From_Resource (Builder, Resource_Path);
   end Gtk_New_From_Resource;

   -------------------------
   -- Gtk_New_From_String --
   -------------------------

   procedure Gtk_New_From_String
      (Builder : out Gtk_Builder;
       String  : UTF8_String;
       Length  : Gssize)
   is
   begin
      Builder := new Gtk_Builder_Record;
      Gtk.Builder.Initialize_From_String (Builder, String, Length);
   end Gtk_New_From_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Builder : not null access Gtk_Builder_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_builder_new");
   begin
      if not Builder.Is_Created then
         Set_Object (Builder, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Builder  : not null access Gtk_Builder_Record'Class;
       Filename : UTF8_String)
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_new_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      if not Builder.Is_Created then
         Tmp_Return := Internal (Tmp_Filename);
         Free (Tmp_Filename);
         Set_Object (Builder, Tmp_Return);
      end if;
   end Initialize_From_File;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Builder       : not null access Gtk_Builder_Record'Class;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Builder.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path);
         Free (Tmp_Resource_Path);
         Set_Object (Builder, Tmp_Return);
      end if;
   end Initialize_From_Resource;

   ----------------------------
   -- Initialize_From_String --
   ----------------------------

   procedure Initialize_From_String
      (Builder : not null access Gtk_Builder_Record'Class;
       String  : UTF8_String;
       Length  : Gssize)
   is
      function Internal
         (String : Gtkada.Types.Chars_Ptr;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "gtk_builder_new_from_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      if not Builder.Is_Created then
         Tmp_Return := Internal (Tmp_String, Length);
         Free (Tmp_String);
         Set_Object (Builder, Tmp_Return);
      end if;
   end Initialize_From_String;

   -------------------
   -- Add_From_File --
   -------------------

   function Add_From_File
      (Builder  : not null access Gtk_Builder_Record;
       Filename : UTF8_String;
       Error    : access Glib.Error.GError) return Guint
   is
      function Internal
         (Builder   : System.Address;
          Filename  : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_builder_add_from_file");
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Filename, Acc_Error'Access);
      Free (Tmp_Filename);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Tmp_Return;
   end Add_From_File;

   -----------------------
   -- Add_From_Resource --
   -----------------------

   function Add_From_Resource
      (Builder       : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String;
       Error         : access Glib.Error.GError) return Guint
   is
      function Internal
         (Builder       : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr;
          Acc_Error     : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_builder_add_from_resource");
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Resource_Path, Acc_Error'Access);
      Free (Tmp_Resource_Path);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Tmp_Return;
   end Add_From_Resource;

   ---------------------
   -- Add_From_String --
   ---------------------

   function Add_From_String
      (Builder : not null access Gtk_Builder_Record;
       Buffer  : UTF8_String;
       Error   : access Glib.Error.GError) return Guint
   is
      function Internal
         (Builder   : System.Address;
          Buffer    : Gtkada.Types.Chars_Ptr;
          Length    : Gsize;
          Acc_Error : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_builder_add_from_string");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Buffer : Gtkada.Types.Chars_Ptr := New_String (Buffer);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Buffer, -1, Acc_Error'Access);
      Free (Tmp_Buffer);
      Error.all := Acc_Error;
      return Tmp_Return;
   end Add_From_String;

   ---------------------------
   -- Add_Objects_From_File --
   ---------------------------

   function Add_Objects_From_File
      (Builder    : not null access Gtk_Builder_Record;
       Filename   : UTF8_String;
       Object_Ids : GNAT.Strings.String_List;
       Error      : access Glib.Error.GError) return Guint
   is
      function Internal
         (Builder    : System.Address;
          Filename   : Gtkada.Types.Chars_Ptr;
          Object_Ids : Gtkada.Types.chars_ptr_array;
          Acc_Error  : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_builder_add_objects_from_file");
      Acc_Error      : aliased Glib.Error.GError;
      Tmp_Filename   : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Object_Ids : Gtkada.Types.chars_ptr_array := From_String_List (Object_Ids);
      Tmp_Return     : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Filename, Tmp_Object_Ids, Acc_Error'Access);
      Gtkada.Types.Free (Tmp_Object_Ids);
      Free (Tmp_Filename);
      Error.all := Acc_Error;
      return Tmp_Return;
   end Add_Objects_From_File;

   -------------------------------
   -- Add_Objects_From_Resource --
   -------------------------------

   function Add_Objects_From_Resource
      (Builder       : not null access Gtk_Builder_Record;
       Resource_Path : UTF8_String;
       Object_Ids    : GNAT.Strings.String_List;
       Error         : access Glib.Error.GError) return Guint
   is
      function Internal
         (Builder       : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr;
          Object_Ids    : Gtkada.Types.chars_ptr_array;
          Acc_Error     : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_builder_add_objects_from_resource");
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Object_Ids    : Gtkada.Types.chars_ptr_array := From_String_List (Object_Ids);
      Tmp_Return        : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Resource_Path, Tmp_Object_Ids, Acc_Error'Access);
      Gtkada.Types.Free (Tmp_Object_Ids);
      Free (Tmp_Resource_Path);
      Error.all := Acc_Error;
      return Tmp_Return;
   end Add_Objects_From_Resource;

   -----------------------------
   -- Add_Objects_From_String --
   -----------------------------

   function Add_Objects_From_String
      (Builder    : not null access Gtk_Builder_Record;
       Buffer     : UTF8_String;
       Length     : Gsize;
       Object_Ids : GNAT.Strings.String_List;
       Error      : access Glib.Error.GError) return Guint
   is
      function Internal
         (Builder    : System.Address;
          Buffer     : Gtkada.Types.Chars_Ptr;
          Length     : Gsize;
          Object_Ids : Gtkada.Types.chars_ptr_array;
          Acc_Error  : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_builder_add_objects_from_string");
      Acc_Error      : aliased Glib.Error.GError;
      Tmp_Buffer     : Gtkada.Types.Chars_Ptr := New_String (Buffer);
      Tmp_Object_Ids : Gtkada.Types.chars_ptr_array := From_String_List (Object_Ids);
      Tmp_Return     : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Buffer, Length, Tmp_Object_Ids, Acc_Error'Access);
      Gtkada.Types.Free (Tmp_Object_Ids);
      Free (Tmp_Buffer);
      Error.all := Acc_Error;
      return Tmp_Return;
   end Add_Objects_From_String;

   ---------------------
   -- Connect_Signals --
   ---------------------

   procedure Connect_Signals
      (Builder   : not null access Gtk_Builder_Record;
       User_Data : System.Address)
   is
      procedure Internal
         (Builder   : System.Address;
          User_Data : System.Address);
      pragma Import (C, Internal, "gtk_builder_connect_signals");
   begin
      Internal (Get_Object (Builder), User_Data);
   end Connect_Signals;

   --------------------------
   -- Connect_Signals_Full --
   --------------------------

   procedure Connect_Signals_Full
      (Builder : not null access Gtk_Builder_Record;
       Func    : Gtk_Builder_Connect_Func)
   is
   begin
      if Func = null then
         C_Gtk_Builder_Connect_Signals_Full (Get_Object (Builder), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Builder_Connect_Signals_Full (Get_Object (Builder), Internal_Gtk_Builder_Connect_Func'Address, To_Address (Func));
      end if;
   end Connect_Signals_Full;

   -------------------
   -- Expose_Object --
   -------------------

   procedure Expose_Object
      (Builder : not null access Gtk_Builder_Record;
       Name    : UTF8_String;
       Object  : not null access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
         (Builder : System.Address;
          Name    : Gtkada.Types.Chars_Ptr;
          Object  : System.Address);
      pragma Import (C, Internal, "gtk_builder_expose_object");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Builder), Tmp_Name, Get_Object (Object));
      Free (Tmp_Name);
   end Expose_Object;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Builder : not null access Gtk_Builder_Record;
       Name    : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Builder : System.Address;
          Name    : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_object");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_GObject : Glib.Object.GObject_Record;
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Name);
      Free (Tmp_Name);
      return Get_User_Data (Tmp_Return, Stub_GObject);
   end Get_Object;

   -----------------
   -- Get_Objects --
   -----------------

   function Get_Objects
      (Builder : not null access Gtk_Builder_Record)
       return Glib.Object.Object_List.GSlist
   is
      function Internal (Builder : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_builder_get_objects");
      Tmp_Return : Glib.Object.Object_List.GSlist;
   begin
      Glib.Object.Object_List.Set_Object (Tmp_Return, Internal (Get_Object (Builder)));
      return Tmp_Return;
   end Get_Objects;

   ----------------------------
   -- Get_Translation_Domain --
   ----------------------------

   function Get_Translation_Domain
      (Builder : not null access Gtk_Builder_Record) return UTF8_String
   is
      function Internal
         (Builder : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_builder_get_translation_domain");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Builder)));
   end Get_Translation_Domain;

   ------------------------
   -- Get_Type_From_Name --
   ------------------------

   function Get_Type_From_Name
      (Builder   : not null access Gtk_Builder_Record;
       Type_Name : UTF8_String) return GType
   is
      function Internal
         (Builder   : System.Address;
          Type_Name : Gtkada.Types.Chars_Ptr) return GType;
      pragma Import (C, Internal, "gtk_builder_get_type_from_name");
      Tmp_Type_Name : Gtkada.Types.Chars_Ptr := New_String (Type_Name);
      Tmp_Return    : GType;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Tmp_Type_Name);
      Free (Tmp_Type_Name);
      return Tmp_Return;
   end Get_Type_From_Name;

   ----------------------------
   -- Lookup_Callback_Symbol --
   ----------------------------

   procedure Lookup_Callback_Symbol
      (Builder       : not null access Gtk_Builder_Record;
       Callback_Name : UTF8_String)
   is
      procedure Internal
         (Builder       : System.Address;
          Callback_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_builder_lookup_callback_symbol");
      Tmp_Callback_Name : Gtkada.Types.Chars_Ptr := New_String (Callback_Name);
   begin
      Internal (Get_Object (Builder), Tmp_Callback_Name);
      Free (Tmp_Callback_Name);
   end Lookup_Callback_Symbol;

   ----------------------------
   -- Set_Translation_Domain --
   ----------------------------

   procedure Set_Translation_Domain
      (Builder : not null access Gtk_Builder_Record;
       Domain  : UTF8_String := "")
   is
      procedure Internal
         (Builder : System.Address;
          Domain  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_builder_set_translation_domain");
      Tmp_Domain : Gtkada.Types.Chars_Ptr;
   begin
      if Domain = "" then
         Tmp_Domain := Gtkada.Types.Null_Ptr;
      else
         Tmp_Domain := New_String (Domain);
      end if;
      Internal (Get_Object (Builder), Tmp_Domain);
      Free (Tmp_Domain);
   end Set_Translation_Domain;

   -----------------------
   -- Value_From_String --
   -----------------------

   procedure Value_From_String
      (Builder : not null access Gtk_Builder_Record;
       Pspec   : in out Glib.Param_Spec;
       String  : UTF8_String;
       Value   : out Glib.Values.GValue;
       Success : out Boolean)
   is
      function Internal
         (Builder   : System.Address;
          Acc_Pspec : access Glib.Param_Spec;
          String    : Gtkada.Types.Chars_Ptr;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_value_from_string");
      Acc_Pspec  : aliased Glib.Param_Spec := Pspec;
      Acc_Value  : aliased Glib.Values.GValue;
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Builder), Acc_Pspec'Access, Tmp_String, Acc_Value'Access);
      Free (Tmp_String);
      Pspec := Acc_Pspec;
      Value := Acc_Value;
      Success := Tmp_Return /= 0;
   end Value_From_String;

   ----------------------------
   -- Value_From_String_Type --
   ----------------------------

   function Value_From_String_Type
      (Builder  : not null access Gtk_Builder_Record;
       The_Type : GType;
       String   : UTF8_String;
       Value    : access Glib.Values.GValue) return Boolean
   is
      function Internal
         (Builder   : System.Address;
          The_Type  : GType;
          String    : Gtkada.Types.Chars_Ptr;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_builder_value_from_string_type");
      Acc_Value  : aliased Glib.Values.GValue;
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Builder), The_Type, Tmp_String, Acc_Value'Access);
      Free (Tmp_String);
      Value.all := Acc_Value;
      return Tmp_Return /= 0;
   end Value_From_String_Type;

end Gtk.Builder;
