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
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Gsk.Glshader is

   package Type_Conversion_Gsk_Glshader is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gsk_Glshader_Record);
   pragma Unreferenced (Type_Conversion_Gsk_Glshader);

   ---------------------------------
   -- Gsk_Glshader_New_From_Bytes --
   ---------------------------------

   function Gsk_Glshader_New_From_Bytes
      (Sourcecode : Glib.Bytes.Gbytes) return Gsk_Glshader
   is
      Self : constant Gsk_Glshader := new Gsk_Glshader_Record;
   begin
      Gsk.Glshader.Initialize_From_Bytes (Self, Sourcecode);
      return Self;
   end Gsk_Glshader_New_From_Bytes;

   ------------------------------------
   -- Gsk_Glshader_New_From_Resource --
   ------------------------------------

   function Gsk_Glshader_New_From_Resource
      (Resource_Path : UTF8_String) return Gsk_Glshader
   is
      Self : constant Gsk_Glshader := new Gsk_Glshader_Record;
   begin
      Gsk.Glshader.Initialize_From_Resource (Self, Resource_Path);
      return Self;
   end Gsk_Glshader_New_From_Resource;

   ------------------------
   -- Gtk_New_From_Bytes --
   ------------------------

   procedure Gtk_New_From_Bytes
      (Self       : out Gsk_Glshader;
       Sourcecode : Glib.Bytes.Gbytes)
   is
   begin
      Self := new Gsk_Glshader_Record;
      Gsk.Glshader.Initialize_From_Bytes (Self, Sourcecode);
   end Gtk_New_From_Bytes;

   ---------------------------
   -- Gtk_New_From_Resource --
   ---------------------------

   procedure Gtk_New_From_Resource
      (Self          : out Gsk_Glshader;
       Resource_Path : UTF8_String)
   is
   begin
      Self := new Gsk_Glshader_Record;
      Gsk.Glshader.Initialize_From_Resource (Self, Resource_Path);
   end Gtk_New_From_Resource;

   ---------------------------
   -- Initialize_From_Bytes --
   ---------------------------

   procedure Initialize_From_Bytes
      (Self       : not null access Gsk_Glshader_Record'Class;
       Sourcecode : Glib.Bytes.Gbytes)
   is
      function Internal (Sourcecode : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_gl_shader_new_from_bytes");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Sourcecode)));
      end if;
   end Initialize_From_Bytes;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Self          : not null access Gsk_Glshader_Record'Class;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gsk_gl_shader_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Resource_Path);
   end Initialize_From_Resource;

   -------------
   -- Compile --
   -------------

   function Compile
      (Self     : not null access Gsk_Glshader_Record;
       Renderer : not null access Gsk.Renderer.Gsk_Renderer_Record'Class;
       Error    : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Renderer  : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_gl_shader_compile");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Renderer), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Compile;

   --------------------------
   -- Find_Uniform_By_Name --
   --------------------------

   function Find_Uniform_By_Name
      (Self : not null access Gsk_Glshader_Record;
       Name : UTF8_String) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return Glib.Gint;
      pragma Import (C, Internal, "gsk_gl_shader_find_uniform_by_name");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Tmp_Return;
   end Find_Uniform_By_Name;

   ------------------
   -- Get_Arg_Bool --
   ------------------

   function Get_Arg_Bool
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Boolean
   is
      function Internal
         (Self : System.Address;
          Args : System.Address;
          Idx  : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_bool");
   begin
      return Internal (Get_Object (Self), Get_Object (Args), Idx) /= 0;
   end Get_Arg_Bool;

   -------------------
   -- Get_Arg_Float --
   -------------------

   function Get_Arg_Float
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Interfaces.C.C_float
   is
      function Internal
         (Self : System.Address;
          Args : System.Address;
          Idx  : Glib.Gint) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_float");
   begin
      return Internal (Get_Object (Self), Get_Object (Args), Idx);
   end Get_Arg_Float;

   -----------------
   -- Get_Arg_Int --
   -----------------

   function Get_Arg_Int
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Gint32
   is
      function Internal
         (Self : System.Address;
          Args : System.Address;
          Idx  : Glib.Gint) return Gint32;
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_int");
   begin
      return Internal (Get_Object (Self), Get_Object (Args), Idx);
   end Get_Arg_Int;

   ------------------
   -- Get_Arg_Uint --
   ------------------

   function Get_Arg_Uint
      (Self : not null access Gsk_Glshader_Record;
       Args : Glib.Bytes.Gbytes;
       Idx  : Glib.Gint) return Guint32
   is
      function Internal
         (Self : System.Address;
          Args : System.Address;
          Idx  : Glib.Gint) return Guint32;
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_uint");
   begin
      return Internal (Get_Object (Self), Get_Object (Args), Idx);
   end Get_Arg_Uint;

   ------------------
   -- Get_Arg_Vec2 --
   ------------------

   procedure Get_Arg_Vec2
      (Self      : not null access Gsk_Glshader_Record;
       Args      : Glib.Bytes.Gbytes;
       Idx       : Glib.Gint;
       Out_Value : not null access Graphene.Vec2.Graphene_Vec2_T)
   is
      procedure Internal
         (Self      : System.Address;
          Args      : System.Address;
          Idx       : Glib.Gint;
          Out_Value : not null access Graphene.Vec2.Graphene_Vec2_T);
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_vec2");
   begin
      Internal (Get_Object (Self), Get_Object (Args), Idx, Out_Value);
   end Get_Arg_Vec2;

   ------------------
   -- Get_Arg_Vec3 --
   ------------------

   procedure Get_Arg_Vec3
      (Self      : not null access Gsk_Glshader_Record;
       Args      : Glib.Bytes.Gbytes;
       Idx       : Glib.Gint;
       Out_Value : not null access Graphene.Vec3.Graphene_Vec3_T)
   is
      procedure Internal
         (Self      : System.Address;
          Args      : System.Address;
          Idx       : Glib.Gint;
          Out_Value : not null access Graphene.Vec3.Graphene_Vec3_T);
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_vec3");
   begin
      Internal (Get_Object (Self), Get_Object (Args), Idx, Out_Value);
   end Get_Arg_Vec3;

   ------------------
   -- Get_Arg_Vec4 --
   ------------------

   procedure Get_Arg_Vec4
      (Self      : not null access Gsk_Glshader_Record;
       Args      : Glib.Bytes.Gbytes;
       Idx       : Glib.Gint;
       Out_Value : not null access Graphene.Vec4.Graphene_Vec4_T)
   is
      procedure Internal
         (Self      : System.Address;
          Args      : System.Address;
          Idx       : Glib.Gint;
          Out_Value : not null access Graphene.Vec4.Graphene_Vec4_T);
      pragma Import (C, Internal, "gsk_gl_shader_get_arg_vec4");
   begin
      Internal (Get_Object (Self), Get_Object (Args), Idx, Out_Value);
   end Get_Arg_Vec4;

   -------------------
   -- Get_Args_Size --
   -------------------

   function Get_Args_Size
      (Self : not null access Gsk_Glshader_Record) return Gsize
   is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gsk_gl_shader_get_args_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Args_Size;

   --------------------
   -- Get_N_Textures --
   --------------------

   function Get_N_Textures
      (Self : not null access Gsk_Glshader_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gsk_gl_shader_get_n_textures");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Textures;

   --------------------
   -- Get_N_Uniforms --
   --------------------

   function Get_N_Uniforms
      (Self : not null access Gsk_Glshader_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gsk_gl_shader_get_n_uniforms");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Uniforms;

   ------------------
   -- Get_Resource --
   ------------------

   function Get_Resource
      (Self : not null access Gsk_Glshader_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gsk_gl_shader_get_resource");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Resource;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
      (Self : not null access Gsk_Glshader_Record) return Glib.Bytes.Gbytes
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_gl_shader_get_source");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Source;

   ----------------------
   -- Get_Uniform_Name --
   ----------------------

   function Get_Uniform_Name
      (Self : not null access Gsk_Glshader_Record;
       Idx  : Glib.Gint) return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Idx  : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gsk_gl_shader_get_uniform_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Idx));
   end Get_Uniform_Name;

   ------------------------
   -- Get_Uniform_Offset --
   ------------------------

   function Get_Uniform_Offset
      (Self : not null access Gsk_Glshader_Record;
       Idx  : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Idx  : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gsk_gl_shader_get_uniform_offset");
   begin
      return Internal (Get_Object (Self), Idx);
   end Get_Uniform_Offset;

   ----------------------
   -- Get_Uniform_Type --
   ----------------------

   function Get_Uniform_Type
      (Self : not null access Gsk_Glshader_Record;
       Idx  : Glib.Gint) return Gsk.Enums.Gsk_GLUniform_Type
   is
      function Internal
         (Self : System.Address;
          Idx  : Glib.Gint) return Gsk.Enums.Gsk_GLUniform_Type;
      pragma Import (C, Internal, "gsk_gl_shader_get_uniform_type");
   begin
      return Internal (Get_Object (Self), Idx);
   end Get_Uniform_Type;

end Gsk.Glshader;
