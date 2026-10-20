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

package body Gtk.GLArea is

   package Type_Conversion_Gtk_GLArea is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_GLArea_Record);
   pragma Unreferenced (Type_Conversion_Gtk_GLArea);

   --------------------
   -- Gtk_GLArea_New --
   --------------------

   function Gtk_GLArea_New return Gtk_GLArea is
      Self : constant Gtk_GLArea := new Gtk_GLArea_Record;
   begin
      Gtk.GLArea.Initialize (Self);
      return Self;
   end Gtk_GLArea_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_GLArea) is
   begin
      Self := new Gtk_GLArea_Record;
      Gtk.GLArea.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_GLArea_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_gl_area_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------
   -- Attach_Buffers --
   --------------------

   procedure Attach_Buffers (Self : not null access Gtk_GLArea_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_gl_area_attach_buffers");
   begin
      Internal (Get_Object (Self));
   end Attach_Buffers;

   ----------------------
   -- Get_Allowed_Apis --
   ----------------------

   function Get_Allowed_Apis
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLAPI
   is
      function Internal
         (Self : System.Address) return Gdk.GLContext.Gdk_GLAPI;
      pragma Import (C, Internal, "gtk_gl_area_get_allowed_apis");
   begin
      return Internal (Get_Object (Self));
   end Get_Allowed_Apis;

   -------------
   -- Get_Api --
   -------------

   function Get_Api
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLAPI
   is
      function Internal
         (Self : System.Address) return Gdk.GLContext.Gdk_GLAPI;
      pragma Import (C, Internal, "gtk_gl_area_get_api");
   begin
      return Internal (Get_Object (Self));
   end Get_Api;

   ---------------------
   -- Get_Auto_Render --
   ---------------------

   function Get_Auto_Render
      (Self : not null access Gtk_GLArea_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gl_area_get_auto_render");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Auto_Render;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLContext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gl_area_get_context");
      Stub_Gdk_GLContext : Gdk.GLContext.Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_GLContext));
   end Get_Context;

   ---------------
   -- Get_Error --
   ---------------

   function Get_Error
      (Self : not null access Gtk_GLArea_Record) return Glib.Error.GError
   is
      function Internal (Self : System.Address) return Glib.Error.GError;
      pragma Import (C, Internal, "gtk_gl_area_get_error");
   begin
      return Internal (Get_Object (Self));
   end Get_Error;

   --------------------------
   -- Get_Has_Depth_Buffer --
   --------------------------

   function Get_Has_Depth_Buffer
      (Self : not null access Gtk_GLArea_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gl_area_get_has_depth_buffer");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Depth_Buffer;

   ----------------------------
   -- Get_Has_Stencil_Buffer --
   ----------------------------

   function Get_Has_Stencil_Buffer
      (Self : not null access Gtk_GLArea_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gl_area_get_has_stencil_buffer");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Stencil_Buffer;

   --------------------------
   -- Get_Required_Version --
   --------------------------

   procedure Get_Required_Version
      (Self  : not null access Gtk_GLArea_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Major : out Glib.Gint;
          Minor : out Glib.Gint);
      pragma Import (C, Internal, "gtk_gl_area_get_required_version");
   begin
      Internal (Get_Object (Self), Major, Minor);
   end Get_Required_Version;

   ----------------
   -- Get_Use_Es --
   ----------------

   function Get_Use_Es
      (Self : not null access Gtk_GLArea_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gl_area_get_use_es");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Es;

   ------------------
   -- Make_Current --
   ------------------

   procedure Make_Current (Self : not null access Gtk_GLArea_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_gl_area_make_current");
   begin
      Internal (Get_Object (Self));
   end Make_Current;

   ------------------
   -- Queue_Render --
   ------------------

   procedure Queue_Render (Self : not null access Gtk_GLArea_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_gl_area_queue_render");
   begin
      Internal (Get_Object (Self));
   end Queue_Render;

   ----------------------
   -- Set_Allowed_Apis --
   ----------------------

   procedure Set_Allowed_Apis
      (Self : not null access Gtk_GLArea_Record;
       Apis : Gdk.GLContext.Gdk_GLAPI)
   is
      procedure Internal
         (Self : System.Address;
          Apis : Gdk.GLContext.Gdk_GLAPI);
      pragma Import (C, Internal, "gtk_gl_area_set_allowed_apis");
   begin
      Internal (Get_Object (Self), Apis);
   end Set_Allowed_Apis;

   ---------------------
   -- Set_Auto_Render --
   ---------------------

   procedure Set_Auto_Render
      (Self        : not null access Gtk_GLArea_Record;
       Auto_Render : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Auto_Render : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gl_area_set_auto_render");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Auto_Render));
   end Set_Auto_Render;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
      (Self  : not null access Gtk_GLArea_Record;
       Error : Glib.Error.GError)
   is
      procedure Internal (Self : System.Address; Error : Glib.Error.GError);
      pragma Import (C, Internal, "gtk_gl_area_set_error");
   begin
      Internal (Get_Object (Self), Error);
   end Set_Error;

   --------------------------
   -- Set_Has_Depth_Buffer --
   --------------------------

   procedure Set_Has_Depth_Buffer
      (Self             : not null access Gtk_GLArea_Record;
       Has_Depth_Buffer : Boolean)
   is
      procedure Internal
         (Self             : System.Address;
          Has_Depth_Buffer : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gl_area_set_has_depth_buffer");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Depth_Buffer));
   end Set_Has_Depth_Buffer;

   ----------------------------
   -- Set_Has_Stencil_Buffer --
   ----------------------------

   procedure Set_Has_Stencil_Buffer
      (Self               : not null access Gtk_GLArea_Record;
       Has_Stencil_Buffer : Boolean)
   is
      procedure Internal
         (Self               : System.Address;
          Has_Stencil_Buffer : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gl_area_set_has_stencil_buffer");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Stencil_Buffer));
   end Set_Has_Stencil_Buffer;

   --------------------------
   -- Set_Required_Version --
   --------------------------

   procedure Set_Required_Version
      (Self  : not null access Gtk_GLArea_Record;
       Major : Glib.Gint;
       Minor : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Major : Glib.Gint;
          Minor : Glib.Gint);
      pragma Import (C, Internal, "gtk_gl_area_set_required_version");
   begin
      Internal (Get_Object (Self), Major, Minor);
   end Set_Required_Version;

   ----------------
   -- Set_Use_Es --
   ----------------

   procedure Set_Use_Es
      (Self   : not null access Gtk_GLArea_Record;
       Use_Es : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Es : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gl_area_set_use_es");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Es));
   end Set_Use_Es;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_GLArea_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_GLArea_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_GLArea_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_GLArea_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_GLArea_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_GLArea_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_GLArea_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_GLArea_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_GLArea_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_GLArea_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_GLArea_Gdk_GLContext, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_GLArea_Gdk_GLContext);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_GLContext, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_GLContext);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_GLArea_Gdk_GLContext_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_GLArea_Gdk_GLContext_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_GLContext_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_GLContext_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_GLArea_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_GLArea_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Void);

   procedure Connect
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_GLArea_Gdk_GLContext;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_GLArea_Gdk_GLContext_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_GLArea_Gint_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_GLContext;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_GLContext_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_GLContext
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_GLContext);

   procedure Marsh_GObject_Gdk_GLContext_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_GLContext_Boolean);

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Void);

   procedure Marsh_Gtk_GLArea_Gdk_GLContext
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_GLArea_Gdk_GLContext);

   procedure Marsh_Gtk_GLArea_Gdk_GLContext_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_GLArea_Gdk_GLContext_Boolean);

   procedure Marsh_Gtk_GLArea_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_GLArea_Gint_Gint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_GLArea_Gdk_GLContext;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_GLArea_Gdk_GLContext'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_GLArea_Gdk_GLContext_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_GLArea_Gdk_GLContext_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_GLArea_Gint_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_GLArea_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_GLContext;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_GLContext'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_GLContext_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_GLContext_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_GLArea_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------
   -- Marsh_GObject_Gdk_GLContext --
   ---------------------------------

   procedure Marsh_GObject_Gdk_GLContext
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_GLContext := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased System.Address := Glib.Object.Get_Object_Or_Null (H (Obj));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_GLContext;

   -----------------------------------------
   -- Marsh_GObject_Gdk_GLContext_Boolean --
   -----------------------------------------

   procedure Marsh_GObject_Gdk_GLContext_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_GLContext_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gdk.GLContext.Gdk_GLContext (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_GLContext_Boolean;

   ----------------------------------
   -- Marsh_GObject_Gint_Gint_Void --
   ----------------------------------

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Void;

   ------------------------------------
   -- Marsh_Gtk_GLArea_Gdk_GLContext --
   ------------------------------------

   procedure Marsh_Gtk_GLArea_Gdk_GLContext
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_GLArea_Gdk_GLContext := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_GLArea := Gtk_GLArea (Unchecked_To_Object (Params, 0));
      V   : aliased System.Address := Glib.Object.Get_Object_Or_Null (H (Obj));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_GLArea_Gdk_GLContext;

   --------------------------------------------
   -- Marsh_Gtk_GLArea_Gdk_GLContext_Boolean --
   --------------------------------------------

   procedure Marsh_Gtk_GLArea_Gdk_GLContext_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_GLArea_Gdk_GLContext_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_GLArea := Gtk_GLArea (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gdk.GLContext.Gdk_GLContext (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_GLArea_Gdk_GLContext_Boolean;

   -------------------------------------
   -- Marsh_Gtk_GLArea_Gint_Gint_Void --
   -------------------------------------

   procedure Marsh_Gtk_GLArea_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_GLArea_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_GLArea := Gtk_GLArea (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_GLArea_Gint_Gint_Void;

   -----------------------
   -- On_Create_Context --
   -----------------------

   procedure On_Create_Context
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_Gtk_GLArea_Gdk_GLContext;
       After : Boolean := False)
   is
   begin
      Connect (Self, "create-context" & ASCII.NUL, Call, After);
   end On_Create_Context;

   -----------------------
   -- On_Create_Context --
   -----------------------

   procedure On_Create_Context
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_GObject_Gdk_GLContext;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "create-context" & ASCII.NUL, Call, After, Slot);
   end On_Create_Context;

   ---------------
   -- On_Render --
   ---------------

   procedure On_Render
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_Gtk_GLArea_Gdk_GLContext_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "render" & ASCII.NUL, Call, After);
   end On_Render;

   ---------------
   -- On_Render --
   ---------------

   procedure On_Render
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_GObject_Gdk_GLContext_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "render" & ASCII.NUL, Call, After, Slot);
   end On_Render;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_Gtk_GLArea_Gint_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "resize" & ASCII.NUL, Call, After);
   end On_Resize;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "resize" & ASCII.NUL, Call, After, Slot);
   end On_Resize;

end Gtk.GLArea;
