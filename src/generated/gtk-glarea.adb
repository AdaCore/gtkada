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
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

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

   -------------------
   -- Get_Has_Alpha --
   -------------------

   function Get_Has_Alpha
      (Self : not null access Gtk_GLArea_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gl_area_get_has_alpha");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Alpha;

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

   -------------------
   -- Set_Has_Alpha --
   -------------------

   procedure Set_Has_Alpha
      (Self      : not null access Gtk_GLArea_Record;
       Has_Alpha : Boolean)
   is
      procedure Internal (Self : System.Address; Has_Alpha : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gl_area_set_has_alpha");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Alpha));
   end Set_Has_Alpha;

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

   use type System.Address;

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
      V   : aliased not null access Gdk.GLContext.Gdk_GLContext_Record'Class := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
      V   : aliased not null access Gdk.GLContext.Gdk_GLContext_Record'Class := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
