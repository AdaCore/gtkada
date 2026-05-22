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
with Gdk.Display;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gdk.Surface is

   package Type_Conversion_Gdk_Surface is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Surface_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Surface);

   -------------------
   -- Gdk_New_Popup --
   -------------------

   procedure Gdk_New_Popup
      (Self     : out Gdk_Surface;
       Parent   : not null access Gdk_Surface_Record'Class;
       Autohide : Boolean)
   is
   begin
      Self := new Gdk_Surface_Record;
      Gdk.Surface.Initialize_Popup (Self, Parent, Autohide);
   end Gdk_New_Popup;

   ----------------------
   -- Gdk_New_Toplevel --
   ----------------------

   procedure Gdk_New_Toplevel
      (Self    : out Gdk_Surface;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
   is
   begin
      Self := new Gdk_Surface_Record;
      Gdk.Surface.Initialize_Toplevel (Self, Display);
   end Gdk_New_Toplevel;

   ---------------------------
   -- Gdk_Surface_New_Popup --
   ---------------------------

   function Gdk_Surface_New_Popup
      (Parent   : not null access Gdk_Surface_Record'Class;
       Autohide : Boolean) return Gdk_Surface
   is
      Self : constant Gdk_Surface := new Gdk_Surface_Record;
   begin
      Gdk.Surface.Initialize_Popup (Self, Parent, Autohide);
      return Self;
   end Gdk_Surface_New_Popup;

   ------------------------------
   -- Gdk_Surface_New_Toplevel --
   ------------------------------

   function Gdk_Surface_New_Toplevel
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gdk_Surface
   is
      Self : constant Gdk_Surface := new Gdk_Surface_Record;
   begin
      Gdk.Surface.Initialize_Toplevel (Self, Display);
      return Self;
   end Gdk_Surface_New_Toplevel;

   ----------------------
   -- Initialize_Popup --
   ----------------------

   procedure Initialize_Popup
      (Self     : not null access Gdk_Surface_Record'Class;
       Parent   : not null access Gdk_Surface_Record'Class;
       Autohide : Boolean)
   is
      function Internal
         (Parent   : System.Address;
          Autohide : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gdk_surface_new_popup");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Parent), Boolean'Pos (Autohide)));
      end if;
   end Initialize_Popup;

   -------------------------
   -- Initialize_Toplevel --
   -------------------------

   procedure Initialize_Toplevel
      (Self    : not null access Gdk_Surface_Record'Class;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
   is
      function Internal (Display : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_surface_new_toplevel");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Display)));
      end if;
   end Initialize_Toplevel;

   ----------
   -- Beep --
   ----------

   procedure Beep (Self : not null access Gdk_Surface_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_surface_beep");
   begin
      Internal (Get_Object (Self));
   end Beep;

   -----------------------
   -- Create_Gl_Context --
   -----------------------

   function Create_Gl_Context
      (Self : not null access Gdk_Surface_Record)
       return Gdk.GLContext.Gdk_GLContext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_surface_create_gl_context");
      Stub_Gdk_GLContext : Gdk.GLContext.Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_GLContext));
   end Create_Gl_Context;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Gdk_Surface_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_surface_destroy");
   begin
      Internal (Get_Object (Self));
   end Destroy;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor
      (Self : not null access Gdk_Surface_Record)
       return Gdk.Cursor.Gdk_Cursor
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_surface_get_cursor");
      Stub_Gdk_Cursor : Gdk.Cursor.Gdk_Cursor_Record;
   begin
      return Gdk.Cursor.Gdk_Cursor (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Cursor));
   end Get_Cursor;

   -----------------------
   -- Get_Device_Cursor --
   -----------------------

   function Get_Device_Cursor
      (Self   : not null access Gdk_Surface_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Cursor.Gdk_Cursor
   is
      function Internal
         (Self   : System.Address;
          Device : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_surface_get_device_cursor");
      Stub_Gdk_Cursor : Gdk.Cursor.Gdk_Cursor_Record;
   begin
      return Gdk.Cursor.Gdk_Cursor (Get_User_Data (Internal (Get_Object (Self), Get_Object (Device)), Stub_Gdk_Cursor));
   end Get_Device_Cursor;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Surface_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_surface_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ---------------------
   -- Get_Frame_Clock --
   ---------------------

   function Get_Frame_Clock
      (Self : not null access Gdk_Surface_Record)
       return Gdk.Frame_Clock.Gdk_Frame_Clock
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_surface_get_frame_clock");
      Stub_Gdk_Frame_Clock : Gdk.Frame_Clock.Gdk_Frame_Clock_Record;
   begin
      return Gdk.Frame_Clock.Gdk_Frame_Clock (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Frame_Clock));
   end Get_Frame_Clock;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Self : not null access Gdk_Surface_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_surface_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   ----------------
   -- Get_Mapped --
   ----------------

   function Get_Mapped
      (Self : not null access Gdk_Surface_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_surface_get_mapped");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Mapped;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
      (Self : not null access Gdk_Surface_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gdk_surface_get_scale");
   begin
      return Internal (Get_Object (Self));
   end Get_Scale;

   ----------------------
   -- Get_Scale_Factor --
   ----------------------

   function Get_Scale_Factor
      (Self : not null access Gdk_Surface_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_surface_get_scale_factor");
   begin
      return Internal (Get_Object (Self));
   end Get_Scale_Factor;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gdk_Surface_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_surface_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ----------
   -- Hide --
   ----------

   procedure Hide (Self : not null access Gdk_Surface_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_surface_hide");
   begin
      Internal (Get_Object (Self));
   end Hide;

   ------------------
   -- Is_Destroyed --
   ------------------

   function Is_Destroyed
      (Self : not null access Gdk_Surface_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_surface_is_destroyed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Destroyed;

   ------------------
   -- Queue_Render --
   ------------------

   procedure Queue_Render (Self : not null access Gdk_Surface_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_surface_queue_render");
   begin
      Internal (Get_Object (Self));
   end Queue_Render;

   --------------------
   -- Request_Layout --
   --------------------

   procedure Request_Layout (Self : not null access Gdk_Surface_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_surface_request_layout");
   begin
      Internal (Get_Object (Self));
   end Request_Layout;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
      (Self   : not null access Gdk_Surface_Record;
       Cursor : access Gdk.Cursor.Gdk_Cursor_Record'Class)
   is
      procedure Internal (Self : System.Address; Cursor : System.Address);
      pragma Import (C, Internal, "gdk_surface_set_cursor");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cursor)));
   end Set_Cursor;

   -----------------------
   -- Set_Device_Cursor --
   -----------------------

   procedure Set_Device_Cursor
      (Self   : not null access Gdk_Surface_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Cursor : not null access Gdk.Cursor.Gdk_Cursor_Record'Class)
   is
      procedure Internal
         (Self   : System.Address;
          Device : System.Address;
          Cursor : System.Address);
      pragma Import (C, Internal, "gdk_surface_set_device_cursor");
   begin
      Internal (Get_Object (Self), Get_Object (Device), Get_Object (Cursor));
   end Set_Device_Cursor;

   ----------------------
   -- Set_Input_Region --
   ----------------------

   procedure Set_Input_Region
      (Self   : not null access Gdk_Surface_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gdk_surface_set_input_region");
   begin
      Internal (Get_Object (Self), Region);
   end Set_Input_Region;

   -----------------------
   -- Set_Opaque_Region --
   -----------------------

   procedure Set_Opaque_Region
      (Self   : not null access Gdk_Surface_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gdk_surface_set_opaque_region");
   begin
      Internal (Get_Object (Self), Region);
   end Set_Opaque_Region;

   ---------------------------
   -- Translate_Coordinates --
   ---------------------------

   function Translate_Coordinates
      (Self : not null access Gdk_Surface_Record;
       To   : not null access Gdk_Surface_Record'Class;
       X    : access Gdouble;
       Y    : access Gdouble) return Boolean
   is
      function Internal
         (Self  : System.Address;
          To    : System.Address;
          Acc_X : access Gdouble;
          Acc_Y : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_surface_translate_coordinates");
      Acc_X      : aliased Gdouble;
      Acc_Y      : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (To), Acc_X'Access, Acc_Y'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      return Tmp_Return /= 0;
   end Translate_Coordinates;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Surface_Gdk_Monitor_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Surface_Gdk_Monitor_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Monitor_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Monitor_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Surface_Address_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Surface_Address_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Address_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Address_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Surface_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Surface_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Void);

   procedure Connect
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Surface_Gdk_Monitor_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Surface_Address_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Surface_Gint_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Monitor_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Address_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Address_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Address_Boolean);

   procedure Marsh_GObject_Gdk_Monitor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Monitor_Void);

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Void);

   procedure Marsh_Gdk_Surface_Address_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Surface_Address_Boolean);

   procedure Marsh_Gdk_Surface_Gdk_Monitor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Surface_Gdk_Monitor_Void);

   procedure Marsh_Gdk_Surface_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Surface_Gint_Gint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Surface_Gdk_Monitor_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Surface_Gdk_Monitor_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Surface_Address_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Surface_Address_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Surface_Gint_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Surface_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Monitor_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Monitor_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Surface_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Address_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Address_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Surface_Record'Class;
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

   -----------------------------------
   -- Marsh_GObject_Address_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Address_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Address_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Address (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Address_Boolean;

   ------------------------------------
   -- Marsh_GObject_Gdk_Monitor_Void --
   ------------------------------------

   procedure Marsh_GObject_Gdk_Monitor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Monitor_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Monitor.Gdk_Monitor (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Monitor_Void;

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

   ---------------------------------------
   -- Marsh_Gdk_Surface_Address_Boolean --
   ---------------------------------------

   procedure Marsh_Gdk_Surface_Address_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Surface_Address_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Surface := Gdk_Surface (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Address (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Surface_Address_Boolean;

   ----------------------------------------
   -- Marsh_Gdk_Surface_Gdk_Monitor_Void --
   ----------------------------------------

   procedure Marsh_Gdk_Surface_Gdk_Monitor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Surface_Gdk_Monitor_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Surface := Gdk_Surface (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Monitor.Gdk_Monitor (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Surface_Gdk_Monitor_Void;

   --------------------------------------
   -- Marsh_Gdk_Surface_Gint_Gint_Void --
   --------------------------------------

   procedure Marsh_Gdk_Surface_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Surface_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Surface := Gdk_Surface (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Surface_Gint_Gint_Void;

   ----------------------
   -- On_Enter_Monitor --
   ----------------------

   procedure On_Enter_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Gdk_Monitor_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "enter-monitor" & ASCII.NUL, Call, After);
   end On_Enter_Monitor;

   ----------------------
   -- On_Enter_Monitor --
   ----------------------

   procedure On_Enter_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Gdk_Monitor_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "enter-monitor" & ASCII.NUL, Call, After, Slot);
   end On_Enter_Monitor;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Address_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "event" & ASCII.NUL, Call, After);
   end On_Event;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Address_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "event" & ASCII.NUL, Call, After, Slot);
   end On_Event;

   ---------------
   -- On_Layout --
   ---------------

   procedure On_Layout
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Gint_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "layout" & ASCII.NUL, Call, After);
   end On_Layout;

   ---------------
   -- On_Layout --
   ---------------

   procedure On_Layout
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "layout" & ASCII.NUL, Call, After, Slot);
   end On_Layout;

   ----------------------
   -- On_Leave_Monitor --
   ----------------------

   procedure On_Leave_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Gdk_Monitor_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "leave-monitor" & ASCII.NUL, Call, After);
   end On_Leave_Monitor;

   ----------------------
   -- On_Leave_Monitor --
   ----------------------

   procedure On_Leave_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Gdk_Monitor_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "leave-monitor" & ASCII.NUL, Call, After, Slot);
   end On_Leave_Monitor;

end Gdk.Surface;
