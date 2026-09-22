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
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gtk.Event_Controller_Key is

   package Type_Conversion_Gtk_Event_Controller_Key is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Event_Controller_Key_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Event_Controller_Key);

   ----------------------------------
   -- Gtk_Event_Controller_Key_New --
   ----------------------------------

   function Gtk_Event_Controller_Key_New return Gtk_Event_Controller_Key is
      Self : constant Gtk_Event_Controller_Key := new Gtk_Event_Controller_Key_Record;
   begin
      Gtk.Event_Controller_Key.Initialize (Self);
      return Self;
   end Gtk_Event_Controller_Key_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Event_Controller_Key) is
   begin
      Self := new Gtk_Event_Controller_Key_Record;
      Gtk.Event_Controller_Key.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Event_Controller_Key_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_key_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------
   -- Forward --
   -------------

   function Forward
      (Self   : not null access Gtk_Event_Controller_Key_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_event_controller_key_forward");
   begin
      return Internal (Get_Object (Self), Get_Object (Widget)) /= 0;
   end Forward;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Self : not null access Gtk_Event_Controller_Key_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_event_controller_key_get_group");
   begin
      return Internal (Get_Object (Self));
   end Get_Group;

   --------------------
   -- Get_Im_Context --
   --------------------

   function Get_Im_Context
      (Self : not null access Gtk_Event_Controller_Key_Record)
       return Gtk.IM_Context.Gtk_IM_Context
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_key_get_im_context");
      Stub_Gtk_IM_Context : Gtk.IM_Context.Gtk_IM_Context_Record;
   begin
      return Gtk.IM_Context.Gtk_IM_Context (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_IM_Context));
   end Get_Im_Context;

   --------------------
   -- Set_Im_Context --
   --------------------

   procedure Set_Im_Context
      (Self       : not null access Gtk_Event_Controller_Key_Record;
       Im_Context : access Gtk.IM_Context.Gtk_IM_Context_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Im_Context : System.Address);
      pragma Import (C, Internal, "gtk_event_controller_key_set_im_context");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Im_Context)));
   end Set_Im_Context;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Key_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Key_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Modifier_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Modifier_Type_Boolean);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Modifier_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Modifier_Type_Boolean);

   procedure Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean);

   procedure Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean);

   procedure Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean);

   procedure Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void);

   procedure Marsh_Gtk_Event_Controller_Key_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Key_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Key_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Key_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Modifier_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Modifier_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------------
   -- Marsh_GObject_Gdk_Modifier_Type_Boolean --
   ---------------------------------------------

   procedure Marsh_GObject_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Modifier_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Modifier_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Modifier_Type_Boolean;

   ---------------------------------------------------------
   -- Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean --
   ---------------------------------------------------------

   procedure Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean;

   ------------------------------------------------------
   -- Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Void --
   ------------------------------------------------------

   procedure Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Guint_Gdk_Modifier_Type_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   --------------------------------------------------------------
   -- Marsh_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean --
   --------------------------------------------------------------

   procedure Marsh_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Key := Gtk_Event_Controller_Key (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Modifier_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean;

   --------------------------------------------------------------------------
   -- Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean --
   --------------------------------------------------------------------------

   procedure Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Key := Gtk_Event_Controller_Key (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean;

   -----------------------------------------------------------------------
   -- Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void --
   -----------------------------------------------------------------------

   procedure Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Key := Gtk_Event_Controller_Key (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void;

   -----------------------------------------
   -- Marsh_Gtk_Event_Controller_Key_Void --
   -----------------------------------------

   procedure Marsh_Gtk_Event_Controller_Key_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Key_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Key := Gtk_Event_Controller_Key (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Key_Void;

   ------------------
   -- On_Im_Update --
   ------------------

   procedure On_Im_Update
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "im-update" & ASCII.NUL, Call, After);
   end On_Im_Update;

   ------------------
   -- On_Im_Update --
   ------------------

   procedure On_Im_Update
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "im-update" & ASCII.NUL, Call, After, Slot);
   end On_Im_Update;

   --------------------
   -- On_Key_Pressed --
   --------------------

   procedure On_Key_Pressed
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "key-pressed" & ASCII.NUL, Call, After);
   end On_Key_Pressed;

   --------------------
   -- On_Key_Pressed --
   --------------------

   procedure On_Key_Pressed
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "key-pressed" & ASCII.NUL, Call, After, Slot);
   end On_Key_Pressed;

   ---------------------
   -- On_Key_Released --
   ---------------------

   procedure On_Key_Released
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "key-released" & ASCII.NUL, Call, After);
   end On_Key_Released;

   ---------------------
   -- On_Key_Released --
   ---------------------

   procedure On_Key_Released
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "key-released" & ASCII.NUL, Call, After, Slot);
   end On_Key_Released;

   ------------------
   -- On_Modifiers --
   ------------------

   procedure On_Modifiers
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "modifiers" & ASCII.NUL, Call, After);
   end On_Modifiers;

   ------------------
   -- On_Modifiers --
   ------------------

   procedure On_Modifiers
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Gdk_Modifier_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "modifiers" & ASCII.NUL, Call, After, Slot);
   end On_Modifiers;

end Gtk.Event_Controller_Key;
