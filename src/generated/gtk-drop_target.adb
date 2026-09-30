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
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gtk.Drop_Target is

   package Type_Conversion_Gtk_Drop_Target is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Drop_Target_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Drop_Target);

   -------------------------
   -- Gtk_Drop_Target_New --
   -------------------------

   function Gtk_Drop_Target_New
      (The_Type : GType;
       Actions  : Gdk.Drag.Drag_Action) return Gtk_Drop_Target
   is
      Self : constant Gtk_Drop_Target := new Gtk_Drop_Target_Record;
   begin
      Gtk.Drop_Target.Initialize (Self, The_Type, Actions);
      return Self;
   end Gtk_Drop_Target_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self     : out Gtk_Drop_Target;
       The_Type : GType;
       Actions  : Gdk.Drag.Drag_Action)
   is
   begin
      Self := new Gtk_Drop_Target_Record;
      Gtk.Drop_Target.Initialize (Self, The_Type, Actions);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self     : not null access Gtk_Drop_Target_Record'Class;
       The_Type : GType;
       Actions  : Gdk.Drag.Drag_Action)
   is
      function Internal
         (The_Type : GType;
          Actions  : Gdk.Drag.Drag_Action) return System.Address;
      pragma Import (C, Internal, "gtk_drop_target_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (The_Type, Actions));
      end if;
   end Initialize;

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Drag.Drag_Action
   is
      function Internal (Self : System.Address) return Gdk.Drag.Drag_Action;
      pragma Import (C, Internal, "gtk_drop_target_get_actions");
   begin
      return Internal (Get_Object (Self));
   end Get_Actions;

   ----------------------
   -- Get_Current_Drop --
   ----------------------

   function Get_Current_Drop
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Drop.Gdk_Drop
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_target_get_current_drop");
      Stub_Gdk_Drop : Gdk.Drop.Gdk_Drop_Record;
   begin
      return Gdk.Drop.Gdk_Drop (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Drop));
   end Get_Current_Drop;

   --------------
   -- Get_Drop --
   --------------

   function Get_Drop
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Drop.Gdk_Drop
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_target_get_drop");
      Stub_Gdk_Drop : Gdk.Drop.Gdk_Drop_Record;
   begin
      return Gdk.Drop.Gdk_Drop (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Drop));
   end Get_Drop;

   -----------------
   -- Get_Formats --
   -----------------

   function Get_Formats
      (Self : not null access Gtk_Drop_Target_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_target_get_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Formats;

   -----------------
   -- Get_Preload --
   -----------------

   function Get_Preload
      (Self : not null access Gtk_Drop_Target_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_drop_target_get_preload");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Preload;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gtk_Drop_Target_Record)
       return access constant GValue
   is
      function Internal
         (Self : System.Address) return access constant GValue;
      pragma Import (C, Internal, "gtk_drop_target_get_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Value;

   ------------
   -- Reject --
   ------------

   procedure Reject (Self : not null access Gtk_Drop_Target_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_drop_target_reject");
   begin
      Internal (Get_Object (Self));
   end Reject;

   -----------------
   -- Set_Actions --
   -----------------

   procedure Set_Actions
      (Self    : not null access Gtk_Drop_Target_Record;
       Actions : Gdk.Drag.Drag_Action)
   is
      procedure Internal
         (Self    : System.Address;
          Actions : Gdk.Drag.Drag_Action);
      pragma Import (C, Internal, "gtk_drop_target_set_actions");
   begin
      Internal (Get_Object (Self), Actions);
   end Set_Actions;

   -----------------
   -- Set_Preload --
   -----------------

   procedure Set_Preload
      (Self    : not null access Gtk_Drop_Target_Record;
       Preload : Boolean)
   is
      procedure Internal (Self : System.Address; Preload : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_drop_target_set_preload");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Preload));
   end Set_Preload;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drop_Target_Gdk_Drop_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drop_Target_Gdk_Drop_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Drop_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Drop_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_GValue_Gdouble_Gdouble_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_GValue_Gdouble_Gdouble_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gdouble_Drag_Action, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gdouble_Drag_Action);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drop_Target_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drop_Target_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_Gdk_Drop_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drop_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GValue_Gdouble_Gdouble_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Drag_Action;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_GValue_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_GValue_Gdouble_Gdouble_Boolean);

   procedure Marsh_GObject_Gdk_Drop_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Drop_Boolean);

   procedure Marsh_GObject_Gdouble_Gdouble_Drag_Action
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Gdouble_Drag_Action);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean);

   procedure Marsh_Gtk_Drop_Target_Gdk_Drop_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drop_Target_Gdk_Drop_Boolean);

   procedure Marsh_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action);

   procedure Marsh_Gtk_Drop_Target_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drop_Target_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_Gdk_Drop_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drop_Target_Gdk_Drop_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Target_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drop_Target_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drop_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Drop_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GValue_Gdouble_Gdouble_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_GValue_Gdouble_Gdouble_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Drag_Action;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Gdouble_Drag_Action'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Target_Record'Class;
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

   --------------------------------------------------
   -- Marsh_GObject_GValue_Gdouble_Gdouble_Boolean --
   --------------------------------------------------

   procedure Marsh_GObject_GValue_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_GValue_Gdouble_Gdouble_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_GValue (Params, 1), Unchecked_To_Gdouble (Params, 2), Unchecked_To_Gdouble (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_GValue_Gdouble_Gdouble_Boolean;

   ------------------------------------
   -- Marsh_GObject_Gdk_Drop_Boolean --
   ------------------------------------

   procedure Marsh_GObject_Gdk_Drop_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Drop_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gdk.Drop.Gdk_Drop (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Drop_Boolean;

   -----------------------------------------------
   -- Marsh_GObject_Gdouble_Gdouble_Drag_Action --
   -----------------------------------------------

   procedure Marsh_GObject_Gdouble_Gdouble_Drag_Action
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Gdouble_Drag_Action := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Gdk.Drag.Drag_Action := H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Gdouble_Drag_Action;

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

   ----------------------------------------------------------
   -- Marsh_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean --
   ----------------------------------------------------------

   procedure Marsh_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drop_Target := Gtk_Drop_Target (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_GValue (Params, 1), Unchecked_To_Gdouble (Params, 2), Unchecked_To_Gdouble (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean;

   --------------------------------------------
   -- Marsh_Gtk_Drop_Target_Gdk_Drop_Boolean --
   --------------------------------------------

   procedure Marsh_Gtk_Drop_Target_Gdk_Drop_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drop_Target_Gdk_Drop_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drop_Target := Gtk_Drop_Target (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gdk.Drop.Gdk_Drop (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drop_Target_Gdk_Drop_Boolean;

   -------------------------------------------------------
   -- Marsh_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action --
   -------------------------------------------------------

   procedure Marsh_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drop_Target := Gtk_Drop_Target (Unchecked_To_Object (Params, 0));
      V   : aliased Gdk.Drag.Drag_Action := H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;

   --------------------------------
   -- Marsh_Gtk_Drop_Target_Void --
   --------------------------------

   procedure Marsh_Gtk_Drop_Target_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drop_Target_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drop_Target := Gtk_Drop_Target (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drop_Target_Void;

   ---------------
   -- On_Accept --
   ---------------

   procedure On_Accept
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Gdk_Drop_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accept" & ASCII.NUL, Call, After);
   end On_Accept;

   ---------------
   -- On_Accept --
   ---------------

   procedure On_Accept
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Gdk_Drop_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accept" & ASCII.NUL, Call, After, Slot);
   end On_Accept;

   -------------
   -- On_Drop --
   -------------

   procedure On_Drop
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drop" & ASCII.NUL, Call, After);
   end On_Drop;

   -------------
   -- On_Drop --
   -------------

   procedure On_Drop
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_GValue_Gdouble_Gdouble_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drop" & ASCII.NUL, Call, After, Slot);
   end On_Drop;

   --------------
   -- On_Enter --
   --------------

   procedure On_Enter
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;
       After : Boolean := False)
   is
   begin
      Connect (Self, "enter" & ASCII.NUL, Call, After);
   end On_Enter;

   --------------
   -- On_Enter --
   --------------

   procedure On_Enter
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Drag_Action;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "enter" & ASCII.NUL, Call, After, Slot);
   end On_Enter;

   --------------
   -- On_Leave --
   --------------

   procedure On_Leave
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "leave" & ASCII.NUL, Call, After);
   end On_Leave;

   --------------
   -- On_Leave --
   --------------

   procedure On_Leave
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "leave" & ASCII.NUL, Call, After, Slot);
   end On_Leave;

   ---------------
   -- On_Motion --
   ---------------

   procedure On_Motion
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_Gtk_Drop_Target_Gdouble_Gdouble_Drag_Action;
       After : Boolean := False)
   is
   begin
      Connect (Self, "motion" & ASCII.NUL, Call, After);
   end On_Motion;

   ---------------
   -- On_Motion --
   ---------------

   procedure On_Motion
      (Self  : not null access Gtk_Drop_Target_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Drag_Action;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "motion" & ASCII.NUL, Call, After, Slot);
   end On_Motion;

end Gtk.Drop_Target;
