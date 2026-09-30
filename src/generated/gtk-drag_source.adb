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

package body Gtk.Drag_Source is

   package Type_Conversion_Gtk_Drag_Source is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Drag_Source_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Drag_Source);

   -------------------------
   -- Gtk_Drag_Source_New --
   -------------------------

   function Gtk_Drag_Source_New return Gtk_Drag_Source is
      Self : constant Gtk_Drag_Source := new Gtk_Drag_Source_Record;
   begin
      Gtk.Drag_Source.Initialize (Self);
      return Self;
   end Gtk_Drag_Source_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Drag_Source) is
   begin
      Self := new Gtk_Drag_Source_Record;
      Gtk.Drag_Source.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Drag_Source_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_drag_source_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------
   -- Drag_Cancel --
   -----------------

   procedure Drag_Cancel (Self : not null access Gtk_Drag_Source_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_drag_cancel");
   begin
      Internal (Get_Object (Self));
   end Drag_Cancel;

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
      (Self : not null access Gtk_Drag_Source_Record)
       return Gdk.Drag.Drag_Action
   is
      function Internal (Self : System.Address) return Gdk.Drag.Drag_Action;
      pragma Import (C, Internal, "gtk_drag_source_get_actions");
   begin
      return Internal (Get_Object (Self));
   end Get_Actions;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
      (Self : not null access Gtk_Drag_Source_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drag_source_get_content");
      Stub_Gdk_Content_Provider : Gdk.Content_Provider.Gdk_Content_Provider_Record;
   begin
      return Gdk.Content_Provider.Gdk_Content_Provider (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Content_Provider));
   end Get_Content;

   --------------
   -- Get_Drag --
   --------------

   function Get_Drag
      (Self : not null access Gtk_Drag_Source_Record)
       return Gdk.Drag.Gdk_Drag
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drag_source_get_drag");
      Stub_Gdk_Drag : Gdk.Drag.Gdk_Drag_Record;
   begin
      return Gdk.Drag.Gdk_Drag (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Drag));
   end Get_Drag;

   -----------------
   -- Set_Actions --
   -----------------

   procedure Set_Actions
      (Self    : not null access Gtk_Drag_Source_Record;
       Actions : Gdk.Drag.Drag_Action)
   is
      procedure Internal
         (Self    : System.Address;
          Actions : Gdk.Drag.Drag_Action);
      pragma Import (C, Internal, "gtk_drag_source_set_actions");
   begin
      Internal (Get_Object (Self), Actions);
   end Set_Actions;

   -----------------
   -- Set_Content --
   -----------------

   procedure Set_Content
      (Self    : not null access Gtk_Drag_Source_Record;
       Content : access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class)
   is
      procedure Internal (Self : System.Address; Content : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_set_content");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Content)));
   end Set_Content;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Self      : not null access Gtk_Drag_Source_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable;
       Hot_X     : Glib.Gint;
       Hot_Y     : Glib.Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Paintable : Gdk.Paintable.Gdk_Paintable;
          Hot_X     : Glib.Gint;
          Hot_Y     : Glib.Gint);
      pragma Import (C, Internal, "gtk_drag_source_set_icon");
   begin
      Internal (Get_Object (Self), Paintable, Hot_X, Hot_Y);
   end Set_Icon;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drag_Source_Gdk_Drag_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drag_Source_Gdk_Drag_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Drag_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Drag_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Drag_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Drag_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider);

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdk_Drag_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Drag_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Drag_Boolean_Void);

   procedure Marsh_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean);

   procedure Marsh_GObject_Gdk_Drag_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Drag_Void);

   procedure Marsh_GObject_Gdouble_Gdouble_Gdk_Content_Provider
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Gdouble_Gdk_Content_Provider);

   procedure Marsh_Gtk_Drag_Source_Gdk_Drag_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drag_Source_Gdk_Drag_Boolean_Void);

   procedure Marsh_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean);

   procedure Marsh_Gtk_Drag_Source_Gdk_Drag_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drag_Source_Gdk_Drag_Void);

   procedure Marsh_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdk_Drag_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drag_Source_Gdk_Drag_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drag_Source_Gdk_Drag_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Drag_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Drag_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Drag_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drag_Source_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Gdouble_Gdk_Content_Provider'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------------
   -- Marsh_GObject_Gdk_Drag_Boolean_Void --
   -----------------------------------------

   procedure Marsh_GObject_Gdk_Drag_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Drag_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Drag.Gdk_Drag (Unchecked_To_Object (Params, 1)), Unchecked_To_Boolean (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Drag_Boolean_Void;

   -------------------------------------------------------
   -- Marsh_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean --
   -------------------------------------------------------

   procedure Marsh_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gdk.Drag.Gdk_Drag (Unchecked_To_Object (Params, 1)), Unchecked_To_Drag_Cancel_Reason (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean;

   ---------------------------------
   -- Marsh_GObject_Gdk_Drag_Void --
   ---------------------------------

   procedure Marsh_GObject_Gdk_Drag_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Drag_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Drag.Gdk_Drag (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Drag_Void;

   --------------------------------------------------------
   -- Marsh_GObject_Gdouble_Gdouble_Gdk_Content_Provider --
   --------------------------------------------------------

   procedure Marsh_GObject_Gdouble_Gdouble_Gdk_Content_Provider
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased System.Address := Glib.Object.Get_Object_Or_Null (H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2)));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Gdouble_Gdk_Content_Provider;

   -------------------------------------------------
   -- Marsh_Gtk_Drag_Source_Gdk_Drag_Boolean_Void --
   -------------------------------------------------

   procedure Marsh_Gtk_Drag_Source_Gdk_Drag_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drag_Source := Gtk_Drag_Source (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Drag.Gdk_Drag (Unchecked_To_Object (Params, 1)), Unchecked_To_Boolean (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drag_Source_Gdk_Drag_Boolean_Void;

   ---------------------------------------------------------------
   -- Marsh_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean --
   ---------------------------------------------------------------

   procedure Marsh_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drag_Source := Gtk_Drag_Source (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gdk.Drag.Gdk_Drag (Unchecked_To_Object (Params, 1)), Unchecked_To_Drag_Cancel_Reason (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean;

   -----------------------------------------
   -- Marsh_Gtk_Drag_Source_Gdk_Drag_Void --
   -----------------------------------------

   procedure Marsh_Gtk_Drag_Source_Gdk_Drag_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drag_Source_Gdk_Drag_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drag_Source := Gtk_Drag_Source (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Drag.Gdk_Drag (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drag_Source_Gdk_Drag_Void;

   ----------------------------------------------------------------
   -- Marsh_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider --
   ----------------------------------------------------------------

   procedure Marsh_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drag_Source := Gtk_Drag_Source (Unchecked_To_Object (Params, 0));
      V   : aliased System.Address := Glib.Object.Get_Object_Or_Null (H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2)));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider;

   -------------------
   -- On_Drag_Begin --
   -------------------

   procedure On_Drag_Begin
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdk_Drag_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-begin" & ASCII.NUL, Call, After);
   end On_Drag_Begin;

   -------------------
   -- On_Drag_Begin --
   -------------------

   procedure On_Drag_Begin
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdk_Drag_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-begin" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Begin;

   --------------------
   -- On_Drag_Cancel --
   --------------------

   procedure On_Drag_Cancel
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-cancel" & ASCII.NUL, Call, After);
   end On_Drag_Cancel;

   --------------------
   -- On_Drag_Cancel --
   --------------------

   procedure On_Drag_Cancel
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdk_Drag_Drag_Cancel_Reason_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-cancel" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Cancel;

   -----------------
   -- On_Drag_End --
   -----------------

   procedure On_Drag_End
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdk_Drag_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-end" & ASCII.NUL, Call, After);
   end On_Drag_End;

   -----------------
   -- On_Drag_End --
   -----------------

   procedure On_Drag_End
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdk_Drag_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-end" & ASCII.NUL, Call, After, Slot);
   end On_Drag_End;

   ----------------
   -- On_Prepare --
   ----------------

   procedure On_Prepare
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_Gtk_Drag_Source_Gdouble_Gdouble_Gdk_Content_Provider;
       After : Boolean := False)
   is
   begin
      Connect (Self, "prepare" & ASCII.NUL, Call, After);
   end On_Prepare;

   ----------------
   -- On_Prepare --
   ----------------

   procedure On_Prepare
      (Self  : not null access Gtk_Drag_Source_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Gdk_Content_Provider;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "prepare" & ASCII.NUL, Call, After, Slot);
   end On_Prepare;

end Gtk.Drag_Source;
