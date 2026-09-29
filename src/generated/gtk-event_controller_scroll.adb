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

package body Gtk.Event_Controller_Scroll is

   package Type_Conversion_Gtk_Event_Controller_Scroll is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Event_Controller_Scroll_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Event_Controller_Scroll);

   -------------------------------------
   -- Gtk_Event_Controller_Scroll_New --
   -------------------------------------

   function Gtk_Event_Controller_Scroll_New
      (Flags : Gtk_Event_Controller_Scroll_Flags)
       return Gtk_Event_Controller_Scroll
   is
      Self : constant Gtk_Event_Controller_Scroll := new Gtk_Event_Controller_Scroll_Record;
   begin
      Gtk.Event_Controller_Scroll.Initialize (Self, Flags);
      return Self;
   end Gtk_Event_Controller_Scroll_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self  : out Gtk_Event_Controller_Scroll;
       Flags : Gtk_Event_Controller_Scroll_Flags)
   is
   begin
      Self := new Gtk_Event_Controller_Scroll_Record;
      Gtk.Event_Controller_Scroll.Initialize (Self, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Event_Controller_Scroll_Record'Class;
       Flags : Gtk_Event_Controller_Scroll_Flags)
   is
      function Internal
         (Flags : Gtk_Event_Controller_Scroll_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_scroll_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Flags));
      end if;
   end Initialize;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gtk_Event_Controller_Scroll_Record)
       return Gtk_Event_Controller_Scroll_Flags
   is
      function Internal
         (Self : System.Address) return Gtk_Event_Controller_Scroll_Flags;
      pragma Import (C, Internal, "gtk_event_controller_scroll_get_flags");
   begin
      return Internal (Get_Object (Self));
   end Get_Flags;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
      (Self : not null access Gtk_Event_Controller_Scroll_Record)
       return Gdk.Event.Scroll_Event.Gdk_Scroll_Unit
   is
      function Internal
         (Self : System.Address)
          return Gdk.Event.Scroll_Event.Gdk_Scroll_Unit;
      pragma Import (C, Internal, "gtk_event_controller_scroll_get_unit");
   begin
      return Internal (Get_Object (Self));
   end Get_Unit;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Flags : Gtk_Event_Controller_Scroll_Flags)
   is
      procedure Internal
         (Self  : System.Address;
          Flags : Gtk_Event_Controller_Scroll_Flags);
      pragma Import (C, Internal, "gtk_event_controller_scroll_set_flags");
   begin
      Internal (Get_Object (Self), Flags);
   end Set_Flags;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gdouble_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gdouble_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Event_Controller_Scroll_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Event_Controller_Scroll_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Scroll_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Gdouble_Boolean);

   procedure Marsh_GObject_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Gdouble_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean);

   procedure Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void);

   procedure Marsh_Gtk_Event_Controller_Scroll_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Event_Controller_Scroll_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Event_Controller_Scroll_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Event_Controller_Scroll_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Gdouble_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Event_Controller_Scroll_Record'Class;
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

   -------------------------------------------
   -- Marsh_GObject_Gdouble_Gdouble_Boolean --
   -------------------------------------------

   procedure Marsh_GObject_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Gdouble_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Gdouble_Boolean;

   ----------------------------------------
   -- Marsh_GObject_Gdouble_Gdouble_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Gdouble_Void;

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

   ---------------------------------------------------------------
   -- Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean --
   ---------------------------------------------------------------

   procedure Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Scroll := Gtk_Event_Controller_Scroll (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean;

   ------------------------------------------------------------
   -- Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void --
   ------------------------------------------------------------

   procedure Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Scroll := Gtk_Event_Controller_Scroll (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void;

   --------------------------------------------
   -- Marsh_Gtk_Event_Controller_Scroll_Void --
   --------------------------------------------

   procedure Marsh_Gtk_Event_Controller_Scroll_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Event_Controller_Scroll_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Event_Controller_Scroll := Gtk_Event_Controller_Scroll (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Event_Controller_Scroll_Void;

   -------------------
   -- On_Decelerate --
   -------------------

   procedure On_Decelerate
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "decelerate" & ASCII.NUL, Call, After);
   end On_Decelerate;

   -------------------
   -- On_Decelerate --
   -------------------

   procedure On_Decelerate
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "decelerate" & ASCII.NUL, Call, After, Slot);
   end On_Decelerate;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Gdouble_Gdouble_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "scroll" & ASCII.NUL, Call, After);
   end On_Scroll;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "scroll" & ASCII.NUL, Call, After, Slot);
   end On_Scroll;

   ---------------------
   -- On_Scroll_Begin --
   ---------------------

   procedure On_Scroll_Begin
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "scroll-begin" & ASCII.NUL, Call, After);
   end On_Scroll_Begin;

   ---------------------
   -- On_Scroll_Begin --
   ---------------------

   procedure On_Scroll_Begin
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "scroll-begin" & ASCII.NUL, Call, After, Slot);
   end On_Scroll_Begin;

   -------------------
   -- On_Scroll_End --
   -------------------

   procedure On_Scroll_End
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_Gtk_Event_Controller_Scroll_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "scroll-end" & ASCII.NUL, Call, After);
   end On_Scroll_End;

   -------------------
   -- On_Scroll_End --
   -------------------

   procedure On_Scroll_End
      (Self  : not null access Gtk_Event_Controller_Scroll_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "scroll-end" & ASCII.NUL, Call, After, Slot);
   end On_Scroll_End;

end Gtk.Event_Controller_Scroll;
