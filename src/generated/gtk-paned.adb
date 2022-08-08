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

package body Gtk.Paned is

   package Type_Conversion_Gtk_Paned is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Paned_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Paned);

   --------------------
   -- Gtk_Hpaned_New --
   --------------------

   function Gtk_Hpaned_New return Gtk_Hpaned is
      Paned : constant Gtk_Hpaned := new Gtk_Hpaned_Record;
   begin
      Gtk.Paned.Initialize_Hpaned (Paned);
      return Paned;
   end Gtk_Hpaned_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Paned       : out Gtk_Paned;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
   begin
      Paned := new Gtk_Paned_Record;
      Gtk.Paned.Initialize (Paned, Orientation);
   end Gtk_New;

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Paned : out Gtk_Hpaned) is
   begin
      Paned := new Gtk_Hpaned_Record;
      Gtk.Paned.Initialize_Hpaned (Paned);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Paned : out Gtk_Vpaned) is
   begin
      Paned := new Gtk_Vpaned_Record;
      Gtk.Paned.Initialize_Vpaned (Paned);
   end Gtk_New_Vpaned;

   -------------------
   -- Gtk_Paned_New --
   -------------------

   function Gtk_Paned_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Paned
   is
      Paned : constant Gtk_Paned := new Gtk_Paned_Record;
   begin
      Gtk.Paned.Initialize (Paned, Orientation);
      return Paned;
   end Gtk_Paned_New;

   --------------------
   -- Gtk_Vpaned_New --
   --------------------

   function Gtk_Vpaned_New return Gtk_Vpaned is
      Paned : constant Gtk_Vpaned := new Gtk_Vpaned_Record;
   begin
      Gtk.Paned.Initialize_Vpaned (Paned);
      return Paned;
   end Gtk_Vpaned_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Paned       : not null access Gtk_Paned_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_paned_new");
   begin
      if not Paned.Is_Created then
         Set_Object (Paned, Internal (Orientation));
      end if;
   end Initialize;

   -----------------------
   -- Initialize_Hpaned --
   -----------------------

   procedure Initialize_Hpaned
      (Paned : not null access Gtk_Hpaned_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");
   begin
      if not Paned.Is_Created then
         Set_Object (Paned, Internal);
      end if;
   end Initialize_Hpaned;

   -----------------------
   -- Initialize_Vpaned --
   -----------------------

   procedure Initialize_Vpaned
      (Paned : not null access Gtk_Vpaned_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");
   begin
      if not Paned.Is_Created then
         Set_Object (Paned, Internal);
      end if;
   end Initialize_Vpaned;

   ----------
   -- Add1 --
   ----------

   procedure Add1
      (Paned : not null access Gtk_Paned_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
      (Paned : not null access Gtk_Paned_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add2;

   ----------------
   -- Get_Child1 --
   ----------------

   function Get_Child1
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_child1");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub_Gtk_Widget));
   end Get_Child1;

   ----------------
   -- Get_Child2 --
   ----------------

   function Get_Child2
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_child2");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub_Gtk_Widget));
   end Get_Child2;

   -----------------------
   -- Get_Handle_Window --
   -----------------------

   function Get_Handle_Window
      (Paned : not null access Gtk_Paned_Record) return Gdk.Gdk_Window
   is
      function Internal (Paned : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_paned_get_handle_window");
   begin
      return Internal (Get_Object (Paned));
   end Get_Handle_Window;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Paned : not null access Gtk_Paned_Record) return Glib.Gint
   is
      function Internal (Paned : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_paned_get_position");
   begin
      return Internal (Get_Object (Paned));
   end Get_Position;

   ---------------------
   -- Get_Wide_Handle --
   ---------------------

   function Get_Wide_Handle
      (Paned : not null access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paned_get_wide_handle");
   begin
      return Internal (Get_Object (Paned)) /= 0;
   end Get_Wide_Handle;

   -----------
   -- Pack1 --
   -----------

   procedure Pack1
      (Paned  : not null access Gtk_Paned_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := True)
   is
      procedure Internal
         (Paned  : System.Address;
          Child  : System.Address;
          Resize : Glib.Gboolean;
          Shrink : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_pack1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child), Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack1;

   -----------
   -- Pack2 --
   -----------

   procedure Pack2
      (Paned  : not null access Gtk_Paned_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := False)
   is
      procedure Internal
         (Paned  : System.Address;
          Child  : System.Address;
          Resize : Glib.Gboolean;
          Shrink : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_pack2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child), Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack2;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Paned    : not null access Gtk_Paned_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Paned : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_paned_set_position");
   begin
      Internal (Get_Object (Paned), Position);
   end Set_Position;

   ---------------------
   -- Set_Wide_Handle --
   ---------------------

   procedure Set_Wide_Handle
      (Paned : not null access Gtk_Paned_Record;
       Wide  : Boolean)
   is
      procedure Internal (Paned : System.Address; Wide : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_set_wide_handle");
   begin
      Internal (Get_Object (Paned), Boolean'Pos (Wide));
   end Set_Wide_Handle;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Paned_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Paned_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Paned_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Paned_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Paned_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Paned_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Boolean);

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Boolean);

   procedure Marsh_Gtk_Paned_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Paned_Boolean);

   procedure Marsh_Gtk_Paned_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Paned_Boolean_Boolean);

   procedure Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Paned_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Paned_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Boolean --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Boolean;

   -----------------------------
   -- Marsh_Gtk_Paned_Boolean --
   -----------------------------

   procedure Marsh_Gtk_Paned_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Paned_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Paned := Gtk_Paned (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Paned_Boolean;

   -------------------------------------
   -- Marsh_Gtk_Paned_Boolean_Boolean --
   -------------------------------------

   procedure Marsh_Gtk_Paned_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Paned_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Paned := Gtk_Paned (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Paned_Boolean_Boolean;

   ---------------------------------------------
   -- Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean --
   ---------------------------------------------

   procedure Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Paned := Gtk_Paned (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean;

   ------------------------
   -- On_Accept_Position --
   ------------------------

   procedure On_Accept_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accept-position" & ASCII.NUL, Call, After);
   end On_Accept_Position;

   ------------------------
   -- On_Accept_Position --
   ------------------------

   procedure On_Accept_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accept-position" & ASCII.NUL, Call, After, Slot);
   end On_Accept_Position;

   ------------------------
   -- On_Cancel_Position --
   ------------------------

   procedure On_Cancel_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel-position" & ASCII.NUL, Call, After);
   end On_Cancel_Position;

   ------------------------
   -- On_Cancel_Position --
   ------------------------

   procedure On_Cancel_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancel-position" & ASCII.NUL, Call, After, Slot);
   end On_Cancel_Position;

   --------------------------
   -- On_Cycle_Child_Focus --
   --------------------------

   procedure On_Cycle_Child_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cycle-child-focus" & ASCII.NUL, Call, After);
   end On_Cycle_Child_Focus;

   --------------------------
   -- On_Cycle_Child_Focus --
   --------------------------

   procedure On_Cycle_Child_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cycle-child-focus" & ASCII.NUL, Call, After, Slot);
   end On_Cycle_Child_Focus;

   ---------------------------
   -- On_Cycle_Handle_Focus --
   ---------------------------

   procedure On_Cycle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cycle-handle-focus" & ASCII.NUL, Call, After);
   end On_Cycle_Handle_Focus;

   ---------------------------
   -- On_Cycle_Handle_Focus --
   ---------------------------

   procedure On_Cycle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cycle-handle-focus" & ASCII.NUL, Call, After, Slot);
   end On_Cycle_Handle_Focus;

   --------------------
   -- On_Move_Handle --
   --------------------

   procedure On_Move_Handle
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-handle" & ASCII.NUL, Call, After);
   end On_Move_Handle;

   --------------------
   -- On_Move_Handle --
   --------------------

   procedure On_Move_Handle
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-handle" & ASCII.NUL, Call, After, Slot);
   end On_Move_Handle;

   ----------------------------
   -- On_Toggle_Handle_Focus --
   ----------------------------

   procedure On_Toggle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-handle-focus" & ASCII.NUL, Call, After);
   end On_Toggle_Handle_Focus;

   ----------------------------
   -- On_Toggle_Handle_Focus --
   ----------------------------

   procedure On_Toggle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-handle-focus" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Handle_Focus;

end Gtk.Paned;
