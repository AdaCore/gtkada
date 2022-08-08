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

package body Gtk.Hsv is

   package Type_Conversion_Gtk_Hsv is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Hsv_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Hsv);

   -----------------
   -- Gtk_Hsv_New --
   -----------------

   function Gtk_Hsv_New return Gtk_Hsv is
      Self : constant Gtk_Hsv := new Gtk_Hsv_Record;
   begin
      Gtk.Hsv.Initialize (Self);
      return Self;
   end Gtk_Hsv_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Hsv) is
   begin
      Self := new Gtk_Hsv_Record;
      Gtk.Hsv.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Hsv_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hsv_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
      (Self : not null access Gtk_Hsv_Record;
       H    : out Gdouble;
       S    : out Gdouble;
       V    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          H    : out Gdouble;
          S    : out Gdouble;
          V    : out Gdouble);
      pragma Import (C, Internal, "gtk_hsv_get_color");
   begin
      Internal (Get_Object (Self), H, S, V);
   end Get_Color;

   -----------------
   -- Get_Metrics --
   -----------------

   procedure Get_Metrics
      (Self       : not null access Gtk_Hsv_Record;
       Size       : out Glib.Gint;
       Ring_Width : out Glib.Gint)
   is
      procedure Internal
         (Self       : System.Address;
          Size       : out Glib.Gint;
          Ring_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_hsv_get_metrics");
   begin
      Internal (Get_Object (Self), Size, Ring_Width);
   end Get_Metrics;

   ------------------
   -- Is_Adjusting --
   ------------------

   function Is_Adjusting
      (Self : not null access Gtk_Hsv_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_hsv_is_adjusting");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Adjusting;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
      (Self : not null access Gtk_Hsv_Record;
       H    : Gdouble;
       S    : Gdouble;
       V    : Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          H    : Gdouble;
          S    : Gdouble;
          V    : Gdouble);
      pragma Import (C, Internal, "gtk_hsv_set_color");
   begin
      Internal (Get_Object (Self), H, S, V);
   end Set_Color;

   -----------------
   -- Set_Metrics --
   -----------------

   procedure Set_Metrics
      (Self       : not null access Gtk_Hsv_Record;
       Size       : Glib.Gint;
       Ring_Width : Glib.Gint)
   is
      procedure Internal
         (Self       : System.Address;
          Size       : Glib.Gint;
          Ring_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_hsv_set_metrics");
   begin
      Internal (Get_Object (Self), Size, Ring_Width);
   end Set_Metrics;

   ------------
   -- To_Rgb --
   ------------

   procedure To_Rgb
      (H : Gdouble;
       S : Gdouble;
       V : Gdouble;
       R : out Gdouble;
       G : out Gdouble;
       B : out Gdouble)
   is
      procedure Internal
         (H : Gdouble;
          S : Gdouble;
          V : Gdouble;
          R : out Gdouble;
          G : out Gdouble;
          B : out Gdouble);
      pragma Import (C, Internal, "gtk_hsv_to_rgb");
   begin
      Internal (H, S, V, R, G, B);
   end To_Rgb;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Hsv_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Hsv_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Hsv_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Hsv_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Void);

   procedure Connect
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Hsv_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Hsv_Gtk_Direction_Type_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Hsv_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Hsv_Gtk_Direction_Type_Void);

   procedure Marsh_Gtk_Hsv_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Hsv_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Hsv_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Hsv_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Hsv_Gtk_Direction_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Hsv_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Hsv_Record'Class;
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
      (Object  : access Gtk_Hsv_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Direction_Type_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Direction_Type_Void;

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
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -------------------------------------------
   -- Marsh_Gtk_Hsv_Gtk_Direction_Type_Void --
   -------------------------------------------

   procedure Marsh_Gtk_Hsv_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Hsv_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Hsv := Gtk_Hsv (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Hsv_Gtk_Direction_Type_Void;

   ------------------------
   -- Marsh_Gtk_Hsv_Void --
   ------------------------

   procedure Marsh_Gtk_Hsv_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Hsv_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Hsv := Gtk_Hsv (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Hsv_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_Gtk_Hsv_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   -------------
   -- On_Move --
   -------------

   procedure On_Move
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_Gtk_Hsv_Gtk_Direction_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move" & ASCII.NUL, Call, After);
   end On_Move;

   -------------
   -- On_Move --
   -------------

   procedure On_Move
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move" & ASCII.NUL, Call, After, Slot);
   end On_Move;

end Gtk.Hsv;
