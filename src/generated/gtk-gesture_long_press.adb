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

package body Gtk.Gesture_Long_Press is

   package Type_Conversion_Gtk_Gesture_Long_Press is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Gesture_Long_Press_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Gesture_Long_Press);

   --------------------------------
   -- Gtk_Gesture_Long_Press_New --
   --------------------------------

   function Gtk_Gesture_Long_Press_New
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Gesture_Long_Press
   is
      Self : constant Gtk_Gesture_Long_Press := new Gtk_Gesture_Long_Press_Record;
   begin
      Gtk.Gesture_Long_Press.Initialize (Self, Widget);
      return Self;
   end Gtk_Gesture_Long_Press_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Gesture_Long_Press;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Self := new Gtk_Gesture_Long_Press_Record;
      Gtk.Gesture_Long_Press.Initialize (Self, Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Gesture_Long_Press_Record'Class;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gesture_long_press_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Widget)));
      end if;
   end Initialize;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Gesture_Long_Press_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Gesture_Long_Press_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gdouble_Void);

   procedure Connect
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Long_Press_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

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

   procedure Marsh_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void);

   procedure Marsh_Gtk_Gesture_Long_Press_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Gesture_Long_Press_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Long_Press_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Gesture_Long_Press_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
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
      (Object  : access Gtk_Gesture_Long_Press_Record'Class;
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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -------------------------------------------------------
   -- Marsh_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void --
   -------------------------------------------------------

   procedure Marsh_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Gesture_Long_Press := Gtk_Gesture_Long_Press (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void;

   ---------------------------------------
   -- Marsh_Gtk_Gesture_Long_Press_Void --
   ---------------------------------------

   procedure Marsh_Gtk_Gesture_Long_Press_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Gesture_Long_Press_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Gesture_Long_Press := Gtk_Gesture_Long_Press (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Gesture_Long_Press_Void;

   ------------------
   -- On_Cancelled --
   ------------------

   procedure On_Cancelled
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_Gtk_Gesture_Long_Press_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancelled" & ASCII.NUL, Call, After);
   end On_Cancelled;

   ------------------
   -- On_Cancelled --
   ------------------

   procedure On_Cancelled
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancelled" & ASCII.NUL, Call, After, Slot);
   end On_Cancelled;

   ----------------
   -- On_Pressed --
   ----------------

   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "pressed" & ASCII.NUL, Call, After);
   end On_Pressed;

   ----------------
   -- On_Pressed --
   ----------------

   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "pressed" & ASCII.NUL, Call, After, Slot);
   end On_Pressed;

end Gtk.Gesture_Long_Press;
