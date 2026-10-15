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
with Interfaces.C.Pointers;

package body Gtk.Gesture_Stylus is

   --------------
   -- Get_Axes --
   --------------

   function Get_Axes
     (Self   : not null access Gtk_Gesture_Stylus_Record;
      Axes   : Gdk_Axis_Use_Array;
      Values : out Gdouble_Array) return Boolean
   is
      use Interfaces.C;
      type Aliased_Gdouble_Array is array (Natural range <>) of aliased Gdouble;
      pragma Convention (C, Aliased_Gdouble_Array);
      package P is new Interfaces.C.Pointers
        (Natural, Gdouble, Aliased_Gdouble_Array, 0.0);
      use P;

      function Internal
        (Self       : System.Address;
         Axes       : System.Address;
         Acc_Values : out P.Pointer) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_stylus_get_axes");
      Acc_Values : P.Pointer := null;
      Acc_Result : Glib.Gboolean;
   begin
      Acc_Result := Internal (Get_Object (Self), Axes'Address, Acc_Values);

      if Acc_Result /= 0 then
         declare
            Arr : constant Gdouble_Array := Gdouble_Array (P.Value (Acc_Values, Axes'Length - 1));
         begin
            Glib.g_free (Acc_Values.all'Address);
            Values := Arr;
         end;
         return True;

      else
         if Acc_Values /= null then
            Glib.g_free (Acc_Values.all'Address);
         end if;
         return False;
      end if;
   end Get_Axes;

   -----------------
   -- Get_Backlog --
   -----------------

   function Get_Backlog
     (Self    : not null access Gtk_Gesture_Stylus_Record;
      Backlog : out Gdk.Device.Gdk_Time_Coord_Array_Access)
   return Boolean
   is
      use Interfaces.C;
      use System;
      type Guint_Access is access Guint;
      type Time_Coord_Array is array (Natural range <>) of aliased Gdk_Time_Coord;
      pragma Convention (C, Time_Coord_Array);
      package P is new Interfaces.C.Pointers
        (Natural, Gdk_Time_Coord, Time_Coord_Array, (0, 0, (others => 0.0)));
      use P;

      type Time_Coord_Array_Access is access Time_Coord_Array;

      function Internal
        (Self        : System.Address;
         Acc_Backlog : out P.Pointer;
         Acc_N_Elems : out Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_stylus_get_backlog");
      Acc_Backlog     : P.Pointer := null;
      Acc_N_Elems     : Guint;
      Elems           : Natural;
      Tmp_Return      : Glib.Gboolean;
   begin
      Backlog := null;

      Tmp_Return := Internal (Get_Object (Self), Acc_Backlog, Acc_N_Elems);
      if Tmp_Return /= 0 then
         Elems := Natural (Acc_N_Elems);
         declare
            Arr : constant Gdk_Time_Coord_Array :=
            Gdk_Time_Coord_Array
              (P.Value (Acc_Backlog, ptrdiff_t (Elems)));
         begin
            Glib.g_free (Acc_Backlog.all'Address);
            Backlog := new Gdk_Time_Coord_Array (1 .. Elems);
            Backlog.all := Arr;
         end;

         return True;

      else
         if Acc_Backlog /= null then
            Glib.g_free (Acc_Backlog.all'Address);
         end if;

         return False;
      end if;
   end Get_Backlog;

   package Type_Conversion_Gtk_Gesture_Stylus is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Gesture_Stylus_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Gesture_Stylus);

   ----------------------------
   -- Gtk_Gesture_Stylus_New --
   ----------------------------

   function Gtk_Gesture_Stylus_New return Gtk_Gesture_Stylus is
      Self : constant Gtk_Gesture_Stylus := new Gtk_Gesture_Stylus_Record;
   begin
      Gtk.Gesture_Stylus.Initialize (Self);
      return Self;
   end Gtk_Gesture_Stylus_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Gesture_Stylus) is
   begin
      Self := new Gtk_Gesture_Stylus_Record;
      Gtk.Gesture_Stylus.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Gesture_Stylus_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_gesture_stylus_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------
   -- Get_Axis --
   --------------

   function Get_Axis
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Axis  : Gdk.Enums.Gdk_Axis_Use;
       Value : out Gdouble) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Axis  : Gdk.Enums.Gdk_Axis_Use;
          Value : out Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_stylus_get_axis");
   begin
      return Internal (Get_Object (Self), Axis, Value) /= 0;
   end Get_Axis;

   ---------------------
   -- Get_Device_Tool --
   ---------------------

   function Get_Device_Tool
      (Self : not null access Gtk_Gesture_Stylus_Record)
       return Gdk.Device_Tool.Gdk_Device_Tool
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gesture_stylus_get_device_tool");
      Stub_Gdk_Device_Tool : Gdk.Device_Tool.Gdk_Device_Tool_Record;
   begin
      return Gdk.Device_Tool.Gdk_Device_Tool (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device_Tool));
   end Get_Device_Tool;

   ---------------------
   -- Get_Stylus_Only --
   ---------------------

   function Get_Stylus_Only
      (Self : not null access Gtk_Gesture_Stylus_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_stylus_get_stylus_only");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Stylus_Only;

   ---------------------
   -- Set_Stylus_Only --
   ---------------------

   procedure Set_Stylus_Only
      (Self        : not null access Gtk_Gesture_Stylus_Record;
       Stylus_Only : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Stylus_Only : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gesture_stylus_set_stylus_only");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Stylus_Only));
   end Set_Stylus_Only;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gdouble_Void);

   procedure Connect
      (Object  : access Gtk_Gesture_Stylus_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Stylus_Record'Class;
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

   procedure Marsh_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Gesture_Stylus_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Stylus_Record'Class;
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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Gdouble_Void;

   ---------------------------------------------------
   -- Marsh_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void --
   ---------------------------------------------------

   procedure Marsh_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Gesture_Stylus := Gtk_Gesture_Stylus (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1), Unchecked_To_Gdouble (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;

   -------------
   -- On_Down --
   -------------

   procedure On_Down
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "down" & ASCII.NUL, Call, After);
   end On_Down;

   -------------
   -- On_Down --
   -------------

   procedure On_Down
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "down" & ASCII.NUL, Call, After, Slot);
   end On_Down;

   ---------------
   -- On_Motion --
   ---------------

   procedure On_Motion
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "motion" & ASCII.NUL, Call, After);
   end On_Motion;

   ---------------
   -- On_Motion --
   ---------------

   procedure On_Motion
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "motion" & ASCII.NUL, Call, After, Slot);
   end On_Motion;

   ------------------
   -- On_Proximity --
   ------------------

   procedure On_Proximity
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "proximity" & ASCII.NUL, Call, After);
   end On_Proximity;

   ------------------
   -- On_Proximity --
   ------------------

   procedure On_Proximity
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "proximity" & ASCII.NUL, Call, After, Slot);
   end On_Proximity;

   -----------
   -- On_Up --
   -----------

   procedure On_Up
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_Gtk_Gesture_Stylus_Gdouble_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "up" & ASCII.NUL, Call, After);
   end On_Up;

   -----------
   -- On_Up --
   -----------

   procedure On_Up
      (Self  : not null access Gtk_Gesture_Stylus_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "up" & ASCII.NUL, Call, After, Slot);
   end On_Up;

end Gtk.Gesture_Stylus;
