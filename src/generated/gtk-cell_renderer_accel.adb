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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Cell_Renderer_Accel is

   package Type_Conversion_Gtk_Cell_Renderer_Accel is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Accel_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer_Accel);

   ---------------------------------
   -- Gtk_Cell_Renderer_Accel_New --
   ---------------------------------

   function Gtk_Cell_Renderer_Accel_New return Gtk_Cell_Renderer_Accel is
      Self : constant Gtk_Cell_Renderer_Accel := new Gtk_Cell_Renderer_Accel_Record;
   begin
      Gtk.Cell_Renderer_Accel.Initialize (Self);
      return Self;
   end Gtk_Cell_Renderer_Accel_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Accel) is
   begin
      Self := new Gtk_Cell_Renderer_Accel_Record;
      Gtk.Cell_Renderer_Accel.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Accel_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_accel_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void);

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void);

   procedure Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Accel_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------------------------------
   -- Marsh_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void --
   ------------------------------------------------------------------

   procedure Marsh_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3), Unchecked_To_Guint (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

   ----------------------------------------------------------------------------------
   -- Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void --
   ----------------------------------------------------------------------------------

   procedure Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Renderer_Accel := Gtk_Cell_Renderer_Accel (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3), Unchecked_To_Guint (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;

   ----------------------------------------------------
   -- Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Void --
   ----------------------------------------------------

   procedure Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Renderer_Accel := Gtk_Cell_Renderer_Accel (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Renderer_Accel_UTF8_String_Void;

   ----------------------
   -- On_Accel_Cleared --
   ----------------------

   procedure On_Accel_Cleared
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accel-cleared" & ASCII.NUL, Call, After);
   end On_Accel_Cleared;

   ----------------------
   -- On_Accel_Cleared --
   ----------------------

   procedure On_Accel_Cleared
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accel-cleared" & ASCII.NUL, Call, After, Slot);
   end On_Accel_Cleared;

   ---------------------
   -- On_Accel_Edited --
   ---------------------

   procedure On_Accel_Edited
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accel-edited" & ASCII.NUL, Call, After);
   end On_Accel_Edited;

   ---------------------
   -- On_Accel_Edited --
   ---------------------

   procedure On_Accel_Edited
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accel-edited" & ASCII.NUL, Call, After, Slot);
   end On_Accel_Edited;

end Gtk.Cell_Renderer_Accel;
