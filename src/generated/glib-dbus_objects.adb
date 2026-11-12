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
with Glib.DBus_Interfaces;     use Glib.DBus_Interfaces;
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;

package body Glib.DBus_Objects is

   -------------------
   -- Get_Interface --
   -------------------

   function Get_Interface
      (Self           : Gdbus_Object;
       Interface_Name : UTF8_String)
       return Glib.DBus_Interfaces.Gdbus_Interface
   is
      function Internal
         (Self           : Gdbus_Object;
          Interface_Name : Gtkada.Types.Chars_Ptr)
          return Glib.DBus_Interfaces.Gdbus_Interface;
      pragma Import (C, Internal, "g_dbus_object_get_interface");
      Tmp_Interface_Name : Gtkada.Types.Chars_Ptr := New_String (Interface_Name);
      Tmp_Return         : Glib.DBus_Interfaces.Gdbus_Interface;
   begin
      Tmp_Return := Internal (Self, Tmp_Interface_Name);
      Free (Tmp_Interface_Name);
      return Tmp_Return;
   end Get_Interface;

   ---------------------
   -- Get_Object_Path --
   ---------------------

   function Get_Object_Path (Self : Gdbus_Object) return UTF8_String is
      function Internal (Self : Gdbus_Object) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_dbus_object_get_object_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Object_Path;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdbus_Object_Gdbus_Interface_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdbus_Object_Gdbus_Interface_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdbus_Interface_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdbus_Interface_Void);

   procedure Connect
      (Object  : Gdbus_Object;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdbus_Object_Gdbus_Interface_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gdbus_Object;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdbus_Interface_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdbus_Interface_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdbus_Interface_Void);

   procedure Marsh_Gdbus_Object_Gdbus_Interface_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdbus_Object_Gdbus_Interface_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gdbus_Object;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdbus_Object_Gdbus_Interface_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdbus_Object_Gdbus_Interface_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gdbus_Object;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdbus_Interface_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdbus_Interface_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------------
   -- Marsh_GObject_Gdbus_Interface_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gdbus_Interface_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdbus_Interface_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdbus_Interface (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdbus_Interface_Void;

   ---------------------------------------------
   -- Marsh_Gdbus_Object_Gdbus_Interface_Void --
   ---------------------------------------------

   procedure Marsh_Gdbus_Object_Gdbus_Interface_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdbus_Object_Gdbus_Interface_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdbus_Object := Gdbus_Object (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdbus_Interface (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gdbus_Object_Gdbus_Interface_Void;

   ------------------------
   -- On_Interface_Added --
   ------------------------

   procedure On_Interface_Added
      (Self  : Gdbus_Object;
       Call  : Cb_Gdbus_Object_Gdbus_Interface_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "interface-added" & ASCII.NUL, Call, After);
   end On_Interface_Added;

   ------------------------
   -- On_Interface_Added --
   ------------------------

   procedure On_Interface_Added
      (Self  : Gdbus_Object;
       Call  : Cb_GObject_Gdbus_Interface_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "interface-added" & ASCII.NUL, Call, After, Slot);
   end On_Interface_Added;

   --------------------------
   -- On_Interface_Removed --
   --------------------------

   procedure On_Interface_Removed
      (Self  : Gdbus_Object;
       Call  : Cb_Gdbus_Object_Gdbus_Interface_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "interface-removed" & ASCII.NUL, Call, After);
   end On_Interface_Removed;

   --------------------------
   -- On_Interface_Removed --
   --------------------------

   procedure On_Interface_Removed
      (Self  : Gdbus_Object;
       Call  : Cb_GObject_Gdbus_Interface_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "interface-removed" & ASCII.NUL, Call, After, Slot);
   end On_Interface_Removed;

   function "+" (W : Gdbus_Object) return Gdbus_Object is
   begin
      return W;
   end "+";

end Glib.DBus_Objects;
