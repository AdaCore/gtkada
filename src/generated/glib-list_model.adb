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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;

package body Glib.List_Model is

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Self     : Glist_Model;
       Position : Guint) return Glib.Object.GObject
   is
      function Internal
         (Self     : Glist_Model;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Self, Position), Stub_GObject);
   end Get_Object;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Glist_Model_Guint_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Glist_Model_Guint_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Guint_Guint_Void);

   procedure Connect
      (Object  : Glist_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Glist_Model_Guint_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Glist_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Guint_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Guint_Guint_Void);

   procedure Marsh_Glist_Model_Guint_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Glist_Model_Guint_Guint_Guint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Glist_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Glist_Model_Guint_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Glist_Model_Guint_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Glist_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------
   -- Marsh_GObject_Guint_Guint_Guint_Void --
   ------------------------------------------

   procedure Marsh_GObject_Guint_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Guint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Guint_Guint_Void;

   ----------------------------------------------
   -- Marsh_Glist_Model_Guint_Guint_Guint_Void --
   ----------------------------------------------

   procedure Marsh_Glist_Model_Guint_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Glist_Model_Guint_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glist_Model := Glist_Model (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Guint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Glist_Model_Guint_Guint_Guint_Void;

   ----------------------
   -- On_Items_Changed --
   ----------------------

   procedure On_Items_Changed
      (Self  : Glist_Model;
       Call  : Cb_Glist_Model_Guint_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "items-changed" & ASCII.NUL, Call, After);
   end On_Items_Changed;

   ----------------------
   -- On_Items_Changed --
   ----------------------

   procedure On_Items_Changed
      (Self  : Glist_Model;
       Call  : Cb_GObject_Guint_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "items-changed" & ASCII.NUL, Call, After, Slot);
   end On_Items_Changed;

   function "+" (W : Glist_Model) return Glist_Model is
   begin
      return W;
   end "+";

end Glib.List_Model;
