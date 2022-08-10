------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2022, AdaCore                     --
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

with Ada.Unchecked_Conversion;

package body Gtk.Arguments is

   use Glib.Values;

   type Cairo_Rectangle_Int_Access is access Cairo.Region.Cairo_Rectangle_Int;
   type Gint_Access is access Gint;
   type Gdouble_Access is access Gdouble;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (Args : Gtk_Args; Num : Positive) return Gint is
   begin
      return Get_Int (Nth (Args, Guint (Num)));
   end To_Gint;

   --------------
   -- To_Guint --
   --------------

   function To_Guint (Args : Gtk_Args; Num : Positive) return Guint is
   begin
      return Get_Uint (Nth (Args, Guint (Num)));
   end To_Guint;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Args : Gtk_Args; Num : Positive) return Boolean is
   begin
      return Get_Boolean (Nth (Args, Guint (Num)));
   end To_Boolean;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Args : Gtk_Args; Num : Positive)
     return Glib.Object.GObject
   is
      Stub : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Get_Address (Nth (Args, Guint (Num))), Stub);
   end To_Object;

   ----------------
   -- To_C_Proxy --
   ----------------

   function To_C_Proxy (Args : Gtk_Args; Num : Positive) return Glib.C_Proxy is
   begin
      return Get_Proxy (Nth (Args, Guint (Num)));
   end To_C_Proxy;

   --------------
   -- To_Event --
   --------------

   function To_Event (Args : Gtk_Args; Num : Positive)
      return Gdk.Event.Gdk_Event
   is
      function Convert is new Ada.Unchecked_Conversion
        (C_Proxy, Gdk.Event.Gdk_Event);
      Proxy : constant C_Proxy := Get_Proxy (Nth (Args, Guint (Num)));
   begin
      return Convert (Proxy);
   end To_Event;

   ----------------------
   -- To_Notebook_Page --
   ----------------------

   function To_Notebook_Page
     (Args : Gtk_Args; Num : Positive) return Gtk_Notebook_Page is
   begin
      return Gtk_Notebook_Page (Get_Proxy (Nth (Args, Guint (Num))));
   end To_Notebook_Page;

   -------------------------------------
   -- Unchecked_To_Gdk_Event_Sequence --
   -------------------------------------

   function Unchecked_To_Gdk_Event_Sequence
     (Args : Glib.Values.C_GValues; Num : Guint)
     return Gdk.Event.Gdk_Event_Sequence
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Gdk.Event.Gdk_Event_Sequence (Get_Proxy (Val));
   end Unchecked_To_Gdk_Event_Sequence;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Args : Gtk_Args; Num : Positive)
     return System.Address is
   begin
      return Get_Address (Nth (Args, Guint (Num)));
   end To_Address;

   --------------------
   -- To_Requisition --
   --------------------

   function To_Requisition
     (Args : Gtk_Args; Num : Positive) return Gtk.Widget.Gtk_Requisition_Access
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Internal is new Ada.Unchecked_Conversion
        (System.Address, Gtk.Widget.Gtk_Requisition_Access);
      pragma Warnings (On);

   begin
      return Internal (Get_Address (Nth (Args, Guint (Num))));
   end To_Requisition;

   ----------------
   -- To_Address --
   ----------------

   function To_Allocation
     (Args : Gtk_Args; Num : Positive)
      return Gtk.Widget.Gtk_Allocation_Access
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Internal is new Ada.Unchecked_Conversion
        (System.Address, Gtk.Widget.Gtk_Allocation_Access);
      pragma Warnings (On);

   begin
      return Internal (Get_Address (Nth (Args, Guint (Num))));
   end To_Allocation;

   ---------------
   -- To_String --
   ---------------

   function To_String  (Args : Gtk_Args; Num : Positive) return UTF8_String is
   begin
      return Get_String (Nth (Args, Guint (Num)));
   end To_String;

   --------------------------
   -- Unchecked_To_Boolean --
   --------------------------

   function Unchecked_To_Boolean
     (Args : Glib.Values.C_GValues; Num : Guint) return Boolean
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Get_Boolean (Val);
   end Unchecked_To_Boolean;

   --------------------------
   -- Unchecked_To_Gdouble --
   --------------------------

   function Unchecked_To_Gdouble
     (Args : Glib.Values.C_GValues; Num : Guint) return Gdouble
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Get_Double (Val);
   end Unchecked_To_Gdouble;

   -----------------------
   -- Unchecked_To_Gint --
   -----------------------

   function Unchecked_To_Gint
     (Args : Glib.Values.C_GValues; Num : Guint) return Gint
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Get_Int (Val);
   end Unchecked_To_Gint;

   ------------------------------
   -- Unchecked_To_Gint_Access --
   ------------------------------

   function Unchecked_To_Gint_Access
     (Args : Glib.Values.C_GValues; Num : Guint) return access Gint
   is
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Gint_Access);
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Convert (Get_Address (Val));
   end Unchecked_To_Gint_Access;

   ---------------------------------
   -- Unchecked_To_Gdouble_Access --
   ---------------------------------

   function Unchecked_To_Gdouble_Access
     (Args : Glib.Values.C_GValues; Num : Guint) return access Gdouble
   is
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Gdouble_Access);
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Convert (Get_Address (Val));
   end Unchecked_To_Gdouble_Access;

   ------------------------
   -- Unchecked_To_Guint --
   ------------------------

   function Unchecked_To_Guint
     (Args : Glib.Values.C_GValues; Num : Guint) return Guint
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Get_Uint (Val);
   end Unchecked_To_Guint;

   -----------------------------
   -- Unchecked_To_Context_Id --
   -----------------------------

   function Unchecked_To_Context_Id
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Gtk.Status_Bar.Context_Id
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Gtk.Status_Bar.Context_Id (Get_Uint (Val));
   end Unchecked_To_Context_Id;

   ------------------------------
   -- Unchecked_To_UTF8_String --
   ------------------------------

   function Unchecked_To_UTF8_String
     (Args : Glib.Values.C_GValues; Num : Guint) return UTF8_String
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Get_String (Val);
   end Unchecked_To_UTF8_String;

   --------------------------
   -- Unchecked_To_Address --
   --------------------------

   function Unchecked_To_Address
     (Args : Glib.Values.C_GValues; Num : Guint) return System.Address
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Get_Address (Val);
   end Unchecked_To_Address;

   -------------------------
   -- Unchecked_To_Object --
   -------------------------

   function Unchecked_To_Object
     (Args : Glib.Values.C_GValues; Num : Guint) return Glib.Object.GObject
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Glib.Object.Convert (Get_Address (Val));
   end Unchecked_To_Object;

   ----------------------------
   -- Unchecked_To_Interface --
   ----------------------------

   function Unchecked_To_Interface
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Glib.Types.GType_Interface
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Glib.Types.GType_Interface (Get_Address (Val));
   end Unchecked_To_Interface;

   ---------------------------------------------
   -- Unchecked_To_Cairo_Rectangle_Int_Access --
   ---------------------------------------------

   function Unchecked_To_Cairo_Rectangle_Int_Access
     (Args : Glib.Values.C_GValues; Num : Guint)
      return access Cairo.Region.Cairo_Rectangle_Int
   is
      function Convert is new Ada.Unchecked_Conversion
        (C_Proxy, Cairo_Rectangle_Int_Access);
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Convert (Get_Proxy (Val));
   end Unchecked_To_Cairo_Rectangle_Int_Access;

   ------------------------------------------
   -- Unchecked_To_Gtk_Entry_Icon_Position --
   ------------------------------------------

   function Unchecked_To_Gtk_Entry_Icon_Position
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Gtk.GEntry.Gtk_Entry_Icon_Position
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Gtk.GEntry.Gtk_Entry_Icon_Position'Val (Get_Enum (Val));
   end Unchecked_To_Gtk_Entry_Icon_Position;

   --------------------------------------
   -- Unchecked_To_Cairo_Rectangle_Int --
   --------------------------------------

   function Unchecked_To_Cairo_Rectangle_Int
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Cairo.Region.Cairo_Rectangle_Int
   is
      function Convert is new Ada.Unchecked_Conversion
        (C_Proxy, Cairo_Rectangle_Int_Access);
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Convert (Get_Proxy (Val)).all;
   end Unchecked_To_Cairo_Rectangle_Int;

   --------------------------------
   -- Unchecked_To_Cairo_Context --
   --------------------------------

   function Unchecked_To_Cairo_Context
     (Args : Glib.Values.C_GValues; Num : Guint)
      return Cairo.Cairo_Context
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Cairo.Get_Context (Val);
   end Unchecked_To_Cairo_Context;

   ---------------------------
   -- Unchecked_To_Gdk_RGBA --
   ---------------------------

   function Unchecked_To_Gdk_RGBA
     (Args : Glib.Values.C_GValues; Num : Guint) return Gdk.RGBA.Gdk_RGBA
   is
      Val : GValue;
   begin
      Unsafe_Nth (Args, Num, Val);
      return Gdk.RGBA.Get_Value (Val);
   end Unchecked_To_Gdk_RGBA;

   -------------------------------
   -- Unchecked_To_Gdk_Key_Type --
   -------------------------------

   function Unchecked_To_Gdk_Key_Type
     (Args : Glib.Values.C_GValues; Num : Guint) return Gdk.Types.Gdk_Key_Type
   is
   begin
      return Gdk.Types.Gdk_Key_Type (Unchecked_To_Guint (Args, Num));
   end Unchecked_To_Gdk_Key_Type;
end Gtk.Arguments;
