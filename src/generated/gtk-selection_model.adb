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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;

package body Gtk.Selection_Model is

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
      (Self : Gtk_Selection_Model) return Gtk.Bitset.Gtk_Bitset
   is
      function Internal (Self : Gtk_Selection_Model) return System.Address;
      pragma Import (C, Internal, "gtk_selection_model_get_selection");
   begin
      return From_Object (Internal (Self));
   end Get_Selection;

   ----------------------------
   -- Get_Selection_In_Range --
   ----------------------------

   function Get_Selection_In_Range
      (Self     : Gtk_Selection_Model;
       Position : Guint;
       N_Items  : Guint) return Gtk.Bitset.Gtk_Bitset
   is
      function Internal
         (Self     : Gtk_Selection_Model;
          Position : Guint;
          N_Items  : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_selection_model_get_selection_in_range");
   begin
      return From_Object (Internal (Self, Position, N_Items));
   end Get_Selection_In_Range;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
      (Self     : Gtk_Selection_Model;
       Position : Guint) return Boolean
   is
      function Internal
         (Self     : Gtk_Selection_Model;
          Position : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_is_selected");
   begin
      return Internal (Self, Position) /= 0;
   end Is_Selected;

   ----------------
   -- Select_All --
   ----------------

   function Select_All (Self : Gtk_Selection_Model) return Boolean is
      function Internal (Self : Gtk_Selection_Model) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_select_all");
   begin
      return Internal (Self) /= 0;
   end Select_All;

   -----------------
   -- Select_Item --
   -----------------

   function Select_Item
      (Self          : Gtk_Selection_Model;
       Position      : Guint;
       Unselect_Rest : Boolean) return Boolean
   is
      function Internal
         (Self          : Gtk_Selection_Model;
          Position      : Guint;
          Unselect_Rest : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_select_item");
   begin
      return Internal (Self, Position, Boolean'Pos (Unselect_Rest)) /= 0;
   end Select_Item;

   ------------------
   -- Select_Range --
   ------------------

   function Select_Range
      (Self          : Gtk_Selection_Model;
       Position      : Guint;
       N_Items       : Guint;
       Unselect_Rest : Boolean) return Boolean
   is
      function Internal
         (Self          : Gtk_Selection_Model;
          Position      : Guint;
          N_Items       : Guint;
          Unselect_Rest : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_select_range");
   begin
      return Internal (Self, Position, N_Items, Boolean'Pos (Unselect_Rest)) /= 0;
   end Select_Range;

   -------------------
   -- Set_Selection --
   -------------------

   function Set_Selection
      (Self     : Gtk_Selection_Model;
       Selected : Gtk.Bitset.Gtk_Bitset;
       Mask     : Gtk.Bitset.Gtk_Bitset) return Boolean
   is
      function Internal
         (Self     : Gtk_Selection_Model;
          Selected : System.Address;
          Mask     : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_set_selection");
   begin
      return Internal (Self, Get_Object (Selected), Get_Object (Mask)) /= 0;
   end Set_Selection;

   ------------------
   -- Unselect_All --
   ------------------

   function Unselect_All (Self : Gtk_Selection_Model) return Boolean is
      function Internal (Self : Gtk_Selection_Model) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_unselect_all");
   begin
      return Internal (Self) /= 0;
   end Unselect_All;

   -------------------
   -- Unselect_Item --
   -------------------

   function Unselect_Item
      (Self     : Gtk_Selection_Model;
       Position : Guint) return Boolean
   is
      function Internal
         (Self     : Gtk_Selection_Model;
          Position : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_unselect_item");
   begin
      return Internal (Self, Position) /= 0;
   end Unselect_Item;

   --------------------
   -- Unselect_Range --
   --------------------

   function Unselect_Range
      (Self     : Gtk_Selection_Model;
       Position : Guint;
       N_Items  : Guint) return Boolean
   is
      function Internal
         (Self     : Gtk_Selection_Model;
          Position : Guint;
          N_Items  : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_unselect_range");
   begin
      return Internal (Self, Position, N_Items) /= 0;
   end Unselect_Range;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Selection_Model_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Selection_Model_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Guint_Void);

   procedure Connect
      (Object  : Gtk_Selection_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Selection_Model_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Selection_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Guint_Void);

   procedure Marsh_Gtk_Selection_Model_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Selection_Model_Guint_Guint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Selection_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Selection_Model_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Selection_Model_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Selection_Model;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------
   -- Marsh_GObject_Guint_Guint_Void --
   ------------------------------------

   procedure Marsh_GObject_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Guint_Void;

   ------------------------------------------------
   -- Marsh_Gtk_Selection_Model_Guint_Guint_Void --
   ------------------------------------------------

   procedure Marsh_Gtk_Selection_Model_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Selection_Model_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Selection_Model := Gtk_Selection_Model (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Selection_Model_Guint_Guint_Void;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self  : Gtk_Selection_Model;
       Call  : Cb_Gtk_Selection_Model_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-changed" & ASCII.NUL, Call, After);
   end On_Selection_Changed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self  : Gtk_Selection_Model;
       Call  : Cb_GObject_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-changed" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Changed;

   function "+" (W : Gtk_Selection_Model) return Gtk_Selection_Model is
   begin
      return W;
   end "+";

end Gtk.Selection_Model;
