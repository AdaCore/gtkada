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

package body Gtk.Editable is

   procedure Insert_Text
     (Editable : Gtk_Editable;
      New_Text : UTF8_String;
      Position : in out Gint) is
   begin
      Insert_Text
        (Editable, New_Text & ASCII.NUL, New_Text'Length, Position);
   end Insert_Text;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Editable  : Gtk_Editable;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String
   is
      function Internal
         (Editable  : Gtk_Editable;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Editable, Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable (Editable : Gtk_Editable) return Boolean is
      function Internal (Editable : Gtk_Editable) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Editable) /= 0;
   end Get_Editable;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Editable      : Gtk_Editable;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Editable      : Gtk_Editable;
          Acc_Start_Pos : access Glib.Gint;
          Acc_End_Pos   : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Glib.Gint;
      Acc_End_Pos   : aliased Glib.Gint;
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Editable, Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Editable        : Gtk_Editable;
       New_Text        : UTF8_String;
       New_Text_Length : Glib.Gint;
       Position        : in out Glib.Gint)
   is
      procedure Internal
         (Editable        : Gtk_Editable;
          New_Text        : Gtkada.Types.Chars_Ptr;
          New_Text_Length : Glib.Gint;
          Position        : in out Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_New_Text : Gtkada.Types.Chars_Ptr := New_String (New_Text);
   begin
      Internal (Editable, Tmp_New_Text, New_Text_Length, Position);
      Free (Tmp_New_Text);
   end Insert_Text;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable (Editable : Gtk_Editable; Is_Editable : Boolean) is
      procedure Internal
         (Editable    : Gtk_Editable;
          Is_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Editable, Boolean'Pos (Is_Editable));
   end Set_Editable;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Editable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Editable_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Editable_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Editable_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Gint_Gint_Void);

   procedure Connect
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Editable_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Editable_Gint_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Void);

   procedure Marsh_GObject_UTF8_String_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Gint_Gint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Editable_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Editable_Gint_Gint_Void);

   procedure Marsh_Gtk_Editable_UTF8_String_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Editable_UTF8_String_Gint_Gint_Void);

   procedure Marsh_Gtk_Editable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Editable_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Editable_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Editable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Editable_Gint_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Editable_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Editable_UTF8_String_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
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
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Editable;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------
   -- Marsh_GObject_Gint_Gint_Void --
   ----------------------------------

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Void;

   ----------------------------------------------
   -- Marsh_GObject_UTF8_String_Gint_Gint_Void --
   ----------------------------------------------

   procedure Marsh_GObject_UTF8_String_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint_Access (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Gint_Gint_Void;

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

   ---------------------------------------
   -- Marsh_Gtk_Editable_Gint_Gint_Void --
   ---------------------------------------

   procedure Marsh_Gtk_Editable_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Editable_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Editable := Gtk_Editable (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Editable_Gint_Gint_Void;

   ---------------------------------------------------
   -- Marsh_Gtk_Editable_UTF8_String_Gint_Gint_Void --
   ---------------------------------------------------

   procedure Marsh_Gtk_Editable_UTF8_String_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Editable := Gtk_Editable (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint_Access (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Editable_UTF8_String_Gint_Gint_Void;

   -----------------------------
   -- Marsh_Gtk_Editable_Void --
   -----------------------------

   procedure Marsh_Gtk_Editable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Editable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Editable := Gtk_Editable (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Editable_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   --------------------
   -- On_Delete_Text --
   --------------------

   procedure On_Delete_Text
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_Gint_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "delete-text" & ASCII.NUL, Call, After);
   end On_Delete_Text;

   --------------------
   -- On_Delete_Text --
   --------------------

   procedure On_Delete_Text
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "delete-text" & ASCII.NUL, Call, After, Slot);
   end On_Delete_Text;

   --------------------
   -- On_Insert_Text --
   --------------------

   procedure On_Insert_Text
      (Self  : Gtk_Editable;
       Call  : Cb_Gtk_Editable_UTF8_String_Gint_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-text" & ASCII.NUL, Call, After);
   end On_Insert_Text;

   --------------------
   -- On_Insert_Text --
   --------------------

   procedure On_Insert_Text
      (Self  : Gtk_Editable;
       Call  : Cb_GObject_UTF8_String_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-text" & ASCII.NUL, Call, After, Slot);
   end On_Insert_Text;

   function "+" (W : Gtk_Editable) return Gtk_Editable is
   begin
      return W;
   end "+";

end Gtk.Editable;
