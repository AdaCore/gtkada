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

package body Gtk.Level_Bar is

   package Type_Conversion_Gtk_Level_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Level_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Level_Bar);

   -----------------------
   -- Gtk_Level_Bar_New --
   -----------------------

   function Gtk_Level_Bar_New return Gtk_Level_Bar is
      Self : constant Gtk_Level_Bar := new Gtk_Level_Bar_Record;
   begin
      Gtk.Level_Bar.Initialize (Self);
      return Self;
   end Gtk_Level_Bar_New;

   ------------------------------------
   -- Gtk_Level_Bar_New_For_Interval --
   ------------------------------------

   function Gtk_Level_Bar_New_For_Interval
      (Min_Value : Gdouble;
       Max_Value : Gdouble) return Gtk_Level_Bar
   is
      Self : constant Gtk_Level_Bar := new Gtk_Level_Bar_Record;
   begin
      Gtk.Level_Bar.Initialize_For_Interval (Self, Min_Value, Max_Value);
      return Self;
   end Gtk_Level_Bar_New_For_Interval;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Level_Bar) is
   begin
      Self := new Gtk_Level_Bar_Record;
      Gtk.Level_Bar.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Gtk_New_For_Interval --
   --------------------------

   procedure Gtk_New_For_Interval
      (Self      : out Gtk_Level_Bar;
       Min_Value : Gdouble;
       Max_Value : Gdouble)
   is
   begin
      Self := new Gtk_Level_Bar_Record;
      Gtk.Level_Bar.Initialize_For_Interval (Self, Min_Value, Max_Value);
   end Gtk_New_For_Interval;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Level_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_level_bar_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------------
   -- Initialize_For_Interval --
   -----------------------------

   procedure Initialize_For_Interval
      (Self      : not null access Gtk_Level_Bar_Record'Class;
       Min_Value : Gdouble;
       Max_Value : Gdouble)
   is
      function Internal
         (Min_Value : Gdouble;
          Max_Value : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_level_bar_new_for_interval");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Min_Value, Max_Value));
      end if;
   end Initialize_For_Interval;

   ----------------------
   -- Add_Offset_Value --
   ----------------------

   procedure Add_Offset_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Name  : UTF8_String;
       Value : Gdouble)
   is
      procedure Internal
         (Self  : System.Address;
          Name  : Gtkada.Types.Chars_Ptr;
          Value : Gdouble);
      pragma Import (C, Internal, "gtk_level_bar_add_offset_value");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name, Value);
      Free (Tmp_Name);
   end Add_Offset_Value;

   ------------------
   -- Get_Inverted --
   ------------------

   function Get_Inverted
      (Self : not null access Gtk_Level_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_level_bar_get_inverted");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Inverted;

   -------------------
   -- Get_Max_Value --
   -------------------

   function Get_Max_Value
      (Self : not null access Gtk_Level_Bar_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_level_bar_get_max_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Value;

   -------------------
   -- Get_Min_Value --
   -------------------

   function Get_Min_Value
      (Self : not null access Gtk_Level_Bar_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_level_bar_get_min_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Min_Value;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Enums.Gtk_Level_Bar_Mode
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Level_Bar_Mode;
      pragma Import (C, Internal, "gtk_level_bar_get_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Mode;

   ----------------------
   -- Get_Offset_Value --
   ----------------------

   function Get_Offset_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Name  : UTF8_String := "";
       Value : access Gdouble) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Name      : Gtkada.Types.Chars_Ptr;
          Acc_Value : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_level_bar_get_offset_value");
      Acc_Value  : aliased Gdouble;
      Tmp_Name   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : Glib.Gboolean;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name, Acc_Value'Access);
      Free (Tmp_Name);
      Value.all := Acc_Value;
      return Tmp_Return /= 0;
   end Get_Offset_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gtk_Level_Bar_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_level_bar_get_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Value;

   -------------------------
   -- Remove_Offset_Value --
   -------------------------

   procedure Remove_Offset_Value
      (Self : not null access Gtk_Level_Bar_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_level_bar_remove_offset_value");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Remove_Offset_Value;

   ------------------
   -- Set_Inverted --
   ------------------

   procedure Set_Inverted
      (Self     : not null access Gtk_Level_Bar_Record;
       Inverted : Boolean)
   is
      procedure Internal (Self : System.Address; Inverted : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_level_bar_set_inverted");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Inverted));
   end Set_Inverted;

   -------------------
   -- Set_Max_Value --
   -------------------

   procedure Set_Max_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Value : Gdouble)
   is
      procedure Internal (Self : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_level_bar_set_max_value");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Max_Value;

   -------------------
   -- Set_Min_Value --
   -------------------

   procedure Set_Min_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Value : Gdouble)
   is
      procedure Internal (Self : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_level_bar_set_min_value");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Min_Value;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
      (Self : not null access Gtk_Level_Bar_Record;
       Mode : Gtk.Enums.Gtk_Level_Bar_Mode)
   is
      procedure Internal
         (Self : System.Address;
          Mode : Gtk.Enums.Gtk_Level_Bar_Mode);
      pragma Import (C, Internal, "gtk_level_bar_set_mode");
   begin
      Internal (Get_Object (Self), Mode);
   end Set_Mode;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Self  : not null access Gtk_Level_Bar_Record;
       Value : Gdouble)
   is
      procedure Internal (Self : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_level_bar_set_value");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Value;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Level_Bar_Record)
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
      (Self        : not null access Gtk_Level_Bar_Record;
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
     (Cb_Gtk_Level_Bar_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Level_Bar_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   procedure Connect
      (Object  : access Gtk_Level_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Level_Bar_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Level_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gtk_Level_Bar_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Level_Bar_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Level_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Level_Bar_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Level_Bar_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Level_Bar_Record'Class;
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

   ------------------------------------------
   -- Marsh_Gtk_Level_Bar_UTF8_String_Void --
   ------------------------------------------

   procedure Marsh_Gtk_Level_Bar_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Level_Bar_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Level_Bar := Gtk_Level_Bar (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Level_Bar_UTF8_String_Void;

   -----------------------
   -- On_Offset_Changed --
   -----------------------

   procedure On_Offset_Changed
      (Self  : not null access Gtk_Level_Bar_Record;
       Call  : Cb_Gtk_Level_Bar_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "offset-changed" & ASCII.NUL, Call, After);
   end On_Offset_Changed;

   -----------------------
   -- On_Offset_Changed --
   -----------------------

   procedure On_Offset_Changed
      (Self  : not null access Gtk_Level_Bar_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "offset-changed" & ASCII.NUL, Call, After, Slot);
   end On_Offset_Changed;

end Gtk.Level_Bar;
