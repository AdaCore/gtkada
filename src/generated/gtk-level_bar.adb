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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
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
       Value : out Gdouble) return Boolean
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
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name, Acc_Value'Access);
      Value := Acc_Value;
      Free (Tmp_Name);
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
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
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

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Level_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Level_Bar_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Level_Bar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Level_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

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

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Level_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Level_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Level_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Level_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Level_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

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

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Level_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Level_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

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
   exception
      when E : others => Process_Exception (E);
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
   exception
      when E : others => Process_Exception (E);
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
