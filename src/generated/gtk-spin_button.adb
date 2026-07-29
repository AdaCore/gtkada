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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Spin_Button is

   package Type_Conversion_Gtk_Spin_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Spin_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Spin_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gtk_Spin_Button;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint := 0)
   is
   begin
      Self := new Gtk_Spin_Button_Record;
      Gtk.Spin_Button.Initialize (Self, Adjustment, Climb_Rate, The_Digits);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Range --
   ------------------------

   procedure Gtk_New_With_Range
      (Self : out Gtk_Spin_Button;
       Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble)
   is
   begin
      Self := new Gtk_Spin_Button_Record;
      Gtk.Spin_Button.Initialize_With_Range (Self, Min, Max, Step);
   end Gtk_New_With_Range;

   -------------------------
   -- Gtk_Spin_Button_New --
   -------------------------

   function Gtk_Spin_Button_New
      (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint := 0) return Gtk_Spin_Button
   is
      Self : constant Gtk_Spin_Button := new Gtk_Spin_Button_Record;
   begin
      Gtk.Spin_Button.Initialize (Self, Adjustment, Climb_Rate, The_Digits);
      return Self;
   end Gtk_Spin_Button_New;

   ------------------------------------
   -- Gtk_Spin_Button_New_With_Range --
   ------------------------------------

   function Gtk_Spin_Button_New_With_Range
      (Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble) return Gtk_Spin_Button
   is
      Self : constant Gtk_Spin_Button := new Gtk_Spin_Button_Record;
   begin
      Gtk.Spin_Button.Initialize_With_Range (Self, Min, Max, Step);
      return Self;
   end Gtk_Spin_Button_New_With_Range;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gtk_Spin_Button_Record'Class;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint := 0)
   is
      function Internal
         (Adjustment : System.Address;
          Climb_Rate : Gdouble;
          The_Digits : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_spin_button_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Adjustment)), Climb_Rate, The_Digits));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_With_Range --
   ---------------------------

   procedure Initialize_With_Range
      (Self : not null access Gtk_Spin_Button_Record'Class;
       Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble)
   is
      function Internal
         (Min  : Gdouble;
          Max  : Gdouble;
          Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_spin_button_new_with_range");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Min, Max, Step));
      end if;
   end Initialize_With_Range;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
      (Self       : not null access Gtk_Spin_Button_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Climb_Rate : Gdouble;
       The_Digits : Guint)
   is
      procedure Internal
         (Self       : System.Address;
          Adjustment : System.Address;
          Climb_Rate : Gdouble;
          The_Digits : Guint);
      pragma Import (C, Internal, "gtk_spin_button_configure");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Adjustment)), Climb_Rate, The_Digits);
   end Configure;

   ---------------------------
   -- Get_Activates_Default --
   ---------------------------

   function Get_Activates_Default
      (Self : not null access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_activates_default");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activates_Default;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_spin_button_get_adjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Adjustment;

   --------------------
   -- Get_Climb_Rate --
   --------------------

   function Get_Climb_Rate
      (Self : not null access Gtk_Spin_Button_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_spin_button_get_climb_rate");
   begin
      return Internal (Get_Object (Self));
   end Get_Climb_Rate;

   ----------------
   -- Get_Digits --
   ----------------

   function Get_Digits
      (Self : not null access Gtk_Spin_Button_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_spin_button_get_digits");
   begin
      return Internal (Get_Object (Self));
   end Get_Digits;

   --------------------
   -- Get_Increments --
   --------------------

   procedure Get_Increments
      (Self : not null access Gtk_Spin_Button_Record;
       Step : out Gdouble;
       Page : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Step : out Gdouble;
          Page : out Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_get_increments");
   begin
      Internal (Get_Object (Self), Step, Page);
   end Get_Increments;

   -----------------
   -- Get_Numeric --
   -----------------

   function Get_Numeric
      (Self : not null access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_numeric");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Numeric;

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
      (Self : not null access Gtk_Spin_Button_Record;
       Min  : out Gdouble;
       Max  : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Min  : out Gdouble;
          Max  : out Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_get_range");
   begin
      Internal (Get_Object (Self), Min, Max);
   end Get_Range;

   -----------------------
   -- Get_Snap_To_Ticks --
   -----------------------

   function Get_Snap_To_Ticks
      (Self : not null access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_snap_to_ticks");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Snap_To_Ticks;

   -----------------------
   -- Get_Update_Policy --
   -----------------------

   function Get_Update_Policy
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk_Spin_Button_Update_Policy
   is
      function Internal
         (Self : System.Address) return Gtk_Spin_Button_Update_Policy;
      pragma Import (C, Internal, "gtk_spin_button_get_update_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Update_Policy;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gtk_Spin_Button_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_spin_button_get_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Value;

   ----------------------
   -- Get_Value_As_Int --
   ----------------------

   function Get_Value_As_Int
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_spin_button_get_value_as_int");
   begin
      return Internal (Get_Object (Self));
   end Get_Value_As_Int;

   --------------
   -- Get_Wrap --
   --------------

   function Get_Wrap
      (Self : not null access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_wrap");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Wrap;

   ---------------------------
   -- Set_Activates_Default --
   ---------------------------

   procedure Set_Activates_Default
      (Self              : not null access Gtk_Spin_Button_Record;
       Activates_Default : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Activates_Default : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_spin_button_set_activates_default");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Activates_Default));
   end Set_Activates_Default;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Self       : not null access Gtk_Spin_Button_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_spin_button_set_adjustment");
   begin
      Internal (Get_Object (Self), Get_Object (Adjustment));
   end Set_Adjustment;

   --------------------
   -- Set_Climb_Rate --
   --------------------

   procedure Set_Climb_Rate
      (Self       : not null access Gtk_Spin_Button_Record;
       Climb_Rate : Gdouble)
   is
      procedure Internal (Self : System.Address; Climb_Rate : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_climb_rate");
   begin
      Internal (Get_Object (Self), Climb_Rate);
   end Set_Climb_Rate;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Self       : not null access Gtk_Spin_Button_Record;
       The_Digits : Guint)
   is
      procedure Internal (Self : System.Address; The_Digits : Guint);
      pragma Import (C, Internal, "gtk_spin_button_set_digits");
   begin
      Internal (Get_Object (Self), The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Increments --
   --------------------

   procedure Set_Increments
      (Self : not null access Gtk_Spin_Button_Record;
       Step : Gdouble;
       Page : Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Step : Gdouble;
          Page : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_increments");
   begin
      Internal (Get_Object (Self), Step, Page);
   end Set_Increments;

   -----------------
   -- Set_Numeric --
   -----------------

   procedure Set_Numeric
      (Self    : not null access Gtk_Spin_Button_Record;
       Numeric : Boolean)
   is
      procedure Internal (Self : System.Address; Numeric : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_spin_button_set_numeric");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Numeric));
   end Set_Numeric;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
      (Self : not null access Gtk_Spin_Button_Record;
       Min  : Gdouble;
       Max  : Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Min  : Gdouble;
          Max  : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_range");
   begin
      Internal (Get_Object (Self), Min, Max);
   end Set_Range;

   -----------------------
   -- Set_Snap_To_Ticks --
   -----------------------

   procedure Set_Snap_To_Ticks
      (Self          : not null access Gtk_Spin_Button_Record;
       Snap_To_Ticks : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Snap_To_Ticks : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_spin_button_set_snap_to_ticks");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Snap_To_Ticks));
   end Set_Snap_To_Ticks;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
      (Self   : not null access Gtk_Spin_Button_Record;
       Policy : Gtk_Spin_Button_Update_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk_Spin_Button_Update_Policy);
      pragma Import (C, Internal, "gtk_spin_button_set_update_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Update_Policy;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Self  : not null access Gtk_Spin_Button_Record;
       Value : Gdouble)
   is
      procedure Internal (Self : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_value");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Value;

   --------------
   -- Set_Wrap --
   --------------

   procedure Set_Wrap
      (Self : not null access Gtk_Spin_Button_Record;
       Wrap : Boolean)
   is
      procedure Internal (Self : System.Address; Wrap : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_spin_button_set_wrap");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Wrap));
   end Set_Wrap;

   ----------
   -- Spin --
   ----------

   procedure Spin
      (Self      : not null access Gtk_Spin_Button_Record;
       Direction : Gtk_Spin_Type;
       Increment : Gdouble)
   is
      procedure Internal
         (Self      : System.Address;
          Direction : Gtk_Spin_Type;
          Increment : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_spin");
   begin
      Internal (Get_Object (Self), Direction, Increment);
   end Spin;

   ------------
   -- Update --
   ------------

   procedure Update (Self : not null access Gtk_Spin_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_spin_button_update");
   begin
      Internal (Get_Object (Self));
   end Update;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Spin_Button_Record;
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

   --------------------------------------------
   -- Delegate_Get_Accessible_Platform_State --
   --------------------------------------------

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Spin_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_delegate_get_accessible_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Delegate_Get_Accessible_Platform_State;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection
      (Self : not null access Gtk_Spin_Button_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");
   begin
      Internal (Get_Object (Self));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
      (Self      : not null access Gtk_Spin_Button_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");
   begin
      Internal (Get_Object (Self), Start_Pos, End_Pos);
   end Delete_Text;

   ------------------
   -- Editing_Done --
   ------------------

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Spin_Button_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_editing_done");
   begin
      Internal (Get_Object (Cell_Editable));
   end Editing_Done;

   ---------------------
   -- Finish_Delegate --
   ---------------------

   procedure Finish_Delegate (Self : not null access Gtk_Spin_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_finish_delegate");
   begin
      Internal (Get_Object (Self));
   end Finish_Delegate;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Spin_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Spin_Button_Record)
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
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
      (Self : not null access Gtk_Spin_Button_Record)
       return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gtk_editable_get_alignment");
   begin
      return Internal (Get_Object (Self));
   end Get_Alignment;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Spin_Button_Record)
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
      (Self   : not null access Gtk_Spin_Button_Record;
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

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Self      : not null access Gtk_Spin_Button_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Delegate --
   ------------------

   function Get_Delegate
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Editable.Gtk_Editable
   is
      function Internal
         (Self : System.Address) return Gtk.Editable.Gtk_Editable;
      pragma Import (C, Internal, "gtk_editable_get_delegate");
   begin
      return Internal (Get_Object (Self));
   end Get_Delegate;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
      (Self : not null access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Editable;

   ---------------------
   -- Get_Enable_Undo --
   ---------------------

   function Get_Enable_Undo
      (Self : not null access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_enable_undo");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Undo;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Spin_Button_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_max_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Width_Chars;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Spin_Button_Record)
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
      (Self : not null access Gtk_Spin_Button_Record)
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
      (Self  : not null access Gtk_Spin_Button_Record;
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

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Spin_Button_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Self          : System.Address;
          Acc_Start_Pos : access Glib.Gint;
          Acc_End_Pos   : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Glib.Gint;
      Acc_End_Pos   : aliased Glib.Gint;
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtk_Spin_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Text;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
      (Self : not null access Gtk_Spin_Button_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Width_Chars;

   -------------------
   -- Init_Delegate --
   -------------------

   procedure Init_Delegate (Self : not null access Gtk_Spin_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_init_delegate");
   begin
      Internal (Get_Object (Self));
   end Init_Delegate;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Self     : not null access Gtk_Spin_Button_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Text     : Gtkada.Types.Chars_Ptr;
          Length   : Glib.Gint;
          Position : in out Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text, Length, Position);
      Free (Tmp_Text);
   end Insert_Text;

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Spin_Button_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_remove_widget");
   begin
      Internal (Get_Object (Cell_Editable));
   end Remove_Widget;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Spin_Button_Record;
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
      (Self     : not null access Gtk_Spin_Button_Record;
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
      (Self  : not null access Gtk_Spin_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Self      : not null access Gtk_Spin_Button_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");
   begin
      Internal (Get_Object (Self), Start_Pos, End_Pos);
   end Select_Region;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Spin_Button_Record;
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

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Self   : not null access Gtk_Spin_Button_Record;
       Xalign : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_editable_set_alignment");
   begin
      Internal (Get_Object (Self), Xalign);
   end Set_Alignment;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (Self        : not null access Gtk_Spin_Button_Record;
       Is_Editable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Is_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ---------------------
   -- Set_Enable_Undo --
   ---------------------

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Spin_Button_Record;
       Enable_Undo : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Enable_Undo : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_enable_undo");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enable_Undo));
   end Set_Enable_Undo;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Spin_Button_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_max_width_chars");
   begin
      Internal (Get_Object (Self), N_Chars);
   end Set_Max_Width_Chars;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Spin_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Self     : not null access Gtk_Spin_Button_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Position;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gtk_Spin_Button_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_editable_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Spin_Button_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_width_chars");
   begin
      Internal (Get_Object (Self), N_Chars);
   end Set_Width_Chars;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Spin_Button_Record;
       Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Cell_Editable : System.Address;
          Event         : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Get_Object (Event));
   end Start_Editing;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Spin_Button_Record;
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
      (Self  : not null access Gtk_Spin_Button_Record;
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
     (Cb_Gtk_Spin_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Spin_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Spin_Button_Gdouble_Gint, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Spin_Button_Gdouble_Gint);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Gint, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Gint);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Spin_Button_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Spin_Button_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Gdouble_Gint;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gint;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_GObject_Gdouble_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Gint);

   procedure Marsh_GObject_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Spin_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Spin_Button_Boolean);

   procedure Marsh_Gtk_Spin_Button_Gdouble_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Spin_Button_Gdouble_Gint);

   procedure Marsh_Gtk_Spin_Button_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Spin_Button_Gtk_Scroll_Type_Void);

   procedure Marsh_Gtk_Spin_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Spin_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Spin_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Spin_Button_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Gdouble_Gint;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Spin_Button_Gdouble_Gint'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Spin_Button_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Spin_Button_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
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
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Gint;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Gint'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Spin_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   --------------------------------
   -- Marsh_GObject_Gdouble_Gint --
   --------------------------------

   procedure Marsh_GObject_Gdouble_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Gint := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Glib.Gint := H (Obj, Unchecked_To_Gdouble_Access (Params, 1).all);
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Gint;

   ----------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Void;

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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -----------------------------------
   -- Marsh_Gtk_Spin_Button_Boolean --
   -----------------------------------

   procedure Marsh_Gtk_Spin_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Spin_Button_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Spin_Button := Gtk_Spin_Button (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Spin_Button_Boolean;

   ----------------------------------------
   -- Marsh_Gtk_Spin_Button_Gdouble_Gint --
   ----------------------------------------

   procedure Marsh_Gtk_Spin_Button_Gdouble_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Spin_Button_Gdouble_Gint := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Spin_Button := Gtk_Spin_Button (Unchecked_To_Object (Params, 0));
      V   : aliased Glib.Gint := H (Obj, Unchecked_To_Gdouble_Access (Params, 1).all);
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Spin_Button_Gdouble_Gint;

   ------------------------------------------------
   -- Marsh_Gtk_Spin_Button_Gtk_Scroll_Type_Void --
   ------------------------------------------------

   procedure Marsh_Gtk_Spin_Button_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Spin_Button := Gtk_Spin_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Spin_Button_Gtk_Scroll_Type_Void;

   --------------------------------
   -- Marsh_Gtk_Spin_Button_Void --
   --------------------------------

   procedure Marsh_Gtk_Spin_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Spin_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Spin_Button := Gtk_Spin_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Spin_Button_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   ---------------------
   -- On_Change_Value --
   ---------------------

   procedure On_Change_Value
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Gtk_Scroll_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "change-value" & ASCII.NUL, Call, After);
   end On_Change_Value;

   ---------------------
   -- On_Change_Value --
   ---------------------

   procedure On_Change_Value
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "change-value" & ASCII.NUL, Call, After, Slot);
   end On_Change_Value;

   --------------
   -- On_Input --
   --------------

   procedure On_Input
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Gdouble_Gint;
       After : Boolean := False)
   is
   begin
      Connect (Self, "input" & ASCII.NUL, Call, After);
   end On_Input;

   --------------
   -- On_Input --
   --------------

   procedure On_Input
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Gdouble_Gint;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "input" & ASCII.NUL, Call, After, Slot);
   end On_Input;

   ---------------
   -- On_Output --
   ---------------

   procedure On_Output
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "output" & ASCII.NUL, Call, After);
   end On_Output;

   ---------------
   -- On_Output --
   ---------------

   procedure On_Output
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "output" & ASCII.NUL, Call, After, Slot);
   end On_Output;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "value-changed" & ASCII.NUL, Call, After);
   end On_Value_Changed;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "value-changed" & ASCII.NUL, Call, After, Slot);
   end On_Value_Changed;

   ----------------
   -- On_Wrapped --
   ----------------

   procedure On_Wrapped
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_Gtk_Spin_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "wrapped" & ASCII.NUL, Call, After);
   end On_Wrapped;

   ----------------
   -- On_Wrapped --
   ----------------

   procedure On_Wrapped
      (Self  : not null access Gtk_Spin_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "wrapped" & ASCII.NUL, Call, After, Slot);
   end On_Wrapped;

end Gtk.Spin_Button;
