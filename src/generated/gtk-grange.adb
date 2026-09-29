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

package body Gtk.GRange is

   package Type_Conversion_Gtk_Range is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Range_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Range);

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
      (Self : not null access Gtk_Range_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_range_get_adjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Adjustment;

   --------------------
   -- Get_Fill_Level --
   --------------------

   function Get_Fill_Level
      (Self : not null access Gtk_Range_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_range_get_fill_level");
   begin
      return Internal (Get_Object (Self));
   end Get_Fill_Level;

   -------------------
   -- Get_Flippable --
   -------------------

   function Get_Flippable
      (Self : not null access Gtk_Range_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_range_get_flippable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Flippable;

   ------------------
   -- Get_Inverted --
   ------------------

   function Get_Inverted
      (Self : not null access Gtk_Range_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_range_get_inverted");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Inverted;

   --------------------
   -- Get_Range_Rect --
   --------------------

   procedure Get_Range_Rect
      (Self       : not null access Gtk_Range_Record;
       Range_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self       : System.Address;
          Range_Rect : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_range_get_range_rect");
   begin
      Internal (Get_Object (Self), Range_Rect);
   end Get_Range_Rect;

   --------------------------------
   -- Get_Restrict_To_Fill_Level --
   --------------------------------

   function Get_Restrict_To_Fill_Level
      (Self : not null access Gtk_Range_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_range_get_restrict_to_fill_level");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Restrict_To_Fill_Level;

   ----------------------
   -- Get_Round_Digits --
   ----------------------

   function Get_Round_Digits
      (Self : not null access Gtk_Range_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_range_get_round_digits");
   begin
      return Internal (Get_Object (Self));
   end Get_Round_Digits;

   -------------------------
   -- Get_Show_Fill_Level --
   -------------------------

   function Get_Show_Fill_Level
      (Self : not null access Gtk_Range_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_range_get_show_fill_level");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Fill_Level;

   ----------------------
   -- Get_Slider_Range --
   ----------------------

   procedure Get_Slider_Range
      (Self         : not null access Gtk_Range_Record;
       Slider_Start : out Glib.Gint;
       Slider_End   : out Glib.Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Slider_Start : out Glib.Gint;
          Slider_End   : out Glib.Gint);
      pragma Import (C, Internal, "gtk_range_get_slider_range");
   begin
      Internal (Get_Object (Self), Slider_Start, Slider_End);
   end Get_Slider_Range;

   ---------------------------
   -- Get_Slider_Size_Fixed --
   ---------------------------

   function Get_Slider_Size_Fixed
      (Self : not null access Gtk_Range_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_range_get_slider_size_fixed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Slider_Size_Fixed;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : not null access Gtk_Range_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_range_get_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Value;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Self       : not null access Gtk_Range_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_range_set_adjustment");
   begin
      Internal (Get_Object (Self), Get_Object (Adjustment));
   end Set_Adjustment;

   --------------------
   -- Set_Fill_Level --
   --------------------

   procedure Set_Fill_Level
      (Self       : not null access Gtk_Range_Record;
       Fill_Level : Gdouble)
   is
      procedure Internal (Self : System.Address; Fill_Level : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_fill_level");
   begin
      Internal (Get_Object (Self), Fill_Level);
   end Set_Fill_Level;

   -------------------
   -- Set_Flippable --
   -------------------

   procedure Set_Flippable
      (Self      : not null access Gtk_Range_Record;
       Flippable : Boolean)
   is
      procedure Internal (Self : System.Address; Flippable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_range_set_flippable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Flippable));
   end Set_Flippable;

   --------------------
   -- Set_Increments --
   --------------------

   procedure Set_Increments
      (Self : not null access Gtk_Range_Record;
       Step : Gdouble;
       Page : Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Step : Gdouble;
          Page : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_increments");
   begin
      Internal (Get_Object (Self), Step, Page);
   end Set_Increments;

   ------------------
   -- Set_Inverted --
   ------------------

   procedure Set_Inverted
      (Self    : not null access Gtk_Range_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_range_set_inverted");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Inverted;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
      (Self : not null access Gtk_Range_Record;
       Min  : Gdouble;
       Max  : Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          Min  : Gdouble;
          Max  : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_range");
   begin
      Internal (Get_Object (Self), Min, Max);
   end Set_Range;

   --------------------------------
   -- Set_Restrict_To_Fill_Level --
   --------------------------------

   procedure Set_Restrict_To_Fill_Level
      (Self                   : not null access Gtk_Range_Record;
       Restrict_To_Fill_Level : Boolean)
   is
      procedure Internal
         (Self                   : System.Address;
          Restrict_To_Fill_Level : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_range_set_restrict_to_fill_level");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Restrict_To_Fill_Level));
   end Set_Restrict_To_Fill_Level;

   ----------------------
   -- Set_Round_Digits --
   ----------------------

   procedure Set_Round_Digits
      (Self         : not null access Gtk_Range_Record;
       Round_Digits : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Round_Digits : Glib.Gint);
      pragma Import (C, Internal, "gtk_range_set_round_digits");
   begin
      Internal (Get_Object (Self), Round_Digits);
   end Set_Round_Digits;

   -------------------------
   -- Set_Show_Fill_Level --
   -------------------------

   procedure Set_Show_Fill_Level
      (Self            : not null access Gtk_Range_Record;
       Show_Fill_Level : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Show_Fill_Level : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_range_set_show_fill_level");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Fill_Level));
   end Set_Show_Fill_Level;

   ---------------------------
   -- Set_Slider_Size_Fixed --
   ---------------------------

   procedure Set_Slider_Size_Fixed
      (Self       : not null access Gtk_Range_Record;
       Size_Fixed : Boolean)
   is
      procedure Internal (Self : System.Address; Size_Fixed : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_range_set_slider_size_fixed");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Size_Fixed));
   end Set_Slider_Size_Fixed;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Self  : not null access Gtk_Range_Record;
       Value : Gdouble)
   is
      procedure Internal (Self : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_value");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Value;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Range_Record;
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
      (Self : not null access Gtk_Range_Record) return UTF8_String
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
      (Self : not null access Gtk_Range_Record)
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
      (Self : not null access Gtk_Range_Record)
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
      (Self : not null access Gtk_Range_Record)
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
      (Self   : not null access Gtk_Range_Record;
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
      (Self : not null access Gtk_Range_Record)
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
      (Self : not null access Gtk_Range_Record)
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
      (Self : not null access Gtk_Range_Record)
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
      (Self  : not null access Gtk_Range_Record;
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
      (Self     : not null access Gtk_Range_Record;
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
      (Self     : not null access Gtk_Range_Record;
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
      (Self  : not null access Gtk_Range_Record;
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
      (Self         : not null access Gtk_Range_Record;
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
      (Self        : not null access Gtk_Range_Record;
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
      (Self        : not null access Gtk_Range_Record;
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
      (Self  : not null access Gtk_Range_Record;
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
     (Cb_Gtk_Range_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Range_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Range_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Range_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Range_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Range_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Gdouble_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Gtk_Scroll_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Void);

   procedure Marsh_GObject_Gtk_Scroll_Type_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Gdouble_Boolean);

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

   procedure Marsh_Gtk_Range_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Range_Gdouble_Void);

   procedure Marsh_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean);

   procedure Marsh_Gtk_Range_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Range_Gtk_Scroll_Type_Void);

   procedure Marsh_Gtk_Range_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Range_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Gdouble_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Range_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Gtk_Scroll_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Range_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Range_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Range_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Gdouble_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Range_Record'Class;
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
      (Object  : access Gtk_Range_Record'Class;
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

   --------------------------------
   -- Marsh_GObject_Gdouble_Void --
   --------------------------------

   procedure Marsh_GObject_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Void;

   ---------------------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Gdouble_Boolean --
   ---------------------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1), Unchecked_To_Gdouble (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Gdouble_Boolean;

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

   ----------------------------------
   -- Marsh_Gtk_Range_Gdouble_Void --
   ----------------------------------

   procedure Marsh_Gtk_Range_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Range_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Range := Gtk_Range (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Range_Gdouble_Void;

   -----------------------------------------------------
   -- Marsh_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean --
   -----------------------------------------------------

   procedure Marsh_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Range := Gtk_Range (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1), Unchecked_To_Gdouble (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean;

   ------------------------------------------
   -- Marsh_Gtk_Range_Gtk_Scroll_Type_Void --
   ------------------------------------------

   procedure Marsh_Gtk_Range_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Range_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Range := Gtk_Range (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Range_Gtk_Scroll_Type_Void;

   --------------------------
   -- Marsh_Gtk_Range_Void --
   --------------------------

   procedure Marsh_Gtk_Range_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Range_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Range := Gtk_Range (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Range_Void;

   ----------------------
   -- On_Adjust_Bounds --
   ----------------------

   procedure On_Adjust_Bounds
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "adjust-bounds" & ASCII.NUL, Call, After);
   end On_Adjust_Bounds;

   ----------------------
   -- On_Adjust_Bounds --
   ----------------------

   procedure On_Adjust_Bounds
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "adjust-bounds" & ASCII.NUL, Call, After, Slot);
   end On_Adjust_Bounds;

   ---------------------
   -- On_Change_Value --
   ---------------------

   procedure On_Change_Value
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Gtk_Scroll_Type_Gdouble_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "change-value" & ASCII.NUL, Call, After);
   end On_Change_Value;

   ---------------------
   -- On_Change_Value --
   ---------------------

   procedure On_Change_Value
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Gdouble_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "change-value" & ASCII.NUL, Call, After, Slot);
   end On_Change_Value;

   --------------------
   -- On_Move_Slider --
   --------------------

   procedure On_Move_Slider
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Gtk_Scroll_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-slider" & ASCII.NUL, Call, After);
   end On_Move_Slider;

   --------------------
   -- On_Move_Slider --
   --------------------

   procedure On_Move_Slider
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-slider" & ASCII.NUL, Call, After, Slot);
   end On_Move_Slider;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_Gtk_Range_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "value-changed" & ASCII.NUL, Call, After);
   end On_Value_Changed;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
      (Self  : not null access Gtk_Range_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "value-changed" & ASCII.NUL, Call, After, Slot);
   end On_Value_Changed;

end Gtk.GRange;
