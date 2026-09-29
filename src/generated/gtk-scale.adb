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
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Scale is

   procedure C_Gtk_Scale_Set_Format_Value_Func
      (Self           : System.Address;
       Func           : System.Address;
       User_Data      : System.Address;
       Destroy_Notify : System.Address);
   pragma Import (C, C_Gtk_Scale_Set_Format_Value_Func, "gtk_scale_set_format_value_func");
   --  Func allows you to change how the scale value is displayed.
   --  The given function will return an allocated string representing Value.
   --  That string will then be used to display the scale's value.
   --  If NULL is passed as Func, the value will be displayed on its own,
   --  rounded according to the value of the [propertyGtk.Scale:digits]
   --  property.
   --  @param Func function that formats the value
   --  @param User_Data user data to pass to Func
   --  @param Destroy_Notify destroy function for User_Data

   function To_Gtk_Scale_Format_Value_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Scale_Format_Value_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Scale_Format_Value_Func, System.Address);

   function Internal_Gtk_Scale_Format_Value_Func
      (Scale     : System.Address;
       Value     : Gdouble;
       User_Data : System.Address) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Internal_Gtk_Scale_Format_Value_Func);
   --  @param Scale The `GtkScale`
   --  @param Value The numeric value to format
   --  @param User_Data user data

   ------------------------------------------
   -- Internal_Gtk_Scale_Format_Value_Func --
   ------------------------------------------

   function Internal_Gtk_Scale_Format_Value_Func
      (Scale     : System.Address;
       Value     : Gdouble;
       User_Data : System.Address) return Gtkada.Types.Chars_Ptr
   is
      Func           : constant Gtk_Scale_Format_Value_Func := To_Gtk_Scale_Format_Value_Func (User_Data);
      Stub_Gtk_Scale : Gtk_Scale_Record;
   begin
      return New_String (Func (Gtk.Scale.Gtk_Scale (Get_User_Data (Scale, Stub_Gtk_Scale)), Value));
   end Internal_Gtk_Scale_Format_Value_Func;

   package Type_Conversion_Gtk_Scale is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scale_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Scale);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Self := new Gtk_Scale_Record;
      Gtk.Scale.Initialize (Self, Orientation, Adjustment);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Range --
   ------------------------

   procedure Gtk_New_With_Range
      (Self        : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble)
   is
   begin
      Self := new Gtk_Scale_Record;
      Gtk.Scale.Initialize_With_Range (Self, Orientation, Min, Max, Step);
   end Gtk_New_With_Range;

   -------------------
   -- Gtk_Scale_New --
   -------------------

   function Gtk_Scale_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scale
   is
      Self : constant Gtk_Scale := new Gtk_Scale_Record;
   begin
      Gtk.Scale.Initialize (Self, Orientation, Adjustment);
      return Self;
   end Gtk_Scale_New;

   ------------------------------
   -- Gtk_Scale_New_With_Range --
   ------------------------------

   function Gtk_Scale_New_With_Range
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble) return Gtk_Scale
   is
      Self : constant Gtk_Scale := new Gtk_Scale_Record;
   begin
      Gtk.Scale.Initialize_With_Range (Self, Orientation, Min, Max, Step);
      return Self;
   end Gtk_Scale_New_With_Range;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self        : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation;
          Adjustment  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Orientation, Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_With_Range --
   ---------------------------

   procedure Initialize_With_Range
      (Self        : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation;
          Min         : Gdouble;
          Max         : Gdouble;
          Step        : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_scale_new_with_range");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Orientation, Min, Max, Step));
      end if;
   end Initialize_With_Range;

   --------------
   -- Add_Mark --
   --------------

   procedure Add_Mark
      (Self     : not null access Gtk_Scale_Record;
       Value    : Gdouble;
       Position : Gtk.Enums.Gtk_Position_Type;
       Markup   : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Value    : Gdouble;
          Position : Gtk.Enums.Gtk_Position_Type;
          Markup   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_scale_add_mark");
      Tmp_Markup : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Markup :=
        (if Markup = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Markup));
      Internal (Get_Object (Self), Value, Position, Tmp_Markup);
      Free (Tmp_Markup);
   end Add_Mark;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Self : not null access Gtk_Scale_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_scale_clear_marks");
   begin
      Internal (Get_Object (Self));
   end Clear_Marks;

   ----------------
   -- Get_Digits --
   ----------------

   function Get_Digits
      (Self : not null access Gtk_Scale_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_scale_get_digits");
   begin
      return Internal (Get_Object (Self));
   end Get_Digits;

   --------------------
   -- Get_Draw_Value --
   --------------------

   function Get_Draw_Value
      (Self : not null access Gtk_Scale_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scale_get_draw_value");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Draw_Value;

   --------------------
   -- Get_Has_Origin --
   --------------------

   function Get_Has_Origin
      (Self : not null access Gtk_Scale_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scale_get_has_origin");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Origin;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Self : not null access Gtk_Scale_Record)
       return Pango.Layout.Pango_Layout
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_get_layout");
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Layout));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
      (Self : not null access Gtk_Scale_Record;
       X    : out Glib.Gint;
       Y    : out Glib.Gint)
   is
      procedure Internal
         (Self : System.Address;
          X    : out Glib.Gint;
          Y    : out Glib.Gint);
      pragma Import (C, Internal, "gtk_scale_get_layout_offsets");
   begin
      Internal (Get_Object (Self), X, Y);
   end Get_Layout_Offsets;

   -------------------
   -- Get_Value_Pos --
   -------------------

   function Get_Value_Pos
      (Self : not null access Gtk_Scale_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_scale_get_value_pos");
   begin
      return Internal (Get_Object (Self));
   end Get_Value_Pos;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Self       : not null access Gtk_Scale_Record;
       The_Digits : Glib.Gint)
   is
      procedure Internal (Self : System.Address; The_Digits : Glib.Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");
   begin
      Internal (Get_Object (Self), The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
      (Self       : not null access Gtk_Scale_Record;
       Draw_Value : Boolean)
   is
      procedure Internal (Self : System.Address; Draw_Value : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Draw_Value));
   end Set_Draw_Value;

   ---------------------------
   -- Set_Format_Value_Func --
   ---------------------------

   procedure Set_Format_Value_Func
      (Self : not null access Gtk_Scale_Record;
       Func : Gtk_Scale_Format_Value_Func)
   is
   begin
      if Func = null then
         C_Gtk_Scale_Set_Format_Value_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Scale_Set_Format_Value_Func (Get_Object (Self), Internal_Gtk_Scale_Format_Value_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Format_Value_Func;

   package body Set_Format_Value_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Scale_Format_Value_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Scale_Format_Value_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Scale_Format_Value_Func, System.Address);

      function Internal_Cb
         (Scale     : System.Address;
          Value     : Gdouble;
          User_Data : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Convention (C, Internal_Cb);
      --  Function that formats the value of a scale.
      --  See [methodGtk.Scale.set_format_value_func].
      --  @param Scale The `GtkScale`
      --  @param Value The numeric value to format
      --  @param User_Data user data
      --  @return A newly allocated string describing a textual representation
      --  of the given numerical value.

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Scale     : System.Address;
          Value     : Gdouble;
          User_Data : System.Address) return Gtkada.Types.Chars_Ptr
      is
         D              : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Scale : Gtk.Scale.Gtk_Scale_Record;
      begin
         return New_String (To_Gtk_Scale_Format_Value_Func (D.Func) (Gtk.Scale.Gtk_Scale (Get_User_Data (Scale, Stub_Gtk_Scale)), Value, D.Data.all));
      end Internal_Cb;

      ---------------------------
      -- Set_Format_Value_Func --
      ---------------------------

      procedure Set_Format_Value_Func
         (Self      : not null access Gtk.Scale.Gtk_Scale_Record'Class;
          Func      : Gtk_Scale_Format_Value_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Scale_Set_Format_Value_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), User_Data);
            C_Gtk_Scale_Set_Format_Value_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Format_Value_Func;

   end Set_Format_Value_Func_User_Data;

   --------------------
   -- Set_Has_Origin --
   --------------------

   procedure Set_Has_Origin
      (Self       : not null access Gtk_Scale_Record;
       Has_Origin : Boolean)
   is
      procedure Internal (Self : System.Address; Has_Origin : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scale_set_has_origin");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Origin));
   end Set_Has_Origin;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
      (Self : not null access Gtk_Scale_Record;
       Pos  : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Self : System.Address;
          Pos  : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");
   begin
      Internal (Get_Object (Self), Pos);
   end Set_Value_Pos;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Scale_Record;
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
      (Self : not null access Gtk_Scale_Record) return UTF8_String
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
      (Self : not null access Gtk_Scale_Record)
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
      (Self : not null access Gtk_Scale_Record)
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
      (Self : not null access Gtk_Scale_Record)
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
      (Self   : not null access Gtk_Scale_Record;
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
      (Self : not null access Gtk_Scale_Record)
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
      (Self : not null access Gtk_Scale_Record)
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
      (Self : not null access Gtk_Scale_Record)
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
      (Self  : not null access Gtk_Scale_Record;
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
      (Self     : not null access Gtk_Scale_Record;
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
      (Self     : not null access Gtk_Scale_Record;
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
      (Self  : not null access Gtk_Scale_Record;
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
      (Self         : not null access Gtk_Scale_Record;
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
      (Self        : not null access Gtk_Scale_Record;
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
      (Self        : not null access Gtk_Scale_Record;
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
      (Self  : not null access Gtk_Scale_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Scale;
