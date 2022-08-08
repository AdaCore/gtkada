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

package body Gtk.Scale is

   package Type_Conversion_Gtk_Scale is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scale_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Scale);

   --------------------
   -- Gtk_Hscale_New --
   --------------------

   function Gtk_Hscale_New
      (Adjustment : Gtk.Adjustment.Gtk_Adjustment := null) return Gtk_Hscale
   is
      Scale : constant Gtk_Hscale := new Gtk_Hscale_Record;
   begin
      Gtk.Scale.Initialize_Hscale (Scale, Adjustment);
      return Scale;
   end Gtk_Hscale_New;

   -------------------------------
   -- Gtk_Hscale_New_With_Range --
   -------------------------------

   function Gtk_Hscale_New_With_Range
      (Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble) return Gtk_Hscale
   is
      Scale : constant Gtk_Hscale := new Gtk_Hscale_Record;
   begin
      Gtk.Scale.Initialize_Hscale (Scale, Min, Max, Step);
      return Scale;
   end Gtk_Hscale_New_With_Range;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Scale       : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Scale := new Gtk_Scale_Record;
      Gtk.Scale.Initialize (Scale, Orientation, Adjustment);
   end Gtk_New;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
      (Scale      : out Gtk_Hscale;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Scale := new Gtk_Hscale_Record;
      Gtk.Scale.Initialize_Hscale (Scale, Adjustment);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
      (Scale : out Gtk_Hscale;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
   begin
      Scale := new Gtk_Hscale_Record;
      Gtk.Scale.Initialize_Hscale (Scale, Min, Max, Step);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Vscale --
   --------------------

   procedure Gtk_New_Vscale
      (Scale      : out Gtk_Vscale;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Scale := new Gtk_Vscale_Record;
      Gtk.Scale.Initialize_Vscale (Scale, Adjustment);
   end Gtk_New_Vscale;

   --------------------
   -- Gtk_New_Vscale --
   --------------------

   procedure Gtk_New_Vscale
      (Scale : out Gtk_Vscale;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
   begin
      Scale := new Gtk_Vscale_Record;
      Gtk.Scale.Initialize_Vscale (Scale, Min, Max, Step);
   end Gtk_New_Vscale;

   ------------------------
   -- Gtk_New_With_Range --
   ------------------------

   procedure Gtk_New_With_Range
      (Scale       : out Gtk_Scale;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Min         : Gdouble;
       Max         : Gdouble;
       Step        : Gdouble)
   is
   begin
      Scale := new Gtk_Scale_Record;
      Gtk.Scale.Initialize_With_Range (Scale, Orientation, Min, Max, Step);
   end Gtk_New_With_Range;

   -------------------
   -- Gtk_Scale_New --
   -------------------

   function Gtk_Scale_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scale
   is
      Scale : constant Gtk_Scale := new Gtk_Scale_Record;
   begin
      Gtk.Scale.Initialize (Scale, Orientation, Adjustment);
      return Scale;
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
      Scale : constant Gtk_Scale := new Gtk_Scale_Record;
   begin
      Gtk.Scale.Initialize_With_Range (Scale, Orientation, Min, Max, Step);
      return Scale;
   end Gtk_Scale_New_With_Range;

   --------------------
   -- Gtk_Vscale_New --
   --------------------

   function Gtk_Vscale_New
      (Adjustment : Gtk.Adjustment.Gtk_Adjustment := null) return Gtk_Vscale
   is
      Scale : constant Gtk_Vscale := new Gtk_Vscale_Record;
   begin
      Gtk.Scale.Initialize_Vscale (Scale, Adjustment);
      return Scale;
   end Gtk_Vscale_New;

   -------------------------------
   -- Gtk_Vscale_New_With_Range --
   -------------------------------

   function Gtk_Vscale_New_With_Range
      (Min  : Gdouble;
       Max  : Gdouble;
       Step : Gdouble) return Gtk_Vscale
   is
      Scale : constant Gtk_Vscale := new Gtk_Vscale_Record;
   begin
      Gtk.Scale.Initialize_Vscale (Scale, Min, Max, Step);
      return Scale;
   end Gtk_Vscale_New_With_Range;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Scale       : not null access Gtk_Scale_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation;
          Adjustment  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_new");
   begin
      if not Scale.Is_Created then
         Set_Object (Scale, Internal (Orientation, Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
      (Scale      : not null access Gtk_Hscale_Record'Class;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new");
   begin
      if not Scale.Is_Created then
         Set_Object (Scale, Internal (Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
      (Scale : not null access Gtk_Hscale_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
      function Internal
         (Min  : Gdouble;
          Max  : Gdouble;
          Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new_with_range");
   begin
      if not Scale.Is_Created then
         Set_Object (Scale, Internal (Min, Max, Step));
      end if;
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
      (Scale      : not null access Gtk_Vscale_Record'Class;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new");
   begin
      if not Scale.Is_Created then
         Set_Object (Scale, Internal (Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize_Vscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
      (Scale : not null access Gtk_Vscale_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
      function Internal
         (Min  : Gdouble;
          Max  : Gdouble;
          Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new_with_range");
   begin
      if not Scale.Is_Created then
         Set_Object (Scale, Internal (Min, Max, Step));
      end if;
   end Initialize_Vscale;

   ---------------------------
   -- Initialize_With_Range --
   ---------------------------

   procedure Initialize_With_Range
      (Scale       : not null access Gtk_Scale_Record'Class;
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
      if not Scale.Is_Created then
         Set_Object (Scale, Internal (Orientation, Min, Max, Step));
      end if;
   end Initialize_With_Range;

   --------------
   -- Add_Mark --
   --------------

   procedure Add_Mark
      (Scale    : not null access Gtk_Scale_Record;
       Value    : Gdouble;
       Position : Gtk.Enums.Gtk_Position_Type;
       Markup   : UTF8_String := "")
   is
      procedure Internal
         (Scale    : System.Address;
          Value    : Gdouble;
          Position : Gtk.Enums.Gtk_Position_Type;
          Markup   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_scale_add_mark");
      Tmp_Markup : Gtkada.Types.Chars_Ptr;
   begin
      if Markup = "" then
         Tmp_Markup := Gtkada.Types.Null_Ptr;
      else
         Tmp_Markup := New_String (Markup);
      end if;
      Internal (Get_Object (Scale), Value, Position, Tmp_Markup);
      Free (Tmp_Markup);
   end Add_Mark;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Scale : not null access Gtk_Scale_Record) is
      procedure Internal (Scale : System.Address);
      pragma Import (C, Internal, "gtk_scale_clear_marks");
   begin
      Internal (Get_Object (Scale));
   end Clear_Marks;

   ----------------
   -- Get_Digits --
   ----------------

   function Get_Digits
      (Scale : not null access Gtk_Scale_Record) return Glib.Gint
   is
      function Internal (Scale : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_scale_get_digits");
   begin
      return Internal (Get_Object (Scale));
   end Get_Digits;

   --------------------
   -- Get_Draw_Value --
   --------------------

   function Get_Draw_Value
      (Scale : not null access Gtk_Scale_Record) return Boolean
   is
      function Internal (Scale : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scale_get_draw_value");
   begin
      return Internal (Get_Object (Scale)) /= 0;
   end Get_Draw_Value;

   --------------------
   -- Get_Has_Origin --
   --------------------

   function Get_Has_Origin
      (Scale : not null access Gtk_Scale_Record) return Boolean
   is
      function Internal (Scale : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scale_get_has_origin");
   begin
      return Internal (Get_Object (Scale)) /= 0;
   end Get_Has_Origin;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Scale : not null access Gtk_Scale_Record)
       return Pango.Layout.Pango_Layout
   is
      function Internal (Scale : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_get_layout");
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Scale)), Stub_Pango_Layout));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
      (Scale : not null access Gtk_Scale_Record;
       X     : out Glib.Gint;
       Y     : out Glib.Gint)
   is
      procedure Internal
         (Scale : System.Address;
          X     : out Glib.Gint;
          Y     : out Glib.Gint);
      pragma Import (C, Internal, "gtk_scale_get_layout_offsets");
   begin
      Internal (Get_Object (Scale), X, Y);
   end Get_Layout_Offsets;

   -------------------
   -- Get_Value_Pos --
   -------------------

   function Get_Value_Pos
      (Scale : not null access Gtk_Scale_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Scale : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_scale_get_value_pos");
   begin
      return Internal (Get_Object (Scale));
   end Get_Value_Pos;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Scale      : not null access Gtk_Scale_Record;
       The_Digits : Glib.Gint)
   is
      procedure Internal (Scale : System.Address; The_Digits : Glib.Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");
   begin
      Internal (Get_Object (Scale), The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
      (Scale      : not null access Gtk_Scale_Record;
       Draw_Value : Boolean)
   is
      procedure Internal
         (Scale      : System.Address;
          Draw_Value : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");
   begin
      Internal (Get_Object (Scale), Boolean'Pos (Draw_Value));
   end Set_Draw_Value;

   --------------------
   -- Set_Has_Origin --
   --------------------

   procedure Set_Has_Origin
      (Scale      : not null access Gtk_Scale_Record;
       Has_Origin : Boolean)
   is
      procedure Internal
         (Scale      : System.Address;
          Has_Origin : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scale_set_has_origin");
   begin
      Internal (Get_Object (Scale), Boolean'Pos (Has_Origin));
   end Set_Has_Origin;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
      (Scale : not null access Gtk_Scale_Record;
       Pos   : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Scale : System.Address;
          Pos   : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");
   begin
      Internal (Get_Object (Scale), Pos);
   end Set_Value_Pos;

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

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Scale_Gdouble_UTF8_String, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Scale_Gdouble_UTF8_String);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_UTF8_String, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_UTF8_String);

   procedure Connect
      (Object  : access Gtk_Scale_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scale_Gdouble_UTF8_String;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Scale_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_UTF8_String;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdouble_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_UTF8_String);

   procedure Marsh_Gtk_Scale_Gdouble_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Scale_Gdouble_UTF8_String);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Scale_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scale_Gdouble_UTF8_String;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Scale_Gdouble_UTF8_String'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Scale_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_UTF8_String;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_UTF8_String'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------
   -- Marsh_GObject_Gdouble_UTF8_String --
   ---------------------------------------

   procedure Marsh_GObject_Gdouble_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_UTF8_String := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased UTF8_String := H (Obj, Unchecked_To_Gdouble (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_UTF8_String;

   -----------------------------------------
   -- Marsh_Gtk_Scale_Gdouble_UTF8_String --
   -----------------------------------------

   procedure Marsh_Gtk_Scale_Gdouble_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Scale_Gdouble_UTF8_String := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Scale := Gtk_Scale (Unchecked_To_Object (Params, 0));
      V   : aliased UTF8_String := H (Obj, Unchecked_To_Gdouble (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Scale_Gdouble_UTF8_String;

   ---------------------
   -- On_Format_Value --
   ---------------------

   procedure On_Format_Value
      (Self  : not null access Gtk_Scale_Record;
       Call  : Cb_Gtk_Scale_Gdouble_UTF8_String;
       After : Boolean := False)
   is
   begin
      Connect (Self, "format-value" & ASCII.NUL, Call, After);
   end On_Format_Value;

   ---------------------
   -- On_Format_Value --
   ---------------------

   procedure On_Format_Value
      (Self  : not null access Gtk_Scale_Record;
       Call  : Cb_GObject_Gdouble_UTF8_String;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "format-value" & ASCII.NUL, Call, After, Slot);
   end On_Format_Value;

end Gtk.Scale;
