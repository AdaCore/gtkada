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
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Style is

   function From_Object_Free (B : access Gtk_Border) return Gtk_Border is
      Result : constant Gtk_Border := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   package Type_Conversion_Gtk_Style is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Style_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Style);

   --------------------
   -- Gtk_Border_New --
   --------------------

   function Gtk_Border_New return Gtk_Border is
      function Internal return Gtk_Border;
      pragma Import (C, Internal, "gtk_border_new");
      Self : Gtk_Border;
   begin
      Self := Internal;
      return Self;
   end Gtk_Border_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Style : out Gtk_Style) is
   begin
      Style := new Gtk_Style_Record;
      Gtk.Style.Initialize (Style);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Border) is
      function Internal return Gtk_Border;
      pragma Import (C, Internal, "gtk_border_new");
   begin
      Self := Internal;
   end Gtk_New;

   -------------------
   -- Gtk_Style_New --
   -------------------

   function Gtk_Style_New return Gtk_Style is
      Style : constant Gtk_Style := new Gtk_Style_Record;
   begin
      Gtk.Style.Initialize (Style);
      return Style;
   end Gtk_Style_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Style : not null access Gtk_Style_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_style_new");
   begin
      if not Style.Is_Created then
         Set_Object (Style, Internal);
      end if;
   end Initialize;

   ------------------------------
   -- Apply_Default_Background --
   ------------------------------

   procedure Apply_Default_Background
      (Style      : not null access Gtk_Style_Record;
       Cr         : Cairo.Cairo_Context;
       Window     : Gdk.Gdk_Window;
       State_Type : Gtk.Enums.Gtk_State_Type;
       X          : Glib.Gint;
       Y          : Glib.Gint;
       Width      : Glib.Gint;
       Height     : Glib.Gint)
   is
      procedure Internal
         (Style      : System.Address;
          Cr         : Cairo.Cairo_Context;
          Window     : Gdk.Gdk_Window;
          State_Type : Gtk.Enums.Gtk_State_Type;
          X          : Glib.Gint;
          Y          : Glib.Gint;
          Width      : Glib.Gint;
          Height     : Glib.Gint);
      pragma Import (C, Internal, "gtk_style_apply_default_background");
   begin
      Internal (Get_Object (Style), Cr, Window, State_Type, X, Y, Width, Height);
   end Apply_Default_Background;

   ------------
   -- Attach --
   ------------

   function Attach
      (Style  : not null access Gtk_Style_Record;
       Window : Gdk.Gdk_Window) return Gtk_Style
   is
      function Internal
         (Style  : System.Address;
          Window : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gtk_style_attach");
      Stub_Gtk_Style : Gtk_Style_Record;
   begin
      return Gtk.Style.Gtk_Style (Get_User_Data (Internal (Get_Object (Style), Window), Stub_Gtk_Style));
   end Attach;

   ----------
   -- Copy --
   ----------

   function Copy (Style : not null access Gtk_Style_Record) return Gtk_Style is
      function Internal (Style : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_copy");
      Stub_Gtk_Style : Gtk_Style_Record;
   begin
      return Gtk.Style.Gtk_Style (Get_User_Data (Internal (Get_Object (Style)), Stub_Gtk_Style));
   end Copy;

   ------------
   -- Detach --
   ------------

   procedure Detach (Style : not null access Gtk_Style_Record) is
      procedure Internal (Style : System.Address);
      pragma Import (C, Internal, "gtk_style_detach");
   begin
      Internal (Get_Object (Style));
   end Detach;

   ------------------------
   -- Get_Style_Property --
   ------------------------

   procedure Get_Style_Property
      (Style         : not null access Gtk_Style_Record;
       Widget_Type   : GType;
       Property_Name : UTF8_String;
       Value         : out Glib.Values.GValue)
   is
      procedure Internal
         (Style         : System.Address;
          Widget_Type   : GType;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_style_get_style_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Style), Widget_Type, Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Get_Style_Property;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context
      (Style : not null access Gtk_Style_Record) return Boolean
   is
      function Internal (Style : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_has_context");
   begin
      return Internal (Get_Object (Style)) /= 0;
   end Has_Context;

   ------------------
   -- Lookup_Color --
   ------------------

   procedure Lookup_Color
      (Style      : not null access Gtk_Style_Record;
       Color_Name : UTF8_String;
       Color      : out Gdk.Color.Gdk_Color;
       Found      : out Boolean)
   is
      function Internal
         (Style      : System.Address;
          Color_Name : Gtkada.Types.Chars_Ptr;
          Acc_Color  : access Gdk.Color.Gdk_Color) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_lookup_color");
      Acc_Color      : aliased Gdk.Color.Gdk_Color;
      Tmp_Color_Name : Gtkada.Types.Chars_Ptr := New_String (Color_Name);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Style), Tmp_Color_Name, Acc_Color'Access);
      Free (Tmp_Color_Name);
      Color := Acc_Color;
      Found := Tmp_Return /= 0;
   end Lookup_Color;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
      (Style      : not null access Gtk_Style_Record;
       Window     : Gdk.Gdk_Window;
       State_Type : Gtk.Enums.Gtk_State_Type)
   is
      procedure Internal
         (Style      : System.Address;
          Window     : Gdk.Gdk_Window;
          State_Type : Gtk.Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_style_set_background");
   begin
      Internal (Get_Object (Style), Window, State_Type);
   end Set_Background;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Style_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Style_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Style_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Style_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Style_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Style_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Style_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Style_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Style_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Style_Record'Class;
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

   --------------------------
   -- Marsh_Gtk_Style_Void --
   --------------------------

   procedure Marsh_Gtk_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Style_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Style := Gtk_Style (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Style_Void;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_Gtk_Style_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "realize" & ASCII.NUL, Call, After);
   end On_Realize;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "realize" & ASCII.NUL, Call, After, Slot);
   end On_Realize;

   ------------------
   -- On_Unrealize --
   ------------------

   procedure On_Unrealize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_Gtk_Style_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unrealize" & ASCII.NUL, Call, After);
   end On_Unrealize;

   ------------------
   -- On_Unrealize --
   ------------------

   procedure On_Unrealize
      (Self  : not null access Gtk_Style_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unrealize" & ASCII.NUL, Call, After, Slot);
   end On_Unrealize;

end Gtk.Style;
