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

package body Gtk.Expander is

   package Type_Conversion_Gtk_Expander is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Expander_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Expander);

   ----------------------
   -- Gtk_Expander_New --
   ----------------------

   function Gtk_Expander_New (Label : UTF8_String := "") return Gtk_Expander is
      Expander : constant Gtk_Expander := new Gtk_Expander_Record;
   begin
      Gtk.Expander.Initialize (Expander, Label);
      return Expander;
   end Gtk_Expander_New;

   ------------------------------------
   -- Gtk_Expander_New_With_Mnemonic --
   ------------------------------------

   function Gtk_Expander_New_With_Mnemonic
      (Label : UTF8_String := "") return Gtk_Expander
   is
      Expander : constant Gtk_Expander := new Gtk_Expander_Record;
   begin
      Gtk.Expander.Initialize_With_Mnemonic (Expander, Label);
      return Expander;
   end Gtk_Expander_New_With_Mnemonic;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Expander : out Gtk_Expander;
       Label    : UTF8_String := "")
   is
   begin
      Expander := new Gtk_Expander_Record;
      Gtk.Expander.Initialize (Expander, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Expander : out Gtk_Expander;
       Label    : UTF8_String := "")
   is
   begin
      Expander := new Gtk_Expander_Record;
      Gtk.Expander.Initialize_With_Mnemonic (Expander, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Expander : not null access Gtk_Expander_Record'Class;
       Label    : UTF8_String := "")
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Expander.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Expander, Tmp_Return);
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Expander : not null access Gtk_Expander_Record'Class;
       Label    : UTF8_String := "")
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new_with_mnemonic");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Expander.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Expander, Tmp_Return);
      end if;
   end Initialize_With_Mnemonic;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
      (Expander : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_expanded");
   begin
      return Internal (Get_Object (Expander)) /= 0;
   end Get_Expanded;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Expander : not null access Gtk_Expander_Record) return UTF8_String
   is
      function Internal
         (Expander : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_expander_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Expander)));
   end Get_Label;

   --------------------
   -- Get_Label_Fill --
   --------------------

   function Get_Label_Fill
      (Expander : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_label_fill");
   begin
      return Internal (Get_Object (Expander)) /= 0;
   end Get_Label_Fill;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Expander : not null access Gtk_Expander_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Expander : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_expander_get_label_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Expander)), Stub_Gtk_Widget));
   end Get_Label_Widget;

   -------------------------
   -- Get_Resize_Toplevel --
   -------------------------

   function Get_Resize_Toplevel
      (Expander : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_resize_toplevel");
   begin
      return Internal (Get_Object (Expander)) /= 0;
   end Get_Resize_Toplevel;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
      (Expander : not null access Gtk_Expander_Record) return Glib.Gint
   is
      function Internal (Expander : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_expander_get_spacing");
   begin
      return Internal (Get_Object (Expander));
   end Get_Spacing;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup
      (Expander : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_use_markup");
   begin
      return Internal (Get_Object (Expander)) /= 0;
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Expander : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_use_underline");
   begin
      return Internal (Get_Object (Expander)) /= 0;
   end Get_Use_Underline;

   ------------------
   -- Set_Expanded --
   ------------------

   procedure Set_Expanded
      (Expander : not null access Gtk_Expander_Record;
       Expanded : Boolean)
   is
      procedure Internal
         (Expander : System.Address;
          Expanded : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_expanded");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Expanded));
   end Set_Expanded;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Expander : not null access Gtk_Expander_Record;
       Label    : UTF8_String := "")
   is
      procedure Internal
         (Expander : System.Address;
          Label    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_expander_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Expander), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   --------------------
   -- Set_Label_Fill --
   --------------------

   procedure Set_Label_Fill
      (Expander   : not null access Gtk_Expander_Record;
       Label_Fill : Boolean)
   is
      procedure Internal
         (Expander   : System.Address;
          Label_Fill : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_label_fill");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Label_Fill));
   end Set_Label_Fill;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Expander     : not null access Gtk_Expander_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Expander     : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_expander_set_label_widget");
   begin
      Internal (Get_Object (Expander), Get_Object_Or_Null (GObject (Label_Widget)));
   end Set_Label_Widget;

   -------------------------
   -- Set_Resize_Toplevel --
   -------------------------

   procedure Set_Resize_Toplevel
      (Expander        : not null access Gtk_Expander_Record;
       Resize_Toplevel : Boolean)
   is
      procedure Internal
         (Expander        : System.Address;
          Resize_Toplevel : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_resize_toplevel");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Resize_Toplevel));
   end Set_Resize_Toplevel;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Expander : not null access Gtk_Expander_Record;
       Spacing  : Glib.Gint)
   is
      procedure Internal (Expander : System.Address; Spacing : Glib.Gint);
      pragma Import (C, Internal, "gtk_expander_set_spacing");
   begin
      Internal (Get_Object (Expander), Spacing);
   end Set_Spacing;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
      (Expander   : not null access Gtk_Expander_Record;
       Use_Markup : Boolean)
   is
      procedure Internal
         (Expander   : System.Address;
          Use_Markup : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_use_markup");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Use_Markup));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Expander      : not null access Gtk_Expander_Record;
       Use_Underline : Boolean)
   is
      procedure Internal
         (Expander      : System.Address;
          Use_Underline : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_use_underline");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Expander_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Expander_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Expander_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Expander_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Expander_Record'Class;
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

   procedure Marsh_Gtk_Expander_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Expander_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Expander_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Expander_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Expander_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Expander_Record'Class;
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

   -----------------------------
   -- Marsh_Gtk_Expander_Void --
   -----------------------------

   procedure Marsh_Gtk_Expander_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Expander_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Expander := Gtk_Expander (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Expander_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Expander_Record;
       Call  : Cb_Gtk_Expander_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Expander_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.Expander;
