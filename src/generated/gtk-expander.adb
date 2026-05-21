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
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Expander is

   package Type_Conversion_Gtk_Expander is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Expander_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Expander);

   ----------------------
   -- Gtk_Expander_New --
   ----------------------

   function Gtk_Expander_New (Label : UTF8_String := "") return Gtk_Expander is
      Self : constant Gtk_Expander := new Gtk_Expander_Record;
   begin
      Gtk.Expander.Initialize (Self, Label);
      return Self;
   end Gtk_Expander_New;

   ------------------------------------
   -- Gtk_Expander_New_With_Mnemonic --
   ------------------------------------

   function Gtk_Expander_New_With_Mnemonic
      (Label : UTF8_String := "") return Gtk_Expander
   is
      Self : constant Gtk_Expander := new Gtk_Expander_Record;
   begin
      Gtk.Expander.Initialize_With_Mnemonic (Self, Label);
      return Self;
   end Gtk_Expander_New_With_Mnemonic;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Expander; Label : UTF8_String := "") is
   begin
      Self := new Gtk_Expander_Record;
      Gtk.Expander.Initialize (Self, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Expander;
       Label : UTF8_String := "")
   is
   begin
      Self := new Gtk_Expander_Record;
      Gtk.Expander.Initialize_With_Mnemonic (Self, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Expander_Record'Class;
       Label : UTF8_String := "")
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Expander_Record'Class;
       Label : UTF8_String := "")
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new_with_mnemonic");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_With_Mnemonic;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_expander_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
      (Self : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_expanded");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Expanded;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Self : not null access Gtk_Expander_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_expander_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Label;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_expander_get_label_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Label_Widget;

   -------------------------
   -- Get_Resize_Toplevel --
   -------------------------

   function Get_Resize_Toplevel
      (Self : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_resize_toplevel");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Resize_Toplevel;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup
      (Self : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_use_markup");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Self : not null access Gtk_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_use_underline");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Underline;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Expander_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_expander_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   ------------------
   -- Set_Expanded --
   ------------------

   procedure Set_Expanded
      (Self     : not null access Gtk_Expander_Record;
       Expanded : Boolean)
   is
      procedure Internal (Self : System.Address; Expanded : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_expanded");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Expanded));
   end Set_Expanded;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Self  : not null access Gtk_Expander_Record;
       Label : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_expander_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Self         : not null access Gtk_Expander_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self         : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_expander_set_label_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Label_Widget)));
   end Set_Label_Widget;

   -------------------------
   -- Set_Resize_Toplevel --
   -------------------------

   procedure Set_Resize_Toplevel
      (Self            : not null access Gtk_Expander_Record;
       Resize_Toplevel : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Resize_Toplevel : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_resize_toplevel");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Resize_Toplevel));
   end Set_Resize_Toplevel;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
      (Self       : not null access Gtk_Expander_Record;
       Use_Markup : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Markup : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_use_markup");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Markup));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Self          : not null access Gtk_Expander_Record;
       Use_Underline : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Use_Underline : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_use_underline");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Expander_Record;
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
      (Self : not null access Gtk_Expander_Record) return UTF8_String
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
      (Self : not null access Gtk_Expander_Record)
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
      (Self : not null access Gtk_Expander_Record)
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
      (Self : not null access Gtk_Expander_Record)
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
      (Self   : not null access Gtk_Expander_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean
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
      X.all := Acc_X;
      Y.all := Acc_Y;
      Width.all := Acc_Width;
      Height.all := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Expander_Record)
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
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Expander_Record;
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
      (Self     : not null access Gtk_Expander_Record;
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
      (Self     : not null access Gtk_Expander_Record;
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
      (Self  : not null access Gtk_Expander_Record;
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
      (Self         : not null access Gtk_Expander_Record;
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

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Expander_Record;
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
      (Self  : not null access Gtk_Expander_Record;
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
