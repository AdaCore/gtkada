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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Inscription is

   package Type_Conversion_Gtk_Inscription is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Inscription_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Inscription);

   -------------------------
   -- Gtk_Inscription_New --
   -------------------------

   function Gtk_Inscription_New
      (Text : UTF8_String := "") return Gtk_Inscription
   is
      Self : constant Gtk_Inscription := new Gtk_Inscription_Record;
   begin
      Gtk.Inscription.Initialize (Self, Text);
      return Self;
   end Gtk_Inscription_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Inscription; Text : UTF8_String := "") is
   begin
      Self := new Gtk_Inscription_Record;
      Gtk.Inscription.Initialize (Self, Text);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Inscription_Record'Class;
       Text : UTF8_String := "")
   is
      function Internal
         (Text : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_inscription_new");
      Tmp_Text   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Text :=
           (if Text = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Text));
         Tmp_Return := Internal (Tmp_Text);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Text);
   end Initialize;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Self : not null access Gtk_Inscription_Record)
       return Pango.Attributes.Pango_Attr_List
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_inscription_get_attributes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Attributes;

   -------------------
   -- Get_Min_Chars --
   -------------------

   function Get_Min_Chars
      (Self : not null access Gtk_Inscription_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_inscription_get_min_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Min_Chars;

   -------------------
   -- Get_Min_Lines --
   -------------------

   function Get_Min_Lines
      (Self : not null access Gtk_Inscription_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_inscription_get_min_lines");
   begin
      return Internal (Get_Object (Self));
   end Get_Min_Lines;

   -------------------
   -- Get_Nat_Chars --
   -------------------

   function Get_Nat_Chars
      (Self : not null access Gtk_Inscription_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_inscription_get_nat_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Nat_Chars;

   -------------------
   -- Get_Nat_Lines --
   -------------------

   function Get_Nat_Lines
      (Self : not null access Gtk_Inscription_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_inscription_get_nat_lines");
   begin
      return Internal (Get_Object (Self));
   end Get_Nat_Lines;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtk_Inscription_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_inscription_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Text;

   -----------------------
   -- Get_Text_Overflow --
   -----------------------

   function Get_Text_Overflow
      (Self : not null access Gtk_Inscription_Record)
       return Gtk_Inscription_Overflow
   is
      function Internal
         (Self : System.Address) return Gtk_Inscription_Overflow;
      pragma Import (C, Internal, "gtk_inscription_get_text_overflow");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Overflow;

   -------------------
   -- Get_Wrap_Mode --
   -------------------

   function Get_Wrap_Mode
      (Self : not null access Gtk_Inscription_Record)
       return Pango.Enums.Wrap_Mode
   is
      function Internal (Self : System.Address) return Pango.Enums.Wrap_Mode;
      pragma Import (C, Internal, "gtk_inscription_get_wrap_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Wrap_Mode;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign
      (Self : not null access Gtk_Inscription_Record)
       return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gtk_inscription_get_xalign");
   begin
      return Internal (Get_Object (Self));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign
      (Self : not null access Gtk_Inscription_Record)
       return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gtk_inscription_get_yalign");
   begin
      return Internal (Get_Object (Self));
   end Get_Yalign;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Self  : not null access Gtk_Inscription_Record;
       Attrs : Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal (Self : System.Address; Attrs : System.Address);
      pragma Import (C, Internal, "gtk_inscription_set_attributes");
   begin
      Internal (Get_Object (Self), Get_Object (Attrs));
   end Set_Attributes;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
      (Self   : not null access Gtk_Inscription_Record;
       Markup : UTF8_String := "")
   is
      procedure Internal
         (Self   : System.Address;
          Markup : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_inscription_set_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Markup :=
        (if Markup = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Markup));
      Internal (Get_Object (Self), Tmp_Markup);
      Free (Tmp_Markup);
   end Set_Markup;

   -------------------
   -- Set_Min_Chars --
   -------------------

   procedure Set_Min_Chars
      (Self      : not null access Gtk_Inscription_Record;
       Min_Chars : Guint)
   is
      procedure Internal (Self : System.Address; Min_Chars : Guint);
      pragma Import (C, Internal, "gtk_inscription_set_min_chars");
   begin
      Internal (Get_Object (Self), Min_Chars);
   end Set_Min_Chars;

   -------------------
   -- Set_Min_Lines --
   -------------------

   procedure Set_Min_Lines
      (Self      : not null access Gtk_Inscription_Record;
       Min_Lines : Guint)
   is
      procedure Internal (Self : System.Address; Min_Lines : Guint);
      pragma Import (C, Internal, "gtk_inscription_set_min_lines");
   begin
      Internal (Get_Object (Self), Min_Lines);
   end Set_Min_Lines;

   -------------------
   -- Set_Nat_Chars --
   -------------------

   procedure Set_Nat_Chars
      (Self      : not null access Gtk_Inscription_Record;
       Nat_Chars : Guint)
   is
      procedure Internal (Self : System.Address; Nat_Chars : Guint);
      pragma Import (C, Internal, "gtk_inscription_set_nat_chars");
   begin
      Internal (Get_Object (Self), Nat_Chars);
   end Set_Nat_Chars;

   -------------------
   -- Set_Nat_Lines --
   -------------------

   procedure Set_Nat_Lines
      (Self      : not null access Gtk_Inscription_Record;
       Nat_Lines : Guint)
   is
      procedure Internal (Self : System.Address; Nat_Lines : Guint);
      pragma Import (C, Internal, "gtk_inscription_set_nat_lines");
   begin
      Internal (Get_Object (Self), Nat_Lines);
   end Set_Nat_Lines;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gtk_Inscription_Record;
       Text : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_inscription_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Text :=
        (if Text = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Text));
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   -----------------------
   -- Set_Text_Overflow --
   -----------------------

   procedure Set_Text_Overflow
      (Self     : not null access Gtk_Inscription_Record;
       Overflow : Gtk_Inscription_Overflow)
   is
      procedure Internal
         (Self     : System.Address;
          Overflow : Gtk_Inscription_Overflow);
      pragma Import (C, Internal, "gtk_inscription_set_text_overflow");
   begin
      Internal (Get_Object (Self), Overflow);
   end Set_Text_Overflow;

   -------------------
   -- Set_Wrap_Mode --
   -------------------

   procedure Set_Wrap_Mode
      (Self      : not null access Gtk_Inscription_Record;
       Wrap_Mode : Pango.Enums.Wrap_Mode)
   is
      procedure Internal
         (Self      : System.Address;
          Wrap_Mode : Pango.Enums.Wrap_Mode);
      pragma Import (C, Internal, "gtk_inscription_set_wrap_mode");
   begin
      Internal (Get_Object (Self), Wrap_Mode);
   end Set_Wrap_Mode;

   ----------------
   -- Set_Xalign --
   ----------------

   procedure Set_Xalign
      (Self   : not null access Gtk_Inscription_Record;
       Xalign : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_inscription_set_xalign");
   begin
      Internal (Get_Object (Self), Xalign);
   end Set_Xalign;

   ----------------
   -- Set_Yalign --
   ----------------

   procedure Set_Yalign
      (Self   : not null access Gtk_Inscription_Record;
       Yalign : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Yalign : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_inscription_set_yalign");
   begin
      Internal (Get_Object (Self), Yalign);
   end Set_Yalign;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Inscription_Record;
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
      (Self : not null access Gtk_Inscription_Record) return UTF8_String
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
      (Self : not null access Gtk_Inscription_Record)
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
      (Self : not null access Gtk_Inscription_Record)
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
      (Self : not null access Gtk_Inscription_Record)
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
      (Self   : not null access Gtk_Inscription_Record;
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
      (Self : not null access Gtk_Inscription_Record)
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
      (Self : not null access Gtk_Inscription_Record)
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
      (Self  : not null access Gtk_Inscription_Record;
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
      (Self     : not null access Gtk_Inscription_Record;
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
      (Self     : not null access Gtk_Inscription_Record;
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
      (Self  : not null access Gtk_Inscription_Record;
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
      (Self         : not null access Gtk_Inscription_Record;
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

   ---------------------------
   -- Update_Caret_Position --
   ---------------------------

   procedure Update_Caret_Position
      (Self : not null access Gtk_Inscription_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_accessible_text_update_caret_position");
   begin
      Internal (Get_Object (Self));
   end Update_Caret_Position;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
      (Self    : not null access Gtk_Inscription_Record;
       Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
       Start   : Guint;
       The_End : Guint)
   is
      procedure Internal
         (Self    : System.Address;
          Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
          Start   : Guint;
          The_End : Guint);
      pragma Import (C, Internal, "gtk_accessible_text_update_contents");
   begin
      Internal (Get_Object (Self), Change, Start, The_End);
   end Update_Contents;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Inscription_Record;
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
      (Self  : not null access Gtk_Inscription_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   ----------------------------
   -- Update_Selection_Bound --
   ----------------------------

   procedure Update_Selection_Bound
      (Self : not null access Gtk_Inscription_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_accessible_text_update_selection_bound");
   begin
      Internal (Get_Object (Self));
   end Update_Selection_Bound;

end Gtk.Inscription;
