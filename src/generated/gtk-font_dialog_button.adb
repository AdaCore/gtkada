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
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Font_Dialog_Button is

   package Type_Conversion_Gtk_Font_Dialog_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Dialog_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Font_Dialog_Button);

   --------------------------------
   -- Gtk_Font_Dialog_Button_New --
   --------------------------------

   function Gtk_Font_Dialog_Button_New
      (Dialog : access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class)
       return Gtk_Font_Dialog_Button
   is
      Self : constant Gtk_Font_Dialog_Button := new Gtk_Font_Dialog_Button_Record;
   begin
      Gtk.Font_Dialog_Button.Initialize (Self, Dialog);
      return Self;
   end Gtk_Font_Dialog_Button_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Font_Dialog_Button;
       Dialog : access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class)
   is
   begin
      Self := new Gtk_Font_Dialog_Button_Record;
      Gtk.Font_Dialog_Button.Initialize (Self, Dialog);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Font_Dialog_Button_Record'Class;
       Dialog : access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class)
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_button_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Dialog))));
      end if;
   end Initialize;

   ----------------
   -- Get_Dialog --
   ----------------

   function Get_Dialog
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk.Font_Dialog.Gtk_Font_Dialog
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_dialog");
      Stub_Gtk_Font_Dialog : Gtk.Font_Dialog.Gtk_Font_Dialog_Record;
   begin
      return Gtk.Font_Dialog.Gtk_Font_Dialog (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Font_Dialog));
   end Get_Dialog;

   -------------------
   -- Get_Font_Desc --
   -------------------

   function Get_Font_Desc
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self : System.Address) return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_font_desc");
   begin
      return Internal (Get_Object (Self));
   end Get_Font_Desc;

   -----------------------
   -- Get_Font_Features --
   -----------------------

   function Get_Font_Features
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_font_features");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Font_Features;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Pango.Language.Pango_Language
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_language");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Language;

   ---------------
   -- Get_Level --
   ---------------

   function Get_Level
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return Gtk_Font_Level
   is
      function Internal (Self : System.Address) return Gtk_Font_Level;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_level");
   begin
      return Internal (Get_Object (Self));
   end Get_Level;

   ------------------
   -- Get_Use_Font --
   ------------------

   function Get_Use_Font
      (Self : not null access Gtk_Font_Dialog_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_use_font");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Font;

   ------------------
   -- Get_Use_Size --
   ------------------

   function Get_Use_Size
      (Self : not null access Gtk_Font_Dialog_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_font_dialog_button_get_use_size");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Size;

   ----------------
   -- Set_Dialog --
   ----------------

   procedure Set_Dialog
      (Self   : not null access Gtk_Font_Dialog_Button_Record;
       Dialog : not null access Gtk.Font_Dialog.Gtk_Font_Dialog_Record'Class)
   is
      procedure Internal (Self : System.Address; Dialog : System.Address);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_dialog");
   begin
      Internal (Get_Object (Self), Get_Object (Dialog));
   end Set_Dialog;

   -------------------
   -- Set_Font_Desc --
   -------------------

   procedure Set_Font_Desc
      (Self      : not null access Gtk_Font_Dialog_Button_Record;
       Font_Desc : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
         (Self      : System.Address;
          Font_Desc : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_font_desc");
   begin
      Internal (Get_Object (Self), Font_Desc);
   end Set_Font_Desc;

   -----------------------
   -- Set_Font_Features --
   -----------------------

   procedure Set_Font_Features
      (Self          : not null access Gtk_Font_Dialog_Button_Record;
       Font_Features : UTF8_String := "")
   is
      procedure Internal
         (Self          : System.Address;
          Font_Features : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_font_features");
      Tmp_Font_Features : Gtkada.Types.Chars_Ptr;
   begin
      if Font_Features = "" then
         Tmp_Font_Features := Gtkada.Types.Null_Ptr;
      else
         Tmp_Font_Features := New_String (Font_Features);
      end if;
      Internal (Get_Object (Self), Tmp_Font_Features);
      Free (Tmp_Font_Features);
   end Set_Font_Features;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Language : Pango.Language.Pango_Language)
   is
      procedure Internal (Self : System.Address; Language : System.Address);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_language");
   begin
      Internal (Get_Object (Self), Get_Object (Language));
   end Set_Language;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       Level : Gtk_Font_Level)
   is
      procedure Internal (Self : System.Address; Level : Gtk_Font_Level);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_level");
   begin
      Internal (Get_Object (Self), Level);
   end Set_Level;

   ------------------
   -- Set_Use_Font --
   ------------------

   procedure Set_Use_Font
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Use_Font : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Font : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_use_font");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Font));
   end Set_Use_Font;

   ------------------
   -- Set_Use_Size --
   ------------------

   procedure Set_Use_Size
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
       Use_Size : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Size : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_font_dialog_button_set_use_size");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Size));
   end Set_Use_Size;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self : not null access Gtk_Font_Dialog_Button_Record)
       return UTF8_String
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
      (Self : not null access Gtk_Font_Dialog_Button_Record)
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
      (Self : not null access Gtk_Font_Dialog_Button_Record)
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
      (Self : not null access Gtk_Font_Dialog_Button_Record)
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
      (Self   : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self : not null access Gtk_Font_Dialog_Button_Record)
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
      (Self : not null access Gtk_Font_Dialog_Button_Record)
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
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self     : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self         : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self        : not null access Gtk_Font_Dialog_Button_Record;
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
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
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
     (Cb_Gtk_Font_Dialog_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Font_Dialog_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Font_Dialog_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Font_Dialog_Button_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Font_Dialog_Button_Record'Class;
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

   procedure Marsh_Gtk_Font_Dialog_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Font_Dialog_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Font_Dialog_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Font_Dialog_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Font_Dialog_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Font_Dialog_Button_Record'Class;
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

   ---------------------------------------
   -- Marsh_Gtk_Font_Dialog_Button_Void --
   ---------------------------------------

   procedure Marsh_Gtk_Font_Dialog_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Font_Dialog_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Font_Dialog_Button := Gtk_Font_Dialog_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Font_Dialog_Button_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       Call  : Cb_Gtk_Font_Dialog_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Font_Dialog_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.Font_Dialog_Button;
