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
with Gdk.Display;
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Dialog is

   --  The ::response signal carries its response id as a gint, so the
   --  marshaller below needs an extractor for Gtk_Response_Type. It cannot
   --  live in Gtk.Arguments with its siblings: that would make
   --  Gtk.Arguments depend on this obsolescent package.

   function Unchecked_To_Gtk_Response_Type
   is new Glib.Values.Unsafe_Enum_Nth (Gtk_Response_Type);

   procedure G_New_Dialog
     (Self  : not null access Gtk_Dialog_Record'Class;
      Flags : Gtk_Dialog_Flags;
      Typ   : Glib.GType := Gtk.Dialog.Get_Type)
   is
      function Internal
        (Typ : GType; Flags : Gtk_Dialog_Flags) return System.Address;
      pragma Import (C, Internal, "ada_g_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Typ, Flags));
      end if;
   end G_New_Dialog;

   package Type_Conversion_Gtk_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Dialog);

   --------------------
   -- Gtk_Dialog_New --
   --------------------

   function Gtk_Dialog_New return Gtk_Dialog is
      Self : constant Gtk_Dialog := new Gtk_Dialog_Record;
   begin
      Gtk.Dialog.Initialize (Self);
      return Self;
   end Gtk_Dialog_New;

   --------------------
   -- Gtk_Dialog_New --
   --------------------

   function Gtk_Dialog_New
      (Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags) return Gtk_Dialog
   is
      Self : constant Gtk_Dialog := new Gtk_Dialog_Record;
   begin
      Gtk.Dialog.Initialize (Self, Title, Parent, Flags);
      return Self;
   end Gtk_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Dialog) is
   begin
      Self := new Gtk_Dialog_Record;
      Gtk.Dialog.Initialize (Self);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Dialog;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags)
   is
   begin
      Self := new Gtk_Dialog_Record;
      Gtk.Dialog.Initialize (Self, Title, Parent, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Dialog_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Dialog_Record'Class;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags)
   is
      function Internal
         (Title  : Gtkada.Types.Chars_Ptr;
          Parent : System.Address;
          Flags  : Gtk_Dialog_Flags) return System.Address;
      pragma Import (C, Internal, "ada_gtk_dialog_new_with_buttons");
      Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Title, Get_Object_Or_Null (GObject (Parent)), Flags);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Title);
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Self        : not null access Gtk_Dialog_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Gtk_Response_Type)
   is
      procedure Internal
         (Self        : System.Address;
          Child       : System.Address;
          Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_add_action_widget");
   begin
      Internal (Get_Object (Self), Get_Object (Child), Response_Id);
   end Add_Action_Widget;

   ----------------
   -- Add_Button --
   ----------------

   function Add_Button
      (Self        : not null access Gtk_Dialog_Record;
       Button_Text : UTF8_String;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self        : System.Address;
          Button_Text : Gtkada.Types.Chars_Ptr;
          Response_Id : Gtk_Response_Type) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_add_button");
      Tmp_Button_Text : Gtkada.Types.Chars_Ptr := New_String (Button_Text);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Button_Text, Response_Id);
      Free (Tmp_Button_Text);
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Tmp_Return, Stub_Gtk_Widget));
   end Add_Button;

   ----------------------
   -- Get_Content_Area --
   ----------------------

   function Get_Content_Area
      (Self : not null access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_content_area");
      Stub_Gtk_Box : Gtk.Box.Gtk_Box_Record;
   begin
      return Gtk.Box.Gtk_Box (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Box));
   end Get_Content_Area;

   --------------------
   -- Get_Header_Bar --
   --------------------

   function Get_Header_Bar
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_header_bar");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Header_Bar;

   -----------------------------
   -- Get_Response_For_Widget --
   -----------------------------

   function Get_Response_For_Widget
      (Self   : not null access Gtk_Dialog_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Response_Type
   is
      function Internal
         (Self   : System.Address;
          Widget : System.Address) return Gtk_Response_Type;
      pragma Import (C, Internal, "gtk_dialog_get_response_for_widget");
   begin
      return Internal (Get_Object (Self), Get_Object (Widget));
   end Get_Response_For_Widget;

   -----------------------------
   -- Get_Widget_For_Response --
   -----------------------------

   function Get_Widget_For_Response
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self        : System.Address;
          Response_Id : Gtk_Response_Type) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_widget_for_response");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self), Response_Id), Stub_Gtk_Widget));
   end Get_Widget_For_Response;

   --------------
   -- Response --
   --------------

   procedure Response
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type)
   is
      procedure Internal
         (Self        : System.Address;
          Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_response");
   begin
      Internal (Get_Object (Self), Response_Id);
   end Response;

   --------------------------
   -- Set_Default_Response --
   --------------------------

   procedure Set_Default_Response
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type)
   is
      procedure Internal
         (Self        : System.Address;
          Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_set_default_response");
   begin
      Internal (Get_Object (Self), Response_Id);
   end Set_Default_Response;

   ----------------------------
   -- Set_Response_Sensitive --
   ----------------------------

   procedure Set_Response_Sensitive
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type;
       Setting     : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Response_Id : Gtk_Response_Type;
          Setting     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_dialog_set_response_sensitive");
   begin
      Internal (Get_Object (Self), Response_Id, Boolean'Pos (Setting));
   end Set_Response_Sensitive;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Dialog_Record;
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
      (Self : not null access Gtk_Dialog_Record) return UTF8_String
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
      (Self : not null access Gtk_Dialog_Record)
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
      (Self : not null access Gtk_Dialog_Record)
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
      (Self : not null access Gtk_Dialog_Record)
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
      (Self   : not null access Gtk_Dialog_Record;
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

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_Dialog_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_focus");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Focus;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Dialog_Record)
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
      (Self  : not null access Gtk_Dialog_Record;
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

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_Dialog_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Dialog_Record;
       X    : out Gdouble;
       Y    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          X    : out Gdouble;
          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_native_get_surface_transform");
   begin
      Internal (Get_Object (Self), X, Y);
   end Get_Surface_Transform;

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Dialog_Record;
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
      (Self     : not null access Gtk_Dialog_Record;
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
      (Self  : not null access Gtk_Dialog_Record;
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
      (Self         : not null access Gtk_Dialog_Record;
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

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
      (Self  : not null access Gtk_Dialog_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Focus : System.Address);
      pragma Import (C, Internal, "gtk_root_set_focus");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Focus)));
   end Set_Focus;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gtk_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Dialog_Record;
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
      (Self  : not null access Gtk_Dialog_Record;
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
     (Cb_Gtk_Dialog_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Dialog_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Dialog_Gtk_Response_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Dialog_Gtk_Response_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Response_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Response_Type_Void);

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Response_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Response_Type_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Dialog_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Dialog_Gtk_Response_Type_Void);

   procedure Marsh_Gtk_Dialog_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Dialog_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Dialog_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Dialog_Gtk_Response_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Dialog_Record'Class;
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
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Response_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Response_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------
   -- Marsh_GObject_Gtk_Response_Type_Void --
   ------------------------------------------

   procedure Marsh_GObject_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Response_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Response_Type (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Response_Type_Void;

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

   ---------------------------------------------
   -- Marsh_Gtk_Dialog_Gtk_Response_Type_Void --
   ---------------------------------------------

   procedure Marsh_Gtk_Dialog_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Dialog_Gtk_Response_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Dialog := Gtk_Dialog (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Response_Type (Params, 1));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Dialog_Gtk_Response_Type_Void;

   ---------------------------
   -- Marsh_Gtk_Dialog_Void --
   ---------------------------

   procedure Marsh_Gtk_Dialog_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Dialog_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Dialog := Gtk_Dialog (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Dialog_Void;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_Gtk_Dialog_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "close" & ASCII.NUL, Call, After);
   end On_Close;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "close" & ASCII.NUL, Call, After, Slot);
   end On_Close;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "response" & ASCII.NUL, Call, After);
   end On_Response;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_GObject_Gtk_Response_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "response" & ASCII.NUL, Call, After, Slot);
   end On_Response;

end Gtk.Dialog;
