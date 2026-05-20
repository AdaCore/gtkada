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

package body Gtk.Paned is

   package Type_Conversion_Gtk_Paned is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Paned_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Paned);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Paned       : out Gtk_Paned;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
   begin
      Paned := new Gtk_Paned_Record;
      Gtk.Paned.Initialize (Paned, Orientation);
   end Gtk_New;

   -------------------
   -- Gtk_Paned_New --
   -------------------

   function Gtk_Paned_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Paned
   is
      Paned : constant Gtk_Paned := new Gtk_Paned_Record;
   begin
      Gtk.Paned.Initialize (Paned, Orientation);
      return Paned;
   end Gtk_Paned_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Paned       : not null access Gtk_Paned_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_paned_new");
   begin
      if not Paned.Is_Created then
         Set_Object (Paned, Internal (Orientation));
      end if;
   end Initialize;

   -------------------
   -- Get_End_Child --
   -------------------

   function Get_End_Child
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_end_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub_Gtk_Widget));
   end Get_End_Child;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Paned : not null access Gtk_Paned_Record) return Glib.Gint
   is
      function Internal (Paned : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_paned_get_position");
   begin
      return Internal (Get_Object (Paned));
   end Get_Position;

   --------------------------
   -- Get_Resize_End_Child --
   --------------------------

   function Get_Resize_End_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paned_get_resize_end_child");
   begin
      return Internal (Get_Object (Paned)) /= 0;
   end Get_Resize_End_Child;

   ----------------------------
   -- Get_Resize_Start_Child --
   ----------------------------

   function Get_Resize_Start_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paned_get_resize_start_child");
   begin
      return Internal (Get_Object (Paned)) /= 0;
   end Get_Resize_Start_Child;

   --------------------------
   -- Get_Shrink_End_Child --
   --------------------------

   function Get_Shrink_End_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paned_get_shrink_end_child");
   begin
      return Internal (Get_Object (Paned)) /= 0;
   end Get_Shrink_End_Child;

   ----------------------------
   -- Get_Shrink_Start_Child --
   ----------------------------

   function Get_Shrink_Start_Child
      (Paned : not null access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paned_get_shrink_start_child");
   begin
      return Internal (Get_Object (Paned)) /= 0;
   end Get_Shrink_Start_Child;

   ---------------------
   -- Get_Start_Child --
   ---------------------

   function Get_Start_Child
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_start_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub_Gtk_Widget));
   end Get_Start_Child;

   ---------------------
   -- Get_Wide_Handle --
   ---------------------

   function Get_Wide_Handle
      (Paned : not null access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paned_get_wide_handle");
   begin
      return Internal (Get_Object (Paned)) /= 0;
   end Get_Wide_Handle;

   -------------------
   -- Set_End_Child --
   -------------------

   procedure Set_End_Child
      (Paned : not null access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_set_end_child");
   begin
      Internal (Get_Object (Paned), Get_Object_Or_Null (GObject (Child)));
   end Set_End_Child;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Paned    : not null access Gtk_Paned_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Paned : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_paned_set_position");
   begin
      Internal (Get_Object (Paned), Position);
   end Set_Position;

   --------------------------
   -- Set_Resize_End_Child --
   --------------------------

   procedure Set_Resize_End_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean)
   is
      procedure Internal (Paned : System.Address; Resize : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_set_resize_end_child");
   begin
      Internal (Get_Object (Paned), Boolean'Pos (Resize));
   end Set_Resize_End_Child;

   ----------------------------
   -- Set_Resize_Start_Child --
   ----------------------------

   procedure Set_Resize_Start_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean)
   is
      procedure Internal (Paned : System.Address; Resize : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_set_resize_start_child");
   begin
      Internal (Get_Object (Paned), Boolean'Pos (Resize));
   end Set_Resize_Start_Child;

   --------------------------
   -- Set_Shrink_End_Child --
   --------------------------

   procedure Set_Shrink_End_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean)
   is
      procedure Internal (Paned : System.Address; Resize : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_set_shrink_end_child");
   begin
      Internal (Get_Object (Paned), Boolean'Pos (Resize));
   end Set_Shrink_End_Child;

   ----------------------------
   -- Set_Shrink_Start_Child --
   ----------------------------

   procedure Set_Shrink_Start_Child
      (Paned  : not null access Gtk_Paned_Record;
       Resize : Boolean)
   is
      procedure Internal (Paned : System.Address; Resize : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_set_shrink_start_child");
   begin
      Internal (Get_Object (Paned), Boolean'Pos (Resize));
   end Set_Shrink_Start_Child;

   ---------------------
   -- Set_Start_Child --
   ---------------------

   procedure Set_Start_Child
      (Paned : not null access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_set_start_child");
   begin
      Internal (Get_Object (Paned), Get_Object_Or_Null (GObject (Child)));
   end Set_Start_Child;

   ---------------------
   -- Set_Wide_Handle --
   ---------------------

   procedure Set_Wide_Handle
      (Paned : not null access Gtk_Paned_Record;
       Wide  : Boolean)
   is
      procedure Internal (Paned : System.Address; Wide : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_paned_set_wide_handle");
   begin
      Internal (Get_Object (Paned), Boolean'Pos (Wide));
   end Set_Wide_Handle;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Paned_Record;
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
      (Self : not null access Gtk_Paned_Record) return UTF8_String
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
      (Self : not null access Gtk_Paned_Record)
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
      (Self : not null access Gtk_Paned_Record)
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
      (Self : not null access Gtk_Paned_Record)
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
      (Self   : not null access Gtk_Paned_Record;
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
      (Self : not null access Gtk_Paned_Record)
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
      (Self : not null access Gtk_Paned_Record)
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
      (Self  : not null access Gtk_Paned_Record;
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
      (Self     : not null access Gtk_Paned_Record;
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
      (Self     : not null access Gtk_Paned_Record;
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
      (Self  : not null access Gtk_Paned_Record;
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
      (Self         : not null access Gtk_Paned_Record;
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
      (Self        : not null access Gtk_Paned_Record;
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
      (Self  : not null access Gtk_Paned_Record;
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
     (Cb_Gtk_Paned_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Paned_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Paned_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Paned_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Boolean);

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Boolean;
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

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Boolean);

   procedure Marsh_Gtk_Paned_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Paned_Boolean);

   procedure Marsh_Gtk_Paned_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Paned_Boolean_Boolean);

   procedure Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Paned_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Paned_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
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

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Paned_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Boolean'Access,
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
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Boolean --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Boolean;

   -----------------------------
   -- Marsh_Gtk_Paned_Boolean --
   -----------------------------

   procedure Marsh_Gtk_Paned_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Paned_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Paned := Gtk_Paned (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Paned_Boolean;

   -------------------------------------
   -- Marsh_Gtk_Paned_Boolean_Boolean --
   -------------------------------------

   procedure Marsh_Gtk_Paned_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Paned_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Paned := Gtk_Paned (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Paned_Boolean_Boolean;

   ---------------------------------------------
   -- Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean --
   ---------------------------------------------

   procedure Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Paned := Gtk_Paned (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Paned_Gtk_Scroll_Type_Boolean;

   ------------------------
   -- On_Accept_Position --
   ------------------------

   procedure On_Accept_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accept-position" & ASCII.NUL, Call, After);
   end On_Accept_Position;

   ------------------------
   -- On_Accept_Position --
   ------------------------

   procedure On_Accept_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accept-position" & ASCII.NUL, Call, After, Slot);
   end On_Accept_Position;

   ------------------------
   -- On_Cancel_Position --
   ------------------------

   procedure On_Cancel_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel-position" & ASCII.NUL, Call, After);
   end On_Cancel_Position;

   ------------------------
   -- On_Cancel_Position --
   ------------------------

   procedure On_Cancel_Position
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancel-position" & ASCII.NUL, Call, After, Slot);
   end On_Cancel_Position;

   --------------------------
   -- On_Cycle_Child_Focus --
   --------------------------

   procedure On_Cycle_Child_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cycle-child-focus" & ASCII.NUL, Call, After);
   end On_Cycle_Child_Focus;

   --------------------------
   -- On_Cycle_Child_Focus --
   --------------------------

   procedure On_Cycle_Child_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cycle-child-focus" & ASCII.NUL, Call, After, Slot);
   end On_Cycle_Child_Focus;

   ---------------------------
   -- On_Cycle_Handle_Focus --
   ---------------------------

   procedure On_Cycle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cycle-handle-focus" & ASCII.NUL, Call, After);
   end On_Cycle_Handle_Focus;

   ---------------------------
   -- On_Cycle_Handle_Focus --
   ---------------------------

   procedure On_Cycle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cycle-handle-focus" & ASCII.NUL, Call, After, Slot);
   end On_Cycle_Handle_Focus;

   --------------------
   -- On_Move_Handle --
   --------------------

   procedure On_Move_Handle
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Gtk_Scroll_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-handle" & ASCII.NUL, Call, After);
   end On_Move_Handle;

   --------------------
   -- On_Move_Handle --
   --------------------

   procedure On_Move_Handle
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-handle" & ASCII.NUL, Call, After, Slot);
   end On_Move_Handle;

   ----------------------------
   -- On_Toggle_Handle_Focus --
   ----------------------------

   procedure On_Toggle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_Gtk_Paned_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-handle-focus" & ASCII.NUL, Call, After);
   end On_Toggle_Handle_Focus;

   ----------------------------
   -- On_Toggle_Handle_Focus --
   ----------------------------

   procedure On_Toggle_Handle_Focus
      (Self  : not null access Gtk_Paned_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-handle-focus" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Handle_Focus;

end Gtk.Paned;
