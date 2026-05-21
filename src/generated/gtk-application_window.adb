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

package body Gtk.Application_Window is

   package Type_Conversion_Gtk_Application_Window is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Application_Window_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Application_Window);

   --------------------------------
   -- Gtk_Application_Window_New --
   --------------------------------

   function Gtk_Application_Window_New
      (Application : not null access Gtk.Application.Gtk_Application_Record'Class)
       return Gtk_Application_Window
   is
      Self : constant Gtk_Application_Window := new Gtk_Application_Window_Record;
   begin
      Gtk.Application_Window.Initialize (Self, Application);
      return Self;
   end Gtk_Application_Window_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gtk_Application_Window;
       Application : not null access Gtk.Application.Gtk_Application_Record'Class)
   is
   begin
      Self := new Gtk_Application_Window_Record;
      Gtk.Application_Window.Initialize (Self, Application);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self        : not null access Gtk_Application_Window_Record'Class;
       Application : not null access Gtk.Application.Gtk_Application_Record'Class)
   is
      function Internal (Application : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_application_window_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Application)));
      end if;
   end Initialize;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
      (Self : not null access Gtk_Application_Window_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_application_window_get_id");
   begin
      return Internal (Get_Object (Self));
   end Get_Id;

   ----------------------
   -- Get_Show_Menubar --
   ----------------------

   function Get_Show_Menubar
      (Self : not null access Gtk_Application_Window_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_application_window_get_show_menubar");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Menubar;

   ----------------------
   -- Set_Show_Menubar --
   ----------------------

   procedure Set_Show_Menubar
      (Self         : not null access Gtk_Application_Window_Record;
       Show_Menubar : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Show_Menubar : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_application_window_set_show_menubar");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Menubar));
   end Set_Show_Menubar;

   ------------------
   -- Action_Added --
   ------------------

   procedure Action_Added
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_action_group_action_added");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Action_Added;

   ----------------------------
   -- Action_Enabled_Changed --
   ----------------------------

   procedure Action_Enabled_Changed
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Enabled     : Glib.Gboolean);
      pragma Import (C, Internal, "g_action_group_action_enabled_changed");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Boolean'Pos (Enabled));
      Free (Tmp_Action_Name);
   end Action_Enabled_Changed;

   --------------------
   -- Action_Removed --
   --------------------

   procedure Action_Removed
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_action_group_action_removed");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Action_Removed;

   --------------------------
   -- Action_State_Changed --
   --------------------------

   procedure Action_State_Changed
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr;
          State       : System.Address);
      pragma Import (C, Internal, "g_action_group_action_state_changed");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (State));
      Free (Tmp_Action_Name);
   end Action_State_Changed;

   ---------------------
   -- Activate_Action --
   ---------------------

   procedure Activate_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Parameter   : System.Address);
      pragma Import (C, Internal, "g_action_group_activate_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (Parameter));
      Free (Tmp_Action_Name);
   end Activate_Action;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
      (Self   : not null access Gtk_Application_Window_Record;
       Action : Glib.Action.Gaction)
   is
      procedure Internal
         (Self   : System.Address;
          Action : Glib.Action.Gaction);
      pragma Import (C, Internal, "g_action_map_add_action");
   begin
      Internal (Get_Object (Self), Action);
   end Add_Action;

   ------------------------
   -- Add_Action_Entries --
   ------------------------

   procedure Add_Action_Entries
      (Self      : not null access Gtk_Application_Window_Record;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address)
   is
      procedure Internal
         (Self      : System.Address;
          Entries   : GAction_Entry_Array;
          N_Entries : Glib.Gint;
          User_Data : System.Address);
      pragma Import (C, Internal, "g_action_map_add_action_entries");
   begin
      Internal (Get_Object (Self), Entries, Entries'Length, User_Data);
   end Add_Action_Entries;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Application_Window_Record;
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

   -------------------------
   -- Change_Action_State --
   -------------------------

   procedure Change_Action_State
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Value       : System.Address);
      pragma Import (C, Internal, "g_action_group_change_action_state");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (Value));
      Free (Tmp_Action_Name);
   end Change_Action_State;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Application_Window_Record)
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
      (Self : not null access Gtk_Application_Window_Record)
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
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   ------------------------
   -- Get_Action_Enabled --
   ------------------------

   function Get_Action_Enabled
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_group_get_action_enabled");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return /= 0;
   end Get_Action_Enabled;

   -------------------------------
   -- Get_Action_Parameter_Type --
   -------------------------------

   function Get_Action_Parameter_Type
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr)
          return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_group_get_action_parameter_type");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Variant.Gvariant_Type;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return;
   end Get_Action_Parameter_Type;

   ----------------------
   -- Get_Action_State --
   ----------------------

   function Get_Action_State
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_action_group_get_action_state");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return From_Object (Tmp_Return);
   end Get_Action_State;

   ---------------------------
   -- Get_Action_State_Hint --
   ---------------------------

   function Get_Action_State_Hint
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_action_group_get_action_state_hint");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return From_Object (Tmp_Return);
   end Get_Action_State_Hint;

   ---------------------------
   -- Get_Action_State_Type --
   ---------------------------

   function Get_Action_State_Type
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr)
          return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_group_get_action_state_type");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Variant.Gvariant_Type;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return;
   end Get_Action_State_Type;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Application_Window_Record)
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
      (Self   : not null access Gtk_Application_Window_Record;
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
      (Self : not null access Gtk_Application_Window_Record)
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
      (Self : not null access Gtk_Application_Window_Record)
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
      (Self : not null access Gtk_Application_Window_Record)
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
      (Self  : not null access Gtk_Application_Window_Record;
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

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Application_Window_Record;
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

   ----------------
   -- Has_Action --
   ----------------

   function Has_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_group_has_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return /= 0;
   end Has_Action;

   ------------------
   -- List_Actions --
   ------------------

   function List_Actions
      (Self : not null access Gtk_Application_Window_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_action_group_list_actions");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end List_Actions;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Action.Gaction;
      pragma Import (C, Internal, "g_action_map_lookup_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Action.Gaction;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return;
   end Lookup_Action;

   ------------------
   -- Query_Action --
   ------------------

   function Query_Action
      (Self           : not null access Gtk_Application_Window_Record;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self               : System.Address;
          Action_Name        : Gtkada.Types.Chars_Ptr;
          Acc_Enabled        : access Glib.Gboolean;
          Acc_Parameter_Type : access Glib.Variant.Gvariant_Type;
          Acc_State_Type     : access Glib.Variant.Gvariant_Type;
          Acc_State_Hint     : access System.Address;
          Acc_State          : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_group_query_action");
      Acc_Enabled        : aliased Boolean;
      Acc_Parameter_Type : aliased Glib.Variant.Gvariant_Type;
      Acc_State_Type     : aliased Glib.Variant.Gvariant_Type;
      Acc_State_Hint     : aliased Glib.Variant.Gvariant;
      Acc_State          : aliased Glib.Variant.Gvariant;
      Tmp_Action_Name    : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Acc_Enabled    : aliased Glib.Gboolean;
      Tmp_Acc_State_Hint : aliased System.Address;
      Tmp_Acc_State      : aliased System.Address;
      Tmp_Return         : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name, Tmp_Acc_Enabled'Access, Acc_Parameter_Type'Access, Acc_State_Type'Access, Tmp_Acc_State_Hint'Access, Tmp_Acc_State'Access);
      Acc_State := From_Object (Tmp_Acc_State);
      Acc_State_Hint := From_Object (Tmp_Acc_State_Hint);
      Acc_Enabled := Tmp_Acc_Enabled /= 0;
      Free (Tmp_Action_Name);
      Enabled.all := Acc_Enabled;
      if Parameter_Type /= null then
         Parameter_Type.all := Acc_Parameter_Type;
      end if;
      if State_Type /= null then
         State_Type.all := Acc_State_Type;
      end if;
      if State_Hint /= null then
         State_Hint.all := Acc_State_Hint;
      end if;
      if State /= null then
         State.all := Acc_State;
      end if;
      return Tmp_Return /= 0;
   end Query_Action;

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_Application_Window_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   -------------------
   -- Remove_Action --
   -------------------

   procedure Remove_Action
      (Self        : not null access Gtk_Application_Window_Record;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_action_map_remove_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Remove_Action;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Application_Window_Record;
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
      (Self     : not null access Gtk_Application_Window_Record;
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
      (Self  : not null access Gtk_Application_Window_Record;
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
      (Self         : not null access Gtk_Application_Window_Record;
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
      (Self  : not null access Gtk_Application_Window_Record;
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

   procedure Unrealize
      (Self : not null access Gtk_Application_Window_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Application_Window_Record;
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
      (Self  : not null access Gtk_Application_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Application_Window;
