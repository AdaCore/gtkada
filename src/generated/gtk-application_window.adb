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

   ----------------------
   -- Get_Help_Overlay --
   ----------------------

   function Get_Help_Overlay
      (Self : not null access Gtk_Application_Window_Record)
       return Gtk.Shortcuts_Window.Gtk_Shortcuts_Window
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_application_window_get_help_overlay");
      Stub_Gtk_Shortcuts_Window : Gtk.Shortcuts_Window.Gtk_Shortcuts_Window_Record;
   begin
      return Gtk.Shortcuts_Window.Gtk_Shortcuts_Window (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Shortcuts_Window));
   end Get_Help_Overlay;

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
   -- Set_Help_Overlay --
   ----------------------

   procedure Set_Help_Overlay
      (Self         : not null access Gtk_Application_Window_Record;
       Help_Overlay : access Gtk.Shortcuts_Window.Gtk_Shortcuts_Window_Record'Class)
   is
      procedure Internal
         (Self         : System.Address;
          Help_Overlay : System.Address);
      pragma Import (C, Internal, "gtk_application_window_set_help_overlay");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Help_Overlay)));
   end Set_Help_Overlay;

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

end Gtk.Application_Window;
