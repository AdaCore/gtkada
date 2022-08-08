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

package body Gtk.Application is

   package Type_Conversion_Gtk_Application is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Application_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Application);

   -------------------------
   -- Gtk_Application_New --
   -------------------------

   function Gtk_Application_New
      (Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags)
       return Gtk_Application
   is
      Self : constant Gtk_Application := new Gtk_Application_Record;
   begin
      Gtk.Application.Initialize (Self, Application_Id, Flags);
      return Self;
   end Gtk_Application_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self           : out Gtk_Application;
       Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags)
   is
   begin
      Self := new Gtk_Application_Record;
      Gtk.Application.Initialize (Self, Application_Id, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self           : not null access Gtk_Application_Record'Class;
       Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags)
   is
      function Internal
         (Application_Id : Gtkada.Types.Chars_Ptr;
          Flags          : Glib.Application.GApplication_Flags)
          return System.Address;
      pragma Import (C, Internal, "gtk_application_new");
      Tmp_Application_Id : Gtkada.Types.Chars_Ptr;
      Tmp_Return         : System.Address;
   begin
      if not Self.Is_Created then
         if Application_Id = "" then
            Tmp_Application_Id := Gtkada.Types.Null_Ptr;
         else
            Tmp_Application_Id := New_String (Application_Id);
         end if;
         Tmp_Return := Internal (Tmp_Application_Id, Flags);
         Free (Tmp_Application_Id);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ---------------------
   -- Add_Accelerator --
   ---------------------

   procedure Add_Accelerator
      (Self        : not null access Gtk_Application_Record;
       Accelerator : UTF8_String;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Accelerator : Gtkada.Types.Chars_Ptr;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Parameter   : System.Address);
      pragma Import (C, Internal, "gtk_application_add_accelerator");
      Tmp_Accelerator : Gtkada.Types.Chars_Ptr := New_String (Accelerator);
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Accelerator, Tmp_Action_Name, Get_Object (Parameter));
      Free (Tmp_Action_Name);
      Free (Tmp_Accelerator);
   end Add_Accelerator;

   ----------------
   -- Add_Window --
   ----------------

   procedure Add_Window
      (Self   : not null access Gtk_Application_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class)
   is
      procedure Internal (Self : System.Address; Window : System.Address);
      pragma Import (C, Internal, "gtk_application_add_window");
   begin
      Internal (Get_Object (Self), Get_Object (Window));
   end Add_Window;

   ---------------------------
   -- Get_Accels_For_Action --
   ---------------------------

   function Get_Accels_For_Action
      (Self                 : not null access Gtk_Application_Record;
       Detailed_Action_Name : UTF8_String) return GNAT.Strings.String_List
   is
      function Internal
         (Self                 : System.Address;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr)
          return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_application_get_accels_for_action");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
      Tmp_Return               : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Detailed_Action_Name);
      Free (Tmp_Detailed_Action_Name);
      return To_String_List_And_Free (Tmp_Return);
   end Get_Accels_For_Action;

   ---------------------------
   -- Get_Actions_For_Accel --
   ---------------------------

   function Get_Actions_For_Accel
      (Self  : not null access Gtk_Application_Record;
       Accel : UTF8_String) return GNAT.Strings.String_List
   is
      function Internal
         (Self  : System.Address;
          Accel : Gtkada.Types.Chars_Ptr) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_application_get_actions_for_accel");
      Tmp_Accel  : Gtkada.Types.Chars_Ptr := New_String (Accel);
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Accel);
      Free (Tmp_Accel);
      return To_String_List_And_Free (Tmp_Return);
   end Get_Actions_For_Accel;

   -----------------------
   -- Get_Active_Window --
   -----------------------

   function Get_Active_Window
      (Self : not null access Gtk_Application_Record)
       return Gtk.Window.Gtk_Window
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_application_get_active_window");
      Stub_Gtk_Window : Gtk.Window.Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Window));
   end Get_Active_Window;

   ------------------
   -- Get_App_Menu --
   ------------------

   function Get_App_Menu
      (Self : not null access Gtk_Application_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_application_get_app_menu");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_App_Menu;

   --------------------
   -- Get_Menu_By_Id --
   --------------------

   function Get_Menu_By_Id
      (Self : not null access Gtk_Application_Record;
       Id   : UTF8_String) return Glib.Menu.Gmenu
   is
      function Internal
         (Self : System.Address;
          Id   : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_application_get_menu_by_id");
      Tmp_Id     : Gtkada.Types.Chars_Ptr := New_String (Id);
      Stub_Gmenu : Glib.Menu.Gmenu_Record;
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Id);
      Free (Tmp_Id);
      return Glib.Menu.Gmenu (Get_User_Data (Tmp_Return, Stub_Gmenu));
   end Get_Menu_By_Id;

   -----------------
   -- Get_Menubar --
   -----------------

   function Get_Menubar
      (Self : not null access Gtk_Application_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_application_get_menubar");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_Menubar;

   ----------------------
   -- Get_Window_By_Id --
   ----------------------

   function Get_Window_By_Id
      (Self : not null access Gtk_Application_Record;
       Id   : Guint) return Gtk.Window.Gtk_Window
   is
      function Internal
         (Self : System.Address;
          Id   : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_application_get_window_by_id");
      Stub_Gtk_Window : Gtk.Window.Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window (Get_User_Data (Internal (Get_Object (Self), Id), Stub_Gtk_Window));
   end Get_Window_By_Id;

   -----------------
   -- Get_Windows --
   -----------------

   function Get_Windows
      (Self : not null access Gtk_Application_Record)
       return Gtk.Widget.Widget_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_application_get_windows");
      Tmp_Return : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Windows;

   -------------
   -- Inhibit --
   -------------

   function Inhibit
      (Self   : not null access Gtk_Application_Record;
       Window : access Gtk.Window.Gtk_Window_Record'Class;
       Flags  : Gtk_Application_Inhibit_Flags;
       Reason : UTF8_String := "") return Guint
   is
      function Internal
         (Self   : System.Address;
          Window : System.Address;
          Flags  : Gtk_Application_Inhibit_Flags;
          Reason : Gtkada.Types.Chars_Ptr) return Guint;
      pragma Import (C, Internal, "gtk_application_inhibit");
      Tmp_Reason : Gtkada.Types.Chars_Ptr;
      Tmp_Return : Guint;
   begin
      if Reason = "" then
         Tmp_Reason := Gtkada.Types.Null_Ptr;
      else
         Tmp_Reason := New_String (Reason);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Window)), Flags, Tmp_Reason);
      Free (Tmp_Reason);
      return Tmp_Return;
   end Inhibit;

   ------------------
   -- Is_Inhibited --
   ------------------

   function Is_Inhibited
      (Self  : not null access Gtk_Application_Record;
       Flags : Gtk_Application_Inhibit_Flags) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Flags : Gtk_Application_Inhibit_Flags) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_application_is_inhibited");
   begin
      return Internal (Get_Object (Self), Flags) /= 0;
   end Is_Inhibited;

   ------------------------------
   -- List_Action_Descriptions --
   ------------------------------

   function List_Action_Descriptions
      (Self : not null access Gtk_Application_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_application_list_action_descriptions");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end List_Action_Descriptions;

   ----------------------
   -- Prefers_App_Menu --
   ----------------------

   function Prefers_App_Menu
      (Self : not null access Gtk_Application_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_application_prefers_app_menu");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Prefers_App_Menu;

   ------------------------
   -- Remove_Accelerator --
   ------------------------

   procedure Remove_Accelerator
      (Self        : not null access Gtk_Application_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Parameter   : System.Address);
      pragma Import (C, Internal, "gtk_application_remove_accelerator");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (Parameter));
      Free (Tmp_Action_Name);
   end Remove_Accelerator;

   -------------------
   -- Remove_Window --
   -------------------

   procedure Remove_Window
      (Self   : not null access Gtk_Application_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class)
   is
      procedure Internal (Self : System.Address; Window : System.Address);
      pragma Import (C, Internal, "gtk_application_remove_window");
   begin
      Internal (Get_Object (Self), Get_Object (Window));
   end Remove_Window;

   ---------------------------
   -- Set_Accels_For_Action --
   ---------------------------

   procedure Set_Accels_For_Action
      (Self                 : not null access Gtk_Application_Record;
       Detailed_Action_Name : UTF8_String;
       Accels               : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self                 : System.Address;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr;
          Accels               : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_application_set_accels_for_action");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
      Tmp_Accels               : Gtkada.Types.chars_ptr_array := From_String_List (Accels);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action_Name, Tmp_Accels);
      Gtkada.Types.Free (Tmp_Accels);
      Free (Tmp_Detailed_Action_Name);
   end Set_Accels_For_Action;

   ------------------
   -- Set_App_Menu --
   ------------------

   procedure Set_App_Menu
      (Self     : not null access Gtk_Application_Record;
       App_Menu : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; App_Menu : System.Address);
      pragma Import (C, Internal, "gtk_application_set_app_menu");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (App_Menu)));
   end Set_App_Menu;

   -----------------
   -- Set_Menubar --
   -----------------

   procedure Set_Menubar
      (Self    : not null access Gtk_Application_Record;
       Menubar : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; Menubar : System.Address);
      pragma Import (C, Internal, "gtk_application_set_menubar");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Menubar)));
   end Set_Menubar;

   ---------------
   -- Uninhibit --
   ---------------

   procedure Uninhibit
      (Self   : not null access Gtk_Application_Record;
       Cookie : Guint)
   is
      procedure Internal (Self : System.Address; Cookie : Guint);
      pragma Import (C, Internal, "gtk_application_uninhibit");
   begin
      Internal (Get_Object (Self), Cookie);
   end Uninhibit;

   ------------------
   -- Action_Added --
   ------------------

   procedure Action_Added
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self   : not null access Gtk_Application_Record;
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
      (Self      : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self : not null access Gtk_Application_Record)
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
      (Self        : not null access Gtk_Application_Record;
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
      (Self           : not null access Gtk_Application_Record;
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
      (Self        : not null access Gtk_Application_Record;
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

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Application_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Application_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Application_Gtk_Window_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Application_Gtk_Window_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Window_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Window_Void);

   procedure Connect
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Application_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Application_Gtk_Window_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Window_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Window_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Window_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Application_Gtk_Window_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Application_Gtk_Window_Void);

   procedure Marsh_Gtk_Application_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Application_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Application_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Application_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Application_Gtk_Window_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Application_Gtk_Window_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Application_Record'Class;
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
      (Object  : access Gtk_Application_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Window_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Window_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------
   -- Marsh_GObject_Gtk_Window_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Window_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Window_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Window.Gtk_Window (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Window_Void;

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

   -------------------------------------------
   -- Marsh_Gtk_Application_Gtk_Window_Void --
   -------------------------------------------

   procedure Marsh_Gtk_Application_Gtk_Window_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Application_Gtk_Window_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Application := Gtk_Application (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Window.Gtk_Window (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Application_Gtk_Window_Void;

   --------------------------------
   -- Marsh_Gtk_Application_Void --
   --------------------------------

   procedure Marsh_Gtk_Application_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Application_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Application := Gtk_Application (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Application_Void;

   ------------------
   -- On_Query_End --
   ------------------

   procedure On_Query_End
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_Gtk_Application_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "query-end" & ASCII.NUL, Call, After);
   end On_Query_End;

   ------------------
   -- On_Query_End --
   ------------------

   procedure On_Query_End
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "query-end" & ASCII.NUL, Call, After, Slot);
   end On_Query_End;

   ---------------------
   -- On_Window_Added --
   ---------------------

   procedure On_Window_Added
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_Gtk_Application_Gtk_Window_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "window-added" & ASCII.NUL, Call, After);
   end On_Window_Added;

   ---------------------
   -- On_Window_Added --
   ---------------------

   procedure On_Window_Added
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_GObject_Gtk_Window_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "window-added" & ASCII.NUL, Call, After, Slot);
   end On_Window_Added;

   -----------------------
   -- On_Window_Removed --
   -----------------------

   procedure On_Window_Removed
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_Gtk_Application_Gtk_Window_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "window-removed" & ASCII.NUL, Call, After);
   end On_Window_Removed;

   -----------------------
   -- On_Window_Removed --
   -----------------------

   procedure On_Window_Removed
      (Self  : not null access Gtk_Application_Record;
       Call  : Cb_GObject_Gtk_Window_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "window-removed" & ASCII.NUL, Call, After, Slot);
   end On_Window_Removed;

end Gtk.Application;
