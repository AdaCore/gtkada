------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with GtkAda.Types;               use GtkAda.Types;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;       use Interfaces.C.Strings;
pragma Warnings(On);

package body Glib.Application is

   gnat_argc : Interfaces.C.int;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   function Run
     (Self : not null access Gapplication_Record) return Gint
   is
      function Internal
        (Self : System.Address;
         Argc : Gint;
         Argv : System.Address) return Gint;
      pragma Import (C, Internal, "g_application_run");
   begin
      return Internal (Self.Get_Object, Gint (gnat_argc), gnat_argv);
   end Run;

   package Type_Conversion_Gapplication is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gapplication_Record);
   pragma Unreferenced (Type_Conversion_Gapplication);

   package Type_Conversion_Gapplication_Command_Line is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type_Command_Line'Access, Gapplication_Command_Line_Record);
   pragma Unreferenced (Type_Conversion_Gapplication_Command_Line);

   -----------
   -- G_New --
   -----------

   procedure G_New
      (Self           : out Gapplication;
       Application_Id : UTF8_String := "";
       Flags          : GApplication_Flags)
   is
   begin
      Self := new Gapplication_Record;
      Glib.Application.Initialize (Self, Application_Id, Flags);
   end G_New;

   ----------------------
   -- Gapplication_New --
   ----------------------

   function Gapplication_New
      (Application_Id : UTF8_String := "";
       Flags          : GApplication_Flags) return Gapplication
   is
      Self : constant Gapplication := new Gapplication_Record;
   begin
      Glib.Application.Initialize (Self, Application_Id, Flags);
      return Self;
   end Gapplication_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self           : not null access Gapplication_Record'Class;
       Application_Id : UTF8_String := "";
       Flags          : GApplication_Flags)
   is
      function Internal
         (Application_Id : Interfaces.C.Strings.chars_ptr;
          Flags          : GApplication_Flags) return System.Address;
      pragma Import (C, Internal, "g_application_new");
      Tmp_Application_Id : Interfaces.C.Strings.chars_ptr;
      Tmp_Return         : System.Address;
   begin
      if not Self.Is_Created then
         if Application_Id = "" then
            Tmp_Application_Id := Interfaces.C.Strings.Null_Ptr;
         else
            Tmp_Application_Id := New_String (Application_Id);
         end if;
         Tmp_Return := Internal (Tmp_Application_Id, Flags);
         Free (Tmp_Application_Id);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   --------------
   -- Activate --
   --------------

   procedure Activate (Self : not null access Gapplication_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_application_activate");
   begin
      Internal (Get_Object (Self));
   end Activate;

   ------------------------
   -- Get_Application_Id --
   ------------------------

   function Get_Application_Id
      (Self : not null access Gapplication_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_application_get_application_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Application_Id;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
      (Self : not null access Gapplication_Command_Line_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address;
          Argc : access Gint) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_application_command_line_get_arguments");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self), null));
   end Get_Arguments;

   -------------
   -- Get_Cwd --
   -------------

   function Get_Cwd
      (Self : not null access Gapplication_Command_Line_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_application_command_line_get_cwd");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Cwd;

   --------------------------
   -- Get_Dbus_Object_Path --
   --------------------------

   function Get_Dbus_Object_Path
      (Self : not null access Gapplication_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_application_get_dbus_object_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Dbus_Object_Path;

   -----------------
   -- Get_Environ --
   -----------------

   function Get_Environ
      (Self : not null access Gapplication_Command_Line_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_application_command_line_get_environ");
   begin
      return To_String_List (Internal (Get_Object (Self)).all);
   end Get_Environ;

   ---------------------
   -- Get_Exit_Status --
   ---------------------

   function Get_Exit_Status
      (Self : not null access Gapplication_Command_Line_Record) return Gint
   is
      function Internal (Self : System.Address) return Gint;
      pragma Import (C, Internal, "g_application_command_line_get_exit_status");
   begin
      return Internal (Get_Object (Self));
   end Get_Exit_Status;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gapplication_Record) return GApplication_Flags
   is
      function Internal (Self : System.Address) return GApplication_Flags;
      pragma Import (C, Internal, "g_application_get_flags");
   begin
      return Internal (Get_Object (Self));
   end Get_Flags;

   ----------------------------
   -- Get_Inactivity_Timeout --
   ----------------------------

   function Get_Inactivity_Timeout
      (Self : not null access Gapplication_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_application_get_inactivity_timeout");
   begin
      return Internal (Get_Object (Self));
   end Get_Inactivity_Timeout;

   -----------------------
   -- Get_Is_Registered --
   -----------------------

   function Get_Is_Registered
      (Self : not null access Gapplication_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "g_application_get_is_registered");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Registered;

   -------------------
   -- Get_Is_Remote --
   -------------------

   function Get_Is_Remote
      (Self : not null access Gapplication_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "g_application_get_is_remote");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Remote;

   -------------------
   -- Get_Is_Remote --
   -------------------

   function Get_Is_Remote
      (Self : not null access Gapplication_Command_Line_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "g_application_command_line_get_is_remote");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Remote;

   -----------------------
   -- Get_Platform_Data --
   -----------------------

   function Get_Platform_Data
      (Self : not null access Gapplication_Command_Line_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_application_command_line_get_platform_data");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Platform_Data;

   ------------
   -- Getenv --
   ------------

   function Getenv
      (Self : not null access Gapplication_Command_Line_Record;
       Name : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Name : Interfaces.C.Strings.chars_ptr)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_application_command_line_getenv");
      Tmp_Name   : Interfaces.C.Strings.chars_ptr := New_String (Name);
      Tmp_Return : Interfaces.C.Strings.chars_ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Getenv;

   ----------
   -- Hold --
   ----------

   procedure Hold (Self : not null access Gapplication_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_application_hold");
   begin
      Internal (Get_Object (Self));
   end Hold;

   ----------
   -- Quit --
   ----------

   procedure Quit (Self : not null access Gapplication_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_application_quit");
   begin
      Internal (Get_Object (Self));
   end Quit;

   --------------
   -- Register --
   --------------

   function Register
      (Self        : not null access Gapplication_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address) return Integer;
      pragma Import (C, Internal, "g_application_register");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release (Self : not null access Gapplication_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_application_release");
   begin
      Internal (Get_Object (Self));
   end Release;

   ---------
   -- Run --
   ---------

   function Run
      (Self : not null access Gapplication_Record;
       Argc : Gint;
       Argv : GNAT.Strings.String_List) return Gint
   is
      function Internal
         (Self : System.Address;
          Argc : Gint;
          Argv : Interfaces.C.Strings.chars_ptr_array) return Gint;
      pragma Import (C, Internal, "g_application_run");
      Tmp_Argv   : Interfaces.C.Strings.chars_ptr_array := From_String_List (Argv);
      Tmp_Return : Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Argc, Tmp_Argv);
      GtkAda.Types.Free (Tmp_Argv);
      return Tmp_Return;
   end Run;

   ----------------------
   -- Set_Action_Group --
   ----------------------

   procedure Set_Action_Group
      (Self         : not null access Gapplication_Record;
       Action_Group : Glib.Action_Group.Gaction_Group)
   is
      procedure Internal
         (Self         : System.Address;
          Action_Group : Glib.Action_Group.Gaction_Group);
      pragma Import (C, Internal, "g_application_set_action_group");
   begin
      Internal (Get_Object (Self), Action_Group);
   end Set_Action_Group;

   ------------------------
   -- Set_Application_Id --
   ------------------------

   procedure Set_Application_Id
      (Self           : not null access Gapplication_Record;
       Application_Id : UTF8_String := "")
   is
      procedure Internal
         (Self           : System.Address;
          Application_Id : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "g_application_set_application_id");
      Tmp_Application_Id : Interfaces.C.Strings.chars_ptr;
   begin
      if Application_Id = "" then
         Tmp_Application_Id := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Application_Id := New_String (Application_Id);
      end if;
      Internal (Get_Object (Self), Tmp_Application_Id);
      Free (Tmp_Application_Id);
   end Set_Application_Id;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default (Self : not null access Gapplication_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_application_set_default");
   begin
      Internal (Get_Object (Self));
   end Set_Default;

   ---------------------
   -- Set_Exit_Status --
   ---------------------

   procedure Set_Exit_Status
      (Self        : not null access Gapplication_Command_Line_Record;
       Exit_Status : Gint)
   is
      procedure Internal (Self : System.Address; Exit_Status : Gint);
      pragma Import (C, Internal, "g_application_command_line_set_exit_status");
   begin
      Internal (Get_Object (Self), Exit_Status);
   end Set_Exit_Status;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
      (Self  : not null access Gapplication_Record;
       Flags : GApplication_Flags)
   is
      procedure Internal (Self : System.Address; Flags : GApplication_Flags);
      pragma Import (C, Internal, "g_application_set_flags");
   begin
      Internal (Get_Object (Self), Flags);
   end Set_Flags;

   ----------------------------
   -- Set_Inactivity_Timeout --
   ----------------------------

   procedure Set_Inactivity_Timeout
      (Self               : not null access Gapplication_Record;
       Inactivity_Timeout : Guint)
   is
      procedure Internal (Self : System.Address; Inactivity_Timeout : Guint);
      pragma Import (C, Internal, "g_application_set_inactivity_timeout");
   begin
      Internal (Get_Object (Self), Inactivity_Timeout);
   end Set_Inactivity_Timeout;

   ------------------
   -- Action_Added --
   ------------------

   procedure Action_Added
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "g_action_group_action_added");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Action_Added;

   ----------------------------
   -- Action_Enabled_Changed --
   ----------------------------

   procedure Action_Enabled_Changed
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr;
          Enabled     : Integer);
      pragma Import (C, Internal, "g_action_group_action_enabled_changed");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Boolean'Pos (Enabled));
      Free (Tmp_Action_Name);
   end Action_Enabled_Changed;

   --------------------
   -- Action_Removed --
   --------------------

   procedure Action_Removed
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "g_action_group_action_removed");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Action_Removed;

   --------------------------
   -- Action_State_Changed --
   --------------------------

   procedure Action_State_Changed
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr;
          State       : System.Address);
      pragma Import (C, Internal, "g_action_group_action_state_changed");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (State));
      Free (Tmp_Action_Name);
   end Action_State_Changed;

   ---------------------
   -- Activate_Action --
   ---------------------

   procedure Activate_Action
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr;
          Parameter   : System.Address);
      pragma Import (C, Internal, "g_action_group_activate_action");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (Parameter));
      Free (Tmp_Action_Name);
   end Activate_Action;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
      (Self   : not null access Gapplication_Record;
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
      (Self      : not null access Gapplication_Record;
       Entries   : GAction_Entry_Array;
       User_Data : System.Address := System.Null_Address)
   is
      procedure Internal
         (Self      : System.Address;
          Entries   : GAction_Entry_Array;
          N_Entries : Gint;
          User_Data : System.Address);
      pragma Import (C, Internal, "g_action_map_add_action_entries");
   begin
      Internal (Get_Object (Self), Entries, Entries'Length, User_Data);
   end Add_Action_Entries;

   -------------------------
   -- Change_Action_State --
   -------------------------

   procedure Change_Action_State
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr;
          Value       : System.Address);
      pragma Import (C, Internal, "g_action_group_change_action_state");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name, Get_Object (Value));
      Free (Tmp_Action_Name);
   end Change_Action_State;

   ------------------------
   -- Get_Action_Enabled --
   ------------------------

   function Get_Action_Enabled
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "g_action_group_get_action_enabled");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
      Tmp_Return      : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return /= 0;
   end Get_Action_Enabled;

   -------------------------------
   -- Get_Action_Parameter_Type --
   -------------------------------

   function Get_Action_Parameter_Type
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr)
          return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_group_get_action_parameter_type");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
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
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr)
          return System.Address;
      pragma Import (C, Internal, "g_action_group_get_action_state");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
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
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr)
          return System.Address;
      pragma Import (C, Internal, "g_action_group_get_action_state_hint");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
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
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr)
          return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_group_get_action_state_type");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
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
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "g_action_group_has_action");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
      Tmp_Return      : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return /= 0;
   end Has_Action;

   ------------------
   -- List_Actions --
   ------------------

   function List_Actions
      (Self : not null access Gapplication_Record)
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
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String) return Glib.Action.Gaction
   is
      function Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr)
          return Glib.Action.Gaction;
      pragma Import (C, Internal, "g_action_map_lookup_action");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
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
      (Self           : not null access Gapplication_Record;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self               : System.Address;
          Action_Name        : Interfaces.C.Strings.chars_ptr;
          Acc_Enabled        : access Integer;
          Acc_Parameter_Type : access Glib.Variant.Gvariant_Type;
          Acc_State_Type     : access Glib.Variant.Gvariant_Type;
          Acc_State_Hint     : access System.Address;
          Acc_State          : access System.Address) return Integer;
      pragma Import (C, Internal, "g_action_group_query_action");
      Acc_Enabled        : aliased Boolean;
      Acc_Parameter_Type : aliased Glib.Variant.Gvariant_Type;
      Acc_State_Type     : aliased Glib.Variant.Gvariant_Type;
      Acc_State_Hint     : aliased Glib.Variant.Gvariant;
      Acc_State          : aliased Glib.Variant.Gvariant;
      Tmp_Action_Name    : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
      Tmp_Acc_Enabled    : aliased Integer;
      Tmp_Acc_State_Hint : aliased System.Address;
      Tmp_Acc_State      : aliased System.Address;
      Tmp_Return         : Integer;
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
      (Self        : not null access Gapplication_Record;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "g_action_map_remove_action");
      Tmp_Action_Name : Interfaces.C.Strings.chars_ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Remove_Action;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gapplication is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_application_get_default");
      Stub_Gapplication : Gapplication_Record;
   begin
      return Glib.Application.Gapplication (Get_User_Data (Internal, Stub_Gapplication));
   end Get_Default;

   -----------------
   -- Id_Is_Valid --
   -----------------

   function Id_Is_Valid (Application_Id : UTF8_String) return Boolean is
      function Internal
         (Application_Id : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "g_application_id_is_valid");
      Tmp_Application_Id : Interfaces.C.Strings.chars_ptr := New_String (Application_Id);
      Tmp_Return         : Integer;
   begin
      Tmp_Return := Internal (Tmp_Application_Id);
      Free (Tmp_Application_Id);
      return Tmp_Return /= 0;
   end Id_Is_Valid;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gapplication_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gapplication_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gapplication_Gapplication_Command_Line_Gint, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gapplication_Gapplication_Command_Line_Gint);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gapplication_Command_Line_Gint, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gapplication_Command_Line_Gint);

   procedure Connect
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gapplication_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gapplication_Gapplication_Command_Line_Gint;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gapplication_Command_Line_Gint;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gapplication_Command_Line_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gapplication_Command_Line_Gint);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gapplication_Gapplication_Command_Line_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gapplication_Gapplication_Command_Line_Gint);

   procedure Marsh_Gapplication_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gapplication_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gapplication_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gapplication_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gapplication_Gapplication_Command_Line_Gint;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gapplication_Gapplication_Command_Line_Gint'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gapplication_Record'Class;
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
      (Object  : access Gapplication_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gapplication_Command_Line_Gint;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gapplication_Command_Line_Gint'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------------------
   -- Marsh_GObject_Gapplication_Command_Line_Gint --
   --------------------------------------------------

   procedure Marsh_GObject_Gapplication_Command_Line_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gapplication_Command_Line_Gint := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Gint := H (Obj, Glib.Application.Gapplication_Command_Line (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gapplication_Command_Line_Gint;

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

   -------------------------------------------------------
   -- Marsh_Gapplication_Gapplication_Command_Line_Gint --
   -------------------------------------------------------

   procedure Marsh_Gapplication_Gapplication_Command_Line_Gint
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gapplication_Gapplication_Command_Line_Gint := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gapplication := Gapplication (Unchecked_To_Object (Params, 0));
      V   : aliased Gint := H (Obj, Glib.Application.Gapplication_Command_Line (Unchecked_To_Object (Params, 1)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gapplication_Gapplication_Command_Line_Gint;

   -----------------------------
   -- Marsh_Gapplication_Void --
   -----------------------------

   procedure Marsh_Gapplication_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gapplication_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gapplication := Gapplication (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gapplication_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   ---------------------
   -- On_Command_Line --
   ---------------------

   procedure On_Command_Line
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Gapplication_Command_Line_Gint;
       After : Boolean := False)
   is
   begin
      Connect (Self, "command-line" & ASCII.NUL, Call, After);
   end On_Command_Line;

   ---------------------
   -- On_Command_Line --
   ---------------------

   procedure On_Command_Line
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Gapplication_Command_Line_Gint;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "command-line" & ASCII.NUL, Call, After, Slot);
   end On_Command_Line;

   -----------------
   -- On_Shutdown --
   -----------------

   procedure On_Shutdown
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "shutdown" & ASCII.NUL, Call, After);
   end On_Shutdown;

   -----------------
   -- On_Shutdown --
   -----------------

   procedure On_Shutdown
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "shutdown" & ASCII.NUL, Call, After, Slot);
   end On_Shutdown;

   ----------------
   -- On_Startup --
   ----------------

   procedure On_Startup
      (Self  : not null access Gapplication_Record;
       Call  : Cb_Gapplication_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "startup" & ASCII.NUL, Call, After);
   end On_Startup;

   ----------------
   -- On_Startup --
   ----------------

   procedure On_Startup
      (Self  : not null access Gapplication_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "startup" & ASCII.NUL, Call, After, Slot);
   end On_Startup;

end Glib.Application;
