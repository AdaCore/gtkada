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

package body Gtk.UI_Manager is

   package Type_Conversion_Gtk_UI_Manager is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_UI_Manager_Record);
   pragma Unreferenced (Type_Conversion_Gtk_UI_Manager);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_UI_Manager) is
   begin
      Self := new Gtk_UI_Manager_Record;
      Gtk.UI_Manager.Initialize (Self);
   end Gtk_New;

   ------------------------
   -- Gtk_UI_Manager_New --
   ------------------------

   function Gtk_UI_Manager_New return Gtk_UI_Manager is
      Self : constant Gtk_UI_Manager := new Gtk_UI_Manager_Record;
   begin
      Gtk.UI_Manager.Initialize (Self);
      return Self;
   end Gtk_UI_Manager_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_UI_Manager_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------
   -- Add_UI --
   ------------

   procedure Add_UI
      (Self     : not null access Gtk_UI_Manager_Record;
       Merge_Id : Guint;
       Path     : UTF8_String;
       Name     : UTF8_String;
       Action   : UTF8_String := "";
       The_Type : Manager_Item_Type := Manager_Auto;
       Top      : Boolean := False)
   is
      procedure Internal
         (Self     : System.Address;
          Merge_Id : Guint;
          Path     : Gtkada.Types.Chars_Ptr;
          Name     : Gtkada.Types.Chars_Ptr;
          Action   : Gtkada.Types.Chars_Ptr;
          The_Type : Manager_Item_Type;
          Top      : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_ui_manager_add_ui");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Action : Gtkada.Types.Chars_Ptr;
   begin
      if Action = "" then
         Tmp_Action := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action := New_String (Action);
      end if;
      Internal (Get_Object (Self), Merge_Id, Tmp_Path, Tmp_Name, Tmp_Action, The_Type, Boolean'Pos (Top));
      Free (Tmp_Action);
      Free (Tmp_Name);
      Free (Tmp_Path);
   end Add_UI;

   ----------------------
   -- Add_UI_From_File --
   ----------------------

   function Add_UI_From_File
      (Self     : not null access Gtk_UI_Manager_Record;
       Filename : UTF8_String) return Guint
   is
      function Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
      return Tmp_Return;
   end Add_UI_From_File;

   --------------------------
   -- Add_UI_From_Resource --
   --------------------------

   function Add_UI_From_Resource
      (Self          : not null access Gtk_UI_Manager_Record;
       Resource_Path : UTF8_String) return Guint
   is
      function Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
      return Tmp_Return;
   end Add_UI_From_Resource;

   ------------------------
   -- Add_UI_From_String --
   ------------------------

   function Add_UI_From_String
      (Self   : not null access Gtk_UI_Manager_Record;
       Buffer : UTF8_String;
       Error  : access Glib.Error.GError) return Guint
   is
      function Internal
         (Self      : System.Address;
          Buffer    : Gtkada.Types.Chars_Ptr;
          Length    : Gssize;
          Acc_Error : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_string");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Buffer : Gtkada.Types.Chars_Ptr := New_String (Buffer);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Buffer, -1, Acc_Error'Access);
      Free (Tmp_Buffer);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Tmp_Return;
   end Add_UI_From_String;

   -------------------
   -- Ensure_Update --
   -------------------

   procedure Ensure_Update (Self : not null access Gtk_UI_Manager_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_ui_manager_ensure_update");
   begin
      Internal (Get_Object (Self));
   end Ensure_Update;

   ---------------------
   -- Get_Accel_Group --
   ---------------------

   function Get_Accel_Group
      (Self : not null access Gtk_UI_Manager_Record)
       return Gtk.Accel_Group.Gtk_Accel_Group
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_accel_group");
      Stub_Gtk_Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group_Record;
   begin
      return Gtk.Accel_Group.Gtk_Accel_Group (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Accel_Group));
   end Get_Accel_Group;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
      (Self : not null access Gtk_UI_Manager_Record;
       Path : UTF8_String) return Gtk.Action.Gtk_Action
   is
      function Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_action");
      Tmp_Path        : Gtkada.Types.Chars_Ptr := New_String (Path);
      Stub_Gtk_Action : Gtk.Action.Gtk_Action_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
      return Gtk.Action.Gtk_Action (Get_User_Data (Tmp_Return, Stub_Gtk_Action));
   end Get_Action;

   -----------------------
   -- Get_Action_Groups --
   -----------------------

   function Get_Action_Groups
      (Self : not null access Gtk_UI_Manager_Record)
       return Glib.Object.Object_Simple_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_action_groups");
      Tmp_Return : Glib.Object.Object_Simple_List.Glist;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Action_Groups;

   ----------------------
   -- Get_Add_Tearoffs --
   ----------------------

   function Get_Add_Tearoffs
      (Self : not null access Gtk_UI_Manager_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_ui_manager_get_add_tearoffs");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Add_Tearoffs;

   -------------------
   -- Get_Toplevels --
   -------------------

   function Get_Toplevels
      (Self  : not null access Gtk_UI_Manager_Record;
       Types : Manager_Item_Type) return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal
         (Self  : System.Address;
          Types : Manager_Item_Type) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_toplevels");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Self), Types));
      return Tmp_Return;
   end Get_Toplevels;

   ------------
   -- Get_Ui --
   ------------

   function Get_Ui
      (Self : not null access Gtk_UI_Manager_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_ui_manager_get_ui");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Ui;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
      (Self : not null access Gtk_UI_Manager_Record;
       Path : UTF8_String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_widget");
      Tmp_Path        : Gtkada.Types.Chars_Ptr := New_String (Path);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Tmp_Return, Stub_Gtk_Widget));
   end Get_Widget;

   -------------------------
   -- Insert_Action_Group --
   -------------------------

   procedure Insert_Action_Group
      (Self         : not null access Gtk_UI_Manager_Record;
       Action_Group : not null access Gtk.Action_Group.Gtk_Action_Group_Record'Class;
       Pos          : Glib.Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Action_Group : System.Address;
          Pos          : Glib.Gint);
      pragma Import (C, Internal, "gtk_ui_manager_insert_action_group");
   begin
      Internal (Get_Object (Self), Get_Object (Action_Group), Pos);
   end Insert_Action_Group;

   ------------------
   -- New_Merge_Id --
   ------------------

   function New_Merge_Id
      (Self : not null access Gtk_UI_Manager_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_new_merge_id");
   begin
      return Internal (Get_Object (Self));
   end New_Merge_Id;

   -------------------------
   -- Remove_Action_Group --
   -------------------------

   procedure Remove_Action_Group
      (Self         : not null access Gtk_UI_Manager_Record;
       Action_Group : not null access Gtk.Action_Group.Gtk_Action_Group_Record'Class)
   is
      procedure Internal
         (Self         : System.Address;
          Action_Group : System.Address);
      pragma Import (C, Internal, "gtk_ui_manager_remove_action_group");
   begin
      Internal (Get_Object (Self), Get_Object (Action_Group));
   end Remove_Action_Group;

   ---------------
   -- Remove_UI --
   ---------------

   procedure Remove_UI
      (Self     : not null access Gtk_UI_Manager_Record;
       Merge_Id : Guint)
   is
      procedure Internal (Self : System.Address; Merge_Id : Guint);
      pragma Import (C, Internal, "gtk_ui_manager_remove_ui");
   begin
      Internal (Get_Object (Self), Merge_Id);
   end Remove_UI;

   ----------------------
   -- Set_Add_Tearoffs --
   ----------------------

   procedure Set_Add_Tearoffs
      (Self         : not null access Gtk_UI_Manager_Record;
       Add_Tearoffs : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Add_Tearoffs : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_ui_manager_set_add_tearoffs");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Add_Tearoffs));
   end Set_Add_Tearoffs;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_UI_Manager_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_UI_Manager_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_UI_Manager_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_UI_Manager_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Action_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Action_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_UI_Manager_Gtk_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_UI_Manager_Gtk_Action_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Action_Void);

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Gtk_Action_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Action_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Action_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Action_Gtk_Widget_Void);

   procedure Marsh_GObject_Gtk_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Action_Void);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void);

   procedure Marsh_Gtk_UI_Manager_Gtk_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_UI_Manager_Gtk_Action_Void);

   procedure Marsh_Gtk_UI_Manager_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_UI_Manager_Gtk_Widget_Void);

   procedure Marsh_Gtk_UI_Manager_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_UI_Manager_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_UI_Manager_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_UI_Manager_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_UI_Manager_Gtk_Action_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_UI_Manager_Gtk_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
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
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Action_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_UI_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Action_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------------------
   -- Marsh_GObject_Gtk_Action_Gtk_Widget_Void --
   ----------------------------------------------

   procedure Marsh_GObject_Gtk_Action_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Action_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Action.Gtk_Action (Unchecked_To_Object (Params, 1)), Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Action_Gtk_Widget_Void;

   -----------------------------------
   -- Marsh_GObject_Gtk_Action_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Action.Gtk_Action (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Action_Void;

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

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

   -----------------------------------------------------
   -- Marsh_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void --
   -----------------------------------------------------

   procedure Marsh_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_UI_Manager := Gtk_UI_Manager (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Action.Gtk_Action (Unchecked_To_Object (Params, 1)), Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;

   ------------------------------------------
   -- Marsh_Gtk_UI_Manager_Gtk_Action_Void --
   ------------------------------------------

   procedure Marsh_Gtk_UI_Manager_Gtk_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_UI_Manager_Gtk_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_UI_Manager := Gtk_UI_Manager (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Action.Gtk_Action (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_UI_Manager_Gtk_Action_Void;

   ------------------------------------------
   -- Marsh_Gtk_UI_Manager_Gtk_Widget_Void --
   ------------------------------------------

   procedure Marsh_Gtk_UI_Manager_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_UI_Manager_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_UI_Manager := Gtk_UI_Manager (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_UI_Manager_Gtk_Widget_Void;

   -------------------------------
   -- Marsh_Gtk_UI_Manager_Void --
   -------------------------------

   procedure Marsh_Gtk_UI_Manager_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_UI_Manager_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_UI_Manager := Gtk_UI_Manager (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_UI_Manager_Void;

   ------------------------
   -- On_Actions_Changed --
   ------------------------

   procedure On_Actions_Changed
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "actions-changed" & ASCII.NUL, Call, After);
   end On_Actions_Changed;

   ------------------------
   -- On_Actions_Changed --
   ------------------------

   procedure On_Actions_Changed
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "actions-changed" & ASCII.NUL, Call, After, Slot);
   end On_Actions_Changed;

   -------------------
   -- On_Add_Widget --
   -------------------

   procedure On_Add_Widget
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "add-widget" & ASCII.NUL, Call, After);
   end On_Add_Widget;

   -------------------
   -- On_Add_Widget --
   -------------------

   procedure On_Add_Widget
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "add-widget" & ASCII.NUL, Call, After, Slot);
   end On_Add_Widget;

   ----------------------
   -- On_Connect_Proxy --
   ----------------------

   procedure On_Connect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "connect-proxy" & ASCII.NUL, Call, After);
   end On_Connect_Proxy;

   ----------------------
   -- On_Connect_Proxy --
   ----------------------

   procedure On_Connect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "connect-proxy" & ASCII.NUL, Call, After, Slot);
   end On_Connect_Proxy;

   -------------------------
   -- On_Disconnect_Proxy --
   -------------------------

   procedure On_Disconnect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "disconnect-proxy" & ASCII.NUL, Call, After);
   end On_Disconnect_Proxy;

   -------------------------
   -- On_Disconnect_Proxy --
   -------------------------

   procedure On_Disconnect_Proxy
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "disconnect-proxy" & ASCII.NUL, Call, After, Slot);
   end On_Disconnect_Proxy;

   ----------------------
   -- On_Post_Activate --
   ----------------------

   procedure On_Post_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "post-activate" & ASCII.NUL, Call, After);
   end On_Post_Activate;

   ----------------------
   -- On_Post_Activate --
   ----------------------

   procedure On_Post_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "post-activate" & ASCII.NUL, Call, After, Slot);
   end On_Post_Activate;

   ---------------------
   -- On_Pre_Activate --
   ---------------------

   procedure On_Pre_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_Gtk_UI_Manager_Gtk_Action_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "pre-activate" & ASCII.NUL, Call, After);
   end On_Pre_Activate;

   ---------------------
   -- On_Pre_Activate --
   ---------------------

   procedure On_Pre_Activate
      (Self  : not null access Gtk_UI_Manager_Record;
       Call  : Cb_GObject_Gtk_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "pre-activate" & ASCII.NUL, Call, After, Slot);
   end On_Pre_Activate;

end Gtk.UI_Manager;
