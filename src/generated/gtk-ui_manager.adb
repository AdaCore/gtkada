------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

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
      Set_Object (Self, Internal);
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
          Path     : Interfaces.C.Strings.chars_ptr;
          Name     : Interfaces.C.Strings.chars_ptr;
          Action   : Interfaces.C.Strings.chars_ptr;
          The_Type : Manager_Item_Type;
          Top      : Integer);
      pragma Import (C, Internal, "gtk_ui_manager_add_ui");
      Tmp_Path   : Interfaces.C.Strings.chars_ptr := New_String (Path);
      Tmp_Name   : Interfaces.C.Strings.chars_ptr := New_String (Name);
      Tmp_Action : Interfaces.C.Strings.chars_ptr;
   begin
      if Action = "" then
         Tmp_Action := Interfaces.C.Strings.Null_Ptr;
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
       Filename : UTF8_String;
       Error    : access Glib.Error.GError) return Guint
   is
      function Internal
         (Self      : System.Address;
          Filename  : Interfaces.C.Strings.chars_ptr;
          Acc_Error : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_file");
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename, Acc_Error'Access);
      Free (Tmp_Filename);
      Error.all := Acc_Error;
      return Tmp_Return;
   end Add_UI_From_File;

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
          Buffer    : Interfaces.C.Strings.chars_ptr;
          Length    : gssize;
          Acc_Error : access Glib.Error.GError) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_string");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Buffer : Interfaces.C.Strings.chars_ptr := New_String (Buffer);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Buffer, -1, Acc_Error'Access);
      Free (Tmp_Buffer);
      Error.all := Acc_Error;
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
          Path : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_action");
      Tmp_Path        : Interfaces.C.Strings.chars_ptr := New_String (Path);
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
       return Glib.Object.Object_Simple_List.GList
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_action_groups");
      Tmp_Return : Glib.Object.Object_Simple_List.GList;
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
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_ui_manager_get_add_tearoffs");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
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
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
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
          Path : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_widget");
      Tmp_Path        : Interfaces.C.Strings.chars_ptr := New_String (Path);
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
       Pos          : Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Action_Group : System.Address;
          Pos          : Gint);
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
      procedure Internal (Self : System.Address; Add_Tearoffs : Integer);
      pragma Import (C, Internal, "gtk_ui_manager_set_add_tearoffs");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Add_Tearoffs));
   end Set_Add_Tearoffs;

   ------------------------
   -- On_Actions_Changed --
   ------------------------

   procedure On_Actions_Changed
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure (Self : access Gtk_UI_Manager_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Actions_Changed;

   ------------------------
   -- On_Actions_Changed --
   ------------------------

   procedure On_Actions_Changed
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self : access Glib.Object.GObject_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Actions_Changed;

   -------------------
   -- On_Add_Widget --
   -------------------

   procedure On_Add_Widget
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Gtk_UI_Manager_Record'Class;
          Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Add_Widget;

   -------------------
   -- On_Add_Widget --
   -------------------

   procedure On_Add_Widget
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Glib.Object.GObject_Record'Class;
          Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Add_Widget;

   ----------------------
   -- On_Connect_Proxy --
   ----------------------

   procedure On_Connect_Proxy
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Gtk_UI_Manager_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class;
          Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Connect_Proxy;

   ----------------------
   -- On_Connect_Proxy --
   ----------------------

   procedure On_Connect_Proxy
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Glib.Object.GObject_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class;
          Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Connect_Proxy;

   -------------------------
   -- On_Disconnect_Proxy --
   -------------------------

   procedure On_Disconnect_Proxy
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Gtk_UI_Manager_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class;
          Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Disconnect_Proxy;

   -------------------------
   -- On_Disconnect_Proxy --
   -------------------------

   procedure On_Disconnect_Proxy
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Glib.Object.GObject_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class;
          Proxy  : not null access Gtk.Widget.Gtk_Widget_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Disconnect_Proxy;

   ----------------------
   -- On_Post_Activate --
   ----------------------

   procedure On_Post_Activate
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Gtk_UI_Manager_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Post_Activate;

   ----------------------
   -- On_Post_Activate --
   ----------------------

   procedure On_Post_Activate
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Glib.Object.GObject_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Post_Activate;

   ---------------------
   -- On_Pre_Activate --
   ---------------------

   procedure On_Pre_Activate
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Gtk_UI_Manager_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Pre_Activate;

   ---------------------
   -- On_Pre_Activate --
   ---------------------

   procedure On_Pre_Activate
      (Self : not null access Gtk_UI_Manager_Record;
       Call : not null access procedure
         (Self   : access Glib.Object.GObject_Record'Class;
          Action : not null access Gtk.Action.Gtk_Action_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Pre_Activate;

end Gtk.UI_Manager;
