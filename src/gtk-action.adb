-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtkada.Bindings;      use Gtkada.Bindings;
with Gtk.Accel_Group;      use Gtk.Accel_Group;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Action is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Action_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Action : access Gtk_Action_Record)
   is
      procedure Internal
        (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_activate");
   begin
      Internal (Get_Object (Action));
   end Activate;

   -------------------------
   -- Block_Activate_From --
   -------------------------

   procedure Block_Activate_From
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Action : System.Address;
         Proxy  : System.Address);
      pragma Import (C, Internal, "gtk_action_block_activate_from");
   begin
      Internal (Get_Object (Action), Get_Object (Proxy));
   end Block_Activate_From;

   -------------------------
   -- Connect_Accelerator --
   -------------------------

   procedure Connect_Accelerator
     (Action : access Gtk_Action_Record)
   is
      procedure Internal
        (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_connect_accelerator");
   begin
      Internal (Get_Object (Action));
   end Connect_Accelerator;

   -------------------
   -- Connect_Proxy --
   -------------------

   procedure Connect_Proxy
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Action : System.Address;
         Proxy  : System.Address);
      pragma Import (C, Internal, "gtk_action_connect_proxy");
   begin
      Internal (Get_Object (Action), Get_Object (Proxy));
   end Connect_Proxy;

   -------------
   -- Convert --
   -------------

   function Convert (C_Object : System.Address) return Gtk_Action is
      Stub : Gtk_Action_Record;
   begin
      return Gtk_Action (Get_User_Data (C_Object, Stub));
   end Convert;

   -----------------
   -- Create_Icon --
   -----------------

   function Create_Icon
     (Action    : access Gtk_Action_Record;
      Icon_Size : Gtk_Icon_Size)
      return Gtk_Widget
   is
      function Internal
        (Action    : System.Address;
         Icon_Size : Gtk_Icon_Size)
         return System.Address;
      pragma Import (C, Internal, "gtk_action_create_icon");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data
          (Internal (Get_Object (Action), Icon_Size), Stub));
   end Create_Icon;

   ----------------------
   -- Create_Menu_Item --
   ----------------------

   function Create_Menu_Item
     (Action : access Gtk_Action_Record)
      return Gtk_Widget
   is
      function Internal
        (Action : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_action_create_menu_item");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data
          (Internal (Get_Object (Action)), Stub));
   end Create_Menu_Item;

   ----------------------
   -- Create_Tool_Item --
   ----------------------

   function Create_Tool_Item
     (Action : access Gtk_Action_Record)
      return Gtk_Widget
   is
      function Internal
        (Action : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_action_create_tool_item");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data
          (Internal (Get_Object (Action)), Stub));
   end Create_Tool_Item;

   ----------------------------
   -- Disconnect_Accelerator --
   ----------------------------

   procedure Disconnect_Accelerator
     (Action : access Gtk_Action_Record)
   is
      procedure Internal
        (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_disconnect_accelerator");
   begin
      Internal (Get_Object (Action));
   end Disconnect_Accelerator;

   ----------------------
   -- Disconnect_Proxy --
   ----------------------

   procedure Disconnect_Proxy
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Action : System.Address;
         Proxy  : System.Address);
      pragma Import (C, Internal, "gtk_action_disconnect_proxy");
   begin
      Internal (Get_Object (Action), Get_Object (Proxy));
   end Disconnect_Proxy;

   --------------------
   -- Get_Accel_Path --
   --------------------

   function Get_Accel_Path
     (Action : access Gtk_Action_Record)
      return String
   is
      function Internal (Action : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_action_get_accel_path");
   begin
      --  Do not free returned value, owned by gtk+
      return Value (Internal (Get_Object (Action)));
   end Get_Accel_Path;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Action : access Gtk_Action_Record)
      return String
   is
      function Internal (Action : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_action_get_name");
   begin
      --  Do not free returned value, owned by gtk+
      return Value (Internal (Get_Object (Action)));
   end Get_Name;

   -----------------
   -- Get_Proxies --
   -----------------

   function Get_Proxies
     (Action : access Gtk_Action_Record)
      return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_get_proxies");
      L : Widget_SList.GSlist;
   begin
      Widget_SList.Set_Object (L, Internal (Get_Object (Action)));
      return L;
   end Get_Proxies;

   -------------------
   -- Get_Sensitive --
   -------------------

   function Get_Sensitive
     (Action : access Gtk_Action_Record)
      return Boolean
   is
      function Internal
        (Action : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_action_get_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Sensitive;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
     (Action : access Gtk_Action_Record)
      return Boolean
   is
      function Internal
        (Action : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_action_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Get_Visible;

   ------------------
   -- Is_Sensitive --
   ------------------

   function Is_Sensitive
     (Action : access Gtk_Action_Record)
      return Boolean
   is
      function Internal
        (Action : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_action_is_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Is_Sensitive;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Action : access Gtk_Action_Record)
      return Boolean
   is
      function Internal
        (Action : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_action_is_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Action)));
   end Is_Visible;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Action   : out Gtk_Action;
      Name     : String;
      Label    : String;
      Tooltip  : String := "";
      Stock_Id : String := "")
   is
   begin
      Action := new Gtk_Action_Record;
      Initialize
         (Action,
          Name,
          Label,
          Tooltip,
          Stock_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Action   : access Gtk_Action_Record'Class;
      Name     : String;
      Label    : String;
      Tooltip  : String := "";
      Stock_Id : String := "")
   is
      function Internal
        (Name     : String;
         Label    : String;
         Tooltip  : chars_ptr;
         Stock_Id : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_action_new");
      T    : chars_ptr := String_Or_Null (Tooltip);
      S    : chars_ptr := String_Or_Null (Stock_Id);
   begin
      Set_Object
        (Action, Internal (Name & ASCII.NUL, Label & ASCII.NUL, T, S));
      Free (T);
      Free (S);
   end Initialize;

   ---------------------
   -- Set_Accel_Group --
   ---------------------

   procedure Set_Accel_Group
     (Action      : access Gtk_Action_Record;
      Accel_Group : Gtk_Accel_Group := null)
   is
      procedure Internal
        (Action      : System.Address;
         Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_action_set_accel_group");
   begin
      if Accel_Group = null then
         Internal (Get_Object (Action), System.Null_Address);
      else
         Internal (Get_Object (Action), Get_Object (Accel_Group));
      end if;
   end Set_Accel_Group;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
     (Action     : access Gtk_Action_Record;
      Accel_Path : String)
   is
      procedure Internal
        (Action     : System.Address;
         Accel_Path : String);
      pragma Import (C, Internal, "gtk_action_set_accel_path");
   begin
      Internal (Get_Object (Action), Accel_Path & ASCII.NUL);
   end Set_Accel_Path;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Action    : access Gtk_Action_Record;
      Sensitive : Boolean)
   is
      procedure Internal
        (Action    : System.Address;
         Sensitive : Gboolean);
      pragma Import (C, Internal, "gtk_action_set_sensitive");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
     (Action  : access Gtk_Action_Record;
      Visible : Boolean)
   is
      procedure Internal
        (Action  : System.Address;
         Visible : Gboolean);
      pragma Import (C, Internal, "gtk_action_set_visible");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Visible));
   end Set_Visible;

   ---------------------------
   -- Unblock_Activate_From --
   ---------------------------

   procedure Unblock_Activate_From
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Action : System.Address;
         Proxy  : System.Address);
      pragma Import (C, Internal, "gtk_action_unblock_activate_from");
   begin
      Internal (Get_Object (Action), Get_Object (Proxy));
   end Unblock_Activate_From;

end Gtk.Action;
