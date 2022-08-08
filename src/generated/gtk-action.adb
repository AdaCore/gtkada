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

package body Gtk.Action is

   package Type_Conversion_Gtk_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Action);

   --------------------
   -- Gtk_Action_New --
   --------------------

   function Gtk_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "") return Gtk_Action
   is
      Action : constant Gtk_Action := new Gtk_Action_Record;
   begin
      Gtk.Action.Initialize (Action, Name, Label, Tooltip, Stock_Id);
      return Action;
   end Gtk_Action_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Action   : out Gtk_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "")
   is
   begin
      Action := new Gtk_Action_Record;
      Gtk.Action.Initialize (Action, Name, Label, Tooltip, Stock_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Action   : not null access Gtk_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "")
   is
      function Internal
         (Name     : Gtkada.Types.Chars_Ptr;
          Label    : Gtkada.Types.Chars_Ptr;
          Tooltip  : Gtkada.Types.Chars_Ptr;
          Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_action_new");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Label    : Gtkada.Types.Chars_Ptr;
      Tmp_Tooltip  : Gtkada.Types.Chars_Ptr;
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr;
      Tmp_Return   : System.Address;
   begin
      if not Action.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         if Tooltip = "" then
            Tmp_Tooltip := Gtkada.Types.Null_Ptr;
         else
            Tmp_Tooltip := New_String (Tooltip);
         end if;
         if Stock_Id = "" then
            Tmp_Stock_Id := Gtkada.Types.Null_Ptr;
         else
            Tmp_Stock_Id := New_String (Stock_Id);
         end if;
         Tmp_Return := Internal (Tmp_Name, Tmp_Label, Tmp_Tooltip, Tmp_Stock_Id);
         Free (Tmp_Stock_Id);
         Free (Tmp_Tooltip);
         Free (Tmp_Label);
         Free (Tmp_Name);
         Set_Object (Action, Tmp_Return);
      end if;
   end Initialize;

   --------------
   -- Activate --
   --------------

   procedure Activate (Action : not null access Gtk_Action_Record) is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_activate");
   begin
      Internal (Get_Object (Action));
   end Activate;

   --------------------
   -- Block_Activate --
   --------------------

   procedure Block_Activate (Action : not null access Gtk_Action_Record) is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_block_activate");
   begin
      Internal (Get_Object (Action));
   end Block_Activate;

   -------------------------
   -- Connect_Accelerator --
   -------------------------

   procedure Connect_Accelerator
      (Action : not null access Gtk_Action_Record)
   is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_connect_accelerator");
   begin
      Internal (Get_Object (Action));
   end Connect_Accelerator;

   -----------------
   -- Create_Icon --
   -----------------

   function Create_Icon
      (Action    : not null access Gtk_Action_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Action    : System.Address;
          Icon_Size : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_action_create_icon");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Action), Icon_Size), Stub_Gtk_Widget));
   end Create_Icon;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_create_menu");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Action)), Stub_Gtk_Widget));
   end Create_Menu;

   ----------------------
   -- Create_Menu_Item --
   ----------------------

   function Create_Menu_Item
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_create_menu_item");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Action)), Stub_Gtk_Widget));
   end Create_Menu_Item;

   ----------------------
   -- Create_Tool_Item --
   ----------------------

   function Create_Tool_Item
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_create_tool_item");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Action)), Stub_Gtk_Widget));
   end Create_Tool_Item;

   ----------------------------
   -- Disconnect_Accelerator --
   ----------------------------

   procedure Disconnect_Accelerator
      (Action : not null access Gtk_Action_Record)
   is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_disconnect_accelerator");
   begin
      Internal (Get_Object (Action));
   end Disconnect_Accelerator;

   --------------------
   -- Get_Accel_Path --
   --------------------

   function Get_Accel_Path
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_accel_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Accel_Path;

   ---------------------------
   -- Get_Always_Show_Image --
   ---------------------------

   function Get_Always_Show_Image
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_get_always_show_image");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Always_Show_Image;

   ---------------
   -- Get_Gicon --
   ---------------

   function Get_Gicon
      (Action : not null access Gtk_Action_Record) return Glib.G_Icon.G_Icon
   is
      function Internal (Action : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_action_get_gicon");
   begin
      return Internal (Get_Object (Action));
   end Get_Gicon;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Icon_Name;

   ----------------------
   -- Get_Is_Important --
   ----------------------

   function Get_Is_Important
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_get_is_important");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Is_Important;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Label;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Name;

   -----------------
   -- Get_Proxies --
   -----------------

   function Get_Proxies
      (Action : not null access Gtk_Action_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_get_proxies");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Action)));
      return Tmp_Return;
   end Get_Proxies;

   -------------------
   -- Get_Sensitive --
   -------------------

   function Get_Sensitive
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_get_sensitive");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Sensitive;

   ---------------------
   -- Get_Short_Label --
   ---------------------

   function Get_Short_Label
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_short_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Short_Label;

   ------------------
   -- Get_Stock_Id --
   ------------------

   function Get_Stock_Id
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_stock_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Stock_Id;

   -----------------
   -- Get_Tooltip --
   -----------------

   function Get_Tooltip
      (Action : not null access Gtk_Action_Record) return UTF8_String
   is
      function Internal
         (Action : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_action_get_tooltip");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Action)));
   end Get_Tooltip;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_get_visible");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Visible;

   ----------------------------
   -- Get_Visible_Horizontal --
   ----------------------------

   function Get_Visible_Horizontal
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_get_visible_horizontal");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Visible_Horizontal;

   --------------------------
   -- Get_Visible_Vertical --
   --------------------------

   function Get_Visible_Vertical
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_get_visible_vertical");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Visible_Vertical;

   ------------------
   -- Is_Sensitive --
   ------------------

   function Is_Sensitive
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_is_sensitive");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Is_Sensitive;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
      (Action : not null access Gtk_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_action_is_visible");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Is_Visible;

   ---------------------
   -- Set_Accel_Group --
   ---------------------

   procedure Set_Accel_Group
      (Action      : not null access Gtk_Action_Record;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
         (Action      : System.Address;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_action_set_accel_group");
   begin
      Internal (Get_Object (Action), Get_Object_Or_Null (GObject (Accel_Group)));
   end Set_Accel_Group;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
      (Action     : not null access Gtk_Action_Record;
       Accel_Path : UTF8_String)
   is
      procedure Internal
         (Action     : System.Address;
          Accel_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_action_set_accel_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
   begin
      Internal (Get_Object (Action), Tmp_Accel_Path);
      Free (Tmp_Accel_Path);
   end Set_Accel_Path;

   ---------------------------
   -- Set_Always_Show_Image --
   ---------------------------

   procedure Set_Always_Show_Image
      (Action      : not null access Gtk_Action_Record;
       Always_Show : Boolean)
   is
      procedure Internal
         (Action      : System.Address;
          Always_Show : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_action_set_always_show_image");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Always_Show));
   end Set_Always_Show_Image;

   ---------------
   -- Set_Gicon --
   ---------------

   procedure Set_Gicon
      (Action : not null access Gtk_Action_Record;
       Icon   : Glib.G_Icon.G_Icon)
   is
      procedure Internal
         (Action : System.Address;
          Icon   : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_action_set_gicon");
   begin
      Internal (Get_Object (Action), Icon);
   end Set_Gicon;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Action    : not null access Gtk_Action_Record;
       Icon_Name : UTF8_String)
   is
      procedure Internal
         (Action    : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_action_set_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
   begin
      Internal (Get_Object (Action), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_Name;

   ----------------------
   -- Set_Is_Important --
   ----------------------

   procedure Set_Is_Important
      (Action       : not null access Gtk_Action_Record;
       Is_Important : Boolean)
   is
      procedure Internal
         (Action       : System.Address;
          Is_Important : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_action_set_is_important");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Is_Important));
   end Set_Is_Important;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Action : not null access Gtk_Action_Record;
       Label  : UTF8_String)
   is
      procedure Internal
         (Action : System.Address;
          Label  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_action_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr := New_String (Label);
   begin
      Internal (Get_Object (Action), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
      (Action    : not null access Gtk_Action_Record;
       Sensitive : Boolean)
   is
      procedure Internal
         (Action    : System.Address;
          Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_action_set_sensitive");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   ---------------------
   -- Set_Short_Label --
   ---------------------

   procedure Set_Short_Label
      (Action      : not null access Gtk_Action_Record;
       Short_Label : UTF8_String)
   is
      procedure Internal
         (Action      : System.Address;
          Short_Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_action_set_short_label");
      Tmp_Short_Label : Gtkada.Types.Chars_Ptr := New_String (Short_Label);
   begin
      Internal (Get_Object (Action), Tmp_Short_Label);
      Free (Tmp_Short_Label);
   end Set_Short_Label;

   ------------------
   -- Set_Stock_Id --
   ------------------

   procedure Set_Stock_Id
      (Action   : not null access Gtk_Action_Record;
       Stock_Id : UTF8_String)
   is
      procedure Internal
         (Action   : System.Address;
          Stock_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_action_set_stock_id");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
   begin
      Internal (Get_Object (Action), Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
   end Set_Stock_Id;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
      (Action  : not null access Gtk_Action_Record;
       Tooltip : UTF8_String)
   is
      procedure Internal
         (Action  : System.Address;
          Tooltip : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_action_set_tooltip");
      Tmp_Tooltip : Gtkada.Types.Chars_Ptr := New_String (Tooltip);
   begin
      Internal (Get_Object (Action), Tmp_Tooltip);
      Free (Tmp_Tooltip);
   end Set_Tooltip;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Action  : not null access Gtk_Action_Record;
       Visible : Boolean)
   is
      procedure Internal (Action : System.Address; Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_action_set_visible");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Visible));
   end Set_Visible;

   ----------------------------
   -- Set_Visible_Horizontal --
   ----------------------------

   procedure Set_Visible_Horizontal
      (Action             : not null access Gtk_Action_Record;
       Visible_Horizontal : Boolean)
   is
      procedure Internal
         (Action             : System.Address;
          Visible_Horizontal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_action_set_visible_horizontal");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Visible_Horizontal));
   end Set_Visible_Horizontal;

   --------------------------
   -- Set_Visible_Vertical --
   --------------------------

   procedure Set_Visible_Vertical
      (Action           : not null access Gtk_Action_Record;
       Visible_Vertical : Boolean)
   is
      procedure Internal
         (Action           : System.Address;
          Visible_Vertical : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_action_set_visible_vertical");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Visible_Vertical));
   end Set_Visible_Vertical;

   ----------------------
   -- Unblock_Activate --
   ----------------------

   procedure Unblock_Activate (Action : not null access Gtk_Action_Record) is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_action_unblock_activate");
   begin
      Internal (Get_Object (Action));
   end Unblock_Activate;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Action_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Action_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Action_Record'Class;
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

   procedure Marsh_Gtk_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Action_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Action_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Action_Record'Class;
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

   ---------------------------
   -- Marsh_Gtk_Action_Void --
   ---------------------------

   procedure Marsh_Gtk_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Action := Gtk_Action (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Action_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Action_Record;
       Call  : Cb_Gtk_Action_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Action_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.Action;
