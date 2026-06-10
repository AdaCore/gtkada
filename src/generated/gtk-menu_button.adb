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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Menu_Button is

   procedure C_Gtk_Menu_Button_Set_Create_Popup_Func
      (Self           : System.Address;
       Func           : System.Address;
       User_Data      : System.Address;
       Destroy_Notify : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_Menu_Button_Set_Create_Popup_Func, "gtk_menu_button_set_create_popup_func");
   --  Sets Func to be called when a popup is about to be shown.
   --  Func should use one of
   --  - [methodGtk.MenuButton.set_popover] -
   --  [methodGtk.MenuButton.set_menu_model]
   --  to set a popup for Menu_Button. If Func is non-null, Menu_Button will
   --  always be sensitive.
   --  Using this function will not reset the menu widget attached to
   --  Menu_Button. Instead, this can be done manually in Func.
   --  @param Func function to call when a popup is about to be shown, but
   --  none has been provided via other means, or null to reset to default
   --  behavior
   --  @param User_Data user data to pass to Func
   --  @param Destroy_Notify destroy notify for User_Data

   function To_Gtk_Menu_Button_Create_Popup_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Menu_Button_Create_Popup_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Menu_Button_Create_Popup_Func, System.Address);

   procedure Internal_Gtk_Menu_Button_Create_Popup_Func
      (Menu_Button : System.Address;
       User_Data   : System.Address);
   pragma Convention (C, Internal_Gtk_Menu_Button_Create_Popup_Func);
   --  @param Menu_Button the `GtkMenuButton`
   --  @param User_Data User data passed to
   --  Gtk.Menu_Button.Set_Create_Popup_Func

   ------------------------------------------------
   -- Internal_Gtk_Menu_Button_Create_Popup_Func --
   ------------------------------------------------

   procedure Internal_Gtk_Menu_Button_Create_Popup_Func
      (Menu_Button : System.Address;
       User_Data   : System.Address)
   is
      Func                 : constant Gtk_Menu_Button_Create_Popup_Func := To_Gtk_Menu_Button_Create_Popup_Func (User_Data);
      Stub_Gtk_Menu_Button : Gtk_Menu_Button_Record;
   begin
      Func (Gtk.Menu_Button.Gtk_Menu_Button (Get_User_Data (Menu_Button, Stub_Gtk_Menu_Button)));
   end Internal_Gtk_Menu_Button_Create_Popup_Func;

   package Type_Conversion_Gtk_Menu_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Button);

   -------------------------
   -- Gtk_Menu_Button_New --
   -------------------------

   function Gtk_Menu_Button_New return Gtk_Menu_Button is
      Self : constant Gtk_Menu_Button := new Gtk_Menu_Button_Record;
   begin
      Gtk.Menu_Button.Initialize (Self);
      return Self;
   end Gtk_Menu_Button_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Menu_Button) is
   begin
      Self := new Gtk_Menu_Button_Record;
      Gtk.Menu_Button.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Menu_Button_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_button_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Self : not null access Gtk_Menu_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_button_get_active");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Active;

   ---------------------------
   -- Get_Always_Show_Arrow --
   ---------------------------

   function Get_Always_Show_Arrow
      (Self : not null access Gtk_Menu_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_button_get_always_show_arrow");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Always_Show_Arrow;

   --------------------
   -- Get_Can_Shrink --
   --------------------

   function Get_Can_Shrink
      (Self : not null access Gtk_Menu_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_button_get_can_shrink");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Can_Shrink;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_button_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Enums.Gtk_Arrow_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Arrow_Type;
      pragma Import (C, Internal, "gtk_menu_button_get_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Direction;

   -------------------
   -- Get_Has_Frame --
   -------------------

   function Get_Has_Frame
      (Self : not null access Gtk_Menu_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_button_get_has_frame");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Frame;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_menu_button_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Icon_Name;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_menu_button_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Label;

   --------------------
   -- Get_Menu_Model --
   --------------------

   function Get_Menu_Model
      (Self : not null access Gtk_Menu_Button_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_button_get_menu_model");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_Menu_Model;

   -----------------
   -- Get_Popover --
   -----------------

   function Get_Popover
      (Self : not null access Gtk_Menu_Button_Record)
       return Gtk.Popover.Gtk_Popover
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_button_get_popover");
      Stub_Gtk_Popover : Gtk.Popover.Gtk_Popover_Record;
   begin
      return Gtk.Popover.Gtk_Popover (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Popover));
   end Get_Popover;

   -----------------
   -- Get_Primary --
   -----------------

   function Get_Primary
      (Self : not null access Gtk_Menu_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_button_get_primary");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Primary;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Self : not null access Gtk_Menu_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_button_get_use_underline");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Underline;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Self : not null access Gtk_Menu_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_menu_button_popdown");
   begin
      Internal (Get_Object (Self));
   end Popdown;

   -----------
   -- Popup --
   -----------

   procedure Popup (Self : not null access Gtk_Menu_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_menu_button_popup");
   begin
      Internal (Get_Object (Self));
   end Popup;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Self   : not null access Gtk_Menu_Button_Record;
       Active : Boolean)
   is
      procedure Internal (Self : System.Address; Active : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_button_set_active");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Active));
   end Set_Active;

   ---------------------------
   -- Set_Always_Show_Arrow --
   ---------------------------

   procedure Set_Always_Show_Arrow
      (Self              : not null access Gtk_Menu_Button_Record;
       Always_Show_Arrow : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Always_Show_Arrow : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_button_set_always_show_arrow");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Always_Show_Arrow));
   end Set_Always_Show_Arrow;

   --------------------
   -- Set_Can_Shrink --
   --------------------

   procedure Set_Can_Shrink
      (Self       : not null access Gtk_Menu_Button_Record;
       Can_Shrink : Boolean)
   is
      procedure Internal (Self : System.Address; Can_Shrink : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_button_set_can_shrink");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Can_Shrink));
   end Set_Can_Shrink;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Menu_Button_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_menu_button_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   ---------------------------
   -- Set_Create_Popup_Func --
   ---------------------------

   procedure Set_Create_Popup_Func
      (Self           : not null access Gtk_Menu_Button_Record;
       Func           : Gtk_Menu_Button_Create_Popup_Func;
       Destroy_Notify : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Func = null then
         C_Gtk_Menu_Button_Set_Create_Popup_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Destroy_Notify);
      else
         C_Gtk_Menu_Button_Set_Create_Popup_Func (Get_Object (Self), Internal_Gtk_Menu_Button_Create_Popup_Func'Address, To_Address (Func), Destroy_Notify);
      end if;
   end Set_Create_Popup_Func;

   package body Set_Create_Popup_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Menu_Button_Create_Popup_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Menu_Button_Create_Popup_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Menu_Button_Create_Popup_Func, System.Address);

      procedure Internal_Cb
         (Menu_Button : System.Address;
          User_Data   : System.Address);
      pragma Convention (C, Internal_Cb);
      --  User-provided callback function to create a popup for a
      --  `GtkMenuButton` on demand.
      --  This function is called when the popup of Menu_Button is shown, but
      --  none has been provided via [methodGtk.MenuButton.set_popover] or
      --  [methodGtk.MenuButton.set_menu_model].
      --  @param Menu_Button the `GtkMenuButton`
      --  @param User_Data User data passed to
      --  Gtk.Menu_Button.Set_Create_Popup_Func

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Menu_Button : System.Address;
          User_Data   : System.Address)
      is
         D                    : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Menu_Button : Gtk.Menu_Button.Gtk_Menu_Button_Record;
      begin
         To_Gtk_Menu_Button_Create_Popup_Func (D.Func) (Gtk.Menu_Button.Gtk_Menu_Button (Get_User_Data (Menu_Button, Stub_Gtk_Menu_Button)), D.Data.all);
      end Internal_Cb;

      ---------------------------
      -- Set_Create_Popup_Func --
      ---------------------------

      procedure Set_Create_Popup_Func
         (Self           : not null access Gtk.Menu_Button.Gtk_Menu_Button_Record'Class;
          Func           : Gtk_Menu_Button_Create_Popup_Func;
          User_Data      : User_Data_Type;
          Destroy_Notify : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Menu_Button_Set_Create_Popup_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Destroy_Notify);
         else
            D := Users.Build (To_Address (Func), User_Data);
            C_Gtk_Menu_Button_Set_Create_Popup_Func (Get_Object (Self), Internal_Cb'Address, D, Destroy_Notify);
         end if;
      end Set_Create_Popup_Func;

   end Set_Create_Popup_Func_User_Data;

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction
      (Self      : not null access Gtk_Menu_Button_Record;
       Direction : Gtk.Enums.Gtk_Arrow_Type)
   is
      procedure Internal
         (Self      : System.Address;
          Direction : Gtk.Enums.Gtk_Arrow_Type);
      pragma Import (C, Internal, "gtk_menu_button_set_direction");
   begin
      Internal (Get_Object (Self), Direction);
   end Set_Direction;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame
      (Self      : not null access Gtk_Menu_Button_Record;
       Has_Frame : Boolean)
   is
      procedure Internal (Self : System.Address; Has_Frame : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_button_set_has_frame");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Frame));
   end Set_Has_Frame;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Self      : not null access Gtk_Menu_Button_Record;
       Icon_Name : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_button_set_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
   begin
      Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_Name;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Self  : not null access Gtk_Menu_Button_Record;
       Label : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_button_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr := New_String (Label);
   begin
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   --------------------
   -- Set_Menu_Model --
   --------------------

   procedure Set_Menu_Model
      (Self       : not null access Gtk_Menu_Button_Record;
       Menu_Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Menu_Model : System.Address);
      pragma Import (C, Internal, "gtk_menu_button_set_menu_model");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Menu_Model)));
   end Set_Menu_Model;

   -----------------
   -- Set_Popover --
   -----------------

   procedure Set_Popover
      (Self    : not null access Gtk_Menu_Button_Record;
       Popover : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Popover : System.Address);
      pragma Import (C, Internal, "gtk_menu_button_set_popover");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Popover)));
   end Set_Popover;

   -----------------
   -- Set_Primary --
   -----------------

   procedure Set_Primary
      (Self    : not null access Gtk_Menu_Button_Record;
       Primary : Boolean)
   is
      procedure Internal (Self : System.Address; Primary : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_button_set_primary");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Primary));
   end Set_Primary;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Self          : not null access Gtk_Menu_Button_Record;
       Use_Underline : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Use_Underline : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_button_set_use_underline");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Menu_Button_Record;
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
      (Self : not null access Gtk_Menu_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Menu_Button_Record)
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
      (Self : not null access Gtk_Menu_Button_Record)
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
      (Self : not null access Gtk_Menu_Button_Record)
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
      (Self   : not null access Gtk_Menu_Button_Record;
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
      (Self : not null access Gtk_Menu_Button_Record)
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
      (Self : not null access Gtk_Menu_Button_Record)
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
      (Self  : not null access Gtk_Menu_Button_Record;
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
      (Self     : not null access Gtk_Menu_Button_Record;
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
      (Self     : not null access Gtk_Menu_Button_Record;
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
      (Self  : not null access Gtk_Menu_Button_Record;
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
      (Self         : not null access Gtk_Menu_Button_Record;
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
      (Self        : not null access Gtk_Menu_Button_Record;
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
      (Self  : not null access Gtk_Menu_Button_Record;
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
     (Cb_Gtk_Menu_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Menu_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Button_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Button_Record'Class;
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

   procedure Marsh_Gtk_Menu_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Button_Record'Class;
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

   --------------------------------
   -- Marsh_Gtk_Menu_Button_Void --
   --------------------------------

   procedure Marsh_Gtk_Menu_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Button := Gtk_Menu_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Button_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Menu_Button_Record;
       Call  : Cb_Gtk_Menu_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Menu_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.Menu_Button;
