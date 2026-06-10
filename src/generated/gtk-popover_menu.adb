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
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Popover_Menu is

   package Type_Conversion_Gtk_Popover_Menu is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Popover_Menu_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Popover_Menu);

   ------------------------
   -- Gtk_New_From_Model --
   ------------------------

   procedure Gtk_New_From_Model
      (Self  : out Gtk_Popover_Menu;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
   begin
      Self := new Gtk_Popover_Menu_Record;
      Gtk.Popover_Menu.Initialize_From_Model (Self, Model);
   end Gtk_New_From_Model;

   -----------------------------
   -- Gtk_New_From_Model_Full --
   -----------------------------

   procedure Gtk_New_From_Model_Full
      (Self  : out Gtk_Popover_Menu;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Flags : Gtk_Popover_Menu_Flags)
   is
   begin
      Self := new Gtk_Popover_Menu_Record;
      Gtk.Popover_Menu.Initialize_From_Model_Full (Self, Model, Flags);
   end Gtk_New_From_Model_Full;

   -------------------------------------
   -- Gtk_Popover_Menu_New_From_Model --
   -------------------------------------

   function Gtk_Popover_Menu_New_From_Model
      (Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Popover_Menu
   is
      Self : constant Gtk_Popover_Menu := new Gtk_Popover_Menu_Record;
   begin
      Gtk.Popover_Menu.Initialize_From_Model (Self, Model);
      return Self;
   end Gtk_Popover_Menu_New_From_Model;

   ------------------------------------------
   -- Gtk_Popover_Menu_New_From_Model_Full --
   ------------------------------------------

   function Gtk_Popover_Menu_New_From_Model_Full
      (Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Flags : Gtk_Popover_Menu_Flags) return Gtk_Popover_Menu
   is
      Self : constant Gtk_Popover_Menu := new Gtk_Popover_Menu_Record;
   begin
      Gtk.Popover_Menu.Initialize_From_Model_Full (Self, Model, Flags);
      return Self;
   end Gtk_Popover_Menu_New_From_Model_Full;

   ---------------------------
   -- Initialize_From_Model --
   ---------------------------

   procedure Initialize_From_Model
      (Self  : not null access Gtk_Popover_Menu_Record'Class;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_menu_new_from_model");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Model))));
      end if;
   end Initialize_From_Model;

   --------------------------------
   -- Initialize_From_Model_Full --
   --------------------------------

   procedure Initialize_From_Model_Full
      (Self  : not null access Gtk_Popover_Menu_Record'Class;
       Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Flags : Gtk_Popover_Menu_Flags)
   is
      function Internal
         (Model : System.Address;
          Flags : Gtk_Popover_Menu_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_popover_menu_new_from_model_full");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Model), Flags));
      end if;
   end Initialize_From_Model_Full;

   ---------------
   -- Add_Child --
   ---------------

   function Add_Child
      (Self  : not null access Gtk_Popover_Menu_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Id    : UTF8_String) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address;
          Id    : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_menu_add_child");
      Tmp_Id     : Gtkada.Types.Chars_Ptr := New_String (Id);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Child), Tmp_Id);
      Free (Tmp_Id);
      return Tmp_Return /= 0;
   end Add_Child;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gtk_Popover_Menu_Flags
   is
      function Internal
         (Self : System.Address) return Gtk_Popover_Menu_Flags;
      pragma Import (C, Internal, "gtk_popover_menu_get_flags");
   begin
      return Internal (Get_Object (Self));
   end Get_Flags;

   --------------------
   -- Get_Menu_Model --
   --------------------

   function Get_Menu_Model
      (Self : not null access Gtk_Popover_Menu_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_menu_get_menu_model");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_Menu_Model;

   ------------------
   -- Remove_Child --
   ------------------

   function Remove_Child
      (Self  : not null access Gtk_Popover_Menu_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_menu_remove_child");
   begin
      return Internal (Get_Object (Self), Get_Object (Child)) /= 0;
   end Remove_Child;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
      (Self  : not null access Gtk_Popover_Menu_Record;
       Flags : Gtk_Popover_Menu_Flags)
   is
      procedure Internal
         (Self  : System.Address;
          Flags : Gtk_Popover_Menu_Flags);
      pragma Import (C, Internal, "gtk_popover_menu_set_flags");
   begin
      Internal (Get_Object (Self), Flags);
   end Set_Flags;

   --------------------
   -- Set_Menu_Model --
   --------------------

   procedure Set_Menu_Model
      (Self  : not null access Gtk_Popover_Menu_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; Model : System.Address);
      pragma Import (C, Internal, "gtk_popover_menu_set_menu_model");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Model)));
   end Set_Menu_Model;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Popover_Menu_Record;
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
      (Self : not null access Gtk_Popover_Menu_Record) return UTF8_String
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
      (Self : not null access Gtk_Popover_Menu_Record)
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
      (Self : not null access Gtk_Popover_Menu_Record)
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
      (Self : not null access Gtk_Popover_Menu_Record)
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
      (Self   : not null access Gtk_Popover_Menu_Record;
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
      (Self : not null access Gtk_Popover_Menu_Record)
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
      (Self : not null access Gtk_Popover_Menu_Record)
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
      (Self  : not null access Gtk_Popover_Menu_Record;
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

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_Popover_Menu_Record)
       return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Popover_Menu_Record;
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

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_Popover_Menu_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Popover_Menu_Record;
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
      (Self     : not null access Gtk_Popover_Menu_Record;
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
      (Self  : not null access Gtk_Popover_Menu_Record;
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
      (Self         : not null access Gtk_Popover_Menu_Record;
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
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gtk_Popover_Menu_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Popover_Menu_Record;
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
      (Self  : not null access Gtk_Popover_Menu_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Popover_Menu;
