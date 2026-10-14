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
with Gdk.Display;
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Page_Setup_Unix_Dialog is

   package Type_Conversion_Gtk_Page_Setup_Unix_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Page_Setup_Unix_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Page_Setup_Unix_Dialog);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Page_Setup_Unix_Dialog;
       Title  : UTF8_String := "";
       Parent : Gtk.Window.Gtk_Window := null)
   is
   begin
      Self := new Gtk_Page_Setup_Unix_Dialog_Record;
      Gtk.Page_Setup_Unix_Dialog.Initialize (Self, Title, Parent);
   end Gtk_New;

   ------------------------------------
   -- Gtk_Page_Setup_Unix_Dialog_New --
   ------------------------------------

   function Gtk_Page_Setup_Unix_Dialog_New
      (Title  : UTF8_String := "";
       Parent : Gtk.Window.Gtk_Window := null)
       return Gtk_Page_Setup_Unix_Dialog
   is
      Self : constant Gtk_Page_Setup_Unix_Dialog := new Gtk_Page_Setup_Unix_Dialog_Record;
   begin
      Gtk.Page_Setup_Unix_Dialog.Initialize (Self, Title, Parent);
      return Self;
   end Gtk_Page_Setup_Unix_Dialog_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Page_Setup_Unix_Dialog_Record'Class;
       Title  : UTF8_String := "";
       Parent : Gtk.Window.Gtk_Window := null)
   is
      function Internal
         (Title  : Gtkada.Types.Chars_Ptr;
          Parent : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_unix_dialog_new");
      Tmp_Title  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Title :=
           (if Title = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Title));
         Tmp_Return := Internal (Tmp_Title, Get_Object_Or_Null (GObject (Parent)));
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Title);
   end Initialize;

   --------------------
   -- Get_Page_Setup --
   --------------------

   function Get_Page_Setup
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_unix_dialog_get_page_setup");
      Stub_Gtk_Page_Setup : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Page_Setup));
   end Get_Page_Setup;

   ------------------------
   -- Get_Print_Settings --
   ------------------------

   function Get_Print_Settings
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gtk.Print_Settings.Gtk_Print_Settings
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_unix_dialog_get_print_settings");
      Stub_Gtk_Print_Settings : Gtk.Print_Settings.Gtk_Print_Settings_Record;
   begin
      return Gtk.Print_Settings.Gtk_Print_Settings (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Print_Settings));
   end Get_Print_Settings;

   --------------------
   -- Set_Page_Setup --
   --------------------

   procedure Set_Page_Setup
      (Self       : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Page_Setup : System.Address);
      pragma Import (C, Internal, "gtk_page_setup_unix_dialog_set_page_setup");
   begin
      Internal (Get_Object (Self), Get_Object (Page_Setup));
   end Set_Page_Setup;

   ------------------------
   -- Set_Print_Settings --
   ------------------------

   procedure Set_Print_Settings
      (Self           : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       Print_Settings : access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class)
   is
      procedure Internal
         (Self           : System.Address;
          Print_Settings : System.Address);
      pragma Import (C, Internal, "gtk_page_setup_unix_dialog_set_print_settings");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Print_Settings)));
   end Set_Print_Settings;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self   : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
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
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_root_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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

   procedure Realize
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self     : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self         : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self : not null access Gtk_Page_Setup_Unix_Dialog_Record)
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
      (Self        : not null access Gtk_Page_Setup_Unix_Dialog_Record;
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
      (Self  : not null access Gtk_Page_Setup_Unix_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Page_Setup_Unix_Dialog;
