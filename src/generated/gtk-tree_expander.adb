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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Tree_Expander is

   package Type_Conversion_Gtk_Tree_Expander is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Expander_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_Expander);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Tree_Expander) is
   begin
      Self := new Gtk_Tree_Expander_Record;
      Gtk.Tree_Expander.Initialize (Self);
   end Gtk_New;

   ---------------------------
   -- Gtk_Tree_Expander_New --
   ---------------------------

   function Gtk_Tree_Expander_New return Gtk_Tree_Expander is
      Self : constant Gtk_Tree_Expander := new Gtk_Tree_Expander_Record;
   begin
      Gtk.Tree_Expander.Initialize (Self);
      return Self;
   end Gtk_Tree_Expander_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Tree_Expander_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_expander_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_expander_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   -----------------------
   -- Get_Hide_Expander --
   -----------------------

   function Get_Hide_Expander
      (Self : not null access Gtk_Tree_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_expander_get_hide_expander");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Hide_Expander;

   --------------------------
   -- Get_Indent_For_Depth --
   --------------------------

   function Get_Indent_For_Depth
      (Self : not null access Gtk_Tree_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_expander_get_indent_for_depth");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Indent_For_Depth;

   -------------------------
   -- Get_Indent_For_Icon --
   -------------------------

   function Get_Indent_For_Icon
      (Self : not null access Gtk_Tree_Expander_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_expander_get_indent_for_icon");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Indent_For_Icon;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self : not null access Gtk_Tree_Expander_Record)
       return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_expander_get_item");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Item;

   ------------------
   -- Get_List_Row --
   ------------------

   function Get_List_Row
      (Self : not null access Gtk_Tree_Expander_Record)
       return Gtk.Tree_List_Row.Gtk_Tree_List_Row
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_expander_get_list_row");
      Stub_Gtk_Tree_List_Row : Gtk.Tree_List_Row.Gtk_Tree_List_Row_Record;
   begin
      return Gtk.Tree_List_Row.Gtk_Tree_List_Row (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Tree_List_Row));
   end Get_List_Row;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Tree_Expander_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_tree_expander_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   -----------------------
   -- Set_Hide_Expander --
   -----------------------

   procedure Set_Hide_Expander
      (Self          : not null access Gtk_Tree_Expander_Record;
       Hide_Expander : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Hide_Expander : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_expander_set_hide_expander");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Hide_Expander));
   end Set_Hide_Expander;

   --------------------------
   -- Set_Indent_For_Depth --
   --------------------------

   procedure Set_Indent_For_Depth
      (Self             : not null access Gtk_Tree_Expander_Record;
       Indent_For_Depth : Boolean)
   is
      procedure Internal
         (Self             : System.Address;
          Indent_For_Depth : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_expander_set_indent_for_depth");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Indent_For_Depth));
   end Set_Indent_For_Depth;

   -------------------------
   -- Set_Indent_For_Icon --
   -------------------------

   procedure Set_Indent_For_Icon
      (Self            : not null access Gtk_Tree_Expander_Record;
       Indent_For_Icon : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Indent_For_Icon : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_expander_set_indent_for_icon");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Indent_For_Icon));
   end Set_Indent_For_Icon;

   ------------------
   -- Set_List_Row --
   ------------------

   procedure Set_List_Row
      (Self     : not null access Gtk_Tree_Expander_Record;
       List_Row : access Gtk.Tree_List_Row.Gtk_Tree_List_Row_Record'Class)
   is
      procedure Internal (Self : System.Address; List_Row : System.Address);
      pragma Import (C, Internal, "gtk_tree_expander_set_list_row");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (List_Row)));
   end Set_List_Row;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Tree_Expander_Record;
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
      (Self : not null access Gtk_Tree_Expander_Record) return UTF8_String
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
      (Self : not null access Gtk_Tree_Expander_Record)
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
      (Self : not null access Gtk_Tree_Expander_Record)
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
      (Self : not null access Gtk_Tree_Expander_Record)
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
      (Self   : not null access Gtk_Tree_Expander_Record;
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

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Tree_Expander_Record)
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
      (Self : not null access Gtk_Tree_Expander_Record)
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
      (Self  : not null access Gtk_Tree_Expander_Record;
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
      (Self     : not null access Gtk_Tree_Expander_Record;
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
      (Self     : not null access Gtk_Tree_Expander_Record;
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
      (Self  : not null access Gtk_Tree_Expander_Record;
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
      (Self         : not null access Gtk_Tree_Expander_Record;
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
      (Self        : not null access Gtk_Tree_Expander_Record;
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
      (Self  : not null access Gtk_Tree_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Tree_Expander;
