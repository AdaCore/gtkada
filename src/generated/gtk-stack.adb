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

package body Gtk.Stack is

   package Type_Conversion_Gtk_Stack is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Stack_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Stack);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Stack) is
   begin
      Self := new Gtk_Stack_Record;
      Gtk.Stack.Initialize (Self);
   end Gtk_New;

   -------------------
   -- Gtk_Stack_New --
   -------------------

   function Gtk_Stack_New return Gtk_Stack is
      Self : constant Gtk_Stack := new Gtk_Stack_Record;
   begin
      Gtk.Stack.Initialize (Self);
      return Self;
   end Gtk_Stack_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Stack_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_stack_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------
   -- Add_Child --
   ---------------

   function Add_Child
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Stack_Page.Gtk_Stack_Page
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_stack_add_child");
      Stub_Gtk_Stack_Page : Gtk.Stack_Page.Gtk_Stack_Page_Record;
   begin
      return Gtk.Stack_Page.Gtk_Stack_Page (Get_User_Data (Internal (Get_Object (Self), Get_Object (Child)), Stub_Gtk_Stack_Page));
   end Add_Child;

   ---------------
   -- Add_Named --
   ---------------

   function Add_Named
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Name  : UTF8_String := "") return Gtk.Stack_Page.Gtk_Stack_Page
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address;
          Name  : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_stack_add_named");
      Tmp_Name            : Gtkada.Types.Chars_Ptr;
      Stub_Gtk_Stack_Page : Gtk.Stack_Page.Gtk_Stack_Page_Record;
      Tmp_Return          : System.Address;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Child), Tmp_Name);
      Free (Tmp_Name);
      return Gtk.Stack_Page.Gtk_Stack_Page (Get_User_Data (Tmp_Return, Stub_Gtk_Stack_Page));
   end Add_Named;

   ----------------
   -- Add_Titled --
   ----------------

   function Add_Titled
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Name  : UTF8_String := "";
       Title : UTF8_String) return Gtk.Stack_Page.Gtk_Stack_Page
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address;
          Name  : Gtkada.Types.Chars_Ptr;
          Title : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_stack_add_titled");
      Tmp_Name            : Gtkada.Types.Chars_Ptr;
      Tmp_Title           : Gtkada.Types.Chars_Ptr := New_String (Title);
      Stub_Gtk_Stack_Page : Gtk.Stack_Page.Gtk_Stack_Page_Record;
      Tmp_Return          : System.Address;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Child), Tmp_Name, Tmp_Title);
      Free (Tmp_Title);
      Free (Tmp_Name);
      return Gtk.Stack_Page.Gtk_Stack_Page (Get_User_Data (Tmp_Return, Stub_Gtk_Stack_Page));
   end Add_Titled;

   -----------------------
   -- Get_Child_By_Name --
   -----------------------

   function Get_Child_By_Name
      (Self : not null access Gtk_Stack_Record;
       Name : UTF8_String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_stack_get_child_by_name");
      Tmp_Name        : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Tmp_Return, Stub_Gtk_Widget));
   end Get_Child_By_Name;

   ----------------------
   -- Get_Hhomogeneous --
   ----------------------

   function Get_Hhomogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_stack_get_hhomogeneous");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Hhomogeneous;

   --------------------------
   -- Get_Interpolate_Size --
   --------------------------

   function Get_Interpolate_Size
      (Self : not null access Gtk_Stack_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_stack_get_interpolate_size");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Interpolate_Size;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Stack_Page.Gtk_Stack_Page
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_stack_get_page");
      Stub_Gtk_Stack_Page : Gtk.Stack_Page.Gtk_Stack_Page_Record;
   begin
      return Gtk.Stack_Page.Gtk_Stack_Page (Get_User_Data (Internal (Get_Object (Self), Get_Object (Child)), Stub_Gtk_Stack_Page));
   end Get_Page;

   ---------------
   -- Get_Pages --
   ---------------

   function Get_Pages
      (Self : not null access Gtk_Stack_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model
   is
      function Internal
         (Self : System.Address)
          return Gtk.Selection_Model.Gtk_Selection_Model;
      pragma Import (C, Internal, "gtk_stack_get_pages");
   begin
      return Internal (Get_Object (Self));
   end Get_Pages;

   -----------------------------
   -- Get_Transition_Duration --
   -----------------------------

   function Get_Transition_Duration
      (Self : not null access Gtk_Stack_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_stack_get_transition_duration");
   begin
      return Internal (Get_Object (Self));
   end Get_Transition_Duration;

   ----------------------------
   -- Get_Transition_Running --
   ----------------------------

   function Get_Transition_Running
      (Self : not null access Gtk_Stack_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_stack_get_transition_running");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Transition_Running;

   -------------------------
   -- Get_Transition_Type --
   -------------------------

   function Get_Transition_Type
      (Self : not null access Gtk_Stack_Record)
       return Gtk_Stack_Transition_Type
   is
      function Internal
         (Self : System.Address) return Gtk_Stack_Transition_Type;
      pragma Import (C, Internal, "gtk_stack_get_transition_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Transition_Type;

   ----------------------
   -- Get_Vhomogeneous --
   ----------------------

   function Get_Vhomogeneous
      (Self : not null access Gtk_Stack_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_stack_get_vhomogeneous");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Vhomogeneous;

   -----------------------
   -- Get_Visible_Child --
   -----------------------

   function Get_Visible_Child
      (Self : not null access Gtk_Stack_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_stack_get_visible_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Visible_Child;

   ----------------------------
   -- Get_Visible_Child_Name --
   ----------------------------

   function Get_Visible_Child_Name
      (Self : not null access Gtk_Stack_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_stack_get_visible_child_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Visible_Child_Name;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_stack_remove");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Remove;

   ----------------------
   -- Set_Hhomogeneous --
   ----------------------

   procedure Set_Hhomogeneous
      (Self         : not null access Gtk_Stack_Record;
       Hhomogeneous : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Hhomogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_stack_set_hhomogeneous");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Hhomogeneous));
   end Set_Hhomogeneous;

   --------------------------
   -- Set_Interpolate_Size --
   --------------------------

   procedure Set_Interpolate_Size
      (Self             : not null access Gtk_Stack_Record;
       Interpolate_Size : Boolean)
   is
      procedure Internal
         (Self             : System.Address;
          Interpolate_Size : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_stack_set_interpolate_size");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Interpolate_Size));
   end Set_Interpolate_Size;

   -----------------------------
   -- Set_Transition_Duration --
   -----------------------------

   procedure Set_Transition_Duration
      (Self     : not null access Gtk_Stack_Record;
       Duration : Guint)
   is
      procedure Internal (Self : System.Address; Duration : Guint);
      pragma Import (C, Internal, "gtk_stack_set_transition_duration");
   begin
      Internal (Get_Object (Self), Duration);
   end Set_Transition_Duration;

   -------------------------
   -- Set_Transition_Type --
   -------------------------

   procedure Set_Transition_Type
      (Self       : not null access Gtk_Stack_Record;
       Transition : Gtk_Stack_Transition_Type)
   is
      procedure Internal
         (Self       : System.Address;
          Transition : Gtk_Stack_Transition_Type);
      pragma Import (C, Internal, "gtk_stack_set_transition_type");
   begin
      Internal (Get_Object (Self), Transition);
   end Set_Transition_Type;

   ----------------------
   -- Set_Vhomogeneous --
   ----------------------

   procedure Set_Vhomogeneous
      (Self         : not null access Gtk_Stack_Record;
       Vhomogeneous : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Vhomogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_stack_set_vhomogeneous");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Vhomogeneous));
   end Set_Vhomogeneous;

   -----------------------
   -- Set_Visible_Child --
   -----------------------

   procedure Set_Visible_Child
      (Self  : not null access Gtk_Stack_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_stack_set_visible_child");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Set_Visible_Child;

   ----------------------------
   -- Set_Visible_Child_Full --
   ----------------------------

   procedure Set_Visible_Child_Full
      (Self       : not null access Gtk_Stack_Record;
       Name       : UTF8_String;
       Transition : Gtk_Stack_Transition_Type)
   is
      procedure Internal
         (Self       : System.Address;
          Name       : Gtkada.Types.Chars_Ptr;
          Transition : Gtk_Stack_Transition_Type);
      pragma Import (C, Internal, "gtk_stack_set_visible_child_full");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name, Transition);
      Free (Tmp_Name);
   end Set_Visible_Child_Full;

   ----------------------------
   -- Set_Visible_Child_Name --
   ----------------------------

   procedure Set_Visible_Child_Name
      (Self : not null access Gtk_Stack_Record;
       Name : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_stack_set_visible_child_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Visible_Child_Name;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Stack_Record;
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
      (Self : not null access Gtk_Stack_Record) return UTF8_String
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
      (Self : not null access Gtk_Stack_Record)
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
      (Self : not null access Gtk_Stack_Record)
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
      (Self : not null access Gtk_Stack_Record)
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
      (Self   : not null access Gtk_Stack_Record;
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
      (Self : not null access Gtk_Stack_Record)
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
      (Self : not null access Gtk_Stack_Record)
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
      (Self  : not null access Gtk_Stack_Record;
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
      (Self     : not null access Gtk_Stack_Record;
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
      (Self     : not null access Gtk_Stack_Record;
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
      (Self  : not null access Gtk_Stack_Record;
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
      (Self         : not null access Gtk_Stack_Record;
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
      (Self        : not null access Gtk_Stack_Record;
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
      (Self  : not null access Gtk_Stack_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Stack;
