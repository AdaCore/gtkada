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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Center_Box is

   package Type_Conversion_Gtk_Center_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Center_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Center_Box);

   ------------------------
   -- Gtk_Center_Box_New --
   ------------------------

   function Gtk_Center_Box_New return Gtk_Center_Box is
      Self : constant Gtk_Center_Box := new Gtk_Center_Box_Record;
   begin
      Gtk.Center_Box.Initialize (Self);
      return Self;
   end Gtk_Center_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Center_Box) is
   begin
      Self := new Gtk_Center_Box_Record;
      Gtk.Center_Box.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Center_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_center_box_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Get_Baseline_Position --
   ---------------------------

   function Get_Baseline_Position
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Enums.Gtk_Baseline_Position
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Baseline_Position;
      pragma Import (C, Internal, "gtk_center_box_get_baseline_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Baseline_Position;

   -----------------------
   -- Get_Center_Widget --
   -----------------------

   function Get_Center_Widget
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_center_box_get_center_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Center_Widget;

   --------------------
   -- Get_End_Widget --
   --------------------

   function Get_End_Widget
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_center_box_get_end_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_End_Widget;

   ----------------------------
   -- Get_Shrink_Center_Last --
   ----------------------------

   function Get_Shrink_Center_Last
      (Self : not null access Gtk_Center_Box_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_center_box_get_shrink_center_last");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Shrink_Center_Last;

   ----------------------
   -- Get_Start_Widget --
   ----------------------

   function Get_Start_Widget
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_center_box_get_start_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Start_Widget;

   ---------------------------
   -- Set_Baseline_Position --
   ---------------------------

   procedure Set_Baseline_Position
      (Self     : not null access Gtk_Center_Box_Record;
       Position : Gtk.Enums.Gtk_Baseline_Position)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Gtk.Enums.Gtk_Baseline_Position);
      pragma Import (C, Internal, "gtk_center_box_set_baseline_position");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Baseline_Position;

   -----------------------
   -- Set_Center_Widget --
   -----------------------

   procedure Set_Center_Widget
      (Self  : not null access Gtk_Center_Box_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_center_box_set_center_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Center_Widget;

   --------------------
   -- Set_End_Widget --
   --------------------

   procedure Set_End_Widget
      (Self  : not null access Gtk_Center_Box_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_center_box_set_end_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_End_Widget;

   ----------------------------
   -- Set_Shrink_Center_Last --
   ----------------------------

   procedure Set_Shrink_Center_Last
      (Self               : not null access Gtk_Center_Box_Record;
       Shrink_Center_Last : Boolean)
   is
      procedure Internal
         (Self               : System.Address;
          Shrink_Center_Last : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_center_box_set_shrink_center_last");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Shrink_Center_Last));
   end Set_Shrink_Center_Last;

   ----------------------
   -- Set_Start_Widget --
   ----------------------

   procedure Set_Start_Widget
      (Self  : not null access Gtk_Center_Box_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_center_box_set_start_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Start_Widget;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Center_Box_Record;
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
      (Self : not null access Gtk_Center_Box_Record) return UTF8_String
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
      (Self : not null access Gtk_Center_Box_Record)
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
      (Self : not null access Gtk_Center_Box_Record)
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
      (Self : not null access Gtk_Center_Box_Record)
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
      (Self   : not null access Gtk_Center_Box_Record;
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
      (Self : not null access Gtk_Center_Box_Record)
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
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Center_Box_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Center_Box_Record;
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
      (Self     : not null access Gtk_Center_Box_Record;
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
      (Self     : not null access Gtk_Center_Box_Record;
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
      (Self  : not null access Gtk_Center_Box_Record;
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
      (Self         : not null access Gtk_Center_Box_Record;
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

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Center_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Center_Box_Record;
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
      (Self  : not null access Gtk_Center_Box_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Center_Box;
