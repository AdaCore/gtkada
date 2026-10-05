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

package body Gtk.Progress_Bar is

   package Type_Conversion_Gtk_Progress_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Progress_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Progress_Bar);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar) is
   begin
      Progress_Bar := new Gtk_Progress_Bar_Record;
      Gtk.Progress_Bar.Initialize (Progress_Bar);
   end Gtk_New;

   --------------------------
   -- Gtk_Progress_Bar_New --
   --------------------------

   function Gtk_Progress_Bar_New return Gtk_Progress_Bar is
      Progress_Bar : constant Gtk_Progress_Bar := new Gtk_Progress_Bar_Record;
   begin
      Gtk.Progress_Bar.Initialize (Progress_Bar);
      return Progress_Bar;
   end Gtk_Progress_Bar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_progress_bar_new");
   begin
      if not Progress_Bar.Is_Created then
         Set_Object (Progress_Bar, Internal);
      end if;
   end Initialize;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Progress_Bar : System.Address)
          return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_progress_bar_get_ellipsize");
   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Ellipsize;

   ------------------
   -- Get_Fraction --
   ------------------

   function Get_Fraction
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Gdouble
   is
      function Internal (Progress_Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_bar_get_fraction");
   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Fraction;

   ------------------
   -- Get_Inverted --
   ------------------

   function Get_Inverted
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Boolean
   is
      function Internal (Progress_Bar : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_progress_bar_get_inverted");
   begin
      return Internal (Get_Object (Progress_Bar)) /= 0;
   end Get_Inverted;

   --------------------
   -- Get_Pulse_Step --
   --------------------

   function Get_Pulse_Step
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Gdouble
   is
      function Internal (Progress_Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_bar_get_pulse_step");
   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Pulse_Step;

   -------------------
   -- Get_Show_Text --
   -------------------

   function Get_Show_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Boolean
   is
      function Internal (Progress_Bar : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_progress_bar_get_show_text");
   begin
      return Internal (Get_Object (Progress_Bar)) /= 0;
   end Get_Show_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return UTF8_String
   is
      function Internal
         (Progress_Bar : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_progress_bar_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Progress_Bar)));
   end Get_Text;

   -----------
   -- Pulse --
   -----------

   procedure Pulse (Progress_Bar : not null access Gtk_Progress_Bar_Record) is
      procedure Internal (Progress_Bar : System.Address);
      pragma Import (C, Internal, "gtk_progress_bar_pulse");
   begin
      Internal (Get_Object (Progress_Bar));
   end Pulse;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Mode         : Pango.Layout.Pango_Ellipsize_Mode)
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Mode         : Pango.Layout.Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "gtk_progress_bar_set_ellipsize");
   begin
      Internal (Get_Object (Progress_Bar), Mode);
   end Set_Ellipsize;

   ------------------
   -- Set_Fraction --
   ------------------

   procedure Set_Fraction
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble)
   is
      procedure Internal (Progress_Bar : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_progress_bar_set_fraction");
   begin
      Internal (Get_Object (Progress_Bar), Fraction);
   end Set_Fraction;

   ------------------
   -- Set_Inverted --
   ------------------

   procedure Set_Inverted
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Inverted     : Boolean)
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Inverted     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_progress_bar_set_inverted");
   begin
      Internal (Get_Object (Progress_Bar), Boolean'Pos (Inverted));
   end Set_Inverted;

   --------------------
   -- Set_Pulse_Step --
   --------------------

   procedure Set_Pulse_Step
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble)
   is
      procedure Internal (Progress_Bar : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_progress_bar_set_pulse_step");
   begin
      Internal (Get_Object (Progress_Bar), Fraction);
   end Set_Pulse_Step;

   -------------------
   -- Set_Show_Text --
   -------------------

   procedure Set_Show_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Show_Text    : Boolean)
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Show_Text    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_progress_bar_set_show_text");
   begin
      Internal (Get_Object (Progress_Bar), Boolean'Pos (Show_Text));
   end Set_Show_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Text         : UTF8_String := "")
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Text         : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_progress_bar_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Text :=
        (if Text = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Text));
      Internal (Get_Object (Progress_Bar), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Progress_Bar_Record;
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
      (Self : not null access Gtk_Progress_Bar_Record) return UTF8_String
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
      (Self : not null access Gtk_Progress_Bar_Record)
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
      (Self : not null access Gtk_Progress_Bar_Record)
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
      (Self : not null access Gtk_Progress_Bar_Record)
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
      (Self   : not null access Gtk_Progress_Bar_Record;
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
      (Self : not null access Gtk_Progress_Bar_Record)
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
      (Self : not null access Gtk_Progress_Bar_Record)
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
      (Self : not null access Gtk_Progress_Bar_Record)
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
      (Self  : not null access Gtk_Progress_Bar_Record;
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
      (Self     : not null access Gtk_Progress_Bar_Record;
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
      (Self     : not null access Gtk_Progress_Bar_Record;
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
      (Self  : not null access Gtk_Progress_Bar_Record;
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
      (Self         : not null access Gtk_Progress_Bar_Record;
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
      (Self        : not null access Gtk_Progress_Bar_Record;
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
      (Self        : not null access Gtk_Progress_Bar_Record;
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
      (Self  : not null access Gtk_Progress_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Progress_Bar;
