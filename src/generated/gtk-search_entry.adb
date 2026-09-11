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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Search_Entry is

   package Type_Conversion_Gtk_Search_Entry is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Search_Entry_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Search_Entry);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Search_Entry) is
   begin
      Self := new Gtk_Search_Entry_Record;
      Gtk.Search_Entry.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Gtk_Search_Entry_New --
   --------------------------

   function Gtk_Search_Entry_New return Gtk_Search_Entry is
      Self : constant Gtk_Search_Entry := new Gtk_Search_Entry_Record;
   begin
      Gtk.Search_Entry.Initialize (Self);
      return Self;
   end Gtk_Search_Entry_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Search_Entry_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_search_entry_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Get_Input_Hints --
   ---------------------

   function Get_Input_Hints
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Enums.Gtk_Input_Hints
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Input_Hints;
      pragma Import (C, Internal, "gtk_search_entry_get_input_hints");
   begin
      return Internal (Get_Object (Self));
   end Get_Input_Hints;

   -----------------------
   -- Get_Input_Purpose --
   -----------------------

   function Get_Input_Purpose
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Enums.Gtk_Input_Purpose
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Input_Purpose;
      pragma Import (C, Internal, "gtk_search_entry_get_input_purpose");
   begin
      return Internal (Get_Object (Self));
   end Get_Input_Purpose;

   ----------------------------
   -- Get_Key_Capture_Widget --
   ----------------------------

   function Get_Key_Capture_Widget
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_search_entry_get_key_capture_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Key_Capture_Widget;

   --------------------------
   -- Get_Placeholder_Text --
   --------------------------

   function Get_Placeholder_Text
      (Self : not null access Gtk_Search_Entry_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_search_entry_get_placeholder_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Placeholder_Text;

   ----------------------
   -- Get_Search_Delay --
   ----------------------

   function Get_Search_Delay
      (Self : not null access Gtk_Search_Entry_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_search_entry_get_search_delay");
   begin
      return Internal (Get_Object (Self));
   end Get_Search_Delay;

   ---------------------
   -- Set_Input_Hints --
   ---------------------

   procedure Set_Input_Hints
      (Self  : not null access Gtk_Search_Entry_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints)
   is
      procedure Internal
         (Self  : System.Address;
          Hints : Gtk.Enums.Gtk_Input_Hints);
      pragma Import (C, Internal, "gtk_search_entry_set_input_hints");
   begin
      Internal (Get_Object (Self), Hints);
   end Set_Input_Hints;

   -----------------------
   -- Set_Input_Purpose --
   -----------------------

   procedure Set_Input_Purpose
      (Self    : not null access Gtk_Search_Entry_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose)
   is
      procedure Internal
         (Self    : System.Address;
          Purpose : Gtk.Enums.Gtk_Input_Purpose);
      pragma Import (C, Internal, "gtk_search_entry_set_input_purpose");
   begin
      Internal (Get_Object (Self), Purpose);
   end Set_Input_Purpose;

   ----------------------------
   -- Set_Key_Capture_Widget --
   ----------------------------

   procedure Set_Key_Capture_Widget
      (Self   : not null access Gtk_Search_Entry_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_search_entry_set_key_capture_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Widget)));
   end Set_Key_Capture_Widget;

   --------------------------
   -- Set_Placeholder_Text --
   --------------------------

   procedure Set_Placeholder_Text
      (Self : not null access Gtk_Search_Entry_Record;
       Text : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_search_entry_set_placeholder_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Text :=
        (if Text = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Text));
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Placeholder_Text;

   ----------------------
   -- Set_Search_Delay --
   ----------------------

   procedure Set_Search_Delay
      (Self      : not null access Gtk_Search_Entry_Record;
       The_Delay : Guint)
   is
      procedure Internal (Self : System.Address; The_Delay : Guint);
      pragma Import (C, Internal, "gtk_search_entry_set_search_delay");
   begin
      Internal (Get_Object (Self), The_Delay);
   end Set_Search_Delay;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Search_Entry_Record;
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

   --------------------------------------------
   -- Delegate_Get_Accessible_Platform_State --
   --------------------------------------------

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Search_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_delegate_get_accessible_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Delegate_Get_Accessible_Platform_State;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection
      (Self : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");
   begin
      Internal (Get_Object (Self));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
      (Self      : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");
   begin
      Internal (Get_Object (Self), Start_Pos, End_Pos);
   end Delete_Text;

   ---------------------
   -- Finish_Delegate --
   ---------------------

   procedure Finish_Delegate
      (Self : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_finish_delegate");
   begin
      Internal (Get_Object (Self));
   end Finish_Delegate;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Search_Entry_Record) return UTF8_String
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
      (Self : not null access Gtk_Search_Entry_Record)
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
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
      (Self : not null access Gtk_Search_Entry_Record)
       return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gtk_editable_get_alignment");
   begin
      return Internal (Get_Object (Self));
   end Get_Alignment;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Search_Entry_Record)
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
      (Self   : not null access Gtk_Search_Entry_Record;
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

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Self      : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Delegate --
   ------------------

   function Get_Delegate
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Editable.Gtk_Editable
   is
      function Internal
         (Self : System.Address) return Gtk.Editable.Gtk_Editable;
      pragma Import (C, Internal, "gtk_editable_get_delegate");
   begin
      return Internal (Get_Object (Self));
   end Get_Delegate;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
      (Self : not null access Gtk_Search_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Editable;

   ---------------------
   -- Get_Enable_Undo --
   ---------------------

   function Get_Enable_Undo
      (Self : not null access Gtk_Search_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_enable_undo");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Undo;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Search_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Search_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_max_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Width_Chars;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Search_Entry_Record)
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
      (Self  : not null access Gtk_Search_Entry_Record;
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

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_Search_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Search_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Self          : System.Address;
          Acc_Start_Pos : access Glib.Gint;
          Acc_End_Pos   : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Glib.Gint;
      Acc_End_Pos   : aliased Glib.Gint;
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtk_Search_Entry_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Text;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
      (Self : not null access Gtk_Search_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Width_Chars;

   -------------------
   -- Init_Delegate --
   -------------------

   procedure Init_Delegate (Self : not null access Gtk_Search_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_init_delegate");
   begin
      Internal (Get_Object (Self));
   end Init_Delegate;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Self     : not null access Gtk_Search_Entry_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Text     : Gtkada.Types.Chars_Ptr;
          Length   : Glib.Gint;
          Position : in out Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text, Length, Position);
      Free (Tmp_Text);
   end Insert_Text;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Search_Entry_Record;
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
      (Self     : not null access Gtk_Search_Entry_Record;
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
      (Self  : not null access Gtk_Search_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Self      : not null access Gtk_Search_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");
   begin
      Internal (Get_Object (Self), Start_Pos, End_Pos);
   end Select_Region;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Search_Entry_Record;
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

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Self   : not null access Gtk_Search_Entry_Record;
       Xalign : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_editable_set_alignment");
   begin
      Internal (Get_Object (Self), Xalign);
   end Set_Alignment;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (Self        : not null access Gtk_Search_Entry_Record;
       Is_Editable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Is_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ---------------------
   -- Set_Enable_Undo --
   ---------------------

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Search_Entry_Record;
       Enable_Undo : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Enable_Undo : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_enable_undo");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enable_Undo));
   end Set_Enable_Undo;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Search_Entry_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_max_width_chars");
   begin
      Internal (Get_Object (Self), N_Chars);
   end Set_Max_Width_Chars;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Self     : not null access Gtk_Search_Entry_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Position;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gtk_Search_Entry_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_editable_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Search_Entry_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_width_chars");
   begin
      Internal (Get_Object (Self), N_Chars);
   end Set_Width_Chars;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Search_Entry_Record;
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
      (Self  : not null access Gtk_Search_Entry_Record;
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
     (Cb_Gtk_Search_Entry_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Search_Entry_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Search_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Search_Entry_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Search_Entry_Record'Class;
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

   procedure Marsh_Gtk_Search_Entry_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Search_Entry_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Search_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Search_Entry_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Search_Entry_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Search_Entry_Record'Class;
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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------------
   -- Marsh_Gtk_Search_Entry_Void --
   ---------------------------------

   procedure Marsh_Gtk_Search_Entry_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Search_Entry_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Search_Entry := Gtk_Search_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Search_Entry_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   -------------------
   -- On_Next_Match --
   -------------------

   procedure On_Next_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "next-match" & ASCII.NUL, Call, After);
   end On_Next_Match;

   -------------------
   -- On_Next_Match --
   -------------------

   procedure On_Next_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "next-match" & ASCII.NUL, Call, After, Slot);
   end On_Next_Match;

   -----------------------
   -- On_Previous_Match --
   -----------------------

   procedure On_Previous_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "previous-match" & ASCII.NUL, Call, After);
   end On_Previous_Match;

   -----------------------
   -- On_Previous_Match --
   -----------------------

   procedure On_Previous_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "previous-match" & ASCII.NUL, Call, After, Slot);
   end On_Previous_Match;

   -----------------------
   -- On_Search_Changed --
   -----------------------

   procedure On_Search_Changed
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "search-changed" & ASCII.NUL, Call, After);
   end On_Search_Changed;

   -----------------------
   -- On_Search_Changed --
   -----------------------

   procedure On_Search_Changed
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "search-changed" & ASCII.NUL, Call, After, Slot);
   end On_Search_Changed;

   -----------------------
   -- On_Search_Started --
   -----------------------

   procedure On_Search_Started
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "search-started" & ASCII.NUL, Call, After);
   end On_Search_Started;

   -----------------------
   -- On_Search_Started --
   -----------------------

   procedure On_Search_Started
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "search-started" & ASCII.NUL, Call, After, Slot);
   end On_Search_Started;

   --------------------
   -- On_Stop_Search --
   --------------------

   procedure On_Stop_Search
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "stop-search" & ASCII.NUL, Call, After);
   end On_Stop_Search;

   --------------------
   -- On_Stop_Search --
   --------------------

   procedure On_Stop_Search
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "stop-search" & ASCII.NUL, Call, After, Slot);
   end On_Stop_Search;

end Gtk.Search_Entry;
