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

package body Gtk.Editable_Label is

   package Type_Conversion_Gtk_Editable_Label is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Editable_Label_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Editable_Label);

   ----------------------------
   -- Gtk_Editable_Label_New --
   ----------------------------

   function Gtk_Editable_Label_New
      (Str : UTF8_String) return Gtk_Editable_Label
   is
      Self : constant Gtk_Editable_Label := new Gtk_Editable_Label_Record;
   begin
      Gtk.Editable_Label.Initialize (Self, Str);
      return Self;
   end Gtk_Editable_Label_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Editable_Label; Str : UTF8_String) is
   begin
      Self := new Gtk_Editable_Label_Record;
      Gtk.Editable_Label.Initialize (Self, Str);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Editable_Label_Record'Class;
       Str  : UTF8_String)
   is
      function Internal (Str : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_editable_label_new");
      Tmp_Str    : Gtkada.Types.Chars_Ptr := New_String (Str);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Str);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Str);
   end Initialize;

   -----------------
   -- Get_Editing --
   -----------------

   function Get_Editing
      (Self : not null access Gtk_Editable_Label_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_label_get_editing");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Editing;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
      (Self : not null access Gtk_Editable_Label_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_label_start_editing");
   begin
      Internal (Get_Object (Self));
   end Start_Editing;

   ------------------
   -- Stop_Editing --
   ------------------

   procedure Stop_Editing
      (Self   : not null access Gtk_Editable_Label_Record;
       Commit : Boolean)
   is
      procedure Internal (Self : System.Address; Commit : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_label_stop_editing");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Commit));
   end Stop_Editing;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Editable_Label_Record;
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
      (Self  : not null access Gtk_Editable_Label_Record;
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self      : not null access Gtk_Editable_Label_Record;
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self : not null access Gtk_Editable_Label_Record) return UTF8_String
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self   : not null access Gtk_Editable_Label_Record;
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
      (Self      : not null access Gtk_Editable_Label_Record;
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self : not null access Gtk_Editable_Label_Record) return Boolean
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
      (Self : not null access Gtk_Editable_Label_Record) return Boolean
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self : not null access Gtk_Editable_Label_Record) return Glib.Gint
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
      (Self : not null access Gtk_Editable_Label_Record)
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
      (Self  : not null access Gtk_Editable_Label_Record;
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
      (Self : not null access Gtk_Editable_Label_Record) return Glib.Gint
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
      (Self          : not null access Gtk_Editable_Label_Record;
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
      (Self : not null access Gtk_Editable_Label_Record) return UTF8_String
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
      (Self : not null access Gtk_Editable_Label_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Width_Chars;

   -------------------
   -- Init_Delegate --
   -------------------

   procedure Init_Delegate
      (Self : not null access Gtk_Editable_Label_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_init_delegate");
   begin
      Internal (Get_Object (Self));
   end Init_Delegate;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Self     : not null access Gtk_Editable_Label_Record;
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
      (Self     : not null access Gtk_Editable_Label_Record;
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
      (Self     : not null access Gtk_Editable_Label_Record;
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
      (Self  : not null access Gtk_Editable_Label_Record;
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
      (Self      : not null access Gtk_Editable_Label_Record;
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
      (Self         : not null access Gtk_Editable_Label_Record;
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
      (Self   : not null access Gtk_Editable_Label_Record;
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
      (Self        : not null access Gtk_Editable_Label_Record;
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
      (Self        : not null access Gtk_Editable_Label_Record;
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
      (Self    : not null access Gtk_Editable_Label_Record;
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
      (Self     : not null access Gtk_Editable_Label_Record;
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
      (Self : not null access Gtk_Editable_Label_Record;
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
      (Self    : not null access Gtk_Editable_Label_Record;
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
      (Self        : not null access Gtk_Editable_Label_Record;
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
      (Self  : not null access Gtk_Editable_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Editable_Label;
