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

package body Gtk.Drop_Down is

   package Type_Conversion_Gtk_Drop_Down is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Drop_Down_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Drop_Down);

   -----------------------
   -- Gtk_Drop_Down_New --
   -----------------------

   function Gtk_Drop_Down_New
      (Model      : Glib.List_Model.Glist_Model;
       Expression : Gtk.Expression.Gtk_Expression) return Gtk_Drop_Down
   is
      Self : constant Gtk_Drop_Down := new Gtk_Drop_Down_Record;
   begin
      Gtk.Drop_Down.Initialize (Self, Model, Expression);
      return Self;
   end Gtk_Drop_Down_New;

   ------------------------------------
   -- Gtk_Drop_Down_New_From_Strings --
   ------------------------------------

   function Gtk_Drop_Down_New_From_Strings
      (Strings : GNAT.Strings.String_List) return Gtk_Drop_Down
   is
      Self : constant Gtk_Drop_Down := new Gtk_Drop_Down_Record;
   begin
      Gtk.Drop_Down.Initialize_From_Strings (Self, Strings);
      return Self;
   end Gtk_Drop_Down_New_From_Strings;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gtk_Drop_Down;
       Model      : Glib.List_Model.Glist_Model;
       Expression : Gtk.Expression.Gtk_Expression)
   is
   begin
      Self := new Gtk_Drop_Down_Record;
      Gtk.Drop_Down.Initialize (Self, Model, Expression);
   end Gtk_New;

   --------------------------
   -- Gtk_New_From_Strings --
   --------------------------

   procedure Gtk_New_From_Strings
      (Self    : out Gtk_Drop_Down;
       Strings : GNAT.Strings.String_List)
   is
   begin
      Self := new Gtk_Drop_Down_Record;
      Gtk.Drop_Down.Initialize_From_Strings (Self, Strings);
   end Gtk_New_From_Strings;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gtk_Drop_Down_Record'Class;
       Model      : Glib.List_Model.Glist_Model;
       Expression : Gtk.Expression.Gtk_Expression)
   is
      function Internal
         (Model      : Glib.List_Model.Glist_Model;
          Expression : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_new");
   begin
      if not Self.Is_Created then
         if Expression /= null then
            --  transfer-ownership='full'
            Adjust (Expression.all);
         end if;
         Set_Object (Self, Internal (Model, Get_Object (Expression)));
      end if;
   end Initialize;

   -----------------------------
   -- Initialize_From_Strings --
   -----------------------------

   procedure Initialize_From_Strings
      (Self    : not null access Gtk_Drop_Down_Record'Class;
       Strings : GNAT.Strings.String_List)
   is
      function Internal
         (Strings : Gtkada.Types.chars_ptr_array) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_new_from_strings");
      Tmp_Strings : Gtkada.Types.chars_ptr_array := From_String_List (Strings);
      Tmp_Return  : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Strings);
         Set_Object (Self, Tmp_Return);
      end if;
      Gtkada.Types.Free (Tmp_Strings);
   end Initialize_From_Strings;

   -----------------------
   -- Get_Enable_Search --
   -----------------------

   function Get_Enable_Search
      (Self : not null access Gtk_Drop_Down_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_drop_down_get_enable_search");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Search;

   --------------------
   -- Get_Expression --
   --------------------

   function Get_Expression
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.Expression.Gtk_Expression
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_get_expression");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Expression;

   -----------------
   -- Get_Factory --
   -----------------

   function Get_Factory
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_get_factory");
      Stub_Gtk_List_Item_Factory : Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record;
   begin
      return Gtk.List_Item_Factory.Gtk_List_Item_Factory (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_List_Item_Factory));
   end Get_Factory;

   ------------------------
   -- Get_Header_Factory --
   ------------------------

   function Get_Header_Factory
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_get_header_factory");
      Stub_Gtk_List_Item_Factory : Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record;
   begin
      return Gtk.List_Item_Factory.Gtk_List_Item_Factory (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_List_Item_Factory));
   end Get_Header_Factory;

   ----------------------
   -- Get_List_Factory --
   ----------------------

   function Get_List_Factory
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_get_list_factory");
      Stub_Gtk_List_Item_Factory : Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record;
   begin
      return Gtk.List_Item_Factory.Gtk_List_Item_Factory (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_List_Item_Factory));
   end Get_List_Factory;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Drop_Down_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_drop_down_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   ---------------------------
   -- Get_Search_Match_Mode --
   ---------------------------

   function Get_Search_Match_Mode
      (Self : not null access Gtk_Drop_Down_Record)
       return Gtk.String_Filter.Gtk_String_Filter_Match_Mode
   is
      function Internal
         (Self : System.Address)
          return Gtk.String_Filter.Gtk_String_Filter_Match_Mode;
      pragma Import (C, Internal, "gtk_drop_down_get_search_match_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Search_Match_Mode;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected
      (Self : not null access Gtk_Drop_Down_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_drop_down_get_selected");
   begin
      return Internal (Get_Object (Self));
   end Get_Selected;

   -----------------------
   -- Get_Selected_Item --
   -----------------------

   function Get_Selected_Item
      (Self : not null access Gtk_Drop_Down_Record)
       return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drop_down_get_selected_item");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Selected_Item;

   --------------------
   -- Get_Show_Arrow --
   --------------------

   function Get_Show_Arrow
      (Self : not null access Gtk_Drop_Down_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_drop_down_get_show_arrow");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Arrow;

   -----------------------
   -- Set_Enable_Search --
   -----------------------

   procedure Set_Enable_Search
      (Self          : not null access Gtk_Drop_Down_Record;
       Enable_Search : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Enable_Search : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_drop_down_set_enable_search");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enable_Search));
   end Set_Enable_Search;

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
      (Self       : not null access Gtk_Drop_Down_Record;
       Expression : Gtk.Expression.Gtk_Expression)
   is
      procedure Internal
         (Self       : System.Address;
          Expression : System.Address);
      pragma Import (C, Internal, "gtk_drop_down_set_expression");
   begin
      Internal (Get_Object (Self), Get_Object (Expression));
   end Set_Expression;

   -----------------
   -- Set_Factory --
   -----------------

   procedure Set_Factory
      (Self    : not null access Gtk_Drop_Down_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      procedure Internal (Self : System.Address; Factory : System.Address);
      pragma Import (C, Internal, "gtk_drop_down_set_factory");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Factory)));
   end Set_Factory;

   ------------------------
   -- Set_Header_Factory --
   ------------------------

   procedure Set_Header_Factory
      (Self    : not null access Gtk_Drop_Down_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      procedure Internal (Self : System.Address; Factory : System.Address);
      pragma Import (C, Internal, "gtk_drop_down_set_header_factory");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Factory)));
   end Set_Header_Factory;

   ----------------------
   -- Set_List_Factory --
   ----------------------

   procedure Set_List_Factory
      (Self    : not null access Gtk_Drop_Down_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      procedure Internal (Self : System.Address; Factory : System.Address);
      pragma Import (C, Internal, "gtk_drop_down_set_list_factory");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Factory)));
   end Set_List_Factory;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Drop_Down_Record;
       Model : Glib.List_Model.Glist_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Glib.List_Model.Glist_Model);
      pragma Import (C, Internal, "gtk_drop_down_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   ---------------------------
   -- Set_Search_Match_Mode --
   ---------------------------

   procedure Set_Search_Match_Mode
      (Self              : not null access Gtk_Drop_Down_Record;
       Search_Match_Mode : Gtk.String_Filter.Gtk_String_Filter_Match_Mode)
   is
      procedure Internal
         (Self              : System.Address;
          Search_Match_Mode : Gtk.String_Filter.Gtk_String_Filter_Match_Mode);
      pragma Import (C, Internal, "gtk_drop_down_set_search_match_mode");
   begin
      Internal (Get_Object (Self), Search_Match_Mode);
   end Set_Search_Match_Mode;

   ------------------
   -- Set_Selected --
   ------------------

   procedure Set_Selected
      (Self     : not null access Gtk_Drop_Down_Record;
       Position : Guint)
   is
      procedure Internal (Self : System.Address; Position : Guint);
      pragma Import (C, Internal, "gtk_drop_down_set_selected");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Selected;

   --------------------
   -- Set_Show_Arrow --
   --------------------

   procedure Set_Show_Arrow
      (Self       : not null access Gtk_Drop_Down_Record;
       Show_Arrow : Boolean)
   is
      procedure Internal (Self : System.Address; Show_Arrow : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_drop_down_set_show_arrow");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Arrow));
   end Set_Show_Arrow;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Drop_Down_Record;
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
      (Self : not null access Gtk_Drop_Down_Record) return UTF8_String
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
      (Self : not null access Gtk_Drop_Down_Record)
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
      (Self : not null access Gtk_Drop_Down_Record)
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
      (Self : not null access Gtk_Drop_Down_Record)
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
      (Self   : not null access Gtk_Drop_Down_Record;
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
      (Self : not null access Gtk_Drop_Down_Record)
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
      (Self : not null access Gtk_Drop_Down_Record)
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
      (Self  : not null access Gtk_Drop_Down_Record;
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
      (Self     : not null access Gtk_Drop_Down_Record;
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
      (Self     : not null access Gtk_Drop_Down_Record;
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
      (Self  : not null access Gtk_Drop_Down_Record;
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
      (Self         : not null access Gtk_Drop_Down_Record;
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
      (Self        : not null access Gtk_Drop_Down_Record;
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
      (Self  : not null access Gtk_Drop_Down_Record;
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
     (Cb_Gtk_Drop_Down_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drop_Down_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Drop_Down_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Down_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Down_Record'Class;
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

   procedure Marsh_Gtk_Drop_Down_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drop_Down_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drop_Down_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drop_Down_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drop_Down_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drop_Down_Record'Class;
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

   ------------------------------
   -- Marsh_Gtk_Drop_Down_Void --
   ------------------------------

   procedure Marsh_Gtk_Drop_Down_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drop_Down_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drop_Down := Gtk_Drop_Down (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drop_Down_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Drop_Down_Record;
       Call  : Cb_Gtk_Drop_Down_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Drop_Down_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.Drop_Down;
