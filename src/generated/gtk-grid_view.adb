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

package body Gtk.Grid_View is

   package Type_Conversion_Gtk_Grid_View is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Grid_View_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Grid_View);

   -----------------------
   -- Gtk_Grid_View_New --
   -----------------------

   function Gtk_Grid_View_New
      (Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
       return Gtk_Grid_View
   is
      Self : constant Gtk_Grid_View := new Gtk_Grid_View_Record;
   begin
      Gtk.Grid_View.Initialize (Self, Model, Factory);
      return Self;
   end Gtk_Grid_View_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self    : out Gtk_Grid_View;
       Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
   begin
      Self := new Gtk_Grid_View_Record;
      Gtk.Grid_View.Initialize (Self, Model, Factory);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self    : not null access Gtk_Grid_View_Record'Class;
       Model   : Gtk.Selection_Model.Gtk_Selection_Model;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      function Internal
         (Model   : Gtk.Selection_Model.Gtk_Selection_Model;
          Factory : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_grid_view_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Model, Get_Object_Or_Null (GObject (Factory))));
      end if;
   end Initialize;

   ---------------------------
   -- Get_Enable_Rubberband --
   ---------------------------

   function Get_Enable_Rubberband
      (Self : not null access Gtk_Grid_View_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_grid_view_get_enable_rubberband");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Rubberband;

   -----------------
   -- Get_Factory --
   -----------------

   function Get_Factory
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.List_Item_Factory.Gtk_List_Item_Factory
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_grid_view_get_factory");
      Stub_Gtk_List_Item_Factory : Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record;
   begin
      return Gtk.List_Item_Factory.Gtk_List_Item_Factory (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_List_Item_Factory));
   end Get_Factory;

   ---------------------
   -- Get_Max_Columns --
   ---------------------

   function Get_Max_Columns
      (Self : not null access Gtk_Grid_View_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_grid_view_get_max_columns");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Columns;

   ---------------------
   -- Get_Min_Columns --
   ---------------------

   function Get_Min_Columns
      (Self : not null access Gtk_Grid_View_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_grid_view_get_min_columns");
   begin
      return Internal (Get_Object (Self));
   end Get_Min_Columns;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model
   is
      function Internal
         (Self : System.Address)
          return Gtk.Selection_Model.Gtk_Selection_Model;
      pragma Import (C, Internal, "gtk_grid_view_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   -------------------------------
   -- Get_Single_Click_Activate --
   -------------------------------

   function Get_Single_Click_Activate
      (Self : not null access Gtk_Grid_View_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_grid_view_get_single_click_activate");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Single_Click_Activate;

   ----------------------
   -- Get_Tab_Behavior --
   ----------------------

   function Get_Tab_Behavior
      (Self : not null access Gtk_Grid_View_Record)
       return Gtk.Enums.Gtk_List_Tab_Behavior
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_List_Tab_Behavior;
      pragma Import (C, Internal, "gtk_grid_view_get_tab_behavior");
   begin
      return Internal (Get_Object (Self));
   end Get_Tab_Behavior;

   ---------------
   -- Scroll_To --
   ---------------

   procedure Scroll_To
      (Self   : not null access Gtk_Grid_View_Record;
       Pos    : Guint;
       Flags  : Gtk.Enums.Gtk_List_Scroll_Flags;
       Scroll : Gtk.Scroll_Info.Gtk_Scroll_Info)
   is
      procedure Internal
         (Self   : System.Address;
          Pos    : Guint;
          Flags  : Gtk.Enums.Gtk_List_Scroll_Flags;
          Scroll : System.Address);
      pragma Import (C, Internal, "gtk_grid_view_scroll_to");
   begin
      Internal (Get_Object (Self), Pos, Flags, Get_Object (Scroll));
   end Scroll_To;

   ---------------------------
   -- Set_Enable_Rubberband --
   ---------------------------

   procedure Set_Enable_Rubberband
      (Self              : not null access Gtk_Grid_View_Record;
       Enable_Rubberband : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Enable_Rubberband : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_grid_view_set_enable_rubberband");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enable_Rubberband));
   end Set_Enable_Rubberband;

   -----------------
   -- Set_Factory --
   -----------------

   procedure Set_Factory
      (Self    : not null access Gtk_Grid_View_Record;
       Factory : access Gtk.List_Item_Factory.Gtk_List_Item_Factory_Record'Class)
   is
      procedure Internal (Self : System.Address; Factory : System.Address);
      pragma Import (C, Internal, "gtk_grid_view_set_factory");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Factory)));
   end Set_Factory;

   ---------------------
   -- Set_Max_Columns --
   ---------------------

   procedure Set_Max_Columns
      (Self        : not null access Gtk_Grid_View_Record;
       Max_Columns : Guint)
   is
      procedure Internal (Self : System.Address; Max_Columns : Guint);
      pragma Import (C, Internal, "gtk_grid_view_set_max_columns");
   begin
      Internal (Get_Object (Self), Max_Columns);
   end Set_Max_Columns;

   ---------------------
   -- Set_Min_Columns --
   ---------------------

   procedure Set_Min_Columns
      (Self        : not null access Gtk_Grid_View_Record;
       Min_Columns : Guint)
   is
      procedure Internal (Self : System.Address; Min_Columns : Guint);
      pragma Import (C, Internal, "gtk_grid_view_set_min_columns");
   begin
      Internal (Get_Object (Self), Min_Columns);
   end Set_Min_Columns;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Grid_View_Record;
       Model : Gtk.Selection_Model.Gtk_Selection_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Gtk.Selection_Model.Gtk_Selection_Model);
      pragma Import (C, Internal, "gtk_grid_view_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   -------------------------------
   -- Set_Single_Click_Activate --
   -------------------------------

   procedure Set_Single_Click_Activate
      (Self                  : not null access Gtk_Grid_View_Record;
       Single_Click_Activate : Boolean)
   is
      procedure Internal
         (Self                  : System.Address;
          Single_Click_Activate : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_grid_view_set_single_click_activate");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Single_Click_Activate));
   end Set_Single_Click_Activate;

   ----------------------
   -- Set_Tab_Behavior --
   ----------------------

   procedure Set_Tab_Behavior
      (Self         : not null access Gtk_Grid_View_Record;
       Tab_Behavior : Gtk.Enums.Gtk_List_Tab_Behavior)
   is
      procedure Internal
         (Self         : System.Address;
          Tab_Behavior : Gtk.Enums.Gtk_List_Tab_Behavior);
      pragma Import (C, Internal, "gtk_grid_view_set_tab_behavior");
   begin
      Internal (Get_Object (Self), Tab_Behavior);
   end Set_Tab_Behavior;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Grid_View_Record;
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
      (Self : not null access Gtk_Grid_View_Record) return UTF8_String
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
      (Self : not null access Gtk_Grid_View_Record)
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
      (Self : not null access Gtk_Grid_View_Record)
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
      (Self : not null access Gtk_Grid_View_Record)
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
      (Self   : not null access Gtk_Grid_View_Record;
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
      (Self : not null access Gtk_Grid_View_Record)
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
      (Self : not null access Gtk_Grid_View_Record)
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
      (Self : not null access Gtk_Grid_View_Record)
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
      (Self  : not null access Gtk_Grid_View_Record;
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
      (Self     : not null access Gtk_Grid_View_Record;
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
      (Self     : not null access Gtk_Grid_View_Record;
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
      (Self  : not null access Gtk_Grid_View_Record;
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
      (Self         : not null access Gtk_Grid_View_Record;
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
      (Self        : not null access Gtk_Grid_View_Record;
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
      (Self        : not null access Gtk_Grid_View_Record;
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
      (Self  : not null access Gtk_Grid_View_Record;
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
     (Cb_Gtk_Grid_View_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Grid_View_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Void);

   procedure Connect
      (Object  : access Gtk_Grid_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Grid_View_Guint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Grid_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Void);

   procedure Marsh_Gtk_Grid_View_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Grid_View_Guint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Grid_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Grid_View_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Grid_View_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Grid_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------
   -- Marsh_GObject_Guint_Void --
   ------------------------------

   procedure Marsh_GObject_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Void;

   ------------------------------------
   -- Marsh_Gtk_Grid_View_Guint_Void --
   ------------------------------------

   procedure Marsh_Gtk_Grid_View_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Grid_View_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Grid_View := Gtk_Grid_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Grid_View_Guint_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Grid_View_Record;
       Call  : Cb_Gtk_Grid_View_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Grid_View_Record;
       Call  : Cb_GObject_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.Grid_View;
