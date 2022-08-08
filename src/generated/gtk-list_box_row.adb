------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.List_Box_Row is

   function Convert (R : Gtk.List_Box_Row.Gtk_List_Box_Row) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.List_Box_Row.Gtk_List_Box_Row is
      Stub : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;begin
         return Gtk.List_Box_Row.Gtk_List_Box_Row (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   package Type_Conversion_Gtk_List_Box_Row is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_List_Box_Row_Record);
   pragma Unreferenced (Type_Conversion_Gtk_List_Box_Row);

   --------------------------
   -- Gtk_List_Box_Row_New --
   --------------------------

   function Gtk_List_Box_Row_New return Gtk_List_Box_Row is
      Self : constant Gtk_List_Box_Row := new Gtk_List_Box_Row_Record;
   begin
      Gtk.List_Box_Row.Initialize (Self);
      return Self;
   end Gtk_List_Box_Row_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_List_Box_Row) is
   begin
      Self := new Gtk_List_Box_Row_Record;
      Gtk.List_Box_Row.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_List_Box_Row_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_list_box_row_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------
   -- Changed --
   -------------

   procedure Changed (Self : not null access Gtk_List_Box_Row_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_row_changed");
   begin
      Internal (Get_Object (Self));
   end Changed;

   ---------------------
   -- Get_Activatable --
   ---------------------

   function Get_Activatable
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_box_row_get_activatable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activatable;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header
      (Self : not null access Gtk_List_Box_Row_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_box_row_get_header");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Header;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
      (Self : not null access Gtk_List_Box_Row_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_list_box_row_get_index");
   begin
      return Internal (Get_Object (Self));
   end Get_Index;

   --------------------
   -- Get_Selectable --
   --------------------

   function Get_Selectable
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_box_row_get_selectable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Selectable;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
      (Self : not null access Gtk_List_Box_Row_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_box_row_is_selected");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Selected;

   ---------------------
   -- Set_Activatable --
   ---------------------

   procedure Set_Activatable
      (Self        : not null access Gtk_List_Box_Row_Record;
       Activatable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Activatable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_list_box_row_set_activatable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Activatable));
   end Set_Activatable;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header
      (Self   : not null access Gtk_List_Box_Row_Record;
       Header : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Header : System.Address);
      pragma Import (C, Internal, "gtk_list_box_row_set_header");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Header)));
   end Set_Header;

   --------------------
   -- Set_Selectable --
   --------------------

   procedure Set_Selectable
      (Self       : not null access Gtk_List_Box_Row_Record;
       Selectable : Boolean)
   is
      procedure Internal (Self : System.Address; Selectable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_list_box_row_set_selectable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Selectable));
   end Set_Selectable;

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_List_Box_Row_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_actionable_get_action_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Action_Name;

   -----------------------------
   -- Get_Action_Target_Value --
   -----------------------------

   function Get_Action_Target_Value
      (Self : not null access Gtk_List_Box_Row_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_actionable_get_action_target_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Action_Target_Value;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : not null access Gtk_List_Box_Row_Record;
       Action_Name : UTF8_String := "")
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_action_name");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Name = "" then
         Tmp_Action_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Name := New_String (Action_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Set_Action_Name;

   -----------------------------
   -- Set_Action_Target_Value --
   -----------------------------

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_List_Box_Row_Record;
       Target_Value : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : System.Address;
          Target_Value : System.Address);
      pragma Import (C, Internal, "gtk_actionable_set_action_target_value");
   begin
      Internal (Get_Object (Self), Get_Object (Target_Value));
   end Set_Action_Target_Value;

   ------------------------------
   -- Set_Detailed_Action_Name --
   ------------------------------

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_List_Box_Row_Record;
       Detailed_Action_Name : UTF8_String)
   is
      procedure Internal
         (Self                 : System.Address;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_detailed_action_name");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action_Name);
      Free (Tmp_Detailed_Action_Name);
   end Set_Detailed_Action_Name;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_List_Box_Row_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_List_Box_Row_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_List_Box_Row_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Row_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Row_Record'Class;
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

   procedure Marsh_Gtk_List_Box_Row_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_List_Box_Row_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_List_Box_Row_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Row_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_List_Box_Row_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Row_Record'Class;
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
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------------
   -- Marsh_Gtk_List_Box_Row_Void --
   ---------------------------------

   procedure Marsh_Gtk_List_Box_Row_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_List_Box_Row_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_List_Box_Row := Gtk_List_Box_Row (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_List_Box_Row_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_List_Box_Row_Record;
       Call  : Cb_Gtk_List_Box_Row_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_List_Box_Row_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

end Gtk.List_Box_Row;
