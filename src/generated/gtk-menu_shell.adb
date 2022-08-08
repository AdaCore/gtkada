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

package body Gtk.Menu_Shell is

   package Type_Conversion_Gtk_Menu_Shell is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Shell_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Shell);

   -------------------
   -- Activate_Item --
   -------------------

   procedure Activate_Item
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Menu_Item        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Force_Deactivate : Boolean)
   is
      procedure Internal
         (Menu_Shell       : System.Address;
          Menu_Item        : System.Address;
          Force_Deactivate : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_shell_activate_item");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Menu_Item), Boolean'Pos (Force_Deactivate));
   end Activate_Item;

   ------------
   -- Append --
   ------------

   procedure Append
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_append");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Append;

   ----------------
   -- Bind_Model --
   ----------------

   procedure Bind_Model
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Model            : access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Action_Namespace : UTF8_String := "";
       With_Separators  : Boolean)
   is
      procedure Internal
         (Menu_Shell       : System.Address;
          Model            : System.Address;
          Action_Namespace : Gtkada.Types.Chars_Ptr;
          With_Separators  : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_shell_bind_model");
      Tmp_Action_Namespace : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Namespace = "" then
         Tmp_Action_Namespace := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Namespace := New_String (Action_Namespace);
      end if;
      Internal (Get_Object (Menu_Shell), Get_Object_Or_Null (GObject (Model)), Tmp_Action_Namespace, Boolean'Pos (With_Separators));
      Free (Tmp_Action_Namespace);
   end Bind_Model;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Menu_Shell : not null access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_cancel");
   begin
      Internal (Get_Object (Menu_Shell));
   end Cancel;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Menu_Shell : not null access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deactivate");
   begin
      Internal (Get_Object (Menu_Shell));
   end Deactivate;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Menu_Shell : not null access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deselect");
   begin
      Internal (Get_Object (Menu_Shell));
   end Deselect;

   ----------------------
   -- Get_Parent_Shell --
   ----------------------

   function Get_Parent_Shell
      (Menu_Shell : not null access Gtk_Menu_Shell_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Shell : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_shell_get_parent_shell");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu_Shell)), Stub_Gtk_Widget));
   end Get_Parent_Shell;

   -----------------------
   -- Get_Selected_Item --
   -----------------------

   function Get_Selected_Item
      (Menu_Shell : not null access Gtk_Menu_Shell_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Shell : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_shell_get_selected_item");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu_Shell)), Stub_Gtk_Widget));
   end Get_Selected_Item;

   --------------------
   -- Get_Take_Focus --
   --------------------

   function Get_Take_Focus
      (Menu_Shell : not null access Gtk_Menu_Shell_Record) return Boolean
   is
      function Internal (Menu_Shell : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_shell_get_take_focus");
   begin
      return Internal (Get_Object (Menu_Shell)) /= 0;
   end Get_Take_Focus;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position   : Glib.Gint)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Child      : System.Address;
          Position   : Glib.Gint);
      pragma Import (C, Internal, "gtk_menu_shell_insert");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child), Position);
   end Insert;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_prepend");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Prepend;

   ------------------
   -- Select_First --
   ------------------

   procedure Select_First
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Search_Sensitive : Boolean)
   is
      procedure Internal
         (Menu_Shell       : System.Address;
          Search_Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_shell_select_first");
   begin
      Internal (Get_Object (Menu_Shell), Boolean'Pos (Search_Sensitive));
   end Select_First;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Menu_Item  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Menu_Item  : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_select_item");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Menu_Item));
   end Select_Item;

   --------------------
   -- Set_Take_Focus --
   --------------------

   procedure Set_Take_Focus
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Take_Focus : Boolean)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Take_Focus : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_shell_set_take_focus");
   begin
      Internal (Get_Object (Menu_Shell), Boolean'Pos (Take_Focus));
   end Set_Take_Focus;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Shell_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Shell_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Shell_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Shell_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Menu_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Menu_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Shell_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Shell_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gint_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Menu_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Void);

   procedure Marsh_GObject_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Boolean);

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Void);

   procedure Marsh_GObject_Gtk_Menu_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Menu_Direction_Type_Void);

   procedure Marsh_GObject_Gtk_Widget_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Gint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Menu_Shell_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Shell_Boolean_Void);

   procedure Marsh_Gtk_Menu_Shell_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Shell_Gint_Boolean);

   procedure Marsh_Gtk_Menu_Shell_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Shell_Gtk_Direction_Type_Void);

   procedure Marsh_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void);

   procedure Marsh_Gtk_Menu_Shell_Gtk_Widget_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Shell_Gtk_Widget_Gint_Void);

   procedure Marsh_Gtk_Menu_Shell_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Shell_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Shell_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Shell_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Shell_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Shell_Gtk_Widget_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Shell_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Shell_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
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

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Menu_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Menu_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Shell_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------
   -- Marsh_GObject_Boolean_Void --
   --------------------------------

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Void;

   --------------------------------
   -- Marsh_GObject_Gint_Boolean --
   --------------------------------

   procedure Marsh_GObject_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Boolean;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Direction_Type_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Direction_Type_Void;

   ------------------------------------------------
   -- Marsh_GObject_Gtk_Menu_Direction_Type_Void --
   ------------------------------------------------

   procedure Marsh_GObject_Gtk_Menu_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Menu_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Menu_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Menu_Direction_Type_Void;

   ----------------------------------------
   -- Marsh_GObject_Gtk_Widget_Gint_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Gint_Void;

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

   ---------------------------------------
   -- Marsh_Gtk_Menu_Shell_Boolean_Void --
   ---------------------------------------

   procedure Marsh_Gtk_Menu_Shell_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Shell_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Shell := Gtk_Menu_Shell (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Shell_Boolean_Void;

   ---------------------------------------
   -- Marsh_Gtk_Menu_Shell_Gint_Boolean --
   ---------------------------------------

   procedure Marsh_Gtk_Menu_Shell_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Shell_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Shell := Gtk_Menu_Shell (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Shell_Gint_Boolean;

   --------------------------------------------------
   -- Marsh_Gtk_Menu_Shell_Gtk_Direction_Type_Void --
   --------------------------------------------------

   procedure Marsh_Gtk_Menu_Shell_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Shell := Gtk_Menu_Shell (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Shell_Gtk_Direction_Type_Void;

   -------------------------------------------------------
   -- Marsh_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void --
   -------------------------------------------------------

   procedure Marsh_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Shell := Gtk_Menu_Shell (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Menu_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void;

   -----------------------------------------------
   -- Marsh_Gtk_Menu_Shell_Gtk_Widget_Gint_Void --
   -----------------------------------------------

   procedure Marsh_Gtk_Menu_Shell_Gtk_Widget_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Shell := Gtk_Menu_Shell (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Shell_Gtk_Widget_Gint_Void;

   -------------------------------
   -- Marsh_Gtk_Menu_Shell_Void --
   -------------------------------

   procedure Marsh_Gtk_Menu_Shell_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Shell_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Shell := Gtk_Menu_Shell (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Shell_Void;

   -------------------------
   -- On_Activate_Current --
   -------------------------

   procedure On_Activate_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-current" & ASCII.NUL, Call, After);
   end On_Activate_Current;

   -------------------------
   -- On_Activate_Current --
   -------------------------

   procedure On_Activate_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-current" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Current;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel" & ASCII.NUL, Call, After);
   end On_Cancel;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancel" & ASCII.NUL, Call, After, Slot);
   end On_Cancel;

   --------------------
   -- On_Cycle_Focus --
   --------------------

   procedure On_Cycle_Focus
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gtk_Direction_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cycle-focus" & ASCII.NUL, Call, After);
   end On_Cycle_Focus;

   --------------------
   -- On_Cycle_Focus --
   --------------------

   procedure On_Cycle_Focus
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cycle-focus" & ASCII.NUL, Call, After, Slot);
   end On_Cycle_Focus;

   -------------------
   -- On_Deactivate --
   -------------------

   procedure On_Deactivate
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "deactivate" & ASCII.NUL, Call, After);
   end On_Deactivate;

   -------------------
   -- On_Deactivate --
   -------------------

   procedure On_Deactivate
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "deactivate" & ASCII.NUL, Call, After, Slot);
   end On_Deactivate;

   ---------------
   -- On_Insert --
   ---------------

   procedure On_Insert
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gtk_Widget_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert" & ASCII.NUL, Call, After);
   end On_Insert;

   ---------------
   -- On_Insert --
   ---------------

   procedure On_Insert
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gtk_Widget_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert" & ASCII.NUL, Call, After, Slot);
   end On_Insert;

   ---------------------
   -- On_Move_Current --
   ---------------------

   procedure On_Move_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gtk_Menu_Direction_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-current" & ASCII.NUL, Call, After);
   end On_Move_Current;

   ---------------------
   -- On_Move_Current --
   ---------------------

   procedure On_Move_Current
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gtk_Menu_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-current" & ASCII.NUL, Call, After, Slot);
   end On_Move_Current;

   ----------------------
   -- On_Move_Selected --
   ----------------------

   procedure On_Move_Selected
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-selected" & ASCII.NUL, Call, After);
   end On_Move_Selected;

   ----------------------
   -- On_Move_Selected --
   ----------------------

   procedure On_Move_Selected
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-selected" & ASCII.NUL, Call, After, Slot);
   end On_Move_Selected;

   -----------------------
   -- On_Selection_Done --
   -----------------------

   procedure On_Selection_Done
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_Gtk_Menu_Shell_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-done" & ASCII.NUL, Call, After);
   end On_Selection_Done;

   -----------------------
   -- On_Selection_Done --
   -----------------------

   procedure On_Selection_Done
      (Self  : not null access Gtk_Menu_Shell_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-done" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Done;

end Gtk.Menu_Shell;
