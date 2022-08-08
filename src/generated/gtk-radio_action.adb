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

package body Gtk.Radio_Action is

   package Type_Conversion_Gtk_Radio_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Radio_Action);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Action   : out Gtk_Radio_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Glib.Gint)
   is
   begin
      Action := new Gtk_Radio_Action_Record;
      Gtk.Radio_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id, Value);
   end Gtk_New;

   --------------------------
   -- Gtk_Radio_Action_New --
   --------------------------

   function Gtk_Radio_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Glib.Gint) return Gtk_Radio_Action
   is
      Action : constant Gtk_Radio_Action := new Gtk_Radio_Action_Record;
   begin
      Gtk.Radio_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id, Value);
      return Action;
   end Gtk_Radio_Action_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Action   : not null access Gtk_Radio_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Glib.Gint)
   is
      function Internal
         (Name     : Gtkada.Types.Chars_Ptr;
          Label    : Gtkada.Types.Chars_Ptr;
          Tooltip  : Gtkada.Types.Chars_Ptr;
          Stock_Id : Gtkada.Types.Chars_Ptr;
          Value    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_new");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Label    : Gtkada.Types.Chars_Ptr;
      Tmp_Tooltip  : Gtkada.Types.Chars_Ptr;
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr;
      Tmp_Return   : System.Address;
   begin
      if not Action.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         if Tooltip = "" then
            Tmp_Tooltip := Gtkada.Types.Null_Ptr;
         else
            Tmp_Tooltip := New_String (Tooltip);
         end if;
         if Stock_Id = "" then
            Tmp_Stock_Id := Gtkada.Types.Null_Ptr;
         else
            Tmp_Stock_Id := New_String (Stock_Id);
         end if;
         Tmp_Return := Internal (Tmp_Name, Tmp_Label, Tmp_Tooltip, Tmp_Stock_Id, Value);
         Free (Tmp_Stock_Id);
         Free (Tmp_Tooltip);
         Free (Tmp_Label);
         Free (Tmp_Name);
         Set_Object (Action, Tmp_Return);
      end if;
   end Initialize;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
      (Action : not null access Gtk_Radio_Action_Record) return Glib.Gint
   is
      function Internal (Action : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_radio_action_get_current_value");
   begin
      return Internal (Get_Object (Action));
   end Get_Current_Value;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Action : not null access Gtk_Radio_Action_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal (Action : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_action_get_group");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Action)));
      return Tmp_Return;
   end Get_Group;

   ----------------
   -- Join_Group --
   ----------------

   procedure Join_Group
      (Action       : not null access Gtk_Radio_Action_Record;
       Group_Source : access Gtk_Radio_Action_Record'Class)
   is
      procedure Internal
         (Action       : System.Address;
          Group_Source : System.Address);
      pragma Import (C, Internal, "gtk_radio_action_join_group");
   begin
      Internal (Get_Object (Action), Get_Object_Or_Null (GObject (Group_Source)));
   end Join_Group;

   -----------------------
   -- Set_Current_Value --
   -----------------------

   procedure Set_Current_Value
      (Action        : not null access Gtk_Radio_Action_Record;
       Current_Value : Glib.Gint)
   is
      procedure Internal
         (Action        : System.Address;
          Current_Value : Glib.Gint);
      pragma Import (C, Internal, "gtk_radio_action_set_current_value");
   begin
      Internal (Get_Object (Action), Current_Value);
   end Set_Current_Value;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Action : not null access Gtk_Radio_Action_Record;
       Group  : Gtk.Widget.Widget_SList.GSlist)
   is
      procedure Internal (Action : System.Address; Group : System.Address);
      pragma Import (C, Internal, "gtk_radio_action_set_group");
   begin
      Internal (Get_Object (Action), Gtk.Widget.Widget_SList.Get_Object (Group));
   end Set_Group;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Radio_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Radio_Action_Void);

   procedure Connect
      (Object  : access Gtk_Radio_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Radio_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Radio_Action_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Radio_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Radio_Action_Void);

   procedure Marsh_Gtk_Radio_Action_Gtk_Radio_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Radio_Action_Gtk_Radio_Action_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Radio_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Radio_Action_Gtk_Radio_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Radio_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Radio_Action_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Radio_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------------
   -- Marsh_GObject_Gtk_Radio_Action_Void --
   -----------------------------------------

   procedure Marsh_GObject_Gtk_Radio_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Radio_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Radio_Action.Gtk_Radio_Action (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Radio_Action_Void;

   --------------------------------------------------
   -- Marsh_Gtk_Radio_Action_Gtk_Radio_Action_Void --
   --------------------------------------------------

   procedure Marsh_Gtk_Radio_Action_Gtk_Radio_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Radio_Action := Gtk_Radio_Action (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Radio_Action.Gtk_Radio_Action (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Radio_Action_Gtk_Radio_Action_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Radio_Action_Record;
       Call  : Cb_Gtk_Radio_Action_Gtk_Radio_Action_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Radio_Action_Record;
       Call  : Cb_GObject_Gtk_Radio_Action_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Gtk.Radio_Action;
