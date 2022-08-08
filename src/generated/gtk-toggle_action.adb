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

package body Gtk.Toggle_Action is

   package Type_Conversion_Gtk_Toggle_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toggle_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Toggle_Action);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Action   : out Gtk_Toggle_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "")
   is
   begin
      Action := new Gtk_Toggle_Action_Record;
      Gtk.Toggle_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id);
   end Gtk_New;

   ---------------------------
   -- Gtk_Toggle_Action_New --
   ---------------------------

   function Gtk_Toggle_Action_New
      (Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "") return Gtk_Toggle_Action
   is
      Action : constant Gtk_Toggle_Action := new Gtk_Toggle_Action_Record;
   begin
      Gtk.Toggle_Action.Initialize (Action, Name, Label, Tooltip, Stock_Id);
      return Action;
   end Gtk_Toggle_Action_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Action   : not null access Gtk_Toggle_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "")
   is
      function Internal
         (Name     : Gtkada.Types.Chars_Ptr;
          Label    : Gtkada.Types.Chars_Ptr;
          Tooltip  : Gtkada.Types.Chars_Ptr;
          Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_action_new");
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
         Tmp_Return := Internal (Tmp_Name, Tmp_Label, Tmp_Tooltip, Tmp_Stock_Id);
         Free (Tmp_Stock_Id);
         Free (Tmp_Tooltip);
         Free (Tmp_Label);
         Free (Tmp_Name);
         Set_Object (Action, Tmp_Return);
      end if;
   end Initialize;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Action : not null access Gtk_Toggle_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_toggle_action_get_active");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Active;

   -----------------------
   -- Get_Draw_As_Radio --
   -----------------------

   function Get_Draw_As_Radio
      (Action : not null access Gtk_Toggle_Action_Record) return Boolean
   is
      function Internal (Action : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_toggle_action_get_draw_as_radio");
   begin
      return Internal (Get_Object (Action)) /= 0;
   end Get_Draw_As_Radio;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Action    : not null access Gtk_Toggle_Action_Record;
       Is_Active : Boolean)
   is
      procedure Internal
         (Action    : System.Address;
          Is_Active : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_toggle_action_set_active");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Is_Active));
   end Set_Active;

   -----------------------
   -- Set_Draw_As_Radio --
   -----------------------

   procedure Set_Draw_As_Radio
      (Action        : not null access Gtk_Toggle_Action_Record;
       Draw_As_Radio : Boolean)
   is
      procedure Internal
         (Action        : System.Address;
          Draw_As_Radio : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_toggle_action_set_draw_as_radio");
   begin
      Internal (Get_Object (Action), Boolean'Pos (Draw_As_Radio));
   end Set_Draw_As_Radio;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Action : not null access Gtk_Toggle_Action_Record) is
      procedure Internal (Action : System.Address);
      pragma Import (C, Internal, "gtk_toggle_action_toggled");
   begin
      Internal (Get_Object (Action));
   end Toggled;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Toggle_Action_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Toggle_Action_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Toggle_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toggle_Action_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Toggle_Action_Record'Class;
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

   procedure Marsh_Gtk_Toggle_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Toggle_Action_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Toggle_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toggle_Action_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Toggle_Action_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Toggle_Action_Record'Class;
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

   ----------------------------------
   -- Marsh_Gtk_Toggle_Action_Void --
   ----------------------------------

   procedure Marsh_Gtk_Toggle_Action_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Toggle_Action_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Toggle_Action := Gtk_Toggle_Action (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Toggle_Action_Void;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled
      (Self  : not null access Gtk_Toggle_Action_Record;
       Call  : Cb_Gtk_Toggle_Action_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggled" & ASCII.NUL, Call, After);
   end On_Toggled;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled
      (Self  : not null access Gtk_Toggle_Action_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggled" & ASCII.NUL, Call, After, Slot);
   end On_Toggled;

end Gtk.Toggle_Action;
