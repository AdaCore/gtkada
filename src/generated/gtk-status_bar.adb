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

package body Gtk.Status_Bar is

   package Type_Conversion_Gtk_Status_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Status_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Status_Bar);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Statusbar : out Gtk_Status_Bar) is
   begin
      Statusbar := new Gtk_Status_Bar_Record;
      Gtk.Status_Bar.Initialize (Statusbar);
   end Gtk_New;

   ------------------------
   -- Gtk_Status_Bar_New --
   ------------------------

   function Gtk_Status_Bar_New return Gtk_Status_Bar is
      Statusbar : constant Gtk_Status_Bar := new Gtk_Status_Bar_Record;
   begin
      Gtk.Status_Bar.Initialize (Statusbar);
      return Statusbar;
   end Gtk_Status_Bar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Statusbar : not null access Gtk_Status_Bar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_statusbar_new");
   begin
      if not Statusbar.Is_Created then
         Set_Object (Statusbar, Internal);
      end if;
   end Initialize;

   --------------------
   -- Get_Context_Id --
   --------------------

   function Get_Context_Id
      (Statusbar           : not null access Gtk_Status_Bar_Record;
       Context_Description : UTF8_String) return Context_Id
   is
      function Internal
         (Statusbar           : System.Address;
          Context_Description : Gtkada.Types.Chars_Ptr) return Context_Id;
      pragma Import (C, Internal, "gtk_statusbar_get_context_id");
      Tmp_Context_Description : Gtkada.Types.Chars_Ptr := New_String (Context_Description);
      Tmp_Return              : Context_Id;
   begin
      Tmp_Return := Internal (Get_Object (Statusbar), Tmp_Context_Description);
      Free (Tmp_Context_Description);
      return Tmp_Return;
   end Get_Context_Id;

   ----------------------
   -- Get_Message_Area --
   ----------------------

   function Get_Message_Area
      (Statusbar : not null access Gtk_Status_Bar_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Statusbar : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_statusbar_get_message_area");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Statusbar)), Stub_Gtk_Widget));
   end Get_Message_Area;

   ---------
   -- Pop --
   ---------

   procedure Pop
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id)
   is
      procedure Internal (Statusbar : System.Address; Context : Context_Id);
      pragma Import (C, Internal, "gtk_statusbar_pop");
   begin
      Internal (Get_Object (Statusbar), Context);
   end Pop;

   ----------
   -- Push --
   ----------

   function Push
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Text      : UTF8_String) return Message_Id
   is
      function Internal
         (Statusbar : System.Address;
          Context   : Context_Id;
          Text      : Gtkada.Types.Chars_Ptr) return Message_Id;
      pragma Import (C, Internal, "gtk_statusbar_push");
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return : Message_Id;
   begin
      Tmp_Return := Internal (Get_Object (Statusbar), Context, Tmp_Text);
      Free (Tmp_Text);
      return Tmp_Return;
   end Push;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Message   : Message_Id)
   is
      procedure Internal
         (Statusbar : System.Address;
          Context   : Context_Id;
          Message   : Message_Id);
      pragma Import (C, Internal, "gtk_statusbar_remove");
   begin
      Internal (Get_Object (Statusbar), Context, Message);
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id)
   is
      procedure Internal (Statusbar : System.Address; Context : Context_Id);
      pragma Import (C, Internal, "gtk_statusbar_remove_all");
   begin
      Internal (Get_Object (Statusbar), Context);
   end Remove_All;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Status_Bar_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Status_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Context_Id_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Context_Id_UTF8_String_Void);

   procedure Connect
      (Object  : access Gtk_Status_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Context_Id_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Context_Id_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Context_Id_UTF8_String_Void);

   procedure Marsh_Gtk_Status_Bar_Context_Id_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Bar_Context_Id_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Bar_Context_Id_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Context_Id_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Context_Id_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------------------
   -- Marsh_GObject_Context_Id_UTF8_String_Void --
   -----------------------------------------------

   procedure Marsh_GObject_Context_Id_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Context_Id_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Context_Id (Params, 1), Unchecked_To_UTF8_String (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Context_Id_UTF8_String_Void;

   ------------------------------------------------------
   -- Marsh_Gtk_Status_Bar_Context_Id_UTF8_String_Void --
   ------------------------------------------------------

   procedure Marsh_Gtk_Status_Bar_Context_Id_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Bar := Gtk_Status_Bar (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Context_Id (Params, 1), Unchecked_To_UTF8_String (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Bar_Context_Id_UTF8_String_Void;

   --------------------
   -- On_Text_Popped --
   --------------------

   procedure On_Text_Popped
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "text-popped" & ASCII.NUL, Call, After);
   end On_Text_Popped;

   --------------------
   -- On_Text_Popped --
   --------------------

   procedure On_Text_Popped
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_GObject_Context_Id_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "text-popped" & ASCII.NUL, Call, After, Slot);
   end On_Text_Popped;

   --------------------
   -- On_Text_Pushed --
   --------------------

   procedure On_Text_Pushed
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "text-pushed" & ASCII.NUL, Call, After);
   end On_Text_Pushed;

   --------------------
   -- On_Text_Pushed --
   --------------------

   procedure On_Text_Pushed
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_GObject_Context_Id_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "text-pushed" & ASCII.NUL, Call, After, Slot);
   end On_Text_Pushed;

end Gtk.Status_Bar;
