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
with Gtk.Settings;               use Gtk.Settings;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Dialog is

   procedure G_New_Dialog
     (Self  : not null access Gtk_Dialog_Record'Class;
      Flags : Gtk_Dialog_Flags;
      Typ   : Glib.GType := Gtk.Dialog.Get_Type)
   is
      function Internal
        (Typ : GType; Flags : Gtk_Dialog_Flags) return System.Address;
      pragma Import (C, Internal, "ada_g_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Typ, Flags));
      end if;
   end G_New_Dialog;

   procedure Set_Alternative_Button_Order_From_Array
     (Dialog    : access Gtk_Dialog_Record;
      New_Order : Response_Type_Array)
   is
      procedure Internal
        (Dialog    : System.Address;
         N_Params  : Gint;
         New_Order : System.Address);
      pragma Import
        (C, Internal, "gtk_dialog_set_alternative_button_order_from_array");
   begin
      Internal (Get_Object (Dialog), New_Order'Length,
         New_Order (New_Order'First)'Address);
   end Set_Alternative_Button_Order_From_Array;

   function Gtk_Alternative_Dialog_Button_Order
     (Screen : Gdk.Screen.Gdk_Screen := null) return Boolean
   is
      function Internal (Screen : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_alternative_dialog_button_order");
   begin
      return Boolean'Val (Internal (Get_Object (Screen)));
   end Gtk_Alternative_Dialog_Button_Order;

   function Use_Header_Bar_From_Settings
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class := null)
   return Gtk_Dialog_Flags
   is
      S : Gtk_Settings;
   begin
      if Widget = null then
         S := Gtk.Settings.Get_Default;
      else
         S := Get_Settings (Widget);
      end if;

      if Get_Property (S, Gtk_Dialogs_Use_Header_Property) then
         return Use_Header_Bar;
      else
         return 0;
      end if;
   end Use_Header_Bar_From_Settings;

   package Type_Conversion_Gtk_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Dialog);

   --------------------
   -- Gtk_Dialog_New --
   --------------------

   function Gtk_Dialog_New return Gtk_Dialog is
      Dialog : constant Gtk_Dialog := new Gtk_Dialog_Record;
   begin
      Gtk.Dialog.Initialize (Dialog);
      return Dialog;
   end Gtk_Dialog_New;

   --------------------
   -- Gtk_Dialog_New --
   --------------------

   function Gtk_Dialog_New
      (Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags) return Gtk_Dialog
   is
      Dialog : constant Gtk_Dialog := new Gtk_Dialog_Record;
   begin
      Gtk.Dialog.Initialize (Dialog, Title, Parent, Flags);
      return Dialog;
   end Gtk_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Dialog : out Gtk_Dialog) is
   begin
      Dialog := new Gtk_Dialog_Record;
      Gtk.Dialog.Initialize (Dialog);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Dialog : out Gtk_Dialog;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags)
   is
   begin
      Dialog := new Gtk_Dialog_Record;
      Gtk.Dialog.Initialize (Dialog, Title, Parent, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dialog : not null access Gtk_Dialog_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");
   begin
      if not Dialog.Is_Created then
         Set_Object (Dialog, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Dialog : not null access Gtk_Dialog_Record'Class;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags)
   is
      function Internal
         (Title  : Gtkada.Types.Chars_Ptr;
          Parent : System.Address;
          Flags  : Gtk_Dialog_Flags) return System.Address;
      pragma Import (C, Internal, "ada_gtk_dialog_new_with_buttons");
      Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      if not Dialog.Is_Created then
         Tmp_Return := Internal (Tmp_Title, Get_Object_Or_Null (GObject (Parent)), Flags);
         Free (Tmp_Title);
         Set_Object (Dialog, Tmp_Return);
      end if;
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Dialog      : not null access Gtk_Dialog_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Gtk_Response_Type)
   is
      procedure Internal
         (Dialog      : System.Address;
          Child       : System.Address;
          Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_add_action_widget");
   begin
      Internal (Get_Object (Dialog), Get_Object (Child), Response_Id);
   end Add_Action_Widget;

   ----------------
   -- Add_Button --
   ----------------

   function Add_Button
      (Dialog      : not null access Gtk_Dialog_Record;
       Text        : UTF8_String;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Dialog      : System.Address;
          Text        : Gtkada.Types.Chars_Ptr;
          Response_Id : Gtk_Response_Type) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_add_button");
      Tmp_Text        : Gtkada.Types.Chars_Ptr := New_String (Text);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Dialog), Tmp_Text, Response_Id);
      Free (Tmp_Text);
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Tmp_Return, Stub_Gtk_Widget));
   end Add_Button;

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area
      (Dialog : not null access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_action_area");
      Stub_Gtk_Box : Gtk.Box.Gtk_Box_Record;
   begin
      return Gtk.Box.Gtk_Box (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Box));
   end Get_Action_Area;

   ----------------------
   -- Get_Content_Area --
   ----------------------

   function Get_Content_Area
      (Dialog : not null access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_content_area");
      Stub_Gtk_Box : Gtk.Box.Gtk_Box_Record;
   begin
      return Gtk.Box.Gtk_Box (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Box));
   end Get_Content_Area;

   --------------------
   -- Get_Header_Bar --
   --------------------

   function Get_Header_Bar
      (Dialog : not null access Gtk_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_header_bar");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Widget));
   end Get_Header_Bar;

   -----------------------------
   -- Get_Response_For_Widget --
   -----------------------------

   function Get_Response_For_Widget
      (Dialog : not null access Gtk_Dialog_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Response_Type
   is
      function Internal
         (Dialog : System.Address;
          Widget : System.Address) return Gtk_Response_Type;
      pragma Import (C, Internal, "gtk_dialog_get_response_for_widget");
   begin
      return Internal (Get_Object (Dialog), Get_Object (Widget));
   end Get_Response_For_Widget;

   -----------------------------
   -- Get_Widget_For_Response --
   -----------------------------

   function Get_Widget_For_Response
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Dialog      : System.Address;
          Response_Id : Gtk_Response_Type) return System.Address;
      pragma Import (C, Internal, "gtk_dialog_get_widget_for_response");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog), Response_Id), Stub_Gtk_Widget));
   end Get_Widget_For_Response;

   --------------
   -- Response --
   --------------

   procedure Response
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type)
   is
      procedure Internal
         (Dialog      : System.Address;
          Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_response");
   begin
      Internal (Get_Object (Dialog), Response_Id);
   end Response;

   ---------
   -- Run --
   ---------

   function Run
      (Dialog : not null access Gtk_Dialog_Record) return Gtk_Response_Type
   is
      function Internal (Dialog : System.Address) return Gtk_Response_Type;
      pragma Import (C, Internal, "gtk_dialog_run");
   begin
      return Internal (Get_Object (Dialog));
   end Run;

   --------------------------
   -- Set_Default_Response --
   --------------------------

   procedure Set_Default_Response
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type)
   is
      procedure Internal
         (Dialog      : System.Address;
          Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_set_default_response");
   begin
      Internal (Get_Object (Dialog), Response_Id);
   end Set_Default_Response;

   ----------------------------
   -- Set_Response_Sensitive --
   ----------------------------

   procedure Set_Response_Sensitive
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type;
       Setting     : Boolean)
   is
      procedure Internal
         (Dialog      : System.Address;
          Response_Id : Gtk_Response_Type;
          Setting     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_dialog_set_response_sensitive");
   begin
      Internal (Get_Object (Dialog), Response_Id, Boolean'Pos (Setting));
   end Set_Response_Sensitive;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Dialog_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Dialog_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Dialog_Gtk_Response_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Dialog_Gtk_Response_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Response_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Response_Type_Void);

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Response_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Response_Type_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Dialog_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Dialog_Gtk_Response_Type_Void);

   procedure Marsh_Gtk_Dialog_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Dialog_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Dialog_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Dialog_Gtk_Response_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Dialog_Record'Class;
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
      (Object  : access Gtk_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Response_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Response_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------
   -- Marsh_GObject_Gtk_Response_Type_Void --
   ------------------------------------------

   procedure Marsh_GObject_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Response_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Response_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Response_Type_Void;

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

   ---------------------------------------------
   -- Marsh_Gtk_Dialog_Gtk_Response_Type_Void --
   ---------------------------------------------

   procedure Marsh_Gtk_Dialog_Gtk_Response_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Dialog_Gtk_Response_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Dialog := Gtk_Dialog (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Response_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Dialog_Gtk_Response_Type_Void;

   ---------------------------
   -- Marsh_Gtk_Dialog_Void --
   ---------------------------

   procedure Marsh_Gtk_Dialog_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Dialog_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Dialog := Gtk_Dialog (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Dialog_Void;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_Gtk_Dialog_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "close" & ASCII.NUL, Call, After);
   end On_Close;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "close" & ASCII.NUL, Call, After, Slot);
   end On_Close;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "response" & ASCII.NUL, Call, After);
   end On_Response;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_GObject_Gtk_Response_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "response" & ASCII.NUL, Call, After, Slot);
   end On_Response;

end Gtk.Dialog;
