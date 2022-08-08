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

package body Gtk.Info_Bar is

   package Type_Conversion_Gtk_Info_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Info_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Info_Bar);

   ----------------------
   -- Gtk_Info_Bar_New --
   ----------------------

   function Gtk_Info_Bar_New return Gtk_Info_Bar is
      Self : constant Gtk_Info_Bar := new Gtk_Info_Bar_Record;
   begin
      Gtk.Info_Bar.Initialize (Self);
      return Self;
   end Gtk_Info_Bar_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Info_Bar) is
   begin
      Self := new Gtk_Info_Bar_Record;
      Gtk.Info_Bar.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Info_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_info_bar_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Self        : not null access Gtk_Info_Bar_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Glib.Gint)
   is
      procedure Internal
         (Self        : System.Address;
          Child       : System.Address;
          Response_Id : Glib.Gint);
      pragma Import (C, Internal, "gtk_info_bar_add_action_widget");
   begin
      Internal (Get_Object (Self), Get_Object (Child), Response_Id);
   end Add_Action_Widget;

   ----------------
   -- Add_Button --
   ----------------

   function Add_Button
      (Self        : not null access Gtk_Info_Bar_Record;
       Button_Text : UTF8_String;
       Response_Id : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self        : System.Address;
          Button_Text : Gtkada.Types.Chars_Ptr;
          Response_Id : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_info_bar_add_button");
      Tmp_Button_Text : Gtkada.Types.Chars_Ptr := New_String (Button_Text);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Button_Text, Response_Id);
      Free (Tmp_Button_Text);
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Tmp_Return, Stub_Gtk_Widget));
   end Add_Button;

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_info_bar_get_action_area");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Action_Area;

   ----------------------
   -- Get_Content_Area --
   ----------------------

   function Get_Content_Area
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_info_bar_get_content_area");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Content_Area;

   ----------------------
   -- Get_Message_Type --
   ----------------------

   function Get_Message_Type
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Message_Dialog.Gtk_Message_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Message_Dialog.Gtk_Message_Type;
      pragma Import (C, Internal, "gtk_info_bar_get_message_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Message_Type;

   ------------------
   -- Get_Revealed --
   ------------------

   function Get_Revealed
      (Self : not null access Gtk_Info_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_info_bar_get_revealed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Revealed;

   ---------------------------
   -- Get_Show_Close_Button --
   ---------------------------

   function Get_Show_Close_Button
      (Self : not null access Gtk_Info_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_info_bar_get_show_close_button");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Close_Button;

   --------------
   -- Response --
   --------------

   procedure Response
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Response_Id : Glib.Gint);
      pragma Import (C, Internal, "gtk_info_bar_response");
   begin
      Internal (Get_Object (Self), Response_Id);
   end Response;

   --------------------------
   -- Set_Default_Response --
   --------------------------

   procedure Set_Default_Response
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Response_Id : Glib.Gint);
      pragma Import (C, Internal, "gtk_info_bar_set_default_response");
   begin
      Internal (Get_Object (Self), Response_Id);
   end Set_Default_Response;

   ----------------------
   -- Set_Message_Type --
   ----------------------

   procedure Set_Message_Type
      (Self         : not null access Gtk_Info_Bar_Record;
       Message_Type : Gtk.Message_Dialog.Gtk_Message_Type)
   is
      procedure Internal
         (Self         : System.Address;
          Message_Type : Gtk.Message_Dialog.Gtk_Message_Type);
      pragma Import (C, Internal, "gtk_info_bar_set_message_type");
   begin
      Internal (Get_Object (Self), Message_Type);
   end Set_Message_Type;

   ----------------------------
   -- Set_Response_Sensitive --
   ----------------------------

   procedure Set_Response_Sensitive
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint;
       Setting     : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Response_Id : Glib.Gint;
          Setting     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_info_bar_set_response_sensitive");
   begin
      Internal (Get_Object (Self), Response_Id, Boolean'Pos (Setting));
   end Set_Response_Sensitive;

   ------------------
   -- Set_Revealed --
   ------------------

   procedure Set_Revealed
      (Self     : not null access Gtk_Info_Bar_Record;
       Revealed : Boolean)
   is
      procedure Internal (Self : System.Address; Revealed : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_info_bar_set_revealed");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Revealed));
   end Set_Revealed;

   ---------------------------
   -- Set_Show_Close_Button --
   ---------------------------

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Info_Bar_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_info_bar_set_show_close_button");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Show_Close_Button;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Info_Bar_Record)
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
      (Self        : not null access Gtk_Info_Bar_Record;
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
     (Cb_Gtk_Info_Bar_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Info_Bar_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Info_Bar_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Info_Bar_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Void);

   procedure Connect
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Info_Bar_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Info_Bar_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Info_Bar_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Info_Bar_Gint_Void);

   procedure Marsh_Gtk_Info_Bar_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Info_Bar_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Info_Bar_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Info_Bar_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Info_Bar_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Info_Bar_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Info_Bar_Record'Class;
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
      (Object  : access Gtk_Info_Bar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------
   -- Marsh_GObject_Gint_Void --
   -----------------------------

   procedure Marsh_GObject_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Void;

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
   -- Marsh_Gtk_Info_Bar_Gint_Void --
   ----------------------------------

   procedure Marsh_Gtk_Info_Bar_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Info_Bar_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Info_Bar := Gtk_Info_Bar (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Info_Bar_Gint_Void;

   -----------------------------
   -- Marsh_Gtk_Info_Bar_Void --
   -----------------------------

   procedure Marsh_Gtk_Info_Bar_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Info_Bar_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Info_Bar := Gtk_Info_Bar (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Info_Bar_Void;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_Gtk_Info_Bar_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "close" & ASCII.NUL, Call, After);
   end On_Close;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
      (Self  : not null access Gtk_Info_Bar_Record;
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
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_Gtk_Info_Bar_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "response" & ASCII.NUL, Call, After);
   end On_Response;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "response" & ASCII.NUL, Call, After, Slot);
   end On_Response;

end Gtk.Info_Bar;
