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

package body Gtk.IM_Context is

   package Type_Conversion_Gtk_IM_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_IM_Context_Record);
   pragma Unreferenced (Type_Conversion_Gtk_IM_Context);

   ------------------------
   -- Delete_Surrounding --
   ------------------------

   function Delete_Surrounding
      (Self    : not null access Gtk_IM_Context_Record;
       Offset  : Glib.Gint;
       N_Chars : Glib.Gint) return Boolean
   is
      function Internal
         (Self    : System.Address;
          Offset  : Glib.Gint;
          N_Chars : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_im_context_delete_surrounding");
   begin
      return Internal (Get_Object (Self), Offset, N_Chars) /= 0;
   end Delete_Surrounding;

   ---------------------
   -- Filter_Keypress --
   ---------------------

   function Filter_Keypress
      (Self  : not null access Gtk_IM_Context_Record;
       Event : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Event : Gdk.Event.Gdk_Event_Key) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_im_context_filter_keypress");
   begin
      return Internal (Get_Object (Self), Event) /= 0;
   end Filter_Keypress;

   --------------
   -- Focus_In --
   --------------

   procedure Focus_In (Self : not null access Gtk_IM_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_im_context_focus_in");
   begin
      Internal (Get_Object (Self));
   end Focus_In;

   ---------------
   -- Focus_Out --
   ---------------

   procedure Focus_Out (Self : not null access Gtk_IM_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_im_context_focus_out");
   begin
      Internal (Get_Object (Self));
   end Focus_Out;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : not null access Gtk_IM_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_im_context_reset");
   begin
      Internal (Get_Object (Self));
   end Reset;

   -----------------------
   -- Set_Client_Window --
   -----------------------

   procedure Set_Client_Window
      (Self   : not null access Gtk_IM_Context_Record;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal (Self : System.Address; Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_im_context_set_client_window");
   begin
      Internal (Get_Object (Self), Window);
   end Set_Client_Window;

   -------------------------
   -- Set_Cursor_Location --
   -------------------------

   procedure Set_Cursor_Location
      (Self : not null access Gtk_IM_Context_Record;
       Area : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self : System.Address;
          Area : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_im_context_set_cursor_location");
   begin
      Internal (Get_Object (Self), Area);
   end Set_Cursor_Location;

   ---------------------
   -- Set_Surrounding --
   ---------------------

   procedure Set_Surrounding
      (Self         : not null access Gtk_IM_Context_Record;
       Text         : UTF8_String;
       Len          : Glib.Gint;
       Cursor_Index : Glib.Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Text         : Gtkada.Types.Chars_Ptr;
          Len          : Glib.Gint;
          Cursor_Index : Glib.Gint);
      pragma Import (C, Internal, "gtk_im_context_set_surrounding");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text, Len, Cursor_Index);
      Free (Tmp_Text);
   end Set_Surrounding;

   ---------------------
   -- Set_Use_Preedit --
   ---------------------

   procedure Set_Use_Preedit
      (Self        : not null access Gtk_IM_Context_Record;
       Use_Preedit : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Use_Preedit : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_im_context_set_use_preedit");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Preedit));
   end Set_Use_Preedit;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_IM_Context_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_IM_Context_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_IM_Context_Gint_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_IM_Context_Gint_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_IM_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_IM_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_IM_Context_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_IM_Context_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_Gint_Gint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_GObject_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Boolean);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_IM_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_IM_Context_Boolean);

   procedure Marsh_Gtk_IM_Context_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_IM_Context_Gint_Gint_Boolean);

   procedure Marsh_Gtk_IM_Context_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_IM_Context_UTF8_String_Void);

   procedure Marsh_Gtk_IM_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_IM_Context_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_IM_Context_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_Gint_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_IM_Context_Gint_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_IM_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_IM_Context_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_IM_Context_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_IM_Context_Record'Class;
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
      (Object  : access Gtk_IM_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   -------------------------------------
   -- Marsh_GObject_Gint_Gint_Boolean --
   -------------------------------------

   procedure Marsh_GObject_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Boolean;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

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
   -- Marsh_Gtk_IM_Context_Boolean --
   ----------------------------------

   procedure Marsh_Gtk_IM_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_IM_Context_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_IM_Context := Gtk_IM_Context (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_IM_Context_Boolean;

   --------------------------------------------
   -- Marsh_Gtk_IM_Context_Gint_Gint_Boolean --
   --------------------------------------------

   procedure Marsh_Gtk_IM_Context_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_IM_Context_Gint_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_IM_Context := Gtk_IM_Context (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_IM_Context_Gint_Gint_Boolean;

   -------------------------------------------
   -- Marsh_Gtk_IM_Context_UTF8_String_Void --
   -------------------------------------------

   procedure Marsh_Gtk_IM_Context_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_IM_Context_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_IM_Context := Gtk_IM_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_IM_Context_UTF8_String_Void;

   -------------------------------
   -- Marsh_Gtk_IM_Context_Void --
   -------------------------------

   procedure Marsh_Gtk_IM_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_IM_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_IM_Context := Gtk_IM_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_IM_Context_Void;

   ---------------
   -- On_Commit --
   ---------------

   procedure On_Commit
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "commit" & ASCII.NUL, Call, After);
   end On_Commit;

   ---------------
   -- On_Commit --
   ---------------

   procedure On_Commit
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "commit" & ASCII.NUL, Call, After, Slot);
   end On_Commit;

   ---------------------------
   -- On_Delete_Surrounding --
   ---------------------------

   procedure On_Delete_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Gint_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "delete-surrounding" & ASCII.NUL, Call, After);
   end On_Delete_Surrounding;

   ---------------------------
   -- On_Delete_Surrounding --
   ---------------------------

   procedure On_Delete_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Gint_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "delete-surrounding" & ASCII.NUL, Call, After, Slot);
   end On_Delete_Surrounding;

   ------------------------
   -- On_Preedit_Changed --
   ------------------------

   procedure On_Preedit_Changed
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "preedit-changed" & ASCII.NUL, Call, After);
   end On_Preedit_Changed;

   ------------------------
   -- On_Preedit_Changed --
   ------------------------

   procedure On_Preedit_Changed
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "preedit-changed" & ASCII.NUL, Call, After, Slot);
   end On_Preedit_Changed;

   --------------------
   -- On_Preedit_End --
   --------------------

   procedure On_Preedit_End
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "preedit-end" & ASCII.NUL, Call, After);
   end On_Preedit_End;

   --------------------
   -- On_Preedit_End --
   --------------------

   procedure On_Preedit_End
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "preedit-end" & ASCII.NUL, Call, After, Slot);
   end On_Preedit_End;

   ----------------------
   -- On_Preedit_Start --
   ----------------------

   procedure On_Preedit_Start
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "preedit-start" & ASCII.NUL, Call, After);
   end On_Preedit_Start;

   ----------------------
   -- On_Preedit_Start --
   ----------------------

   procedure On_Preedit_Start
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "preedit-start" & ASCII.NUL, Call, After, Slot);
   end On_Preedit_Start;

   -----------------------------
   -- On_Retrieve_Surrounding --
   -----------------------------

   procedure On_Retrieve_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "retrieve-surrounding" & ASCII.NUL, Call, After);
   end On_Retrieve_Surrounding;

   -----------------------------
   -- On_Retrieve_Surrounding --
   -----------------------------

   procedure On_Retrieve_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "retrieve-surrounding" & ASCII.NUL, Call, After, Slot);
   end On_Retrieve_Surrounding;

end Gtk.IM_Context;
