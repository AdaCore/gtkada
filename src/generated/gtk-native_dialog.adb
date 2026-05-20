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

package body Gtk.Native_Dialog is

   package Type_Conversion_Gtk_Native_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Native_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Native_Dialog);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Gtk_Native_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_dialog_destroy");
   begin
      Internal (Get_Object (Self));
   end Destroy;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_Native_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_native_dialog_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Native_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_native_dialog_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   -----------------------
   -- Get_Transient_For --
   -----------------------

   function Get_Transient_For
      (Self : not null access Gtk_Native_Dialog_Record)
       return Gtk.Window.Gtk_Window
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_dialog_get_transient_for");
      Stub_Gtk_Window : Gtk.Window.Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Window));
   end Get_Transient_For;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Self : not null access Gtk_Native_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_native_dialog_get_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Visible;

   ----------
   -- Hide --
   ----------

   procedure Hide (Self : not null access Gtk_Native_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_dialog_hide");
   begin
      Internal (Get_Object (Self));
   end Hide;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_Native_Dialog_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_native_dialog_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Native_Dialog_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_native_dialog_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   -----------------------
   -- Set_Transient_For --
   -----------------------

   procedure Set_Transient_For
      (Self   : not null access Gtk_Native_Dialog_Record;
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
      procedure Internal (Self : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_native_dialog_set_transient_for");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)));
   end Set_Transient_For;

   ----------
   -- Show --
   ----------

   procedure Show (Self : not null access Gtk_Native_Dialog_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_dialog_show");
   begin
      Internal (Get_Object (Self));
   end Show;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Native_Dialog_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Native_Dialog_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Void);

   procedure Connect
      (Object  : access Gtk_Native_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Native_Dialog_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Native_Dialog_Record'Class;
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

   procedure Marsh_Gtk_Native_Dialog_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Native_Dialog_Gint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Native_Dialog_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Native_Dialog_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Native_Dialog_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Native_Dialog_Record'Class;
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

   ---------------------------------------
   -- Marsh_Gtk_Native_Dialog_Gint_Void --
   ---------------------------------------

   procedure Marsh_Gtk_Native_Dialog_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Native_Dialog_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Native_Dialog := Gtk_Native_Dialog (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Native_Dialog_Gint_Void;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Native_Dialog_Record;
       Call  : Cb_Gtk_Native_Dialog_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "response" & ASCII.NUL, Call, After);
   end On_Response;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
      (Self  : not null access Gtk_Native_Dialog_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "response" & ASCII.NUL, Call, After, Slot);
   end On_Response;

end Gtk.Native_Dialog;
