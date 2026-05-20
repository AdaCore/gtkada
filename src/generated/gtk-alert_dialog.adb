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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Alert_Dialog is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self    : out Gtk_Alert_Dialog;
      Message : UTF8_String := "") is
   begin
      Self := new Gtk_Alert_Dialog_Record;
      Initialize (Self, Message);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : not null access Gtk_Alert_Dialog_Record'Class;
      Message : UTF8_String := "") is
   begin
      if not Self.Is_Created then
         Glib.Object.G_New (Self, Get_Type);
         if Message /= "" then
            Set_Message (Self, Message);
         end if;
      end if;
   end Initialize;

   --------------------------
   -- Gtk_Alert_Dialog_New --
   --------------------------

   function Gtk_Alert_Dialog_New
     (Message : UTF8_String := "") return Gtk_Alert_Dialog
   is
      Self : Gtk_Alert_Dialog;
   begin
      Gtk_New (Self, Message);
      return Self;
   end Gtk_Alert_Dialog_New;

   procedure C_Gtk_Alert_Dialog_Choose
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_Alert_Dialog_Choose, "gtk_alert_dialog_choose");
   --  Shows the alert to the user.
   --  It is ok to pass `NULL` for the callback if the alert does not have
   --  more than one button. A simpler API for this case is
   --  [methodGtk.AlertDialog.show].
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param User_Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   package Type_Conversion_Gtk_Alert_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Alert_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Alert_Dialog);

   ------------
   -- Choose --
   ------------

   procedure Choose
      (Self        : not null access Gtk_Alert_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Alert_Dialog_Choose (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Alert_Dialog_Choose (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Choose;

   -------------------
   -- Choose_Finish --
   -------------------

   function Choose_Finish
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Result : Glib.G_Async_Result) return Glib.Gint
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gint;
      pragma Import (C, Internal, "gtk_alert_dialog_choose_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Choose_Finish;

   -----------------
   -- Get_Buttons --
   -----------------

   function Get_Buttons
      (Self : not null access Gtk_Alert_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_alert_dialog_get_buttons");
   begin
      return To_String_List (Internal (Get_Object (Self)).all);
   end Get_Buttons;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
      (Self : not null access Gtk_Alert_Dialog_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_alert_dialog_get_cancel_button");
   begin
      return Internal (Get_Object (Self));
   end Get_Cancel_Button;

   ------------------------
   -- Get_Default_Button --
   ------------------------

   function Get_Default_Button
      (Self : not null access Gtk_Alert_Dialog_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_alert_dialog_get_default_button");
   begin
      return Internal (Get_Object (Self));
   end Get_Default_Button;

   ----------------
   -- Get_Detail --
   ----------------

   function Get_Detail
      (Self : not null access Gtk_Alert_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_alert_dialog_get_detail");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Detail;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
      (Self : not null access Gtk_Alert_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_alert_dialog_get_message");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Message;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_Alert_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_alert_dialog_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   -----------------
   -- Set_Buttons --
   -----------------

   procedure Set_Buttons
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Labels : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self   : System.Address;
          Labels : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_alert_dialog_set_buttons");
      Tmp_Labels : Gtkada.Types.chars_ptr_array := From_String_List (Labels);
   begin
      Internal (Get_Object (Self), Tmp_Labels);
      Gtkada.Types.Free (Tmp_Labels);
   end Set_Buttons;

   -----------------------
   -- Set_Cancel_Button --
   -----------------------

   procedure Set_Cancel_Button
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Button : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Button : Glib.Gint);
      pragma Import (C, Internal, "gtk_alert_dialog_set_cancel_button");
   begin
      Internal (Get_Object (Self), Button);
   end Set_Cancel_Button;

   ------------------------
   -- Set_Default_Button --
   ------------------------

   procedure Set_Default_Button
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Button : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Button : Glib.Gint);
      pragma Import (C, Internal, "gtk_alert_dialog_set_default_button");
   begin
      Internal (Get_Object (Self), Button);
   end Set_Default_Button;

   ----------------
   -- Set_Detail --
   ----------------

   procedure Set_Detail
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Detail : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          Detail : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_alert_dialog_set_detail");
      Tmp_Detail : Gtkada.Types.Chars_Ptr := New_String (Detail);
   begin
      Internal (Get_Object (Self), Tmp_Detail);
      Free (Tmp_Detail);
   end Set_Detail;

   -----------------
   -- Set_Message --
   -----------------

   procedure Set_Message
      (Self    : not null access Gtk_Alert_Dialog_Record;
       Message : UTF8_String)
   is
      procedure Internal
         (Self    : System.Address;
          Message : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_alert_dialog_set_message");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message);
      Free (Tmp_Message);
   end Set_Message;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_Alert_Dialog_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_alert_dialog_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   ----------
   -- Show --
   ----------

   procedure Show
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
   is
      procedure Internal (Self : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_alert_dialog_show");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)));
   end Show;

end Gtk.Alert_Dialog;
