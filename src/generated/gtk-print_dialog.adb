------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Gtk.Print_Dialog is

   procedure C_Gtk_Print_Dialog_Print
      (Self        : System.Address;
       Parent      : System.Address;
       Setup       : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_Print_Dialog_Print, "gtk_print_dialog_print");
   --  This function prints content from a stream.
   --  If you pass `NULL` as Setup, then this method will present a print
   --  dialog. Otherwise, it will attempt to print directly, without user
   --  interaction.
   --  The Callback will be called when the printing is done.
   --  Since: gtk+ 4.14
   --  @param Parent the parent `GtkWindow`
   --  @param Setup the `GtkPrintSetup` to use
   --  @param Cancellable a `GCancellable` to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_Print_Dialog_Print_File
      (Self        : System.Address;
       Parent      : System.Address;
       Setup       : System.Address;
       File        : Glib.GFile.Gfile;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_Print_Dialog_Print_File, "gtk_print_dialog_print_file");
   --  This function prints a file.
   --  If you pass `NULL` as Setup, then this method will present a print
   --  dialog. Otherwise, it will attempt to print directly, without user
   --  interaction.
   --  Since: gtk+ 4.14
   --  @param Parent the parent `GtkWindow`
   --  @param Setup the `GtkPrintSetup` to use
   --  @param File the `GFile` to print
   --  @param Cancellable a `GCancellable` to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_Print_Dialog_Setup
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_Print_Dialog_Setup, "gtk_print_dialog_setup");
   --  This function presents a print dialog to let the user select a printer,
   --  and set up print settings and page setup.
   --  The Callback will be called when the dialog is dismissed. The obtained
   --  [structGtk.PrintSetup] can then be passed to
   --  [methodGtk.PrintDialog.print] or [methodGtk.PrintDialog.print_file].
   --  One possible use for this method is to have the user select a printer,
   --  then show a page setup UI in the application (e.g. to arrange images on
   --  a page), then call [methodGtk.PrintDialog.print] on Self to do the
   --  printing without further user interaction.
   --  Since: gtk+ 4.14
   --  @param Parent the parent `GtkWindow`
   --  @param Cancellable a `GCancellable` to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       Data          : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       Data          : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   package Type_Conversion_Gtk_Print_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Print_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Print_Dialog);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Print_Dialog) is
   begin
      Self := new Gtk_Print_Dialog_Record;
      Gtk.Print_Dialog.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Gtk_Print_Dialog_New --
   --------------------------

   function Gtk_Print_Dialog_New return Gtk_Print_Dialog is
      Self : constant Gtk_Print_Dialog := new Gtk_Print_Dialog_Record;
   begin
      Gtk.Print_Dialog.Initialize (Self);
      return Self;
   end Gtk_Print_Dialog_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Print_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_print_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Get_Accept_Label --
   ----------------------

   function Get_Accept_Label
      (Self : not null access Gtk_Print_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_dialog_get_accept_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Accept_Label;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_Print_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_dialog_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   --------------------
   -- Get_Page_Setup --
   --------------------

   function Get_Page_Setup
      (Self : not null access Gtk_Print_Dialog_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_dialog_get_page_setup");
      Stub_Gtk_Page_Setup : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Page_Setup));
   end Get_Page_Setup;

   ------------------------
   -- Get_Print_Settings --
   ------------------------

   function Get_Print_Settings
      (Self : not null access Gtk_Print_Dialog_Record)
       return Gtk.Print_Settings.Gtk_Print_Settings
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_dialog_get_print_settings");
      Stub_Gtk_Print_Settings : Gtk.Print_Settings.Gtk_Print_Settings_Record;
   begin
      return Gtk.Print_Settings.Gtk_Print_Settings (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Print_Settings));
   end Get_Print_Settings;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Print_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_print_dialog_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   -----------
   -- Print --
   -----------

   procedure Print
      (Self        : not null access Gtk_Print_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Setup       : Gtk.Print_Setup.Gtk_Print_Setup;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Print_Dialog_Print (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object (Setup), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Print_Dialog_Print (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object (Setup), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Print;

   ----------------
   -- Print_File --
   ----------------

   procedure Print_File
      (Self        : not null access Gtk_Print_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Setup       : Gtk.Print_Setup.Gtk_Print_Setup;
       File        : Glib.GFile.Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Print_Dialog_Print_File (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object (Setup), File, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Print_Dialog_Print_File (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object (Setup), File, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Print_File;

   -----------------------
   -- Print_File_Finish --
   -----------------------

   function Print_File_Finish
      (Self   : not null access Gtk_Print_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_dialog_print_file_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Print_File_Finish;

   ------------------
   -- Print_Finish --
   ------------------

   function Print_Finish
      (Self   : not null access Gtk_Print_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError)
       return Glib.Output_Stream.Goutput_Stream
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_print_dialog_print_finish");
      Acc_Error           : aliased Glib.Error.GError;
      Return_Obj          : Glib.Output_Stream.Goutput_Stream;
      Stub_Goutput_Stream : Glib.Output_Stream.Goutput_Stream_Record;
      Tmp_Return          : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Glib.Output_Stream.Goutput_Stream (Get_User_Data (Tmp_Return, Stub_Goutput_Stream));
      end if;
      return Return_Obj;
   end Print_Finish;

   ----------------------
   -- Set_Accept_Label --
   ----------------------

   procedure Set_Accept_Label
      (Self         : not null access Gtk_Print_Dialog_Record;
       Accept_Label : UTF8_String)
   is
      procedure Internal
         (Self         : System.Address;
          Accept_Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_dialog_set_accept_label");
      Tmp_Accept_Label : Gtkada.Types.Chars_Ptr := New_String (Accept_Label);
   begin
      Internal (Get_Object (Self), Tmp_Accept_Label);
      Free (Tmp_Accept_Label);
   end Set_Accept_Label;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_Print_Dialog_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_print_dialog_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   --------------------
   -- Set_Page_Setup --
   --------------------

   procedure Set_Page_Setup
      (Self       : not null access Gtk_Print_Dialog_Record;
       Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Page_Setup : System.Address);
      pragma Import (C, Internal, "gtk_print_dialog_set_page_setup");
   begin
      Internal (Get_Object (Self), Get_Object (Page_Setup));
   end Set_Page_Setup;

   ------------------------
   -- Set_Print_Settings --
   ------------------------

   procedure Set_Print_Settings
      (Self           : not null access Gtk_Print_Dialog_Record;
       Print_Settings : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class)
   is
      procedure Internal
         (Self           : System.Address;
          Print_Settings : System.Address);
      pragma Import (C, Internal, "gtk_print_dialog_set_print_settings");
   begin
      Internal (Get_Object (Self), Get_Object (Print_Settings));
   end Set_Print_Settings;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Print_Dialog_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_print_dialog_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   -----------
   -- Setup --
   -----------

   procedure Setup
      (Self        : not null access Gtk_Print_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Print_Dialog_Setup (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Print_Dialog_Setup (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Setup;

   ------------------
   -- Setup_Finish --
   ------------------

   function Setup_Finish
      (Self   : not null access Gtk_Print_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError)
       return Gtk.Print_Setup.Gtk_Print_Setup
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_print_dialog_setup_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Return_Obj : Gtk.Print_Setup.Gtk_Print_Setup := Null_Gtk_Print_Setup;
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := From_Object (Tmp_Return);
      end if;
      return Return_Obj;
   end Setup_Finish;

end Gtk.Print_Dialog;
