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

package body Gtk.File_Dialog is

   procedure C_Gtk_File_Dialog_Open
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Open, "gtk_file_dialog_open");
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to select a single file.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Open_Multiple
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Open_Multiple, "gtk_file_dialog_open_multiple");
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to select multiple files.
   --  The file chooser dialog will initially be opened in the directory
   --  [propertyGtk.FileDialog:initial-folder].
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Open_Multiple_Text_Files
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Open_Multiple_Text_Files, "gtk_file_dialog_open_multiple_text_files");
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to select multiple files.
   --  The file chooser dialog will initially be opened in the directory
   --  [propertyGtk.FileDialog:initial-folder].
   --  In contrast to [methodGtk.FileDialog.open], this function lets the user
   --  select the text encoding for the files, if possible.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.18
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Open_Text_File
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Open_Text_File, "gtk_file_dialog_open_text_file");
   --  Initiates a file selection operation by presenting a file chooser
   --  dialog to the user.
   --  In contrast to [methodGtk.FileDialog.open], this function lets the user
   --  select the text encoding for the file, if possible.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.18
   --  @param Parent the parent `GtkWindow`
   --  @param Cancellable a `GCancellable` to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Save
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Save, "gtk_file_dialog_save");
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be save mode.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Save_Text_File
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Save_Text_File, "gtk_file_dialog_save_text_file");
   --  Initiates a file save operation by presenting a file chooser dialog to
   --  the user.
   --  In contrast to [methodGtk.FileDialog.save], this function lets the user
   --  select the text encoding and line endings for the text file, if
   --  possible.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.18
   --  @param Parent the parent `GtkWindow`
   --  @param Cancellable a `GCancellable` to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Select_Folder
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Select_Folder, "gtk_file_dialog_select_folder");
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to select a single folder.
   --  If you pass Initial_Folder, the file chooser dialog will initially be
   --  opened in the parent directory of that folder, otherwise, it will be in
   --  the directory [propertyGtk.FileDialog:initial-folder].
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Dialog_Select_Multiple_Folders
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Dialog_Select_Multiple_Folders, "gtk_file_dialog_select_multiple_folders");
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to allow selecting multiple
   --  folders.
   --  The file chooser dialog will initially be opened in the directory
   --  [propertyGtk.FileDialog:initial-folder].
   --  The Callback will be called when the dialog is closed.
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

   package Type_Conversion_Gtk_File_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_File_Dialog);

   -------------------------
   -- Gtk_File_Dialog_New --
   -------------------------

   function Gtk_File_Dialog_New return Gtk_File_Dialog is
      Self : constant Gtk_File_Dialog := new Gtk_File_Dialog_Record;
   begin
      Gtk.File_Dialog.Initialize (Self);
      return Self;
   end Gtk_File_Dialog_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_File_Dialog) is
   begin
      Self := new Gtk_File_Dialog_Record;
      Gtk.File_Dialog.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_File_Dialog_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_file_dialog_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Get_Accept_Label --
   ----------------------

   function Get_Accept_Label
      (Self : not null access Gtk_File_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_file_dialog_get_accept_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Accept_Label;

   ------------------------
   -- Get_Default_Filter --
   ------------------------

   function Get_Default_Filter
      (Self : not null access Gtk_File_Dialog_Record)
       return Gtk.File_Filter.Gtk_File_Filter
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_file_dialog_get_default_filter");
      Stub_Gtk_File_Filter : Gtk.File_Filter.Gtk_File_Filter_Record;
   begin
      return Gtk.File_Filter.Gtk_File_Filter (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_File_Filter));
   end Get_Default_Filter;

   -----------------
   -- Get_Filters --
   -----------------

   function Get_Filters
      (Self : not null access Gtk_File_Dialog_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_file_dialog_get_filters");
   begin
      return Internal (Get_Object (Self));
   end Get_Filters;

   ----------------------
   -- Get_Initial_File --
   ----------------------

   function Get_Initial_File
      (Self : not null access Gtk_File_Dialog_Record)
       return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_get_initial_file");
   begin
      return Internal (Get_Object (Self));
   end Get_Initial_File;

   ------------------------
   -- Get_Initial_Folder --
   ------------------------

   function Get_Initial_Folder
      (Self : not null access Gtk_File_Dialog_Record)
       return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_get_initial_folder");
   begin
      return Internal (Get_Object (Self));
   end Get_Initial_Folder;

   ----------------------
   -- Get_Initial_Name --
   ----------------------

   function Get_Initial_Name
      (Self : not null access Gtk_File_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_file_dialog_get_initial_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Initial_Name;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_File_Dialog_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_file_dialog_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_File_Dialog_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_file_dialog_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   ----------
   -- Open --
   ----------

   procedure Open
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Open (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Open (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Open;

   -----------------
   -- Open_Finish --
   -----------------

   function Open_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.GFile.Gfile
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_open_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.GFile.Gfile;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return
        (if Error = null
         then Tmp_Return
         else Null_Gfile);
   end Open_Finish;

   -------------------
   -- Open_Multiple --
   -------------------

   procedure Open_Multiple
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Open_Multiple (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Open_Multiple (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Open_Multiple;

   --------------------------
   -- Open_Multiple_Finish --
   --------------------------

   function Open_Multiple_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError)
          return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_file_dialog_open_multiple_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.List_Model.Glist_Model;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return
        (if Error = null
         then Tmp_Return
         else Null_Glist_Model);
   end Open_Multiple_Finish;

   ------------------------------
   -- Open_Multiple_Text_Files --
   ------------------------------

   procedure Open_Multiple_Text_Files
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Open_Multiple_Text_Files (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Open_Multiple_Text_Files (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Open_Multiple_Text_Files;

   -------------------------------------
   -- Open_Multiple_Text_Files_Finish --
   -------------------------------------

   function Open_Multiple_Text_Files_Finish
      (Self     : not null access Gtk_File_Dialog_Record;
       Result   : Glib.G_Async_Result;
       Encoding : out Ada.Strings.Unbounded.Unbounded_String;
       Error    : out Glib.Error.GError) return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address;
          Result : Glib.G_Async_Result;
          Encoding : access Gtkada.Types.Chars_Ptr;
          Error : access Glib.Error.GError) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_file_dialog_open_multiple_text_files_finish");
      C_Encoding : aliased Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      C_Error : aliased Glib.Error.GError := null;
      File : Glib.List_Model.Glist_Model;
   begin
      File := Internal (Get_Object (Self), Result, C_Encoding'Access, C_Error'Access);
      Error := C_Error;
      Encoding := Ada.Strings.Unbounded.To_Unbounded_String
        (Gtkada.Bindings.Value_Allowing_Null (C_Encoding));
      return (if Error = null then File else Glib.List_Model.Null_Glist_Model);
   end Open_Multiple_Text_Files_Finish;

   --------------------
   -- Open_Text_File --
   --------------------

   procedure Open_Text_File
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Open_Text_File (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Open_Text_File (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Open_Text_File;

   ---------------------------
   -- Open_Text_File_Finish --
   ---------------------------

   function Open_Text_File_Finish
      (Self     : not null access Gtk_File_Dialog_Record;
       Result   : Glib.G_Async_Result;
       Encoding : out Ada.Strings.Unbounded.Unbounded_String;
       Error    : out Glib.Error.GError) return Glib.GFile.Gfile
   is
      function Internal
         (Self : System.Address;
          Result : Glib.G_Async_Result;
          Encoding : access Gtkada.Types.Chars_Ptr;
          Error : access Glib.Error.GError) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_open_text_file_finish");
      C_Encoding : aliased Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      C_Error : aliased Glib.Error.GError := null;
      File : Glib.GFile.Gfile;
   begin
      File := Internal (Get_Object (Self), Result, C_Encoding'Access, C_Error'Access);
      Error := C_Error;
      Encoding := Ada.Strings.Unbounded.To_Unbounded_String
        (Gtkada.Bindings.Value_Allowing_Null (C_Encoding));
      return (if Error = null then File else Glib.GFile.Null_Gfile);
   end Open_Text_File_Finish;

   ----------
   -- Save --
   ----------

   procedure Save
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Save (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Save (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Save;

   -----------------
   -- Save_Finish --
   -----------------

   function Save_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.GFile.Gfile
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_save_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.GFile.Gfile;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return
        (if Error = null
         then Tmp_Return
         else Null_Gfile);
   end Save_Finish;

   --------------------
   -- Save_Text_File --
   --------------------

   procedure Save_Text_File
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Save_Text_File (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Save_Text_File (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Save_Text_File;

   ---------------------------
   -- Save_Text_File_Finish --
   ---------------------------

   function Save_Text_File_Finish
      (Self        : not null access Gtk_File_Dialog_Record;
       Result      : Glib.G_Async_Result;
       Encoding    : out Ada.Strings.Unbounded.Unbounded_String;
       Line_Ending : out Ada.Strings.Unbounded.Unbounded_String;
       Error       : out Glib.Error.GError) return Glib.GFile.Gfile
   is
      function Internal
         (Self : System.Address;
          Result : Glib.G_Async_Result;
          Encoding : access Gtkada.Types.Chars_Ptr;
          Line_Ending : access Gtkada.Types.Chars_Ptr;
          Error : access Glib.Error.GError) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_save_text_file_finish");
      C_Encoding : aliased Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      C_Line_Ending : aliased Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      C_Error : aliased Glib.Error.GError := null;
      File : Glib.GFile.Gfile;
   begin
      File := Internal (Get_Object (Self), Result, C_Encoding'Access, C_Line_Ending'Access, C_Error'Access);
      Error := C_Error;
      Encoding := Ada.Strings.Unbounded.To_Unbounded_String
        (Gtkada.Bindings.Value_Allowing_Null (C_Encoding));
      Line_Ending := Ada.Strings.Unbounded.To_Unbounded_String
        (Gtkada.Bindings.Value_Allowing_Null (C_Line_Ending));
      return (if Error = null then File else Glib.GFile.Null_Gfile);
   end Save_Text_File_Finish;

   -------------------
   -- Select_Folder --
   -------------------

   procedure Select_Folder
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Select_Folder (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Select_Folder (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Select_Folder;

   --------------------------
   -- Select_Folder_Finish --
   --------------------------

   function Select_Folder_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.GFile.Gfile
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_dialog_select_folder_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.GFile.Gfile;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return
        (if Error = null
         then Tmp_Return
         else Null_Gfile);
   end Select_Folder_Finish;

   -----------------------------
   -- Select_Multiple_Folders --
   -----------------------------

   procedure Select_Multiple_Folders
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Dialog_Select_Multiple_Folders (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Dialog_Select_Multiple_Folders (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Select_Multiple_Folders;

   ------------------------------------
   -- Select_Multiple_Folders_Finish --
   ------------------------------------

   function Select_Multiple_Folders_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError)
          return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_file_dialog_select_multiple_folders_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.List_Model.Glist_Model;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return
        (if Error = null
         then Tmp_Return
         else Null_Glist_Model);
   end Select_Multiple_Folders_Finish;

   ----------------------
   -- Set_Accept_Label --
   ----------------------

   procedure Set_Accept_Label
      (Self         : not null access Gtk_File_Dialog_Record;
       Accept_Label : UTF8_String := "")
   is
      procedure Internal
         (Self         : System.Address;
          Accept_Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_dialog_set_accept_label");
      Tmp_Accept_Label : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Accept_Label :=
        (if Accept_Label = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Accept_Label));
      Internal (Get_Object (Self), Tmp_Accept_Label);
      Free (Tmp_Accept_Label);
   end Set_Accept_Label;

   ------------------------
   -- Set_Default_Filter --
   ------------------------

   procedure Set_Default_Filter
      (Self   : not null access Gtk_File_Dialog_Record;
       Filter : access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal (Self : System.Address; Filter : System.Address);
      pragma Import (C, Internal, "gtk_file_dialog_set_default_filter");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Filter)));
   end Set_Default_Filter;

   -----------------
   -- Set_Filters --
   -----------------

   procedure Set_Filters
      (Self    : not null access Gtk_File_Dialog_Record;
       Filters : Glib.List_Model.Glist_Model)
   is
      procedure Internal
         (Self    : System.Address;
          Filters : Glib.List_Model.Glist_Model);
      pragma Import (C, Internal, "gtk_file_dialog_set_filters");
   begin
      Internal (Get_Object (Self), Filters);
   end Set_Filters;

   ----------------------
   -- Set_Initial_File --
   ----------------------

   procedure Set_Initial_File
      (Self : not null access Gtk_File_Dialog_Record;
       File : Glib.GFile.Gfile)
   is
      procedure Internal (Self : System.Address; File : Glib.GFile.Gfile);
      pragma Import (C, Internal, "gtk_file_dialog_set_initial_file");
   begin
      Internal (Get_Object (Self), File);
   end Set_Initial_File;

   ------------------------
   -- Set_Initial_Folder --
   ------------------------

   procedure Set_Initial_Folder
      (Self   : not null access Gtk_File_Dialog_Record;
       Folder : Glib.GFile.Gfile)
   is
      procedure Internal (Self : System.Address; Folder : Glib.GFile.Gfile);
      pragma Import (C, Internal, "gtk_file_dialog_set_initial_folder");
   begin
      Internal (Get_Object (Self), Folder);
   end Set_Initial_Folder;

   ----------------------
   -- Set_Initial_Name --
   ----------------------

   procedure Set_Initial_Name
      (Self : not null access Gtk_File_Dialog_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_dialog_set_initial_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Name :=
        (if Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name));
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Initial_Name;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_File_Dialog_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_file_dialog_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_File_Dialog_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_dialog_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

end Gtk.File_Dialog;
