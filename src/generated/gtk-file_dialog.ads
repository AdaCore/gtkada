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

--  Asynchronous API to present a file chooser dialog.
--
--  `GtkFileDialog` collects the arguments that are needed to present the
--  dialog to the user, such as a title for the dialog and whether it should be
--  modal.
--
--  The dialog is shown with [methodGtk.FileDialog.open],
--  [methodGtk.FileDialog.save], etc.
--
--  <group>Dialogs</group>
--  <gtkada_demo>create_file_chooser.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Glib;                  use Glib;
with Glib.Cancellable;      use Glib.Cancellable;
with Glib.Error;            use Glib.Error;
with Glib.GFile;            use Glib.GFile;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Gtk.File_Filter;       use Gtk.File_Filter;
with Gtk.Window;            use Gtk.Window;

package Gtk.File_Dialog is

   type Gtk_File_Dialog_Record is new GObject_Record with null record;
   type Gtk_File_Dialog is access all Gtk_File_Dialog_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the thread-default main context (see
   --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  The asynchronous operation is guaranteed to have held a reference to
   --  Source_Object from the time when the `*_async` function was called,
   --  until after this callback returns.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_File_Dialog);
   procedure Initialize
      (Self : not null access Gtk_File_Dialog_Record'Class);
   --  Creates a new `GtkFileDialog` object.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_File_Dialog_New return Gtk_File_Dialog;
   --  Creates a new `GtkFileDialog` object.
   --  Since: gtk+ 4.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Accept_Label
      (Self : not null access Gtk_File_Dialog_Record) return UTF8_String;
   --  Retrieves the text used by the dialog on its accept button.
   --  Since: gtk+ 4.10
   --  @return the label shown on the file chooser's accept button

   procedure Set_Accept_Label
      (Self         : not null access Gtk_File_Dialog_Record;
       Accept_Label : UTF8_String := "");
   --  Sets the label shown on the file chooser's accept button.
   --  Leaving the accept label unset or setting it as `NULL` will fall back
   --  to a default label, depending on what API is used to launch the file
   --  dialog.
   --  Since: gtk+ 4.10
   --  @param Accept_Label the new accept label

   function Get_Default_Filter
      (Self : not null access Gtk_File_Dialog_Record)
       return Gtk.File_Filter.Gtk_File_Filter;
   --  Gets the filter that will be selected by default in the file chooser
   --  dialog.
   --  Since: gtk+ 4.10
   --  @return the default filter. Has transfer-ownership='none'.

   procedure Set_Default_Filter
      (Self   : not null access Gtk_File_Dialog_Record;
       Filter : access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   --  Sets the filter that will be selected by default in the file chooser
   --  dialog.
   --  If set to `NULL`, the first item in [propertyGtk.FileDialog:filters]
   --  will be used as the default filter. If that list is empty, the dialog
   --  will be unfiltered.
   --  Since: gtk+ 4.10
   --  @param Filter the file filter

   function Get_Filters
      (Self : not null access Gtk_File_Dialog_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the filters that will be offered to the user in the file chooser
   --  dialog.
   --  Since: gtk+ 4.10
   --  @return the filters, as a list model of [classGtk.FileFilter]

   procedure Set_Filters
      (Self    : not null access Gtk_File_Dialog_Record;
       Filters : Glib.List_Model.Glist_Model);
   --  Sets the filters that will be offered to the user in the file chooser
   --  dialog.
   --  Since: gtk+ 4.10
   --  @param Filters a list model of [classGtk.FileFilter]

   function Get_Initial_File
      (Self : not null access Gtk_File_Dialog_Record)
       return Glib.GFile.Gfile;
   --  Gets the file that will be initially selected in the file chooser
   --  dialog.
   --  Since: gtk+ 4.10
   --  @return the file

   procedure Set_Initial_File
      (Self : not null access Gtk_File_Dialog_Record;
       File : Glib.GFile.Gfile);
   --  Sets the file that will be initially selected in the file chooser
   --  dialog.
   --  This function is a shortcut for calling both
   --  [methodGtk.FileDialog.set_initial_folder] and
   --  [methodGtk.FileDialog.set_initial_name] with the directory and name of
   --  File, respectively.
   --  Since: gtk+ 4.10
   --  @param File a file

   function Get_Initial_Folder
      (Self : not null access Gtk_File_Dialog_Record)
       return Glib.GFile.Gfile;
   --  Gets the folder that will be set as the initial folder in the file
   --  chooser dialog.
   --  Since: gtk+ 4.10
   --  @return the folder

   procedure Set_Initial_Folder
      (Self   : not null access Gtk_File_Dialog_Record;
       Folder : Glib.GFile.Gfile);
   --  Sets the folder that will be set as the initial folder in the file
   --  chooser dialog.
   --  Since: gtk+ 4.10
   --  @param Folder a file

   function Get_Initial_Name
      (Self : not null access Gtk_File_Dialog_Record) return UTF8_String;
   --  Gets the filename that will be initially selected.
   --  Since: gtk+ 4.10
   --  @return the name

   procedure Set_Initial_Name
      (Self : not null access Gtk_File_Dialog_Record;
       Name : UTF8_String := "");
   --  Sets the filename that will be initially selected.
   --  For save dialogs, Name will usually be pre-entered into the name field.
   --  If a file with this name already exists in the directory set via
   --  [propertyGtk.FileDialog:initial-folder], the dialog will preselect it.
   --  Since: gtk+ 4.10
   --  @param Name a string

   function Get_Modal
      (Self : not null access Gtk_File_Dialog_Record) return Boolean;
   --  Returns whether the file chooser dialog blocks interaction with the
   --  parent window while it is presented.
   --  Since: gtk+ 4.10
   --  @return true if the file chooser dialog is modal

   procedure Set_Modal
      (Self  : not null access Gtk_File_Dialog_Record;
       Modal : Boolean);
   --  Sets whether the file chooser dialog blocks interaction with the parent
   --  window while it is presented.
   --  Since: gtk+ 4.10
   --  @param Modal the new value

   function Get_Title
      (Self : not null access Gtk_File_Dialog_Record) return UTF8_String;
   --  Returns the title that will be shown on the file chooser dialog.
   --  Since: gtk+ 4.10
   --  @return the title

   procedure Set_Title
      (Self  : not null access Gtk_File_Dialog_Record;
       Title : UTF8_String);
   --  Sets the title that will be shown on the file chooser dialog.
   --  Since: gtk+ 4.10
   --  @param Title the new title

   procedure Open
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to select a single file.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Open_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.GFile.Gfile;
   --  Finishes the [methodGtk.FileDialog.open] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return the file that was selected

   procedure Open_Multiple
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be set up to select multiple files.
   --  The file chooser dialog will initially be opened in the directory
   --  [propertyGtk.FileDialog:initial-folder].
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Open_Multiple_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.List_Model.Glist_Model;
   --  Finishes the [methodGtk.FileDialog.open] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return the files that were selected, as a list model of
   --  [ifaceGio.File]

   procedure Open_Multiple_Text_Files
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Open_Multiple_Text_Files_Finish
      (Self     : not null access Gtk_File_Dialog_Record;
       Result   : Glib.G_Async_Result;
       Encoding : out Ada.Strings.Unbounded.Unbounded_String;
       Error    : out Glib.Error.GError) return Glib.List_Model.Glist_Model;
   --  Finishes the [methodGtk.FileDialog.open] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.18
   --  @param Result the result
   --  @param Encoding return location for the text encoding to use
   --  @param Error the return location for a recoverable error
   --  @return the files that were selected, as a list model of
   --  [ifaceGio.File]

   procedure Open_Text_File
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Initiates a file selection operation by presenting a file chooser
   --  dialog to the user.
   --  In contrast to [methodGtk.FileDialog.open], this function lets the user
   --  select the text encoding for the file, if possible.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.18
   --  @param Parent the parent `GtkWindow`
   --  @param Cancellable a `GCancellable` to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Open_Text_File_Finish
      (Self     : not null access Gtk_File_Dialog_Record;
       Result   : Glib.G_Async_Result;
       Encoding : out Ada.Strings.Unbounded.Unbounded_String;
       Error    : out Glib.Error.GError) return Glib.GFile.Gfile;
   --  Finishes the [methodGtk.FileDialog.open_text_file] call and returns the
   --  resulting file and text encoding.
   --  If the user has explicitly selected a text encoding to use for the
   --  file, then Encoding will be set to a codeset name that is suitable for
   --  passing to iconv_open. Otherwise, it will be `NULL`.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.18
   --  @param Result a `GAsyncResult`
   --  @param Encoding return location for the text encoding to use
   --  @param Error the return location for a recoverable error
   --  @return the file that was selected

   procedure Save
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Presents a file chooser dialog to the user.
   --  The file chooser dialog will be save mode.
   --  The Callback will be called when the dialog is closed.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Save_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.GFile.Gfile;
   --  Finishes the [methodGtk.FileDialog.save] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return the file that was selected

   procedure Save_Text_File
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Save_Text_File_Finish
      (Self        : not null access Gtk_File_Dialog_Record;
       Result      : Glib.G_Async_Result;
       Encoding    : out Ada.Strings.Unbounded.Unbounded_String;
       Line_Ending : out Ada.Strings.Unbounded.Unbounded_String;
       Error       : out Glib.Error.GError) return Glib.GFile.Gfile;
   --  Finishes the [methodGtk.FileDialog.save_text_file] call and returns the
   --  resulting file, text encoding and line endings.
   --  If the user has explicitly selected a text encoding to use for the
   --  file, then Encoding will be set to a codeset name that is suitable for
   --  passing to iconv_open. Otherwise, it will be `NULL`.
   --  The Line_Ending will be set to one of "\n", "\r\n", "\r" or "", where
   --  the latter means to preserve existing line endings.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.18
   --  @param Result a `GAsyncResult`
   --  @param Encoding return location for the text encoding to use
   --  @param Line_Ending return location for the line endings to use
   --  @param Error the return location for a recoverable error
   --  @return the file that was selected.

   procedure Select_Folder
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Select_Folder_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.GFile.Gfile;
   --  Finishes the [methodGtk.FileDialog.select_folder] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return the folder that was selected

   procedure Select_Multiple_Folders
      (Self        : not null access Gtk_File_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Select_Multiple_Folders_Finish
      (Self   : not null access Gtk_File_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Glib.List_Model.Glist_Model;
   --  Finishes the [methodGtk.FileDialog.select_multiple_folders] call.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return the folders that were selected, as a list model of
   --  [ifaceGio.File]

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accept_Label_Property : constant Glib.Properties.Property_String;
   --  Label for the file chooser's accept button.

   Default_Filter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.File_Filter.Gtk_File_Filter
   --  The default filter.
   --
   --  This filter is initially active in the file chooser dialog.
   --
   --  If the default filter is `NULL`, the first filter of
   --  [propertyGtk.FileDialog:filters] is used as the default filter. If that
   --  property contains no filter, the dialog will be unfiltered.
   --
   --  If [propertyGtk.FileDialog:filters] is not `NULL`, the default filter
   --  should be part of the list. If it is not, the dialog may choose to not
   --  make it available.

   Filters_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.List_Model.Glist_Model
   --  The list of filters.
   --
   --  See [propertyGtk.FileDialog:default-filter] about how these two
   --  properties interact.

   Initial_File_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.GFile.Gfile
   --  The initial file.
   --
   --  This file is initially selected in the file chooser dialog
   --
   --  This is a utility property that sets both
   --  [propertyGtk.FileDialog:initial-folder] and
   --  [propertyGtk.FileDialog:initial-name].

   Initial_Folder_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.GFile.Gfile
   --  The initial folder.
   --
   --  This is the directory that is initially opened in the file chooser
   --  dialog.

   Initial_Name_Property : constant Glib.Properties.Property_String;
   --  The initial name.
   --
   --  This is the name of the file that is initially selected in the file
   --  chooser dialog.

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the file chooser dialog is modal.

   Title_Property : constant Glib.Properties.Property_String;
   --  A title that may be shown on the file chooser dialog.

private
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Initial_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("initial-name");
   Initial_Folder_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("initial-folder");
   Initial_File_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("initial-file");
   Filters_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("filters");
   Default_Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("default-filter");
   Accept_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accept-label");
end Gtk.File_Dialog;
