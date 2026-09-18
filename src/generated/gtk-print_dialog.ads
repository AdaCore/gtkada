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

--  Asynchronous API to present a print dialog to the user.
--
--  `GtkPrintDialog` collects the arguments that are needed to present the
--  dialog, such as a title for the dialog and whether it should be modal.
--
--  The dialog is shown with the [methodGtk.PrintDialog.setup] function.
--
--  The actual printing can be done with [methodGtk.PrintDialog.print] or
--  [methodGtk.PrintDialog.print_file]. These APIs follows the GIO async
--  pattern, and the results can be obtained by calling the corresponding
--  finish methods.

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Cancellable;   use Glib.Cancellable;
with Glib.Error;         use Glib.Error;
with Glib.GFile;         use Glib.GFile;
with Glib.Object;        use Glib.Object;
with Glib.Output_Stream; use Glib.Output_Stream;
with Glib.Properties;    use Glib.Properties;
with Gtk.Page_Setup;     use Gtk.Page_Setup;
with Gtk.Print_Settings; use Gtk.Print_Settings;
with Gtk.Print_Setup;    use Gtk.Print_Setup;
with Gtk.Window;         use Gtk.Window;

package Gtk.Print_Dialog is

   type Gtk_Print_Dialog_Record is new GObject_Record with null record;
   type Gtk_Print_Dialog is access all Gtk_Print_Dialog_Record'Class;

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

   procedure Gtk_New (Self : out Gtk_Print_Dialog);
   procedure Initialize
      (Self : not null access Gtk_Print_Dialog_Record'Class);
   --  Creates a new `GtkPrintDialog` object.
   --  Since: gtk+ 4.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Print_Dialog_New return Gtk_Print_Dialog;
   --  Creates a new `GtkPrintDialog` object.
   --  Since: gtk+ 4.14

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Accept_Label
      (Self : not null access Gtk_Print_Dialog_Record) return UTF8_String;
   --  Returns the label that will be shown on the accept button of the print
   --  dialog.
   --  Since: gtk+ 4.14
   --  @return the accept label

   procedure Set_Accept_Label
      (Self         : not null access Gtk_Print_Dialog_Record;
       Accept_Label : UTF8_String);
   --  Sets the label that will be shown on the accept button of the print
   --  dialog shown for [methodGtk.PrintDialog.setup].
   --  Since: gtk+ 4.14
   --  @param Accept_Label the new accept label

   function Get_Modal
      (Self : not null access Gtk_Print_Dialog_Record) return Boolean;
   --  Returns whether the print dialog blocks interaction with the parent
   --  window while it is presented.
   --  Since: gtk+ 4.14
   --  @return whether the print dialog is modal

   procedure Set_Modal
      (Self  : not null access Gtk_Print_Dialog_Record;
       Modal : Boolean);
   --  Sets whether the print dialog blocks interaction with the parent window
   --  while it is presented.
   --  Since: gtk+ 4.14
   --  @param Modal the new value

   function Get_Page_Setup
      (Self : not null access Gtk_Print_Dialog_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Returns the page setup.
   --  Since: gtk+ 4.14
   --  @return the page setup. Has transfer-ownership='none'.

   procedure Set_Page_Setup
      (Self       : not null access Gtk_Print_Dialog_Record;
       Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);
   --  Set the page setup for the print dialog.
   --  Since: gtk+ 4.14
   --  @param Page_Setup the new page setup

   function Get_Print_Settings
      (Self : not null access Gtk_Print_Dialog_Record)
       return Gtk.Print_Settings.Gtk_Print_Settings;
   --  Returns the print settings for the print dialog.
   --  Since: gtk+ 4.14
   --  @return the settings. Has transfer-ownership='none'.

   procedure Set_Print_Settings
      (Self           : not null access Gtk_Print_Dialog_Record;
       Print_Settings : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class);
   --  Sets the print settings for the print dialog.
   --  Since: gtk+ 4.14
   --  @param Print_Settings the new print settings

   function Get_Title
      (Self : not null access Gtk_Print_Dialog_Record) return UTF8_String;
   --  Returns the title that will be shown on the print dialog.
   --  Since: gtk+ 4.14
   --  @return the title

   procedure Set_Title
      (Self  : not null access Gtk_Print_Dialog_Record;
       Title : UTF8_String);
   --  Sets the title that will be shown on the print dialog.
   --  Since: gtk+ 4.14
   --  @param Title the new title

   procedure Print
      (Self        : not null access Gtk_Print_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Setup       : Gtk.Print_Setup.Gtk_Print_Setup;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   procedure Print_File
      (Self        : not null access Gtk_Print_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Setup       : Gtk.Print_Setup.Gtk_Print_Setup;
       File        : Glib.GFile.Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Print_File_Finish
      (Self   : not null access Gtk_Print_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes the [methodGtk.PrintDialog.print_file] call and returns the
   --  results.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.14
   --  @param Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error
   --  @return Whether the call was successful

   function Print_Finish
      (Self   : not null access Gtk_Print_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError)
       return Glib.Output_Stream.Goutput_Stream;
   --  Finishes the [methodGtk.PrintDialog.print] call and returns the
   --  results.
   --  If the call was successful, the content to be printed should be written
   --  to the returned output stream. Otherwise, `NULL` is returned.
   --  The overall results of the print operation will be returned in the
   --  [methodGio.OutputStream.close] call, so if you are interested in the
   --  results, you need to explicitly close the output stream (it will be
   --  closed automatically if you just unref it). Be aware that the close call
   --  may not be instant as it operation will for the printer to finish
   --  printing.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.14
   --  @param Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error
   --  @return a [classGio.OutputStream]. Has transfer-ownership='full'.

   procedure Setup
      (Self        : not null access Gtk_Print_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Setup_Finish
      (Self   : not null access Gtk_Print_Dialog_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError)
       return Gtk.Print_Setup.Gtk_Print_Setup;
   --  Finishes the [methodGtk.PrintDialog.setup] call.
   --  If the call was successful, it returns a [structGtk.PrintSetup] which
   --  contains the print settings and page setup information that will be used
   --  to print.
   --  Note that this function returns a [errorGtk.DialogError.DISMISSED]
   --  error if the user cancels the dialog.
   --  Since: gtk+ 4.14
   --  @param Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error
   --  @return the resulting `[structGtk.PrintSetup]`. Has
   --  transfer-ownership='full'.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accept_Label_Property : constant Glib.Properties.Property_String;
   --  A label that may be shown on the accept button of a print dialog that
   --  is presented by [methodGtk.PrintDialog.setup].

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the print dialog is modal.

   Page_Setup_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Page_Setup.Gtk_Page_Setup
   --  The page setup to use.

   Print_Settings_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Print_Settings.Gtk_Print_Settings
   --  The print settings to use.

   Title_Property : constant Glib.Properties.Property_String;
   --  A title that may be shown on the print dialog that is presented by
   --  [methodGtk.PrintDialog.setup].

private
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Print_Settings_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("print-settings");
   Page_Setup_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("page-setup");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Accept_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accept-label");
end Gtk.Print_Dialog;
