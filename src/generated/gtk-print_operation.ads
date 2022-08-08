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

--  <description>
--  GtkPrintOperation is the high-level, portable printing API. It looks a bit
--  different than other GTK+ dialogs such as the
--  Gtk.File_Chooser.Gtk_File_Chooser, since some platforms don't expose enough
--  infrastructure to implement a good print dialog. On such platforms,
--  GtkPrintOperation uses the native print dialog. On platforms which do not
--  provide a native print dialog, GTK+ uses its own, see
--  Gtk_Print_Unix_Dialog.
--
--  The typical way to use the high-level printing API is to create a
--  GtkPrintOperation object with Gtk.Print_Operation.Gtk_New when the user
--  selects to print. Then you set some properties on it, e.g. the page size,
--  any Gtk.Print_Settings.Gtk_Print_Settings from previous print operations,
--  the number of pages, the current page, etc.
--
--  Then you start the print operation by calling Gtk.Print_Operation.Run. It
--  will then show a dialog, let the user select a printer and options. When
--  the user finished the dialog various signals will be emitted on the
--  Gtk.Print_Operation.Gtk_Print_Operation, the main one being
--  Gtk.Print_Operation.Gtk_Print_Operation::draw-page, which you are supposed
--  to catch and render the page on the provided
--  Gtk.Print_Context.Gtk_Print_Context using Cairo.
--
--  # The high-level printing API
--
--  |[<!-- language="C" --> static GtkPrintSettings *settings = NULL;
--
--  static void do_print (void) { GtkPrintOperation *print;
--  GtkPrintOperationResult res;
--
--  print = gtk_print_operation_new ();
--
--  if (settings != NULL) gtk_print_operation_set_print_settings (print,
--  settings);
--
--  g_signal_connect (print, "begin_print", G_CALLBACK (begin_print), NULL);
--  g_signal_connect (print, "draw_page", G_CALLBACK (draw_page), NULL);
--
--  res = gtk_print_operation_run (print,
--  GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, GTK_WINDOW (main_window), NULL);
--
--  if (res == GTK_PRINT_OPERATION_RESULT_APPLY) { if (settings != NULL)
--  g_object_unref (settings); settings = g_object_ref
--  (gtk_print_operation_get_print_settings (print)); }
--
--  g_object_unref (print); } ]|
--
--  By default GtkPrintOperation uses an external application to do print
--  preview. To implement a custom print preview, an application must connect
--  to the preview signal. The functions
--  Gtk.Print_Operation_Preview.Render_Page,
--  Gtk.Print_Operation_Preview.End_Preview and
--  Gtk.Print_Operation_Preview.Is_Selected are useful when implementing a
--  print preview.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                        use Glib;
with Glib.Generic_Properties;     use Glib.Generic_Properties;
with Glib.Object;                 use Glib.Object;
with Glib.Properties;             use Glib.Properties;
with Glib.Types;                  use Glib.Types;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Page_Setup;              use Gtk.Page_Setup;
with Gtk.Print_Context;           use Gtk.Print_Context;
with Gtk.Print_Operation_Preview; use Gtk.Print_Operation_Preview;
with Gtk.Print_Settings;          use Gtk.Print_Settings;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Window;                  use Gtk.Window;

package Gtk.Print_Operation is

   type Gtk_Print_Operation_Record is new GObject_Record with null record;
   type Gtk_Print_Operation is access all Gtk_Print_Operation_Record'Class;

   type Gtk_Print_Status is (
      Status_Initial,
      Status_Preparing,
      Status_Generating_Data,
      Status_Sending_Data,
      Status_Pending,
      Status_Pending_Issue,
      Status_Printing,
      Status_Finished,
      Status_Finished_Aborted);
   pragma Convention (C, Gtk_Print_Status);
   --  The status gives a rough indication of the completion of a running
   --  print operation.

   type Gtk_Print_Operation_Result is (
      Result_Error,
      Result_Apply,
      Result_Cancel,
      Result_In_Progress);
   pragma Convention (C, Gtk_Print_Operation_Result);
   --  A value of this type is returned by Gtk.Print_Operation.Run.

   type Gtk_Print_Operation_Action is (
      Action_Print_Dialog,
      Action_Print,
      Action_Preview,
      Action_Export);
   pragma Convention (C, Gtk_Print_Operation_Action);
   --  The Action parameter to Gtk.Print_Operation.Run determines what action
   --  the print operation should perform.

   type Gtk_Print_Error is (
      Error_General,
      Error_Internal_Error,
      Error_Nomem,
      Error_Invalid_File);
   pragma Convention (C, Gtk_Print_Error);
   --  Error codes that identify various errors that can occur while using the
   --  GTK+ printing support.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Print_Status_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Status);
   type Property_Gtk_Print_Status is new Gtk_Print_Status_Properties.Property;

   package Gtk_Print_Operation_Result_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Operation_Result);
   type Property_Gtk_Print_Operation_Result is new Gtk_Print_Operation_Result_Properties.Property;

   package Gtk_Print_Operation_Action_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Operation_Action);
   type Property_Gtk_Print_Operation_Action is new Gtk_Print_Operation_Action_Properties.Property;

   package Gtk_Print_Error_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Print_Error);
   type Property_Gtk_Print_Error is new Gtk_Print_Error_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Print_Operation);
   procedure Initialize
      (Self : not null access Gtk_Print_Operation_Record'Class);
   --  Creates a new Gtk.Print_Operation.Gtk_Print_Operation.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Print_Operation_New return Gtk_Print_Operation;
   --  Creates a new Gtk.Print_Operation.Gtk_Print_Operation.
   --  Since: gtk+ 2.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_operation_get_type");

   -------------
   -- Methods --
   -------------

   procedure Cancel (Self : not null access Gtk_Print_Operation_Record);
   --  Cancels a running print operation. This function may be called from a
   --  Gtk.Print_Operation.Gtk_Print_Operation::begin-print,
   --  Gtk.Print_Operation.Gtk_Print_Operation::paginate or
   --  Gtk.Print_Operation.Gtk_Print_Operation::draw-page signal handler to
   --  stop the currently running print operation.
   --  Since: gtk+ 2.10

   procedure Draw_Page_Finish
      (Self : not null access Gtk_Print_Operation_Record);
   --  Signalize that drawing of particular page is complete.
   --  It is called after completion of page drawing (e.g. drawing in another
   --  thread). If Gtk.Print_Operation.Set_Defer_Drawing was called before,
   --  then this function has to be called by application. In another case it
   --  is called by the library itself.
   --  Since: gtk+ 2.16

   function Get_Default_Page_Setup
      (Self : not null access Gtk_Print_Operation_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Returns the default page setup, see
   --  Gtk.Print_Operation.Set_Default_Page_Setup.
   --  Since: gtk+ 2.10

   procedure Set_Default_Page_Setup
      (Self               : not null access Gtk_Print_Operation_Record;
       Default_Page_Setup : access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);
   --  Makes Default_Page_Setup the default page setup for Op.
   --  This page setup will be used by Gtk.Print_Operation.Run, but it can be
   --  overridden on a per-page basis by connecting to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::request-page-setup signal.
   --  Since: gtk+ 2.10
   --  "default_page_setup": a Gtk.Page_Setup.Gtk_Page_Setup, or null

   function Get_Embed_Page_Setup
      (Self : not null access Gtk_Print_Operation_Record) return Boolean;
   --  Gets the value of
   --  Gtk.Print_Operation.Gtk_Print_Operation:embed-page-setup property.
   --  Since: gtk+ 2.18

   procedure Set_Embed_Page_Setup
      (Self  : not null access Gtk_Print_Operation_Record;
       Embed : Boolean);
   --  Embed page size combo box and orientation combo box into page setup
   --  page. Selected page setup is stored as default page setup in
   --  Gtk.Print_Operation.Gtk_Print_Operation.
   --  Since: gtk+ 2.18
   --  "embed": True to embed page setup selection in the
   --  Gtk_Print_Unix_Dialog

   procedure Get_Error (Self : not null access Gtk_Print_Operation_Record);
   --  Call this when the result of a print operation is
   --  Gtk.Print_Operation.Result_Error, either as returned by
   --  Gtk.Print_Operation.Run, or in the
   --  Gtk.Print_Operation.Gtk_Print_Operation::done signal handler. The
   --  returned Gerror.Gerror will contain more details on what went wrong.
   --  Since: gtk+ 2.10

   function Get_Has_Selection
      (Self : not null access Gtk_Print_Operation_Record) return Boolean;
   --  Gets the value of Gtk.Print_Operation.Gtk_Print_Operation:has-selection
   --  property.
   --  Since: gtk+ 2.18

   procedure Set_Has_Selection
      (Self          : not null access Gtk_Print_Operation_Record;
       Has_Selection : Boolean);
   --  Sets whether there is a selection to print.
   --  Application has to set number of pages to which the selection will draw
   --  by Gtk.Print_Operation.Set_N_Pages in a callback of
   --  Gtk.Print_Operation.Gtk_Print_Operation::begin-print.
   --  Since: gtk+ 2.18
   --  "has_selection": True indicates that a selection exists

   function Get_N_Pages_To_Print
      (Self : not null access Gtk_Print_Operation_Record) return Glib.Gint;
   --  Returns the number of pages that will be printed.
   --  Note that this value is set during print preparation phase
   --  (Gtk.Print_Operation.Status_Preparing), so this function should never be
   --  called before the data generation phase
   --  (Gtk.Print_Operation.Status_Generating_Data). You can connect to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::status-changed signal and call
   --  Gtk.Print_Operation.Get_N_Pages_To_Print when print status is
   --  Gtk.Print_Operation.Status_Generating_Data. This is typically used to
   --  track the progress of print operation.
   --  Since: gtk+ 2.18

   function Get_Print_Settings
      (Self : not null access Gtk_Print_Operation_Record)
       return Gtk.Print_Settings.Gtk_Print_Settings;
   --  Returns the current print settings.
   --  Note that the return value is null until either
   --  Gtk.Print_Operation.Set_Print_Settings or Gtk.Print_Operation.Run have
   --  been called.
   --  Since: gtk+ 2.10

   procedure Set_Print_Settings
      (Self           : not null access Gtk_Print_Operation_Record;
       Print_Settings : access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class);
   --  Sets the print settings for Op. This is typically used to re-establish
   --  print settings from a previous print operation, see
   --  Gtk.Print_Operation.Run.
   --  Since: gtk+ 2.10
   --  "print_settings": Gtk.Print_Settings.Gtk_Print_Settings

   function Get_Status
      (Self : not null access Gtk_Print_Operation_Record)
       return Gtk_Print_Status;
   --  Returns the status of the print operation. Also see
   --  Gtk.Print_Operation.Get_Status_String.
   --  Since: gtk+ 2.10

   function Get_Status_String
      (Self : not null access Gtk_Print_Operation_Record) return UTF8_String;
   --  Returns a string representation of the status of the print operation.
   --  The string is translated and suitable for displaying the print status
   --  e.g. in a Gtk.Status_Bar.Gtk_Status_Bar.
   --  Use Gtk.Print_Operation.Get_Status to obtain a status value that is
   --  suitable for programmatic use.
   --  Since: gtk+ 2.10

   function Get_Support_Selection
      (Self : not null access Gtk_Print_Operation_Record) return Boolean;
   --  Gets the value of
   --  Gtk.Print_Operation.Gtk_Print_Operation:support-selection property.
   --  Since: gtk+ 2.18

   procedure Set_Support_Selection
      (Self              : not null access Gtk_Print_Operation_Record;
       Support_Selection : Boolean);
   --  Sets whether selection is supported by
   --  Gtk.Print_Operation.Gtk_Print_Operation.
   --  Since: gtk+ 2.18
   --  "support_selection": True to support selection

   function Is_Finished
      (Self : not null access Gtk_Print_Operation_Record) return Boolean;
   --  A convenience function to find out if the print operation is finished,
   --  either successfully (Gtk.Print_Operation.Status_Finished) or
   --  unsuccessfully (Gtk.Print_Operation.Status_Finished_Aborted).
   --  Note: when you enable print status tracking the print operation can be
   --  in a non-finished state even after done has been called, as the
   --  operation status then tracks the print job status on the printer.
   --  Since: gtk+ 2.10

   function Run
      (Self   : not null access Gtk_Print_Operation_Record;
       Action : Gtk_Print_Operation_Action;
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
       return Gtk_Print_Operation_Result;
   --  Runs the print operation, by first letting the user modify print
   --  settings in the print dialog, and then print the document.
   --  Normally that this function does not return until the rendering of all
   --  pages is complete. You can connect to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::status-changed signal on Op to
   --  obtain some information about the progress of the print operation.
   --  Furthermore, it may use a recursive mainloop to show the print dialog.
   --  If you call Gtk.Print_Operation.Set_Allow_Async or set the
   --  Gtk.Print_Operation.Gtk_Print_Operation:allow-async property the
   --  operation will run asynchronously if this is supported on the platform.
   --  The Gtk.Print_Operation.Gtk_Print_Operation::done signal will be emitted
   --  with the result of the operation when the it is done (i.e. when the
   --  dialog is canceled, or when the print succeeds or fails). |[<!--
   --  language="C" --> if (settings != NULL)
   --  gtk_print_operation_set_print_settings (print, settings); if (page_setup
   --  != NULL) gtk_print_operation_set_default_page_setup (print, page_setup);
   --  g_signal_connect (print, "begin-print", G_CALLBACK (begin_print),
   --  &data); g_signal_connect (print, "draw-page", G_CALLBACK (draw_page),
   --  &data); res = gtk_print_operation_run (print,
   --  GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, parent, &error); if (res ==
   --  GTK_PRINT_OPERATION_RESULT_ERROR) { error_dialog =
   --  gtk_message_dialog_new (GTK_WINDOW (parent),
   --  GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
   --  "Error printing file:\n%s", error->message); g_signal_connect
   --  (error_dialog, "response", G_CALLBACK (gtk_widget_destroy), NULL);
   --  gtk_widget_show (error_dialog); g_error_free (error); } else if (res ==
   --  GTK_PRINT_OPERATION_RESULT_APPLY) { if (settings != NULL) g_object_unref
   --  (settings); settings = g_object_ref
   --  (gtk_print_operation_get_print_settings (print)); } ]|
   --  Note that Gtk.Print_Operation.Run can only be called once on a given
   --  Gtk.Print_Operation.Gtk_Print_Operation.
   --  Since: gtk+ 2.10
   --  "action": the action to start
   --  "parent": Transient parent of the dialog

   procedure Set_Allow_Async
      (Self        : not null access Gtk_Print_Operation_Record;
       Allow_Async : Boolean);
   --  Sets whether the Gtk.Print_Operation.Run may return before the print
   --  operation is completed. Note that some platforms may not allow
   --  asynchronous operation.
   --  Since: gtk+ 2.10
   --  "allow_async": True to allow asynchronous operation

   procedure Set_Current_Page
      (Self         : not null access Gtk_Print_Operation_Record;
       Current_Page : Glib.Gint);
   --  Sets the current page.
   --  If this is called before Gtk.Print_Operation.Run, the user will be able
   --  to select to print only the current page.
   --  Note that this only makes sense for pre-paginated documents.
   --  Since: gtk+ 2.10
   --  "current_page": the current page, 0-based

   procedure Set_Custom_Tab_Label
      (Self  : not null access Gtk_Print_Operation_Record;
       Label : UTF8_String := "");
   --  Sets the label for the tab holding custom widgets.
   --  Since: gtk+ 2.10
   --  "label": the label to use, or null to use the default label

   procedure Set_Defer_Drawing
      (Self : not null access Gtk_Print_Operation_Record);
   --  Sets up the Gtk.Print_Operation.Gtk_Print_Operation to wait for calling
   --  of Gtk.Print_Operation.Draw_Page_Finish from application. It can be used
   --  for drawing page in another thread.
   --  This function must be called in the callback of "draw-page" signal.
   --  Since: gtk+ 2.16

   procedure Set_Export_Filename
      (Self     : not null access Gtk_Print_Operation_Record;
       Filename : UTF8_String);
   --  Sets up the Gtk.Print_Operation.Gtk_Print_Operation to generate a file
   --  instead of showing the print dialog. The indended use of this function
   --  is for implementing "Export to PDF" actions. Currently, PDF is the only
   --  supported format.
   --  "Print to PDF" support is independent of this and is done by letting
   --  the user pick the "Print to PDF" item from the list of printers in the
   --  print dialog.
   --  Since: gtk+ 2.10
   --  "filename": the filename for the exported file

   procedure Set_Job_Name
      (Self     : not null access Gtk_Print_Operation_Record;
       Job_Name : UTF8_String);
   --  Sets the name of the print job. The name is used to identify the job
   --  (e.g. in monitoring applications like eggcups).
   --  If you don't set a job name, GTK+ picks a default one by numbering
   --  successive print jobs.
   --  Since: gtk+ 2.10
   --  "job_name": a string that identifies the print job

   procedure Set_N_Pages
      (Self    : not null access Gtk_Print_Operation_Record;
       N_Pages : Glib.Gint);
   --  Sets the number of pages in the document.
   --  This must be set to a positive number before the rendering starts. It
   --  may be set in a Gtk.Print_Operation.Gtk_Print_Operation::begin-print
   --  signal hander.
   --  Note that the page numbers passed to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::request-page-setup and
   --  Gtk.Print_Operation.Gtk_Print_Operation::draw-page signals are 0-based,
   --  i.e. if the user chooses to print all pages, the last ::draw-page signal
   --  will be for page N_Pages - 1.
   --  Since: gtk+ 2.10
   --  "n_pages": the number of pages

   procedure Set_Show_Progress
      (Self          : not null access Gtk_Print_Operation_Record;
       Show_Progress : Boolean);
   --  If Show_Progress is True, the print operation will show a progress
   --  dialog during the print operation.
   --  Since: gtk+ 2.10
   --  "show_progress": True to show a progress dialog

   procedure Set_Track_Print_Status
      (Self         : not null access Gtk_Print_Operation_Record;
       Track_Status : Boolean);
   --  If track_status is True, the print operation will try to continue
   --  report on the status of the print job in the printer queues and printer.
   --  This can allow your application to show things like "out of paper"
   --  issues, and when the print job actually reaches the printer.
   --  This function is often implemented using some form of polling, so it
   --  should not be enabled unless needed.
   --  Since: gtk+ 2.10
   --  "track_status": True to track status after printing

   procedure Set_Unit
      (Self : not null access Gtk_Print_Operation_Record;
       Unit : Gtk.Enums.Gtk_Unit);
   --  Sets up the transformation for the cairo context obtained from
   --  Gtk.Print_Context.Gtk_Print_Context in such a way that distances are
   --  measured in units of Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit to use

   procedure Set_Use_Full_Page
      (Self      : not null access Gtk_Print_Operation_Record;
       Full_Page : Boolean);
   --  If Full_Page is True, the transformation for the cairo context obtained
   --  from Gtk.Print_Context.Gtk_Print_Context puts the origin at the top left
   --  corner of the page (which may not be the top left corner of the sheet,
   --  depending on page orientation and the number of pages per sheet).
   --  Otherwise, the origin is at the top left corner of the imageable area
   --  (i.e. inside the margins).
   --  Since: gtk+ 2.10
   --  "full_page": True to set up the Gtk.Print_Context.Gtk_Print_Context for
   --  the full page

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure End_Preview
      (Preview : not null access Gtk_Print_Operation_Record);

   function Is_Selected
      (Preview : not null access Gtk_Print_Operation_Record;
       Page_Nr : Glib.Gint) return Boolean;

   procedure Render_Page
      (Preview : not null access Gtk_Print_Operation_Record;
       Page_Nr : Glib.Gint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Allow_Async_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the print operation may run asynchronously or not.
   --
   --  Some systems don't support asynchronous printing, but those that do
   --  will return Gtk.Print_Operation.Result_In_Progress as the status, and
   --  emit the Gtk.Print_Operation.Gtk_Print_Operation::done signal when the
   --  operation is actually done.
   --
   --  The Windows port does not support asynchronous operation at all (this
   --  is unlikely to change). On other platforms, all actions except for
   --  Gtk.Print_Operation.Action_Export support asynchronous operation.

   Current_Page_Property : constant Glib.Properties.Property_Int;
   --  The current page in the document.
   --
   --  If this is set before Gtk.Print_Operation.Run, the user will be able to
   --  select to print only the current page.
   --
   --  Note that this only makes sense for pre-paginated documents.

   Custom_Tab_Label_Property : constant Glib.Properties.Property_String;
   --  Used as the label of the tab containing custom widgets. Note that this
   --  property may be ignored on some platforms.
   --
   --  If this is null, GTK+ uses a default label.

   Default_Page_Setup_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Page_Setup.Gtk_Page_Setup
   --  The Gtk.Page_Setup.Gtk_Page_Setup used by default.
   --
   --  This page setup will be used by Gtk.Print_Operation.Run, but it can be
   --  overridden on a per-page basis by connecting to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::request-page-setup signal.

   Embed_Page_Setup_Property : constant Glib.Properties.Property_Boolean;
   --  If True, page size combo box and orientation combo box are embedded
   --  into page setup page.

   Export_Filename_Property : constant Glib.Properties.Property_String;
   --  The name of a file to generate instead of showing the print dialog.
   --  Currently, PDF is the only supported format.
   --
   --  The intended use of this property is for implementing "Export to PDF"
   --  actions.
   --
   --  "Print to PDF" support is independent of this and is done by letting
   --  the user pick the "Print to PDF" item from the list of printers in the
   --  print dialog.

   Has_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether there is a selection in your application. This can
   --  allow your application to print the selection. This is typically used to
   --  make a "Selection" button sensitive.

   Job_Name_Property : constant Glib.Properties.Property_String;
   --  A string used to identify the job (e.g. in monitoring applications like
   --  eggcups).
   --
   --  If you don't set a job name, GTK+ picks a default one by numbering
   --  successive print jobs.

   N_Pages_Property : constant Glib.Properties.Property_Int;
   --  The number of pages in the document.
   --
   --  This must be set to a positive number before the rendering starts. It
   --  may be set in a Gtk.Print_Operation.Gtk_Print_Operation::begin-print
   --  signal hander.
   --
   --  Note that the page numbers passed to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::request-page-setup and
   --  Gtk.Print_Operation.Gtk_Print_Operation::draw-page signals are 0-based,
   --  i.e. if the user chooses to print all pages, the last ::draw-page signal
   --  will be for page N_Pages - 1.

   N_Pages_To_Print_Property : constant Glib.Properties.Property_Int;
   --  The number of pages that will be printed.
   --
   --  Note that this value is set during print preparation phase
   --  (Gtk.Print_Operation.Status_Preparing), so this value should never be
   --  get before the data generation phase
   --  (Gtk.Print_Operation.Status_Generating_Data). You can connect to the
   --  Gtk.Print_Operation.Gtk_Print_Operation::status-changed signal and call
   --  Gtk.Print_Operation.Get_N_Pages_To_Print when print status is
   --  Gtk.Print_Operation.Status_Generating_Data. This is typically used to
   --  track the progress of print operation.

   Print_Settings_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Print_Settings.Gtk_Print_Settings
   --  The Gtk.Print_Settings.Gtk_Print_Settings used for initializing the
   --  dialog.
   --
   --  Setting this property is typically used to re-establish print settings
   --  from a previous print operation, see Gtk.Print_Operation.Run.

   Show_Progress_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether to show a progress dialog during the print
   --  operation.

   Status_Property : constant Gtk.Print_Operation.Property_Gtk_Print_Status;
   --  Type: Gtk_Print_Status
   --  The status of the print operation.

   Status_String_Property : constant Glib.Properties.Property_String;
   --  A string representation of the status of the print operation. The
   --  string is translated and suitable for displaying the print status e.g.
   --  in a Gtk.Status_Bar.Gtk_Status_Bar.
   --
   --  See the Gtk.Print_Operation.Gtk_Print_Operation:status property for a
   --  status value that is suitable for programmatic use.

   Support_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the print operation will support print of selection. This
   --  allows the print dialog to show a "Selection" button.

   Track_Print_Status_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the print operation will try to continue report on the status
   --  of the print job in the printer queues and printer. This can allow your
   --  application to show things like "out of paper" issues, and when the
   --  print job actually reaches the printer. However, this is often
   --  implemented using polling, and should not be enabled unless needed.

   Unit_Property : constant Gtk.Enums.Property_Gtk_Unit;
   --  The transformation for the cairo context obtained from
   --  Gtk.Print_Context.Gtk_Print_Context is set up in such a way that
   --  distances are measured in units of Unit.

   Use_Full_Page_Property : constant Glib.Properties.Property_Boolean;
   --  If True, the transformation for the cairo context obtained from
   --  Gtk.Print_Context.Gtk_Print_Context puts the origin at the top left
   --  corner of the page (which may not be the top left corner of the sheet,
   --  depending on page orientation and the number of pages per sheet).
   --  Otherwise, the origin is at the top left corner of the imageable area
   --  (i.e. inside the margins).

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Print_Operation_Gtk_Print_Context_Void is not null access procedure
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class);

   type Cb_GObject_Gtk_Print_Context_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class);

   Signal_Begin_Print : constant Glib.Signal_Name := "begin-print";
   procedure On_Begin_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Void;
       After : Boolean := False);
   procedure On_Begin_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted after the user has finished changing print settings in the
   --  dialog, before the actual rendering starts.
   --
   --  A typical use for ::begin-print is to use the parameters from the
   --  Gtk.Print_Context.Gtk_Print_Context and paginate the document
   --  accordingly, and then set the number of pages with
   --  Gtk.Print_Operation.Set_N_Pages.

   type Cb_Gtk_Print_Operation_GObject is not null access function
     (Self : access Gtk_Print_Operation_Record'Class)
   return Glib.Object.GObject;

   type Cb_GObject_GObject is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Glib.Object.GObject;

   Signal_Create_Custom_Widget : constant Glib.Signal_Name := "create-custom-widget";
   procedure On_Create_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_GObject;
       After : Boolean := False);
   procedure On_Create_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_GObject;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when displaying the print dialog. If you return a widget in a
   --  handler for this signal it will be added to a custom tab in the print
   --  dialog. You typically return a container widget with multiple widgets in
   --  it.
   --
   --  The print dialog owns the returned widget, and its lifetime is not
   --  controlled by the application. However, the widget is guaranteed to stay
   --  around until the
   --  Gtk.Print_Operation.Gtk_Print_Operation::custom-widget-apply signal is
   --  emitted on the operation. Then you can read out any information you need
   --  from the widgets.
   -- 
   --  Callback parameters:
   --    --  Returns A custom widget that gets embedded in
   --          the print dialog, or null

   type Cb_Gtk_Print_Operation_Gtk_Widget_Void is not null access procedure
     (Self   : access Gtk_Print_Operation_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Custom_Widget_Apply : constant Glib.Signal_Name := "custom-widget-apply";
   procedure On_Custom_Widget_Apply
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Custom_Widget_Apply
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted right before
   --  Gtk.Print_Operation.Gtk_Print_Operation::begin-print if you added a
   --  custom widget in the
   --  Gtk.Print_Operation.Gtk_Print_Operation::create-custom-widget handler.
   --  When you get this signal you should read the information from the custom
   --  widgets, as the widgets are not guaraneed to be around at a later time.

   type Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void is not null access procedure
     (Self   : access Gtk_Print_Operation_Record'Class;
      Result : Gtk_Print_Operation_Result);

   type Cb_GObject_Gtk_Print_Operation_Result_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Result : Gtk_Print_Operation_Result);

   Signal_Done : constant Glib.Signal_Name := "done";
   procedure On_Done
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Result_Void;
       After : Boolean := False);
   procedure On_Done
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Operation_Result_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the print operation run has finished doing everything
   --  required for printing.
   --
   --  Result gives you information about what happened during the run. If
   --  Result is Gtk.Print_Operation.Result_Error then you can call
   --  Gtk.Print_Operation.Get_Error for more information.
   --
   --  If you enabled print status tracking then
   --  Gtk.Print_Operation.Is_Finished may still return False after
   --  Gtk.Print_Operation.Gtk_Print_Operation::done was emitted.

   type Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void is not null access procedure
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Page_Nr : Glib.Gint);

   type Cb_GObject_Gtk_Print_Context_Gint_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Page_Nr : Glib.Gint);

   Signal_Draw_Page : constant Glib.Signal_Name := "draw-page";
   procedure On_Draw_Page
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Void;
       After : Boolean := False);
   procedure On_Draw_Page
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted for every page that is printed. The signal handler must render
   --  the Page_Nr's page onto the cairo context obtained from Context using
   --  Gtk.Print_Context.Get_Cairo_Context. |[<!-- language="C" --> static void
   --  draw_page (GtkPrintOperation *operation, GtkPrintContext *context, gint
   --  page_nr, gpointer user_data) { cairo_t *cr; PangoLayout *layout; gdouble
   --  width, text_height; gint layout_height; PangoFontDescription *desc; cr =
   --  gtk_print_context_get_cairo_context (context); width =
   --  gtk_print_context_get_width (context); cairo_rectangle (cr, 0, 0, width,
   --  HEADER_HEIGHT); cairo_set_source_rgb (cr, 0.8, 0.8, 0.8); cairo_fill
   --  (cr); layout = gtk_print_context_create_pango_layout (context); desc =
   --  pango_font_description_from_string ("sans 14");
   --  pango_layout_set_font_description (layout, desc);
   --  pango_font_description_free (desc); pango_layout_set_text (layout, "some
   --  text", -1); pango_layout_set_width (layout, width * PANGO_SCALE);
   --  pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);
   --  pango_layout_get_size (layout, NULL, &layout_height); text_height =
   --  (gdouble)layout_height / PANGO_SCALE; cairo_move_to (cr, width / 2,
   --  (HEADER_HEIGHT - text_height) / 2); pango_cairo_show_layout (cr,
   --  layout); g_object_unref (layout); } ]|
   --
   --  Use Gtk.Print_Operation.Set_Use_Full_Page and
   --  Gtk.Print_Operation.Set_Unit before starting the print operation to set
   --  up the transformation of the cairo context according to your needs.
   -- 
   --  Callback parameters:
   --    --  "context": the Gtk.Print_Context.Gtk_Print_Context for the current
   --    --  operation
   --    --  "page_nr": the number of the currently printed page (0-based)

   Signal_End_Print : constant Glib.Signal_Name := "end-print";
   procedure On_End_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Void;
       After : Boolean := False);
   procedure On_End_Print
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted after all pages have been rendered. A handler for this signal
   --  can clean up any resources that have been allocated in the
   --  Gtk.Print_Operation.Gtk_Print_Operation::begin-print handler.

   type Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean is not null access function
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class)
   return Boolean;

   type Cb_GObject_Gtk_Print_Context_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class)
   return Boolean;

   Signal_Paginate : constant Glib.Signal_Name := "paginate";
   procedure On_Paginate
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Boolean;
       After : Boolean := False);
   procedure On_Paginate
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted after the Gtk.Print_Operation.Gtk_Print_Operation::begin-print
   --  signal, but before the actual rendering starts. It keeps getting emitted
   --  until a connected signal handler returns True.
   --
   --  The ::paginate signal is intended to be used for paginating a document
   --  in small chunks, to avoid blocking the user interface for a long time.
   --  The signal handler should update the number of pages using
   --  Gtk.Print_Operation.Set_N_Pages, and return True if the document has
   --  been completely paginated.
   --
   --  If you don't need to do pagination in chunks, you can simply do it all
   --  in the ::begin-print handler, and set the number of pages from there.
   -- 
   --  Callback parameters:
   --    --  "context": the Gtk.Print_Context.Gtk_Print_Context for the current
   --    --  operation
   --    --  Returns True if pagination is complete

   type Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean is not null access function
     (Self    : access Gtk_Print_Operation_Record'Class;
      Preview : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class)
   return Boolean;

   type Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Preview : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Parent  : access Gtk.Window.Gtk_Window_Record'Class)
   return Boolean;

   Signal_Preview : constant Glib.Signal_Name := "preview";
   procedure On_Preview
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       After : Boolean := False);
   procedure On_Preview
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Window_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when a preview is requested from the native dialog.
   --
   --  The default handler for this signal uses an external viewer application
   --  to preview.
   --
   --  To implement a custom print preview, an application must return True
   --  from its handler for this signal. In order to use the provided Context
   --  for the preview implementation, it must be given a suitable cairo
   --  context with Gtk.Print_Context.Set_Cairo_Context.
   --
   --  The custom preview implementation can use
   --  Gtk.Print_Operation_Preview.Is_Selected and
   --  Gtk.Print_Operation_Preview.Render_Page to find pages which are selected
   --  for print and render them. The preview must be finished by calling
   --  Gtk.Print_Operation_Preview.End_Preview (typically in response to the
   --  user clicking a close button).
   -- 
   --  Callback parameters:
   --    --  "preview": the Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview
   --    --  for the current operation
   --    --  "context": the Gtk.Print_Context.Gtk_Print_Context that will be used
   --    --  "parent": the Gtk.Window.Gtk_Window to use as window parent, or null
   --    --  Returns True if the listener wants to take over control of the preview

   type Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void is not null access procedure
     (Self    : access Gtk_Print_Operation_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Page_Nr : Glib.Gint;
      Setup   : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);

   type Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Page_Nr : Glib.Gint;
      Setup   : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);

   Signal_Request_Page_Setup : constant Glib.Signal_Name := "request-page-setup";
   procedure On_Request_Page_Setup
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       After : Boolean := False);
   procedure On_Request_Page_Setup
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Print_Context_Gint_Gtk_Page_Setup_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted once for every page that is printed, to give the application a
   --  chance to modify the page setup. Any changes done to Setup will be in
   --  force only for printing this page.
   -- 
   --  Callback parameters:
   --    --  "context": the Gtk.Print_Context.Gtk_Print_Context for the current
   --    --  operation
   --    --  "page_nr": the number of the currently printed page (0-based)
   --    --  "setup": the Gtk.Page_Setup.Gtk_Page_Setup

   type Cb_Gtk_Print_Operation_Void is not null access procedure
     (Self : access Gtk_Print_Operation_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Status_Changed : constant Glib.Signal_Name := "status-changed";
   procedure On_Status_Changed
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Void;
       After : Boolean := False);
   procedure On_Status_Changed
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted at between the various phases of the print operation. See
   --  Gtk.Print_Operation.Gtk_Print_Status for the phases that are being
   --  discriminated. Use Gtk.Print_Operation.Get_Status to find out the
   --  current status.

   type Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void is not null access procedure
     (Self     : access Gtk_Print_Operation_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class);

   type Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Setup    : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class;
      Settings : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class);

   Signal_Update_Custom_Widget : constant Glib.Signal_Name := "update-custom-widget";
   procedure On_Update_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_Gtk_Print_Operation_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       After : Boolean := False);
   procedure On_Update_Custom_Widget
      (Self  : not null access Gtk_Print_Operation_Record;
       Call  : Cb_GObject_Gtk_Widget_Gtk_Page_Setup_Gtk_Print_Settings_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted after change of selected printer. The actual page setup and
   --  print settings are passed to the custom widget, which can actualize
   --  itself according to this change.
   -- 
   --  Callback parameters:
   --    --  "widget": the custom widget added in create-custom-widget
   --    --  "setup": actual page setup
   --    --  "settings": actual print settings

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "PrintOperationPreview"

   package Implements_Gtk_Print_Operation_Preview is new Glib.Types.Implements
     (Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview, Gtk_Print_Operation_Record, Gtk_Print_Operation);
   function "+"
     (Widget : access Gtk_Print_Operation_Record'Class)
   return Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview
   renames Implements_Gtk_Print_Operation_Preview.To_Interface;
   function "-"
     (Interf : Gtk.Print_Operation_Preview.Gtk_Print_Operation_Preview)
   return Gtk_Print_Operation
   renames Implements_Gtk_Print_Operation_Preview.To_Object;

private
   Use_Full_Page_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-full-page");
   Unit_Property : constant Gtk.Enums.Property_Gtk_Unit :=
     Gtk.Enums.Build ("unit");
   Track_Print_Status_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("track-print-status");
   Support_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("support-selection");
   Status_String_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("status-string");
   Status_Property : constant Gtk.Print_Operation.Property_Gtk_Print_Status :=
     Gtk.Print_Operation.Build ("status");
   Show_Progress_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-progress");
   Print_Settings_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("print-settings");
   N_Pages_To_Print_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("n-pages-to-print");
   N_Pages_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("n-pages");
   Job_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("job-name");
   Has_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-selection");
   Export_Filename_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("export-filename");
   Embed_Page_Setup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("embed-page-setup");
   Default_Page_Setup_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("default-page-setup");
   Custom_Tab_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("custom-tab-label");
   Current_Page_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("current-page");
   Allow_Async_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow-async");
end Gtk.Print_Operation;
