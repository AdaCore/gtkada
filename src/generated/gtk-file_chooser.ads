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
--  Gtk.File_Chooser.Gtk_File_Chooser is an interface that can be implemented
--  by file selection widgets. In GTK+, the main objects that implement this
--  interface are Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget,
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog, and
--  Gtk.File_Chooser_Button.Gtk_File_Chooser_Button. You do not need to write
--  an object that implements the Gtk.File_Chooser.Gtk_File_Chooser interface
--  unless you are trying to adapt an existing file selector to expose a
--  standard programming interface.
--
--  Gtk.File_Chooser.Gtk_File_Chooser allows for shortcuts to various places
--  in the filesystem. In the default implementation these are displayed in the
--  left pane. It may be a bit confusing at first that these shortcuts come
--  from various sources and in various flavours, so lets explain the
--  terminology here:
--
--  - Bookmarks: are created by the user, by dragging folders from the right
--  pane to the left pane, or by using the "Add". Bookmarks can be renamed and
--  deleted by the user.
--
--  - Shortcuts: can be provided by the application. For example, a Paint
--  program may want to add a shortcut for a Clipart folder. Shortcuts cannot
--  be modified by the user.
--
--  - Volumes: are provided by the underlying filesystem abstraction. They are
--  the "roots" of the filesystem.
--
--  # File Names and Encodings
--
--  When the user is finished selecting files in a
--  Gtk.File_Chooser.Gtk_File_Chooser, your program can get the selected names
--  either as filenames or as URIs. For URIs, the normal escaping rules are
--  applied if the URI contains non-ASCII characters. However, filenames are
--  always returned in the character set specified by the `G_FILENAME_ENCODING`
--  environment variable. Please see the GLib documentation for more details
--  about this variable.
--
--  This means that while you can pass the result of
--  Gtk.File_Chooser.Get_Filename to g_open or g_fopen, you may not be able to
--  directly set it as the text of a Gtk.Label.Gtk_Label widget unless you
--  convert it first to UTF-8, which all GTK+ widgets expect. You should use
--  g_filename_to_utf8 to convert filenames into strings that can be passed to
--  GTK+ widgets.
--
--  # Adding a Preview Widget
--
--  You can add a custom preview widget to a file chooser and then get
--  notification about when the preview needs to be updated. To install a
--  preview widget, use Gtk.File_Chooser.Set_Preview_Widget. Then, connect to
--  the Gtk.File_Chooser.Gtk_File_Chooser::update-preview signal to get
--  notified when you need to update the contents of the preview.
--
--  Your callback should use Gtk.File_Chooser.Get_Preview_Filename to see what
--  needs previewing. Once you have generated the preview for the corresponding
--  file, you must call Gtk.File_Chooser.Set_Preview_Widget_Active with a
--  boolean flag that indicates whether your callback could successfully
--  generate a preview.
--
--  ## Example: Using a Preview Widget ## {gtkfilechooser-preview} |[<!--
--  language="C" --> { GtkImage *preview;
--
--  ...
--
--  preview = gtk_image_new ();
--
--  gtk_file_chooser_set_preview_widget (my_file_chooser, preview);
--  g_signal_connect (my_file_chooser, "update-preview", G_CALLBACK
--  (update_preview_cb), preview); }
--
--  static void update_preview_cb (GtkFileChooser *file_chooser, gpointer
--  data) { GtkWidget *preview; char *filename; GdkPixbuf *pixbuf; gboolean
--  have_preview;
--
--  preview = GTK_WIDGET (data); filename =
--  gtk_file_chooser_get_preview_filename (file_chooser);
--
--  pixbuf = gdk_pixbuf_new_from_file_at_size (filename, 128, 128, NULL);
--  have_preview = (pixbuf != NULL); g_free (filename);
--
--  gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf); if (pixbuf)
--  g_object_unref (pixbuf);
--
--  gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview); }
--  ]|
--
--  # Adding Extra Widgets
--
--  You can add extra widgets to a file chooser to provide options that are
--  not present in the default design. For example, you can add a toggle button
--  to give the user the option to open a file in read-only mode. You can use
--  Gtk.File_Chooser.Set_Extra_Widget to insert additional widgets in a file
--  chooser.
--
--  An example for adding extra widgets: |[<!-- language="C" -->
--
--  GtkWidget *toggle;
--
--  ...
--
--  toggle = gtk_check_button_new_with_label ("Open file read-only");
--  gtk_widget_show (toggle); gtk_file_chooser_set_extra_widget
--  (my_file_chooser, toggle); } ]|
--
--  If you want to set more than one extra widget in the file chooser, you can
--  a container such as a Gtk.Box.Gtk_Box or a Gtk.Grid.Gtk_Grid and include
--  your widgets in it. Then, set the container as the whole extra widget.
--
--  </description>
--  <group></group>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.File_Filter;         use Gtk.File_Filter;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.File_Chooser is

   type Gtk_File_Chooser is new Glib.Types.GType_Interface;
   Null_Gtk_File_Chooser : constant Gtk_File_Chooser;

   type Gtk_File_Chooser_Action is (
      Action_Open,
      Action_Save,
      Action_Select_Folder,
      Action_Create_Folder);
   pragma Convention (C, Gtk_File_Chooser_Action);
   --  Describes whether a Gtk.File_Chooser.Gtk_File_Chooser is being used to
   --  open existing files or to save to a possibly new file.

   type Gtk_File_Chooser_Confirmation is (
      Confirmation_Confirm,
      Confirmation_Accept_Filename,
      Confirmation_Select_Again);
   pragma Convention (C, Gtk_File_Chooser_Confirmation);
   --  Used as a return value of handlers for the
   --  Gtk.File_Chooser.Gtk_File_Chooser::confirm-overwrite signal of a
   --  Gtk.File_Chooser.Gtk_File_Chooser. This value determines whether the
   --  file chooser will present the stock confirmation dialog, accept the
   --  user's choice of a filename, or let the user choose another filename.

   type Gtk_File_Chooser_Error is (
      Error_Nonexistent,
      Error_Bad_Filename,
      Error_Already_Exists,
      Error_Incomplete_Hostname);
   pragma Convention (C, Gtk_File_Chooser_Error);
   --  These identify the various errors that can occur while calling
   --  Gtk.File_Chooser.Gtk_File_Chooser functions.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_File_Chooser_Action_Properties is
      new Generic_Internal_Discrete_Property (Gtk_File_Chooser_Action);
   type Property_Gtk_File_Chooser_Action is new Gtk_File_Chooser_Action_Properties.Property;

   package Gtk_File_Chooser_Confirmation_Properties is
      new Generic_Internal_Discrete_Property (Gtk_File_Chooser_Confirmation);
   type Property_Gtk_File_Chooser_Confirmation is new Gtk_File_Chooser_Confirmation_Properties.Property;

   package Gtk_File_Chooser_Error_Properties is
      new Generic_Internal_Discrete_Property (Gtk_File_Chooser_Error);
   type Property_Gtk_File_Chooser_Error is new Gtk_File_Chooser_Error_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_chooser_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Choice
      (Chooser       : Gtk_File_Chooser;
       Id            : UTF8_String;
       Label         : UTF8_String;
       Options       : GNAT.Strings.String_List;
       Option_Labels : GNAT.Strings.String_List);
   --  Adds a 'choice' to the file chooser. This is typically implemented as a
   --  combobox or, for boolean choices, as a checkbutton. You can select a
   --  value using Gtk.File_Chooser.Set_Choice before the dialog is shown, and
   --  you can obtain the user-selected value in the ::response signal handler
   --  using Gtk.File_Chooser.Get_Choice.
   --  Compare Gtk.File_Chooser.Set_Extra_Widget.
   --  Since: gtk+ 3.22
   --  "id": id for the added choice
   --  "label": user-visible label for the added choice
   --  "options": ids for the options of the choice, or null for a boolean
   --  choice
   --  "option_labels": user-visible labels for the options, must be the same
   --  length as Options

   procedure Add_Filter
      (Chooser : Gtk_File_Chooser;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   --  Adds Filter to the list of filters that the user can select between.
   --  When a filter is selected, only files that are passed by that filter are
   --  displayed.
   --  Note that the Chooser takes ownership of the filter, so you have to ref
   --  and sink it if you want to keep a reference.
   --  Since: gtk+ 2.4
   --  "filter": a Gtk.File_Filter.Gtk_File_Filter

   function Add_Shortcut_Folder
      (Chooser : Gtk_File_Chooser;
       Folder  : UTF8_String) return Boolean;
   --  Adds a folder to be displayed with the shortcut folders in a file
   --  chooser. Note that shortcut folders do not get saved, as they are
   --  provided by the application. For example, you can use this to add a
   --  "/usr/share/mydrawprogram/Clipart" folder to the volume list.
   --  Since: gtk+ 2.4
   --  "folder": filename of the folder to add

   function Add_Shortcut_Folder_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Adds a folder URI to be displayed with the shortcut folders in a file
   --  chooser. Note that shortcut folders do not get saved, as they are
   --  provided by the application. For example, you can use this to add a
   --  "file:///usr/share/mydrawprogram/Clipart" folder to the volume list.
   --  Since: gtk+ 2.4
   --  "uri": URI of the folder to add

   function Get_Action
      (Chooser : Gtk_File_Chooser) return Gtk_File_Chooser_Action;
   pragma Import (C, Get_Action, "gtk_file_chooser_get_action");
   --  Gets the type of operation that the file chooser is performing; see
   --  Gtk.File_Chooser.Set_Action.
   --  Since: gtk+ 2.4

   procedure Set_Action
      (Chooser : Gtk_File_Chooser;
       Action  : Gtk_File_Chooser_Action);
   pragma Import (C, Set_Action, "gtk_file_chooser_set_action");
   --  Sets the type of operation that the chooser is performing; the user
   --  interface is adapted to suit the selected action. For example, an option
   --  to create a new folder might be shown if the action is
   --  Gtk.File_Chooser.Action_Save but not if the action is
   --  Gtk.File_Chooser.Action_Open.
   --  Since: gtk+ 2.4
   --  "action": the action that the file selector is performing

   function Get_Choice
      (Chooser : Gtk_File_Chooser;
       Id      : UTF8_String) return UTF8_String;
   --  Gets the currently selected option in the 'choice' with the given ID.
   --  Since: gtk+ 3.22
   --  "id": the ID of the choice to get

   procedure Set_Choice
      (Chooser : Gtk_File_Chooser;
       Id      : UTF8_String;
       Option  : UTF8_String);
   --  Selects an option in a 'choice' that has been added with
   --  Gtk.File_Chooser.Add_Choice. For a boolean choice, the possible options
   --  are "true" and "false".
   --  Since: gtk+ 3.22
   --  "id": the ID of the choice to set
   --  "option": the ID of the option to select

   function Get_Create_Folders (Chooser : Gtk_File_Chooser) return Boolean;
   --  Gets whether file choser will offer to create new folders. See
   --  Gtk.File_Chooser.Set_Create_Folders.
   --  Since: gtk+ 2.18

   procedure Set_Create_Folders
      (Chooser        : Gtk_File_Chooser;
       Create_Folders : Boolean);
   --  Sets whether file choser will offer to create new folders. This is only
   --  relevant if the action is not set to be Gtk.File_Chooser.Action_Open.
   --  Since: gtk+ 2.18
   --  "create_folders": True if the Create Folder button should be displayed

   function Get_Current_Folder
      (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the current folder of Chooser as a local filename. See
   --  Gtk.File_Chooser.Set_Current_Folder.
   --  Note that this is the folder that the file chooser is currently
   --  displaying (e.g. "/home/username/Documents"), which is not the same as
   --  the currently-selected folder if the chooser is in
   --  Gtk.File_Chooser.Action_Select_Folder mode (e.g.
   --  "/home/username/Documents/selected-folder/". To get the
   --  currently-selected folder in that mode, use Gtk.File_Chooser.Get_Uri as
   --  the usual way to get the selection.
   --  Since: gtk+ 2.4

   function Set_Current_Folder
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean;
   --  Sets the current folder for Chooser from a local filename. The user
   --  will be shown the full contents of the current folder, plus user
   --  interface elements for navigating to other folders.
   --  In general, you should not use this function. See the [section on
   --  setting up a file chooser dialog][gtkfilechooserdialog-setting-up] for
   --  the rationale behind this.
   --  Since: gtk+ 2.4
   --  "filename": the full path of the new current folder

   function Get_Current_Folder_Uri
      (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the current folder of Chooser as an URI. See
   --  Gtk.File_Chooser.Set_Current_Folder_Uri.
   --  Note that this is the folder that the file chooser is currently
   --  displaying (e.g. "file:///home/username/Documents"), which is not the
   --  same as the currently-selected folder if the chooser is in
   --  Gtk.File_Chooser.Action_Select_Folder mode (e.g.
   --  "file:///home/username/Documents/selected-folder/". To get the
   --  currently-selected folder in that mode, use Gtk.File_Chooser.Get_Uri as
   --  the usual way to get the selection.
   --  Since: gtk+ 2.4

   function Set_Current_Folder_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Sets the current folder for Chooser from an URI. The user will be shown
   --  the full contents of the current folder, plus user interface elements
   --  for navigating to other folders.
   --  In general, you should not use this function. See the [section on
   --  setting up a file chooser dialog][gtkfilechooserdialog-setting-up] for
   --  the rationale behind this.
   --  Since: gtk+ 2.4
   --  "uri": the URI for the new current folder

   function Get_Current_Name (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the current name in the file selector, as entered by the user in
   --  the text entry for "Name".
   --  This is meant to be used in save dialogs, to get the currently typed
   --  filename when the file itself does not exist yet. For example, an
   --  application that adds a custom extra widget to the file chooser for
   --  "file format" may want to change the extension of the typed filename
   --  based on the chosen format, say, from ".jpg" to ".png".
   --  Since: gtk+ 3.10

   procedure Set_Current_Name
      (Chooser : Gtk_File_Chooser;
       Name    : UTF8_String);
   --  Sets the current name in the file selector, as if entered by the user.
   --  Note that the name passed in here is a UTF-8 string rather than a
   --  filename. This function is meant for such uses as a suggested name in a
   --  "Save As..." dialog. You can pass "Untitled.doc" or a similarly suitable
   --  suggestion for the Name.
   --  If you want to preselect a particular existing file, you should use
   --  Gtk.File_Chooser.Set_Filename or Gtk.File_Chooser.Set_Uri instead.
   --  Please see the documentation for those functions for an example of using
   --  Gtk.File_Chooser.Set_Current_Name as well.
   --  Since: gtk+ 2.4
   --  "name": the filename to use, as a UTF-8 string

   function Get_Do_Overwrite_Confirmation
      (Chooser : Gtk_File_Chooser) return Boolean;
   --  Queries whether a file chooser is set to confirm for overwriting when
   --  the user types a file name that already exists.
   --  Since: gtk+ 2.8

   procedure Set_Do_Overwrite_Confirmation
      (Chooser                   : Gtk_File_Chooser;
       Do_Overwrite_Confirmation : Boolean);
   --  Sets whether a file chooser in Gtk.File_Chooser.Action_Save mode will
   --  present a confirmation dialog if the user types a file name that already
   --  exists. This is False by default.
   --  If set to True, the Chooser will emit the
   --  Gtk.File_Chooser.Gtk_File_Chooser::confirm-overwrite signal when
   --  appropriate.
   --  If all you need is the stock confirmation dialog, set this property to
   --  True. You can override the way confirmation is done by actually handling
   --  the Gtk.File_Chooser.Gtk_File_Chooser::confirm-overwrite signal; please
   --  refer to its documentation for the details.
   --  Since: gtk+ 2.8
   --  "do_overwrite_confirmation": whether to confirm overwriting in save
   --  mode

   function Get_Extra_Widget
      (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget;
   --  Gets the current extra widget; see Gtk.File_Chooser.Set_Extra_Widget.
   --  Since: gtk+ 2.4

   procedure Set_Extra_Widget
      (Chooser      : Gtk_File_Chooser;
       Extra_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets an application-supplied widget to provide extra options to the
   --  user.
   --  Since: gtk+ 2.4
   --  "extra_widget": widget for extra options

   function Get_Filename (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the filename for the currently selected file in the file selector.
   --  The filename is returned as an absolute path. If multiple files are
   --  selected, one of the filenames will be returned at random.
   --  If the file chooser is in folder mode, this function returns the
   --  selected folder.
   --  Since: gtk+ 2.4

   function Set_Filename
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean;
   --  Sets Filename as the current filename for the file chooser, by changing
   --  to the file's parent folder and actually selecting the file in list; all
   --  other files will be unselected. If the Chooser is in
   --  Gtk.File_Chooser.Action_Save mode, the file's base name will also appear
   --  in the dialog's file name entry.
   --  Note that the file must exist, or nothing will be done except for the
   --  directory change.
   --  You should use this function only when implementing a save dialog for
   --  which you already have a file name to which the user may save. For
   --  example, when the user opens an existing file and then does Save As...
   --  to save a copy or a modified version. If you don't have a file name
   --  already — for example, if the user just created a new file and is saving
   --  it for the first time, do not call this function. Instead, use something
   --  similar to this: |[<!-- language="C" --> if (document_is_new) { // the
   --  user just created a new document gtk_file_chooser_set_current_name
   --  (chooser, "Untitled document"); } else { // the user edited an existing
   --  document gtk_file_chooser_set_filename (chooser, existing_filename); }
   --  ]|
   --  In the first case, the file chooser will present the user with useful
   --  suggestions as to where to save his new file. In the second case, the
   --  file's existing location is already known, so the file chooser will use
   --  it.
   --  Since: gtk+ 2.4
   --  "filename": the filename to set as current

   function Get_Filenames
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Lists all the selected files and subfolders in the current folder of
   --  Chooser. The returned names are full absolute paths. If files in the
   --  current folder cannot be represented as local filenames they will be
   --  ignored. (See Gtk.File_Chooser.Get_Uris)
   --  Since: gtk+ 2.4

   function Get_Filter
      (Chooser : Gtk_File_Chooser) return Gtk.File_Filter.Gtk_File_Filter;
   --  Gets the current filter; see Gtk.File_Chooser.Set_Filter.
   --  Since: gtk+ 2.4

   procedure Set_Filter
      (Chooser : Gtk_File_Chooser;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   --  Sets the current filter; only the files that pass the filter will be
   --  displayed. If the user-selectable list of filters is non-empty, then the
   --  filter should be one of the filters in that list. Setting the current
   --  filter when the list of filters is empty is useful if you want to
   --  restrict the displayed set of files without letting the user change it.
   --  Since: gtk+ 2.4
   --  "filter": a Gtk.File_Filter.Gtk_File_Filter

   function Get_Local_Only (Chooser : Gtk_File_Chooser) return Boolean;
   --  Gets whether only local files can be selected in the file selector. See
   --  Gtk.File_Chooser.Set_Local_Only
   --  Since: gtk+ 2.4

   procedure Set_Local_Only
      (Chooser    : Gtk_File_Chooser;
       Local_Only : Boolean);
   --  Sets whether only local files can be selected in the file selector. If
   --  Local_Only is True (the default), then the selected file or files are
   --  guaranteed to be accessible through the operating systems native file
   --  system and therefore the application only needs to worry about the
   --  filename functions in Gtk.File_Chooser.Gtk_File_Chooser, like
   --  Gtk.File_Chooser.Get_Filename, rather than the URI functions like
   --  Gtk.File_Chooser.Get_Uri,
   --  On some systems non-native files may still be available using the
   --  native filesystem via a userspace filesystem (FUSE).
   --  Since: gtk+ 2.4
   --  "local_only": True if only local files can be selected

   function Get_Preview_Filename
      (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the filename that should be previewed in a custom preview widget.
   --  See Gtk.File_Chooser.Set_Preview_Widget.
   --  Since: gtk+ 2.4

   function Get_Preview_Uri (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the URI that should be previewed in a custom preview widget. See
   --  Gtk.File_Chooser.Set_Preview_Widget.
   --  Since: gtk+ 2.4

   function Get_Preview_Widget
      (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget;
   --  Gets the current preview widget; see
   --  Gtk.File_Chooser.Set_Preview_Widget.
   --  Since: gtk+ 2.4

   procedure Set_Preview_Widget
      (Chooser        : Gtk_File_Chooser;
       Preview_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets an application-supplied widget to use to display a custom preview
   --  of the currently selected file. To implement a preview, after setting
   --  the preview widget, you connect to the
   --  Gtk.File_Chooser.Gtk_File_Chooser::update-preview signal, and call
   --  Gtk.File_Chooser.Get_Preview_Filename or
   --  Gtk.File_Chooser.Get_Preview_Uri on each change. If you can display a
   --  preview of the new file, update your widget and set the preview active
   --  using Gtk.File_Chooser.Set_Preview_Widget_Active. Otherwise, set the
   --  preview inactive.
   --  When there is no application-supplied preview widget, or the
   --  application-supplied preview widget is not active, the file chooser will
   --  display no preview at all.
   --  Since: gtk+ 2.4
   --  "preview_widget": widget for displaying preview.

   function Get_Preview_Widget_Active
      (Chooser : Gtk_File_Chooser) return Boolean;
   --  Gets whether the preview widget set by
   --  Gtk.File_Chooser.Set_Preview_Widget should be shown for the current
   --  filename. See Gtk.File_Chooser.Set_Preview_Widget_Active.
   --  Since: gtk+ 2.4

   procedure Set_Preview_Widget_Active
      (Chooser : Gtk_File_Chooser;
       Active  : Boolean);
   --  Sets whether the preview widget set by
   --  Gtk.File_Chooser.Set_Preview_Widget should be shown for the current
   --  filename. When Active is set to false, the file chooser may display an
   --  internally generated preview of the current file or it may display no
   --  preview at all. See Gtk.File_Chooser.Set_Preview_Widget for more
   --  details.
   --  Since: gtk+ 2.4
   --  "active": whether to display the user-specified preview widget

   function Get_Select_Multiple (Chooser : Gtk_File_Chooser) return Boolean;
   --  Gets whether multiple files can be selected in the file selector. See
   --  Gtk.File_Chooser.Set_Select_Multiple.
   --  Since: gtk+ 2.4

   procedure Set_Select_Multiple
      (Chooser         : Gtk_File_Chooser;
       Select_Multiple : Boolean);
   --  Sets whether multiple files can be selected in the file selector. This
   --  is only relevant if the action is set to be Gtk.File_Chooser.Action_Open
   --  or Gtk.File_Chooser.Action_Select_Folder.
   --  Since: gtk+ 2.4
   --  "select_multiple": True if multiple files can be selected.

   function Get_Show_Hidden (Chooser : Gtk_File_Chooser) return Boolean;
   --  Gets whether hidden files and folders are displayed in the file
   --  selector. See Gtk.File_Chooser.Set_Show_Hidden.
   --  Since: gtk+ 2.6

   procedure Set_Show_Hidden
      (Chooser     : Gtk_File_Chooser;
       Show_Hidden : Boolean);
   --  Sets whether hidden files and folders are displayed in the file
   --  selector.
   --  Since: gtk+ 2.6
   --  "show_hidden": True if hidden files and folders should be displayed.

   function Get_Uri (Chooser : Gtk_File_Chooser) return UTF8_String;
   --  Gets the URI for the currently selected file in the file selector. If
   --  multiple files are selected, one of the filenames will be returned at
   --  random.
   --  If the file chooser is in folder mode, this function returns the
   --  selected folder.
   --  Since: gtk+ 2.4

   function Set_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Sets the file referred to by Uri as the current file for the file
   --  chooser, by changing to the URI's parent folder and actually selecting
   --  the URI in the list. If the Chooser is Gtk.File_Chooser.Action_Save
   --  mode, the URI's base name will also appear in the dialog's file name
   --  entry.
   --  Note that the URI must exist, or nothing will be done except for the
   --  directory change.
   --  You should use this function only when implementing a save dialog for
   --  which you already have a file name to which the user may save. For
   --  example, when the user opens an existing file and then does Save As...
   --  to save a copy or a modified version. If you don't have a file name
   --  already — for example, if the user just created a new file and is saving
   --  it for the first time, do not call this function. Instead, use something
   --  similar to this: |[<!-- language="C" --> if (document_is_new) { // the
   --  user just created a new document gtk_file_chooser_set_current_name
   --  (chooser, "Untitled document"); } else { // the user edited an existing
   --  document gtk_file_chooser_set_uri (chooser, existing_uri); } ]|
   --  In the first case, the file chooser will present the user with useful
   --  suggestions as to where to save his new file. In the second case, the
   --  file's existing location is already known, so the file chooser will use
   --  it.
   --  Since: gtk+ 2.4
   --  "uri": the URI to set as current

   function Get_Uris
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Lists all the selected files and subfolders in the current folder of
   --  Chooser. The returned names are full absolute URIs.
   --  Since: gtk+ 2.4

   function Get_Use_Preview_Label
      (Chooser : Gtk_File_Chooser) return Boolean;
   --  Gets whether a stock label should be drawn with the name of the
   --  previewed file. See Gtk.File_Chooser.Set_Use_Preview_Label.

   procedure Set_Use_Preview_Label
      (Chooser   : Gtk_File_Chooser;
       Use_Label : Boolean);
   --  Sets whether the file chooser should display a stock label with the
   --  name of the file that is being previewed; the default is True.
   --  Applications that want to draw the whole preview area themselves should
   --  set this to False and display the name themselves in their preview
   --  widget.
   --  See also: Gtk.File_Chooser.Set_Preview_Widget
   --  Since: gtk+ 2.4
   --  "use_label": whether to display a stock label with the name of the
   --  previewed file

   function List_Filters
      (Chooser : Gtk_File_Chooser) return Glib.Object.Object_List.GSlist;
   --  Lists the current set of user-selectable filters; see
   --  Gtk.File_Chooser.Add_Filter, Gtk.File_Chooser.Remove_Filter.
   --  Since: gtk+ 2.4

   function List_Shortcut_Folder_Uris
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Queries the list of shortcut folders in the file chooser, as set by
   --  Gtk.File_Chooser.Add_Shortcut_Folder_Uri.
   --  Since: gtk+ 2.4

   function List_Shortcut_Folders
      (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist;
   --  Queries the list of shortcut folders in the file chooser, as set by
   --  Gtk.File_Chooser.Add_Shortcut_Folder.
   --  Since: gtk+ 2.4

   procedure Remove_Choice (Chooser : Gtk_File_Chooser; Id : UTF8_String);
   --  Removes a 'choice' that has been added with
   --  Gtk.File_Chooser.Add_Choice.
   --  Since: gtk+ 3.22
   --  "id": the ID of the choice to remove

   procedure Remove_Filter
      (Chooser : Gtk_File_Chooser;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);
   --  Removes Filter from the list of filters that the user can select
   --  between.
   --  Since: gtk+ 2.4
   --  "filter": a Gtk.File_Filter.Gtk_File_Filter

   function Remove_Shortcut_Folder
      (Chooser : Gtk_File_Chooser;
       Folder  : UTF8_String) return Boolean;
   --  Removes a folder from a file chooser's list of shortcut folders.
   --  Since: gtk+ 2.4
   --  "folder": filename of the folder to remove

   function Remove_Shortcut_Folder_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Removes a folder URI from a file chooser's list of shortcut folders.
   --  Since: gtk+ 2.4
   --  "uri": URI of the folder to remove

   procedure Select_All (Chooser : Gtk_File_Chooser);
   pragma Import (C, Select_All, "gtk_file_chooser_select_all");
   --  Selects all the files in the current folder of a file chooser.
   --  Since: gtk+ 2.4

   function Select_Filename
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String) return Boolean;
   --  Selects a filename. If the file name isn't in the current folder of
   --  Chooser, then the current folder of Chooser will be changed to the
   --  folder containing Filename.
   --  Since: gtk+ 2.4
   --  "filename": the filename to select

   function Select_Uri
      (Chooser : Gtk_File_Chooser;
       URI     : UTF8_String) return Boolean;
   --  Selects the file to by Uri. If the URI doesn't refer to a file in the
   --  current folder of Chooser, then the current folder of Chooser will be
   --  changed to the folder containing Filename.
   --  Since: gtk+ 2.4
   --  "uri": the URI to select

   procedure Unselect_All (Chooser : Gtk_File_Chooser);
   pragma Import (C, Unselect_All, "gtk_file_chooser_unselect_all");
   --  Unselects all the files in the current folder of a file chooser.
   --  Since: gtk+ 2.4

   procedure Unselect_Filename
      (Chooser  : Gtk_File_Chooser;
       Filename : UTF8_String);
   --  Unselects a currently selected filename. If the filename is not in the
   --  current directory, does not exist, or is otherwise not currently
   --  selected, does nothing.
   --  Since: gtk+ 2.4
   --  "filename": the filename to unselect

   procedure Unselect_Uri (Chooser : Gtk_File_Chooser; URI : UTF8_String);
   --  Unselects the file referred to by Uri. If the file is not in the
   --  current directory, does not exist, or is otherwise not currently
   --  selected, does nothing.
   --  Since: gtk+ 2.4
   --  "uri": the URI to unselect

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Action_Property : constant Gtk.File_Chooser.Property_Gtk_File_Chooser_Action;
   --  Type: Gtk_File_Chooser_Action

   Create_Folders_Property : constant Glib.Properties.Property_Boolean;
   --  Whether a file chooser not in Gtk.File_Chooser.Action_Open mode will
   --  offer the user to create new folders.

   Do_Overwrite_Confirmation_Property : constant Glib.Properties.Property_Boolean;
   --  Whether a file chooser in Gtk.File_Chooser.Action_Save mode will
   --  present an overwrite confirmation dialog if the user selects a file name
   --  that already exists.

   Extra_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Filter_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.File_Filter.Gtk_File_Filter

   Local_Only_Property : constant Glib.Properties.Property_Boolean;

   Preview_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Preview_Widget_Active_Property : constant Glib.Properties.Property_Boolean;

   Select_Multiple_Property : constant Glib.Properties.Property_Boolean;

   Show_Hidden_Property : constant Glib.Properties.Property_Boolean;

   Use_Preview_Label_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_File_Chooser_Gtk_File_Chooser_Confirmation is not null access function
     (Self : Gtk_File_Chooser)
   return Gtk_File_Chooser_Confirmation;

   type Cb_GObject_Gtk_File_Chooser_Confirmation is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Gtk_File_Chooser_Confirmation;

   Signal_Confirm_Overwrite : constant Glib.Signal_Name := "confirm-overwrite";
   procedure On_Confirm_Overwrite
      (Self  : Gtk_File_Chooser;
       Call  : Cb_Gtk_File_Chooser_Gtk_File_Chooser_Confirmation;
       After : Boolean := False);
   procedure On_Confirm_Overwrite
      (Self  : Gtk_File_Chooser;
       Call  : Cb_GObject_Gtk_File_Chooser_Confirmation;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal gets emitted whenever it is appropriate to present a
   --  confirmation dialog when the user has selected a file name that already
   --  exists. The signal only gets emitted when the file chooser is in
   --  Gtk.File_Chooser.Action_Save mode.
   --
   --  Most applications just need to turn on the
   --  Gtk.File_Chooser.Gtk_File_Chooser:do-overwrite-confirmation property (or
   --  call the Gtk.File_Chooser.Set_Do_Overwrite_Confirmation function), and
   --  they will automatically get a stock confirmation dialog. Applications
   --  which need to customize this behavior should do that, and also connect
   --  to the Gtk.File_Chooser.Gtk_File_Chooser::confirm-overwrite signal.
   --
   --  A signal handler for this signal must return a
   --  Gtk.File_Chooser.Gtk_File_Chooser_Confirmation value, which indicates
   --  the action to take. If the handler determines that the user wants to
   --  select a different filename, it should return
   --  Gtk.File_Chooser.Confirmation_Select_Again. If it determines that the
   --  user is satisfied with his choice of file name, it should return
   --  Gtk.File_Chooser.Confirmation_Accept_Filename. On the other hand, if it
   --  determines that the stock confirmation dialog should be used, it should
   --  return Gtk.File_Chooser.Confirmation_Confirm. The following example
   --  illustrates this.
   --
   --  ## Custom confirmation ## {gtkfilechooser-confirmation}
   --
   --  |[<!-- language="C" --> static GtkFileChooserConfirmation
   --  confirm_overwrite_callback (GtkFileChooser *chooser, gpointer data) {
   --  char *uri;
   --
   --  uri = gtk_file_chooser_get_uri (chooser);
   --
   --  if (is_uri_read_only (uri)) { if (user_wants_to_replace_read_only_file
   --  (uri)) return GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME; else return
   --  GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN; } else return
   --  GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM; // fall back to the default
   --  dialog }
   --
   --  ...
   --
   --  chooser = gtk_file_chooser_dialog_new (...);
   --
   --  gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER
   --  (dialog), TRUE); g_signal_connect (chooser, "confirm-overwrite",
   --  G_CALLBACK (confirm_overwrite_callback), NULL);
   --
   --  if (gtk_dialog_run (chooser) == GTK_RESPONSE_ACCEPT) save_to_file
   --  (gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
   --
   --  gtk_widget_destroy (chooser); ]|
   -- 
   --  Callback parameters:
   --    --  Returns a Gtk.File_Chooser.Gtk_File_Chooser_Confirmation value that indicates which
   --  action to take after emitting the signal.

   type Cb_Gtk_File_Chooser_Void is not null access procedure (Self : Gtk_File_Chooser);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Current_Folder_Changed : constant Glib.Signal_Name := "current-folder-changed";
   procedure On_Current_Folder_Changed
      (Self  : Gtk_File_Chooser;
       Call  : Cb_Gtk_File_Chooser_Void;
       After : Boolean := False);
   procedure On_Current_Folder_Changed
      (Self  : Gtk_File_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the current folder in a
   --  Gtk.File_Chooser.Gtk_File_Chooser changes. This can happen due to the
   --  user performing some action that changes folders, such as selecting a
   --  bookmark or visiting a folder on the file list. It can also happen as a
   --  result of calling a function to explicitly change the current folder in
   --  a file chooser.
   --
   --  Normally you do not need to connect to this signal, unless you need to
   --  keep track of which folder a file chooser is showing.
   --
   --  See also: Gtk.File_Chooser.Set_Current_Folder,
   --  Gtk.File_Chooser.Get_Current_Folder,
   --  Gtk.File_Chooser.Set_Current_Folder_Uri,
   --  Gtk.File_Chooser.Get_Current_Folder_Uri.

   Signal_File_Activated : constant Glib.Signal_Name := "file-activated";
   procedure On_File_Activated
      (Self  : Gtk_File_Chooser;
       Call  : Cb_Gtk_File_Chooser_Void;
       After : Boolean := False);
   procedure On_File_Activated
      (Self  : Gtk_File_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the user "activates" a file in the file
   --  chooser. This can happen by double-clicking on a file in the file list,
   --  or by pressing `Enter`.
   --
   --  Normally you do not need to connect to this signal. It is used
   --  internally by Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog to know
   --  when to activate the default button in the dialog.
   --
   --  See also: Gtk.File_Chooser.Get_Filename,
   --  Gtk.File_Chooser.Get_Filenames, Gtk.File_Chooser.Get_Uri,
   --  Gtk.File_Chooser.Get_Uris.

   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";
   procedure On_Selection_Changed
      (Self  : Gtk_File_Chooser;
       Call  : Cb_Gtk_File_Chooser_Void;
       After : Boolean := False);
   procedure On_Selection_Changed
      (Self  : Gtk_File_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when there is a change in the set of selected
   --  files in a Gtk.File_Chooser.Gtk_File_Chooser. This can happen when the
   --  user modifies the selection with the mouse or the keyboard, or when
   --  explicitly calling functions to change the selection.
   --
   --  Normally you do not need to connect to this signal, as it is easier to
   --  wait for the file chooser to finish running, and then to get the list of
   --  selected files using the functions mentioned below.
   --
   --  See also: Gtk.File_Chooser.Select_Filename,
   --  Gtk.File_Chooser.Unselect_Filename, Gtk.File_Chooser.Get_Filename,
   --  Gtk.File_Chooser.Get_Filenames, Gtk.File_Chooser.Select_Uri,
   --  Gtk.File_Chooser.Unselect_Uri, Gtk.File_Chooser.Get_Uri,
   --  Gtk.File_Chooser.Get_Uris.

   Signal_Update_Preview : constant Glib.Signal_Name := "update-preview";
   procedure On_Update_Preview
      (Self  : Gtk_File_Chooser;
       Call  : Cb_Gtk_File_Chooser_Void;
       After : Boolean := False);
   procedure On_Update_Preview
      (Self  : Gtk_File_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the preview in a file chooser should be
   --  regenerated. For example, this can happen when the currently selected
   --  file changes. You should use this signal if you want your file chooser
   --  to have a preview widget.
   --
   --  Once you have installed a preview widget with
   --  Gtk.File_Chooser.Set_Preview_Widget, you should update it when this
   --  signal is emitted. You can use the functions
   --  Gtk.File_Chooser.Get_Preview_Filename or
   --  Gtk.File_Chooser.Get_Preview_Uri to get the name of the file to preview.
   --  Your widget may not be able to preview all kinds of files; your callback
   --  must call Gtk.File_Chooser.Set_Preview_Widget_Active to inform the file
   --  chooser about whether the preview was generated successfully or not.
   --
   --  Please see the example code in [Using a Preview
   --  Widget][gtkfilechooser-preview].
   --
   --  See also: Gtk.File_Chooser.Set_Preview_Widget,
   --  Gtk.File_Chooser.Set_Preview_Widget_Active,
   --  Gtk.File_Chooser.Set_Use_Preview_Label,
   --  Gtk.File_Chooser.Get_Preview_Filename, Gtk.File_Chooser.Get_Preview_Uri.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_File_Chooser"

   function "+" (W : Gtk_File_Chooser) return Gtk_File_Chooser;
   pragma Inline ("+");

private
   Use_Preview_Label_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-preview-label");
   Show_Hidden_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-hidden");
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("select-multiple");
   Preview_Widget_Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("preview-widget-active");
   Preview_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("preview-widget");
   Local_Only_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("local-only");
   Filter_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("filter");
   Extra_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("extra-widget");
   Do_Overwrite_Confirmation_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("do-overwrite-confirmation");
   Create_Folders_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("create-folders");
   Action_Property : constant Gtk.File_Chooser.Property_Gtk_File_Chooser_Action :=
     Gtk.File_Chooser.Build ("action");

Null_Gtk_File_Chooser : constant Gtk_File_Chooser :=
   Gtk_File_Chooser (Glib.Types.Null_Interface);
end Gtk.File_Chooser;
