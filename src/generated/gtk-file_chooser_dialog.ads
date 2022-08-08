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
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog is a dialog box suitable
--  for use with "File/Open" or "File/Save as" commands. This widget works by
--  putting a Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget inside a
--  Gtk.Dialog.Gtk_Dialog. It exposes the Gtk.File_Chooser.Gtk_File_Chooser
--  interface, so you can use all of the Gtk.File_Chooser.Gtk_File_Chooser
--  functions on the file chooser dialog as well as those for
--  Gtk.Dialog.Gtk_Dialog.
--
--  Note that Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog does not have
--  any methods of its own. Instead, you should use the functions that work on
--  a Gtk.File_Chooser.Gtk_File_Chooser.
--
--  If you want to integrate well with the platform you should use the
--  Gtk.File_Chooser_Native.Gtk_File_Chooser_Native API, which will use a
--  platform-specific dialog if available and fall back to GtkFileChooserDialog
--  otherwise.
--
--  ## Typical usage ## {gtkfilechooser-typical-usage}
--
--  In the simplest of cases, you can the following code to use
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog to select a file for
--  opening:
--
--  |[ GtkWidget *dialog; GtkFileChooserAction action =
--  GTK_FILE_CHOOSER_ACTION_OPEN; gint res;
--
--  dialog = gtk_file_chooser_dialog_new ("Open File", parent_window, action,
--  _("_Cancel"), GTK_RESPONSE_CANCEL, _("_Open"), GTK_RESPONSE_ACCEPT, NULL);
--
--  res = gtk_dialog_run (GTK_DIALOG (dialog)); if (res ==
--  GTK_RESPONSE_ACCEPT) { char *filename; GtkFileChooser *chooser =
--  GTK_FILE_CHOOSER (dialog); filename = gtk_file_chooser_get_filename
--  (chooser); open_file (filename); g_free (filename); }
--
--  gtk_widget_destroy (dialog); ]|
--
--  To use a dialog for saving, you can use this:
--
--  |[ GtkWidget *dialog; GtkFileChooser *chooser; GtkFileChooserAction action
--  = GTK_FILE_CHOOSER_ACTION_SAVE; gint res;
--
--  dialog = gtk_file_chooser_dialog_new ("Save File", parent_window, action,
--  _("_Cancel"), GTK_RESPONSE_CANCEL, _("_Save"), GTK_RESPONSE_ACCEPT, NULL);
--  chooser = GTK_FILE_CHOOSER (dialog);
--
--  gtk_file_chooser_set_do_overwrite_confirmation (chooser, TRUE);
--
--  if (user_edited_a_new_document) gtk_file_chooser_set_current_name
--  (chooser, _("Untitled document")); else gtk_file_chooser_set_filename
--  (chooser, existing_filename);
--
--  res = gtk_dialog_run (GTK_DIALOG (dialog)); if (res ==
--  GTK_RESPONSE_ACCEPT) { char *filename;
--
--  filename = gtk_file_chooser_get_filename (chooser); save_to_file
--  (filename); g_free (filename); }
--
--  gtk_widget_destroy (dialog); ]|
--
--  ## Setting up a file chooser dialog ## {gtkfilechooserdialog-setting-up}
--
--  There are various cases in which you may need to use a
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog:
--
--  - To select a file for opening. Use GTK_FILE_CHOOSER_ACTION_OPEN.
--
--  - To save a file for the first time. Use GTK_FILE_CHOOSER_ACTION_SAVE, and
--  suggest a name such as "Untitled" with Gtk.File_Chooser.Set_Current_Name.
--
--  - To save a file under a different name. Use GTK_FILE_CHOOSER_ACTION_SAVE,
--  and set the existing filename with Gtk.File_Chooser.Set_Filename.
--
--  - To choose a folder instead of a file. Use
--  GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER.
--
--  Note that old versions of the file chooser's documentation suggested using
--  Gtk.File_Chooser.Set_Current_Folder in various situations, with the
--  intention of letting the application suggest a reasonable default folder.
--  This is no longer considered to be a good policy, as now the file chooser
--  is able to make good suggestions on its own. In general, you should only
--  cause the file chooser to show a specific folder when it is appropriate to
--  use Gtk.File_Chooser.Set_Filename, i.e. when you are doing a Save As
--  command and you already have a file saved somewhere.
--
--  ## Response Codes ## {gtkfilechooserdialog-responses}
--
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog inherits from
--  Gtk.Dialog.Gtk_Dialog, so buttons that go in its action area have response
--  codes such as GTK_RESPONSE_ACCEPT and GTK_RESPONSE_CANCEL. For example, you
--  could call Gtk_New as follows:
--
--  |[ GtkWidget *dialog; GtkFileChooserAction action =
--  GTK_FILE_CHOOSER_ACTION_OPEN;
--
--  dialog = gtk_file_chooser_dialog_new ("Open File", parent_window, action,
--  _("_Cancel"), GTK_RESPONSE_CANCEL, _("_Open"), GTK_RESPONSE_ACCEPT, NULL);
--  ]|
--
--  This will create buttons for "Cancel" and "Open" that use stock response
--  identifiers from Gtk_Response_Type. For most dialog boxes you can use your
--  own custom response codes rather than the ones in Gtk_Response_Type, but
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog assumes that its
--  "accept"-type action, e.g. an "Open" or "Save" button, will have one of the
--  following response codes:
--
--  - GTK_RESPONSE_ACCEPT - GTK_RESPONSE_OK - GTK_RESPONSE_YES -
--  GTK_RESPONSE_APPLY
--
--  This is because Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog must
--  intercept responses and switch to folders if appropriate, rather than
--  letting the dialog terminate — the implementation uses these known response
--  codes to know which responses can be blocked if appropriate.
--
--  To summarize, make sure you use a [stock response
--  code][gtkfilechooserdialog-responses] when you use
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog to ensure proper operation.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;     use GNAT.Strings;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.File_Chooser; use Gtk.File_Chooser;
with Gtk.File_Filter;  use Gtk.File_Filter;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;

package Gtk.File_Chooser_Dialog is

   type Gtk_File_Chooser_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_File_Chooser_Dialog is access all Gtk_File_Chooser_Dialog_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Dialog : out Gtk_File_Chooser_Dialog;
       Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action);
   procedure Initialize
      (Dialog : not null access Gtk_File_Chooser_Dialog_Record'Class;
       Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action);
   --  Creates a new Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog. This
   --  function is analogous to gtk_dialog_new_with_buttons.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "title": Title of the dialog, or null
   --  "parent": Transient parent of the dialog, or null
   --  "action": Open or save mode for the dialog

   function Gtk_File_Chooser_Dialog_New
      (Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action)
       return Gtk_File_Chooser_Dialog;
   --  Creates a new Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog. This
   --  function is analogous to gtk_dialog_new_with_buttons.
   --  Since: gtk+ 2.4
   --  "title": Title of the dialog, or null
   --  "parent": Transient parent of the dialog, or null
   --  "action": Open or save mode for the dialog

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_chooser_dialog_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Choice
      (Chooser       : not null access Gtk_File_Chooser_Dialog_Record;
       Id            : UTF8_String;
       Label         : UTF8_String;
       Options       : GNAT.Strings.String_List;
       Option_Labels : GNAT.Strings.String_List);

   procedure Add_Filter
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Add_Shortcut_Folder
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Folder  : UTF8_String) return Boolean;

   function Add_Shortcut_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Action
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.File_Chooser.Gtk_File_Chooser_Action;

   procedure Set_Action
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Action  : Gtk.File_Chooser.Gtk_File_Chooser_Action);

   function Get_Choice
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Id      : UTF8_String) return UTF8_String;

   procedure Set_Choice
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Id      : UTF8_String;
       Option  : UTF8_String);

   function Get_Create_Folders
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Create_Folders
      (Chooser        : not null access Gtk_File_Chooser_Dialog_Record;
       Create_Folders : Boolean);

   function Get_Current_Folder
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   function Set_Current_Folder
      (Chooser  : not null access Gtk_File_Chooser_Dialog_Record;
       Filename : UTF8_String) return Boolean;

   function Get_Current_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   function Set_Current_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   procedure Set_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Name    : UTF8_String);

   function Get_Do_Overwrite_Confirmation
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Do_Overwrite_Confirmation
      (Chooser                   : not null access Gtk_File_Chooser_Dialog_Record;
       Do_Overwrite_Confirmation : Boolean);

   function Get_Extra_Widget
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Extra_Widget
      (Chooser      : not null access Gtk_File_Chooser_Dialog_Record;
       Extra_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Filename
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   function Set_Filename
      (Chooser  : not null access Gtk_File_Chooser_Dialog_Record;
       Filename : UTF8_String) return Boolean;

   function Get_Filenames
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.Enums.String_SList.GSlist;

   function Get_Filter
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.File_Filter.Gtk_File_Filter;

   procedure Set_Filter
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Get_Local_Only
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_File_Chooser_Dialog_Record;
       Local_Only : Boolean);

   function Get_Preview_Filename
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   function Get_Preview_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   function Get_Preview_Widget
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Preview_Widget
      (Chooser        : not null access Gtk_File_Chooser_Dialog_Record;
       Preview_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Preview_Widget_Active
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Preview_Widget_Active
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Active  : Boolean);

   function Get_Select_Multiple
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_File_Chooser_Dialog_Record;
       Select_Multiple : Boolean);

   function Get_Show_Hidden
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Show_Hidden
      (Chooser     : not null access Gtk_File_Chooser_Dialog_Record;
       Show_Hidden : Boolean);

   function Get_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return UTF8_String;

   function Set_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Uris
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.Enums.String_SList.GSlist;

   function Get_Use_Preview_Label
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Use_Preview_Label
      (Chooser   : not null access Gtk_File_Chooser_Dialog_Record;
       Use_Label : Boolean);

   function List_Filters
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Glib.Object.Object_List.GSlist;

   function List_Shortcut_Folder_Uris
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.Enums.String_SList.GSlist;

   function List_Shortcut_Folders
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record)
       return Gtk.Enums.String_SList.GSlist;

   procedure Remove_Choice
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Id      : UTF8_String);

   procedure Remove_Filter
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Remove_Shortcut_Folder
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       Folder  : UTF8_String) return Boolean;

   function Remove_Shortcut_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   procedure Select_All
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record);

   function Select_Filename
      (Chooser  : not null access Gtk_File_Chooser_Dialog_Record;
       Filename : UTF8_String) return Boolean;

   function Select_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   procedure Unselect_All
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record);

   procedure Unselect_Filename
      (Chooser  : not null access Gtk_File_Chooser_Dialog_Record;
       Filename : UTF8_String);

   procedure Unselect_Uri
      (Chooser : not null access Gtk_File_Chooser_Dialog_Record;
       URI     : UTF8_String);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "FileChooser"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_File_Chooser_Dialog_Record, Gtk_File_Chooser_Dialog);
   function "+"
     (Widget : access Gtk_File_Chooser_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_File_Chooser_Dialog
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser, Gtk_File_Chooser_Dialog_Record, Gtk_File_Chooser_Dialog);
   function "+"
     (Widget : access Gtk_File_Chooser_Dialog_Record'Class)
   return Gtk.File_Chooser.Gtk_File_Chooser
   renames Implements_Gtk_File_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.File_Chooser.Gtk_File_Chooser)
   return Gtk_File_Chooser_Dialog
   renames Implements_Gtk_File_Chooser.To_Object;

end Gtk.File_Chooser_Dialog;
