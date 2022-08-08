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
--  The Gtk.File_Chooser_Button.Gtk_File_Chooser_Button is a widget that lets
--  the user select a file. It implements the Gtk.File_Chooser.Gtk_File_Chooser
--  interface. Visually, it is a file name with a button to bring up a
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog. The user can then use that
--  dialog to change the file associated with that button. This widget does not
--  support setting the Gtk.File_Chooser.Gtk_File_Chooser:select-multiple
--  property to True.
--
--  ## Create a button to let the user select a file in /etc
--
--  |[<!-- language="C" --> { GtkWidget *button;
--
--  button = gtk_file_chooser_button_new (_("Select a file"),
--  GTK_FILE_CHOOSER_ACTION_OPEN); gtk_file_chooser_set_current_folder
--  (GTK_FILE_CHOOSER (button), "/etc"); } ]|
--
--  The Gtk.File_Chooser_Button.Gtk_File_Chooser_Button supports the
--  Gtk_File_Chooser_Actions Gtk.File_Chooser.Action_Open and
--  Gtk.File_Chooser.Action_Select_Folder.
--
--  > The Gtk.File_Chooser_Button.Gtk_File_Chooser_Button will ellipsize the
--  label, and will thus > request little horizontal space. To give the button
--  more space, > you should call Gtk.Widget.Get_Preferred_Size, >
--  Gtk.File_Chooser_Button.Set_Width_Chars, or pack the button in > such a way
--  that other interface elements give space to the > widget.
--
--  # CSS nodes
--
--  GtkFileChooserButton has a CSS node with name "filechooserbutton",
--  containing a subnode for the internal button with name "button" and style
--  class ".file".
--
--  </description>
--  <group>Buttons and Toggles</group>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;     use GNAT.Strings;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Box;          use Gtk.Box;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.File_Chooser; use Gtk.File_Chooser;
with Gtk.File_Filter;  use Gtk.File_Filter;
with Gtk.Orientable;   use Gtk.Orientable;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.File_Chooser_Button is

   type Gtk_File_Chooser_Button_Record is new Gtk_Box_Record with null record;
   type Gtk_File_Chooser_Button is access all Gtk_File_Chooser_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Button : out Gtk_File_Chooser_Button;
       Title  : UTF8_String;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action);
   procedure Initialize
      (Button : not null access Gtk_File_Chooser_Button_Record'Class;
       Title  : UTF8_String;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action);
   --  Creates a new file-selecting button widget.
   --  Since: gtk+ 2.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "title": the title of the browse dialog.
   --  "action": the open mode for the widget.

   function Gtk_File_Chooser_Button_New
      (Title  : UTF8_String;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action)
       return Gtk_File_Chooser_Button;
   --  Creates a new file-selecting button widget.
   --  Since: gtk+ 2.6
   --  "title": the title of the browse dialog.
   --  "action": the open mode for the widget.

   procedure Gtk_New_With_Dialog
      (Button : out Gtk_File_Chooser_Button;
       Dialog : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize_With_Dialog
      (Button : not null access Gtk_File_Chooser_Button_Record'Class;
       Dialog : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Creates a Gtk.File_Chooser_Button.Gtk_File_Chooser_Button widget which
   --  uses Dialog as its file-picking window.
   --  Note that Dialog must be a Gtk.Dialog.Gtk_Dialog (or subclass) which
   --  implements the Gtk.File_Chooser.Gtk_File_Chooser interface and must not
   --  have GTK_DIALOG_DESTROY_WITH_PARENT set.
   --  Also note that the dialog needs to have its confirmative button added
   --  with response GTK_RESPONSE_ACCEPT or GTK_RESPONSE_OK in order for the
   --  button to take over the file selected in the dialog.
   --  Since: gtk+ 2.6
   --  Initialize_With_Dialog does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "dialog": the widget to use as dialog

   function Gtk_File_Chooser_Button_New_With_Dialog
      (Dialog : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_File_Chooser_Button;
   --  Creates a Gtk.File_Chooser_Button.Gtk_File_Chooser_Button widget which
   --  uses Dialog as its file-picking window.
   --  Note that Dialog must be a Gtk.Dialog.Gtk_Dialog (or subclass) which
   --  implements the Gtk.File_Chooser.Gtk_File_Chooser interface and must not
   --  have GTK_DIALOG_DESTROY_WITH_PARENT set.
   --  Also note that the dialog needs to have its confirmative button added
   --  with response GTK_RESPONSE_ACCEPT or GTK_RESPONSE_OK in order for the
   --  button to take over the file selected in the dialog.
   --  Since: gtk+ 2.6
   --  "dialog": the widget to use as dialog

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_chooser_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Focus_On_Click
      (Button : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;
   pragma Obsolescent (Get_Focus_On_Click);
   --  Returns whether the button grabs focus when it is clicked with the
   --  mouse. See Gtk.File_Chooser_Button.Set_Focus_On_Click.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.20, 1

   procedure Set_Focus_On_Click
      (Button         : not null access Gtk_File_Chooser_Button_Record;
       Focus_On_Click : Boolean);
   pragma Obsolescent (Set_Focus_On_Click);
   --  Sets whether the button will grab focus when it is clicked with the
   --  mouse. Making mouse clicks not grab focus is useful in places like
   --  toolbars where you don't want the keyboard focus removed from the main
   --  area of the application.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.20, 1
   --  "focus_on_click": whether the button grabs focus when clicked with the
   --  mouse

   function Get_Title
      (Button : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;
   --  Retrieves the title of the browse dialog used by Button. The returned
   --  value should not be modified or freed.
   --  Since: gtk+ 2.6

   procedure Set_Title
      (Button : not null access Gtk_File_Chooser_Button_Record;
       Title  : UTF8_String);
   --  Modifies the Title of the browse dialog used by Button.
   --  Since: gtk+ 2.6
   --  "title": the new browse dialog title.

   function Get_Width_Chars
      (Button : not null access Gtk_File_Chooser_Button_Record)
       return Glib.Gint;
   --  Retrieves the width in characters of the Button widget's entry and/or
   --  label.
   --  Since: gtk+ 2.6

   procedure Set_Width_Chars
      (Button  : not null access Gtk_File_Chooser_Button_Record;
       N_Chars : Glib.Gint);
   --  Sets the width (in characters) that Button will use to N_Chars.
   --  Since: gtk+ 2.6
   --  "n_chars": the new width, in characters.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Choice
      (Chooser       : not null access Gtk_File_Chooser_Button_Record;
       Id            : UTF8_String;
       Label         : UTF8_String;
       Options       : GNAT.Strings.String_List;
       Option_Labels : GNAT.Strings.String_List);

   procedure Add_Filter
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Add_Shortcut_Folder
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Folder  : UTF8_String) return Boolean;

   function Add_Shortcut_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Action
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.File_Chooser.Gtk_File_Chooser_Action;

   procedure Set_Action
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Action  : Gtk.File_Chooser.Gtk_File_Chooser_Action);

   function Get_Choice
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Id      : UTF8_String) return UTF8_String;

   procedure Set_Choice
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Id      : UTF8_String;
       Option  : UTF8_String);

   function Get_Create_Folders
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Create_Folders
      (Chooser        : not null access Gtk_File_Chooser_Button_Record;
       Create_Folders : Boolean);

   function Get_Current_Folder
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   function Set_Current_Folder
      (Chooser  : not null access Gtk_File_Chooser_Button_Record;
       Filename : UTF8_String) return Boolean;

   function Get_Current_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   function Set_Current_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   procedure Set_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Name    : UTF8_String);

   function Get_Do_Overwrite_Confirmation
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Do_Overwrite_Confirmation
      (Chooser                   : not null access Gtk_File_Chooser_Button_Record;
       Do_Overwrite_Confirmation : Boolean);

   function Get_Extra_Widget
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Extra_Widget
      (Chooser      : not null access Gtk_File_Chooser_Button_Record;
       Extra_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Filename
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   function Set_Filename
      (Chooser  : not null access Gtk_File_Chooser_Button_Record;
       Filename : UTF8_String) return Boolean;

   function Get_Filenames
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Enums.String_SList.GSlist;

   function Get_Filter
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.File_Filter.Gtk_File_Filter;

   procedure Set_Filter
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Get_Local_Only
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_File_Chooser_Button_Record;
       Local_Only : Boolean);

   function Get_Preview_Filename
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   function Get_Preview_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   function Get_Preview_Widget
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Preview_Widget
      (Chooser        : not null access Gtk_File_Chooser_Button_Record;
       Preview_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Preview_Widget_Active
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Preview_Widget_Active
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Active  : Boolean);

   function Get_Select_Multiple
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_File_Chooser_Button_Record;
       Select_Multiple : Boolean);

   function Get_Show_Hidden
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Show_Hidden
      (Chooser     : not null access Gtk_File_Chooser_Button_Record;
       Show_Hidden : Boolean);

   function Get_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return UTF8_String;

   function Set_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Uris
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Enums.String_SList.GSlist;

   function Get_Use_Preview_Label
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Boolean;

   procedure Set_Use_Preview_Label
      (Chooser   : not null access Gtk_File_Chooser_Button_Record;
       Use_Label : Boolean);

   function List_Filters
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Glib.Object.Object_List.GSlist;

   function List_Shortcut_Folder_Uris
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Enums.String_SList.GSlist;

   function List_Shortcut_Folders
      (Chooser : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Enums.String_SList.GSlist;

   procedure Remove_Choice
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Id      : UTF8_String);

   procedure Remove_Filter
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Remove_Shortcut_Folder
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       Folder  : UTF8_String) return Boolean;

   function Remove_Shortcut_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       URI     : UTF8_String) return Boolean;

   procedure Select_All
      (Chooser : not null access Gtk_File_Chooser_Button_Record);

   function Select_Filename
      (Chooser  : not null access Gtk_File_Chooser_Button_Record;
       Filename : UTF8_String) return Boolean;

   function Select_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       URI     : UTF8_String) return Boolean;

   procedure Unselect_All
      (Chooser : not null access Gtk_File_Chooser_Button_Record);

   procedure Unselect_Filename
      (Chooser  : not null access Gtk_File_Chooser_Button_Record;
       Filename : UTF8_String);

   procedure Unselect_Uri
      (Chooser : not null access Gtk_File_Chooser_Button_Record;
       URI     : UTF8_String);

   function Get_Orientation
      (Self : not null access Gtk_File_Chooser_Button_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_File_Chooser_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Dialog_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.File_Chooser.Gtk_File_Chooser
   --  Flags: write
   --  Instance of the Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog
   --  associated with the button.

   Title_Property : constant Glib.Properties.Property_String;
   --  Title to put on the Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog
   --  associated with the button.

   Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The width of the entry and label inside the button, in characters.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_File_Chooser_Button_Void is not null access procedure
     (Self : access Gtk_File_Chooser_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_File_Set : constant Glib.Signal_Name := "file-set";
   procedure On_File_Set
      (Self  : not null access Gtk_File_Chooser_Button_Record;
       Call  : Cb_Gtk_File_Chooser_Button_Void;
       After : Boolean := False);
   procedure On_File_Set
      (Self  : not null access Gtk_File_Chooser_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::file-set signal is emitted when the user selects a file.
   --
   --  Note that this signal is only emitted when the user changes the file.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "FileChooser"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_File_Chooser_Button_Record, Gtk_File_Chooser_Button);
   function "+"
     (Widget : access Gtk_File_Chooser_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_File_Chooser_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser, Gtk_File_Chooser_Button_Record, Gtk_File_Chooser_Button);
   function "+"
     (Widget : access Gtk_File_Chooser_Button_Record'Class)
   return Gtk.File_Chooser.Gtk_File_Chooser
   renames Implements_Gtk_File_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.File_Chooser.Gtk_File_Chooser)
   return Gtk_File_Chooser_Button
   renames Implements_Gtk_File_Chooser.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_File_Chooser_Button_Record, Gtk_File_Chooser_Button);
   function "+"
     (Widget : access Gtk_File_Chooser_Button_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_File_Chooser_Button
   renames Implements_Gtk_Orientable.To_Object;

private
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Dialog_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("dialog");
end Gtk.File_Chooser_Button;
