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
--  Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget is a widget for choosing
--  files. It exposes the Gtk.File_Chooser.Gtk_File_Chooser interface, and you
--  should use the methods of this interface to interact with the widget.
--
--  # CSS nodes
--
--  GtkFileChooserWidget has a single CSS node with name filechooser.
--
--  </description>

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

package Gtk.File_Chooser_Widget is

   type Gtk_File_Chooser_Widget_Record is new Gtk_Box_Record with null record;
   type Gtk_File_Chooser_Widget is access all Gtk_File_Chooser_Widget_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_File_Chooser_Widget;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action);
   procedure Initialize
      (Self   : not null access Gtk_File_Chooser_Widget_Record'Class;
       Action : Gtk.File_Chooser.Gtk_File_Chooser_Action);
   --  Creates a new Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget. This is
   --  a file chooser widget that can be embedded in custom windows, and it is
   --  the same widget that is used by
   --  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "action": Open or save mode for the widget

   function Gtk_File_Chooser_Widget_New
      (Action : Gtk.File_Chooser.Gtk_File_Chooser_Action)
       return Gtk_File_Chooser_Widget;
   --  Creates a new Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget. This is
   --  a file chooser widget that can be embedded in custom windows, and it is
   --  the same widget that is used by
   --  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog.
   --  Since: gtk+ 2.4
   --  "action": Open or save mode for the widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_chooser_widget_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Choice
      (Chooser       : not null access Gtk_File_Chooser_Widget_Record;
       Id            : UTF8_String;
       Label         : UTF8_String;
       Options       : GNAT.Strings.String_List;
       Option_Labels : GNAT.Strings.String_List);

   procedure Add_Filter
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Add_Shortcut_Folder
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Folder  : UTF8_String) return Boolean;

   function Add_Shortcut_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Action
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.File_Chooser.Gtk_File_Chooser_Action;

   procedure Set_Action
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Action  : Gtk.File_Chooser.Gtk_File_Chooser_Action);

   function Get_Choice
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Id      : UTF8_String) return UTF8_String;

   procedure Set_Choice
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Id      : UTF8_String;
       Option  : UTF8_String);

   function Get_Create_Folders
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Create_Folders
      (Chooser        : not null access Gtk_File_Chooser_Widget_Record;
       Create_Folders : Boolean);

   function Get_Current_Folder
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   function Set_Current_Folder
      (Chooser  : not null access Gtk_File_Chooser_Widget_Record;
       Filename : UTF8_String) return Boolean;

   function Get_Current_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   function Set_Current_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   procedure Set_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Name    : UTF8_String);

   function Get_Do_Overwrite_Confirmation
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Do_Overwrite_Confirmation
      (Chooser                   : not null access Gtk_File_Chooser_Widget_Record;
       Do_Overwrite_Confirmation : Boolean);

   function Get_Extra_Widget
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Extra_Widget
      (Chooser      : not null access Gtk_File_Chooser_Widget_Record;
       Extra_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Filename
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   function Set_Filename
      (Chooser  : not null access Gtk_File_Chooser_Widget_Record;
       Filename : UTF8_String) return Boolean;

   function Get_Filenames
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Enums.String_SList.GSlist;

   function Get_Filter
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.File_Filter.Gtk_File_Filter;

   procedure Set_Filter
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Get_Local_Only
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_File_Chooser_Widget_Record;
       Local_Only : Boolean);

   function Get_Preview_Filename
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   function Get_Preview_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   function Get_Preview_Widget
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Preview_Widget
      (Chooser        : not null access Gtk_File_Chooser_Widget_Record;
       Preview_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Preview_Widget_Active
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Preview_Widget_Active
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Active  : Boolean);

   function Get_Select_Multiple
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_File_Chooser_Widget_Record;
       Select_Multiple : Boolean);

   function Get_Show_Hidden
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Show_Hidden
      (Chooser     : not null access Gtk_File_Chooser_Widget_Record;
       Show_Hidden : Boolean);

   function Get_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return UTF8_String;

   function Set_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Uris
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Enums.String_SList.GSlist;

   function Get_Use_Preview_Label
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Use_Preview_Label
      (Chooser   : not null access Gtk_File_Chooser_Widget_Record;
       Use_Label : Boolean);

   function List_Filters
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Glib.Object.Object_List.GSlist;

   function List_Shortcut_Folder_Uris
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Enums.String_SList.GSlist;

   function List_Shortcut_Folders
      (Chooser : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Enums.String_SList.GSlist;

   procedure Remove_Choice
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Id      : UTF8_String);

   procedure Remove_Filter
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Filter  : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class);

   function Remove_Shortcut_Folder
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Folder  : UTF8_String) return Boolean;

   function Remove_Shortcut_Folder_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       URI     : UTF8_String) return Boolean;

   procedure Select_All
      (Chooser : not null access Gtk_File_Chooser_Widget_Record);

   function Select_Filename
      (Chooser  : not null access Gtk_File_Chooser_Widget_Record;
       Filename : UTF8_String) return Boolean;

   function Select_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       URI     : UTF8_String) return Boolean;

   procedure Unselect_All
      (Chooser : not null access Gtk_File_Chooser_Widget_Record);

   procedure Unselect_Filename
      (Chooser  : not null access Gtk_File_Chooser_Widget_Record;
       Filename : UTF8_String);

   procedure Unselect_Uri
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       URI     : UTF8_String);

   function Get_Orientation
      (Self : not null access Gtk_File_Chooser_Widget_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_File_Chooser_Widget_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Search_Mode_Property : constant Glib.Properties.Property_Boolean;

   Subtitle_Property : constant Glib.Properties.Property_String;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_File_Chooser_Widget_Void is not null access procedure
     (Self : access Gtk_File_Chooser_Widget_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Desktop_Folder : constant Glib.Signal_Name := "desktop-folder";
   procedure On_Desktop_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Desktop_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::desktop-folder signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser show the user's Desktop folder in
   --  the file list.
   --
   --  The default binding for this signal is `Alt + D`.

   Signal_Down_Folder : constant Glib.Signal_Name := "down-folder";
   procedure On_Down_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Down_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::down-folder signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser go to a child of the current
   --  folder in the file hierarchy. The subfolder that will be used is
   --  displayed in the path bar widget of the file chooser. For example, if
   --  the path bar is showing "/foo/bar/baz", with bar currently displayed,
   --  then this will cause the file chooser to switch to the "baz" subfolder.
   --
   --  The default binding for this signal is `Alt + Down`.

   Signal_Home_Folder : constant Glib.Signal_Name := "home-folder";
   procedure On_Home_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Home_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::home-folder signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser show the user's home folder in
   --  the file list.
   --
   --  The default binding for this signal is `Alt + Home`.

   type Cb_Gtk_File_Chooser_Widget_UTF8_String_Void is not null access procedure
     (Self : access Gtk_File_Chooser_Widget_Record'Class;
      Path : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String);

   Signal_Location_Popup : constant Glib.Signal_Name := "location-popup";
   procedure On_Location_Popup
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Location_Popup
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::location-popup signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser show a "Location" prompt which
   --  the user can use to manually type the name of the file he wishes to
   --  select.
   --
   --  The default bindings for this signal are `Control + L` with a Path
   --  string of "" (the empty string). It is also bound to `/` with a Path
   --  string of "`/`" (a slash): this lets you type `/` and immediately type a
   --  path name. On Unix systems, this is bound to `~` (tilde) with a Path
   --  string of "~" itself for access to home directories.

   Signal_Location_Popup_On_Paste : constant Glib.Signal_Name := "location-popup-on-paste";
   procedure On_Location_Popup_On_Paste
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Location_Popup_On_Paste
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::location-popup-on-paste signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser show a "Location" prompt when the
   --  user pastes into a Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget.
   --
   --  The default binding for this signal is `Control + V`.

   Signal_Location_Toggle_Popup : constant Glib.Signal_Name := "location-toggle-popup";
   procedure On_Location_Toggle_Popup
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Location_Toggle_Popup
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::location-toggle-popup signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user asks for it.
   --
   --  This is used to toggle the visibility of a "Location" prompt which the
   --  user can use to manually type the name of the file he wishes to select.
   --
   --  The default binding for this signal is `Control + L`.

   Signal_Places_Shortcut : constant Glib.Signal_Name := "places-shortcut";
   procedure On_Places_Shortcut
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Places_Shortcut
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::places-shortcut signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to move the focus to the places sidebar.
   --
   --  The default binding for this signal is `Alt + P`.

   type Cb_Gtk_File_Chooser_Widget_Gint_Void is not null access procedure
     (Self           : access Gtk_File_Chooser_Widget_Record'Class;
      Bookmark_Index : Glib.Gint);

   type Cb_GObject_Gint_Void is not null access procedure
     (Self           : access Glib.Object.GObject_Record'Class;
      Bookmark_Index : Glib.Gint);

   Signal_Quick_Bookmark : constant Glib.Signal_Name := "quick-bookmark";
   procedure On_Quick_Bookmark
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Gint_Void;
       After : Boolean := False);
   procedure On_Quick_Bookmark
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::quick-bookmark signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser switch to the bookmark specified
   --  in the Bookmark_Index parameter. For example, if you have three
   --  bookmarks, you can pass 0, 1, 2 to this signal to switch to each of
   --  them, respectively.
   --
   --  The default binding for this signal is `Alt + 1`, `Alt + 2`, etc. until
   --  `Alt + 0`. Note that in the default binding, that `Alt + 1` is actually
   --  defined to switch to the bookmark at index 0, and so on successively;
   --  `Alt + 0` is defined to switch to the bookmark at index 10.

   Signal_Recent_Shortcut : constant Glib.Signal_Name := "recent-shortcut";
   procedure On_Recent_Shortcut
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Recent_Shortcut
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::recent-shortcut signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser show the Recent location.
   --
   --  The default binding for this signal is `Alt + R`.

   Signal_Search_Shortcut : constant Glib.Signal_Name := "search-shortcut";
   procedure On_Search_Shortcut
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Search_Shortcut
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::search-shortcut signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser show the search entry.
   --
   --  The default binding for this signal is `Alt + S`.

   Signal_Show_Hidden : constant Glib.Signal_Name := "show-hidden";
   procedure On_Show_Hidden
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Show_Hidden
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::show-hidden signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser display hidden files.
   --
   --  The default binding for this signal is `Control + H`.

   Signal_Up_Folder : constant Glib.Signal_Name := "up-folder";
   procedure On_Up_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_Gtk_File_Chooser_Widget_Void;
       After : Boolean := False);
   procedure On_Up_Folder
      (Self  : not null access Gtk_File_Chooser_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::up-folder signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted when the user asks for it.
   --
   --  This is used to make the file chooser go to the parent of the current
   --  folder in the file hierarchy.
   --
   --  The default binding for this signal is `Alt + Up`.

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
     (Gtk.Buildable.Gtk_Buildable, Gtk_File_Chooser_Widget_Record, Gtk_File_Chooser_Widget);
   function "+"
     (Widget : access Gtk_File_Chooser_Widget_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_File_Chooser_Widget
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser, Gtk_File_Chooser_Widget_Record, Gtk_File_Chooser_Widget);
   function "+"
     (Widget : access Gtk_File_Chooser_Widget_Record'Class)
   return Gtk.File_Chooser.Gtk_File_Chooser
   renames Implements_Gtk_File_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.File_Chooser.Gtk_File_Chooser)
   return Gtk_File_Chooser_Widget
   renames Implements_Gtk_File_Chooser.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_File_Chooser_Widget_Record, Gtk_File_Chooser_Widget);
   function "+"
     (Widget : access Gtk_File_Chooser_Widget_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_File_Chooser_Widget
   renames Implements_Gtk_Orientable.To_Object;

private
   Subtitle_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("subtitle");
   Search_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("search-mode");
end Gtk.File_Chooser_Widget;
