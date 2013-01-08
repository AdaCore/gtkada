------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget is a widget suitable for
--  selecting files. It is the main building block of a
--  Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog. Most applications will
--  only need to use the latter; you can use
--  Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget as part of a larger window
--  if you have special needs.
--
--  Note that Gtk.File_Chooser_Widget.Gtk_File_Chooser_Widget does not have
--  any methods of its own. Instead, you should use the functions that work on
--  a Gtk.File_Chooser.Gtk_File_Chooser.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
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

   procedure Set_Current_Name
      (Chooser : not null access Gtk_File_Chooser_Widget_Record;
       Name    : UTF8_String);

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

end Gtk.File_Chooser_Widget;
