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
--  Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog is a dialog box
--  suitable for displaying the recently used documents. This widgets works by
--  putting a Gtk.Recent_Chooser_Widget.Gtk_Recent_Chooser_Widget inside a
--  Gtk.Dialog.Gtk_Dialog. It exposes the Gtk_Recent_Chooser_Iface interface,
--  so you can use all the Gtk.Recent_Chooser.Gtk_Recent_Chooser functions on
--  the recent chooser dialog as well as those for Gtk.Dialog.Gtk_Dialog.
--
--  Note that Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog does not
--  have any methods of its own. Instead, you should use the functions that
--  work on a Gtk.Recent_Chooser.Gtk_Recent_Chooser.
--
--  ## Typical usage ## {gtkrecentchooser-typical-usage}
--
--  In the simplest of cases, you can use the following code to use a
--  Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog to select a recently
--  used file:
--
--  |[<!-- language="C" --> GtkWidget *dialog; gint res;
--
--  dialog = gtk_recent_chooser_dialog_new ("Recent Documents", parent_window,
--  _("_Cancel"), GTK_RESPONSE_CANCEL, _("_Open"), GTK_RESPONSE_ACCEPT, NULL);
--
--  res = gtk_dialog_run (GTK_DIALOG (dialog)); if (res ==
--  GTK_RESPONSE_ACCEPT) { GtkRecentInfo *info; GtkRecentChooser *chooser =
--  GTK_RECENT_CHOOSER (dialog);
--
--  info = gtk_recent_chooser_get_current_item (chooser); open_file
--  (gtk_recent_info_get_uri (info)); gtk_recent_info_unref (info); }
--
--  gtk_widget_destroy (dialog); ]|
--
--  Recently used files are supported since GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Types;         use Glib.Types;
with Gtk.Buildable;      use Gtk.Buildable;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Recent_Chooser; use Gtk.Recent_Chooser;
with Gtk.Recent_Filter;  use Gtk.Recent_Filter;
with Gtk.Recent_Info;    use Gtk.Recent_Info;
with Gtk.Recent_Manager; use Gtk.Recent_Manager;
with Gtk.Window;         use Gtk.Window;

package Gtk.Recent_Chooser_Dialog is

   type Gtk_Recent_Chooser_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Recent_Chooser_Dialog is access all Gtk_Recent_Chooser_Dialog_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Recent_Sort_Func is access function
     (A : Gtk.Recent_Info.Gtk_Recent_Info;
      B : Gtk.Recent_Info.Gtk_Recent_Info) return Glib.Gint;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Widget : out Gtk_Recent_Chooser_Dialog;
       Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class);
   procedure Initialize
      (Widget : not null access Gtk_Recent_Chooser_Dialog_Record'Class;
       Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class);
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Recent_Chooser_Dialog_New
      (Title  : UTF8_String := "";
       Parent : access Gtk.Window.Gtk_Window_Record'Class)
       return Gtk_Recent_Chooser_Dialog;

   procedure Gtk_New_For_Manager
      (Widget  : out Gtk_Recent_Chooser_Dialog;
       Title   : UTF8_String := "";
       Parent  : access Gtk.Window.Gtk_Window_Record'Class;
       Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class);
   procedure Initialize_For_Manager
      (Widget  : not null access Gtk_Recent_Chooser_Dialog_Record'Class;
       Title   : UTF8_String := "";
       Parent  : access Gtk.Window.Gtk_Window_Record'Class;
       Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class);
   --  Initialize_For_Manager does nothing if the object was already created
   --  with another call to Initialize* or G_New.

   function Gtk_Recent_Chooser_Dialog_New_For_Manager
      (Title   : UTF8_String := "";
       Parent  : access Gtk.Window.Gtk_Window_Record'Class;
       Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
       return Gtk_Recent_Chooser_Dialog;

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_chooser_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set_Sort_Func
      (Chooser      : not null access Gtk_Recent_Chooser_Dialog_Record;
       Sort_Func    : Gtk_Recent_Sort_Func;
       Data_Destroy : Glib.G_Destroy_Notify_Address);
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  Chooser has the sort type set to GTK_RECENT_SORT_CUSTOM then the chooser
   --  will sort using this function.
   --  To the comparison function will be passed two
   --  Gtk.Recent_Info.Gtk_Recent_Info structs and Sort_Data; Sort_Func should
   --  return a positive integer if the first item comes before the second,
   --  zero if the two items are equal and a negative integer if the first item
   --  comes after the second.
   --  Since: gtk+ 2.10
   --  "sort_func": the comparison function
   --  "data_destroy": destroy notifier for Sort_Data, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Sort_Func_User_Data is

      type Gtk_Recent_Sort_Func is access function
        (A         : Gtk.Recent_Info.Gtk_Recent_Info;
         B         : Gtk.Recent_Info.Gtk_Recent_Info;
         User_Data : User_Data_Type) return Glib.Gint;

      procedure Set_Sort_Func
         (Chooser      : not null access Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog_Record'Class;
          Sort_Func    : Gtk_Recent_Sort_Func;
          Sort_Data    : User_Data_Type;
          Data_Destroy : Glib.G_Destroy_Notify_Address);
      --  Sets the comparison function used when sorting to be Sort_Func. If
      --  the Chooser has the sort type set to GTK_RECENT_SORT_CUSTOM then the
      --  chooser will sort using this function.
      --  To the comparison function will be passed two
      --  Gtk.Recent_Info.Gtk_Recent_Info structs and Sort_Data; Sort_Func
      --  should return a positive integer if the first item comes before the
      --  second, zero if the two items are equal and a negative integer if the
      --  first item comes after the second.
      --  Since: gtk+ 2.10
      --  "sort_func": the comparison function
      --  "sort_data": user data to pass to Sort_Func, or null
      --  "data_destroy": destroy notifier for Sort_Data, or null

   end Set_Sort_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   function Get_Current_Item
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Gtk.Recent_Info.Gtk_Recent_Info;

   function Get_Current_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return UTF8_String;

   function Set_Current_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   function Get_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter;

   procedure Set_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   function Get_Items
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;

   function Get_Limit
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Glib.Gint;

   procedure Set_Limit
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       Limit   : Glib.Gint);

   function Get_Local_Only
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_Recent_Chooser_Dialog_Record;
       Local_Only : Boolean);

   function Get_Select_Multiple
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_Recent_Chooser_Dialog_Record;
       Select_Multiple : Boolean);

   function Get_Show_Icons
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Show_Icons
      (Chooser    : not null access Gtk_Recent_Chooser_Dialog_Record;
       Show_Icons : Boolean);

   function Get_Show_Not_Found
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Show_Not_Found
      (Chooser        : not null access Gtk_Recent_Chooser_Dialog_Record;
       Show_Not_Found : Boolean);

   function Get_Show_Private
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Show_Private
      (Chooser      : not null access Gtk_Recent_Chooser_Dialog_Record;
       Show_Private : Boolean);

   function Get_Show_Tips
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Boolean;

   procedure Set_Show_Tips
      (Chooser   : not null access Gtk_Recent_Chooser_Dialog_Record;
       Show_Tips : Boolean);

   function Get_Sort_Type
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Gtk.Recent_Chooser.Gtk_Recent_Sort_Type;

   procedure Set_Sort_Type
      (Chooser   : not null access Gtk_Recent_Chooser_Dialog_Record;
       Sort_Type : Gtk.Recent_Chooser.Gtk_Recent_Sort_Type);

   function List_Filters
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;

   procedure Remove_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class);

   procedure Select_All
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record);

   function Select_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       URI     : UTF8_String) return Boolean;

   procedure Unselect_All
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record);

   procedure Unselect_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Dialog_Record;
       URI     : UTF8_String);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "RecentChooser"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Recent_Chooser_Dialog_Record, Gtk_Recent_Chooser_Dialog);
   function "+"
     (Widget : access Gtk_Recent_Chooser_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Recent_Chooser_Dialog
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Recent_Chooser is new Glib.Types.Implements
     (Gtk.Recent_Chooser.Gtk_Recent_Chooser, Gtk_Recent_Chooser_Dialog_Record, Gtk_Recent_Chooser_Dialog);
   function "+"
     (Widget : access Gtk_Recent_Chooser_Dialog_Record'Class)
   return Gtk.Recent_Chooser.Gtk_Recent_Chooser
   renames Implements_Gtk_Recent_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.Recent_Chooser.Gtk_Recent_Chooser)
   return Gtk_Recent_Chooser_Dialog
   renames Implements_Gtk_Recent_Chooser.To_Object;

end Gtk.Recent_Chooser_Dialog;
