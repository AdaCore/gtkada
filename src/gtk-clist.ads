-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------


with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gdk.Window;
with Gtk.Button;
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

with Interfaces.C.Strings;

package Gtk.CList is

   type Gtk_CList is new Gtk.Container.Gtk_Container with private;

   type Line_Data is array (Gint range <>)
     of Interfaces.C.Strings.chars_ptr;

   procedure Free_Line_Data (Data : in out Line_Data);
   --  Free all the strings in Data


   function Append
      (Clist : in Gtk_CList'Class;
       Text  : in Line_Data)
       return      Gint;
   --  Return the index of the row

   procedure Clear (Clist : in Gtk_CList'Class);
   procedure Column_Title_Active
      (Clist  : in Gtk_CList'Class;
       Column : in Gint);
   procedure Column_Title_Passive
      (Clist  : in Gtk_CList'Class;
       Column : in Gint);
   procedure Column_Titles_Active (Clist : in Gtk_CList'Class);
   procedure Column_Titles_Hide (Clist : in Gtk_CList'Class);
   procedure Column_Titles_Passive (Clist : in Gtk_CList'Class);
   procedure Column_Titles_Show (Clist : in Gtk_CList'Class);
   procedure Freeze (Clist : in Gtk_CList'Class);
   function Get_Cell_Type
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint)
       return      Gtk_Cell_Type;

   function Get_Clist_Window (Clist : in Gtk_CList'Class)
                              return     Gdk.Window.Gdk_Window;

   function Get_Column_Button (Clist  : in Gtk_CList'Class;
                               Column : in Gint)
                               return Gtk.Button.Gtk_Button;
   --  This function does not exist in C. It returns the button at the top
   --  of the column

   procedure Get_Pixmap
     (Clist    : in Gtk_CList'Class;
      Row      : in Gint;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap'Class;
      Is_Valid : out Boolean);
   --  The result is meaningful only if Is_Valid is True

   procedure  Get_Pixtext
      (Clist    : in Gtk_CList'Class;
       Row      : in Gint;
       Column   : in Gint;
       Text     : in out String;
       Spacing  : in out Guint8;
       Pixmap   : in out Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask     : in out Gdk.Bitmap.Gdk_Bitmap'Class;
       Is_Valid : out Boolean);
   --  The result is not meaningful is Is_Valid is false

   function Get_Selection (Widget : in Gtk_CList'Class)
                           return      Gint_List.Glist;
   procedure Get_Selection_Info
      (Clist    : in Gtk_CList'Class;
       X        : in Gint;
       Y        : in Gint;
       Row      : out Gint;
       Column   : out Gint;
       Is_Valid : out Boolean);
   --  The result is valid only if Is_Valid is true

   procedure Get_Text
     (Clist    : in Gtk_CList'Class;
      Row      : in Gint;
      Column   : in Gint;
      Text     : out String;
      Is_Valid : out Boolean);
   --  The result is meaningful only if Is_Valid is True

   procedure Gtk_New (Widget  : out Gtk_CList;
                      Columns : in Gint);
   procedure Gtk_New
      (Widget  : out Gtk_CList;
       Columns : in Gint;
       Titles  : in Line_Data);
   procedure Insert
      (Clist : in Gtk_CList'Class;
       Row   : in Gint;
       Text  : in Line_Data);
   procedure Moveto
      (Clist     : in Gtk_CList'Class;
       Row       : in Gint;
       Column    : in Gint;
       Row_Align : in Gfloat;
       Col_Align : in Gfloat);
   procedure Remove
      (Clist : in Gtk_CList'Class;
       Row   : in Gint);
   function Row_Is_Visible
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint)
       return      Gtk_Visibility;
   procedure Select_Row
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint);
   procedure Set_Background
      (Clist : in Gtk_CList'Class;
       Row   : in Gint;
       Color : in Gdk.Color.Gdk_Color'Class);
   procedure Set_Border
      (Clist  : in Gtk_CList'Class;
       Border : in Gtk_Shadow_Type);
   procedure Set_Column_Justification
      (Clist         : in Gtk_CList'Class;
       Column        : in Gint;
       Justification : in Gtk_Justification);
   procedure Set_Column_Title
      (Clist  : in Gtk_CList'Class;
       Column : in Gint;
       Title  : in String);
   procedure Set_Column_Widget
      (Clist  : in Gtk_CList'Class;
       Column : in Gint;
       Widget : in Gtk.Widget.Gtk_Widget'Class);
   procedure Set_Column_Width
      (Clist  : in Gtk_CList'Class;
       Column : in Gint;
       Width  : in Gint);
   procedure Set_Foreground
      (Clist : in Gtk_CList'Class;
       Row   : in Gint;
       Color : in Gdk.Color.Gdk_Color'Class);
   procedure Set_Pixmap
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set_Pixtext
      (Clist   : in Gtk_CList'Class;
       Row     : in Gint;
       Column  : in Gint;
       Text    : in String;
       Spacing : in Guint8;
       Pixmap  : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask    : in Gdk.Bitmap.Gdk_Bitmap'Class);
   procedure Set_Policy
      (Clist             : in Gtk_CList'Class;
       Vscrollbar_Policy : in Gtk_Policy_Type;
       Hscrollbar_Policy : in Gtk_Policy_Type);
   procedure Set_Row_Height
      (Clist  : in Gtk_CList'Class;
       Height : in Gint);
   procedure Set_Selection_Mode
      (Clist : in Gtk_CList'Class;
       Mode  : in Gtk_Selection_Mode);
   procedure Set_Shift
      (Clist      : in Gtk_CList'Class;
       Row        : in Gint;
       Column     : in Gint;
       Vertical   : in Gint;
       Horizontal : in Gint);
   procedure Set_Text
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint;
       Text   : in String);
   procedure Thaw (Clist : in Gtk_CList'Class);
   procedure Unselect_Row
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint);


   ---------------
   -- Row_Data --
   ---------------

   generic
      type Data_Type (<>) is private;
   package Row_Data is
      function Get (Object : in Gtk_CList'Class;
                    Row    : in Gint)
                    return Data_Type;
      --  mapping: Row_Data.Get gtkclist.h gtk_clist_get_row_data

      procedure Set (Object : in Gtk_CList'Class;
                     Row    : in Gint;
                     Data   : in Data_Type);
      --  mapping: Row_Data.Set gtkclist.h gtk_clist_set_row_data
   end Row_Data;

   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability


private
   type Gtk_CList is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Append gtkclist.h gtk_clist_append
   --  mapping: Clear gtkclist.h gtk_clist_clear
   --  mapping: Column_Title_Active gtkclist.h gtk_clist_column_title_active
   --  mapping: Column_Title_Passive gtkclist.h gtk_clist_column_title_passive
   --  mapping: Column_Titles_Active gtkclist.h gtk_clist_column_titles_active
   --  mapping: Column_Titles_Hide gtkclist.h gtk_clist_column_titles_hide
   --  mapping: Column_Titles_Passive gtkclist.h \
   --  mapping:      gtk_clist_column_titles_passive
   --  mapping: Column_Titles_Show gtkclist.h gtk_clist_column_titles_show
   --  mapping: NOT_IMPLEMENTED gtkclist.h gtk_clist_construct
   --  mapping: NOT_IMPLEMENTED gtkclist.h gtk_clist_find_row_from_data
   --  mapping: Freeze gtkclist.h gtk_clist_freeze
   --  mapping: Get_Cell_Type gtkclist.h gtk_clist_get_cell_type
   --  mapping: Get_Pixmap gtkclist.h gtk_clist_get_pixmap
   --  mapping: Get_Pixtext gtkclist.h gtk_clist_get_pixtext
   --  mapping: Get_Selection gtkclist.h GtkCList->selection
   --  mapping: Get_Selection_Info gtkclist.h gtk_clist_get_selection_info
   --  mapping: Get_Text gtkclist.h gtk_clist_get_text
   --  mapping: NOT_IMPLEMENTED gtkclist.h gtk_clist_get_type
   --  mapping: Gtk_New gtkclist.h gtk_clist_new
   --  mapping: Gtk_New gtkclist.h gtk_clist_new_with_titles
   --  mapping: Insert gtkclist.h gtk_clist_insert
   --  mapping: Moveto gtkclist.h gtk_clist_moveto
   --  mapping: Remove gtkclist.h gtk_clist_remove
   --  mapping: Row_Is_Visible gtkclist.h gtk_clist_row_is_visible
   --  mapping: Select_Row gtkclist.h gtk_clist_select_row
   --  mapping: Set_Background gtkclist.h gtk_clist_set_background
   --  mapping: Set_Border gtkclist.h gtk_clist_set_border
   --  mapping: Set_Column_Justification gtkclist.h \
   --  mapping:      gtk_clist_set_column_justification
   --  mapping: Set_Column_Title gtkclist.h gtk_clist_set_column_title
   --  mapping: Set_Column_Widget gtkclist.h gtk_clist_set_column_widget
   --  mapping: Set_Column_Width gtkclist.h gtk_clist_set_column_width
   --  mapping: Set_Foreground gtkclist.h gtk_clist_set_foreground
   --  mapping: Set_Pixmap gtkclist.h gtk_clist_set_pixmap
   --  mapping: Set_Pixtext gtkclist.h gtk_clist_set_pixtext
   --  mapping: Set_Policy gtkclist.h gtk_clist_set_policy
   --  mapping: NOT_IMPLEMENTED gtkclist.h gtk_clist_set_row_data_full
   --  mapping: Set_Row_Height gtkclist.h gtk_clist_set_row_height
   --  mapping: Set_Selection_Mode gtkclist.h gtk_clist_set_selection_mode
   --  mapping: Set_Shift gtkclist.h gtk_clist_set_shift
   --  mapping: Set_Text gtkclist.h gtk_clist_set_text
   --  mapping: Thaw gtkclist.h gtk_clist_thaw
   --  mapping: Unselect_Row gtkclist.h gtk_clist_unselect_row
end Gtk.CList;
