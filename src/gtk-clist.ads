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
-- General Public License for more details.                          --
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
      (Clist : in Gtk_CList;
       Text  : in Line_Data)
       return      Gint;
   --  Return the index of the row

   procedure Clear (Clist : in Gtk_CList);

   procedure Column_Title_Active (Clist : in Gtk_CList; Column : in Gint);

   procedure Column_Title_Passive (Clist : in Gtk_CList; Column : in Gint);

   procedure Column_Titles_Active (Clist : in Gtk_CList);

   procedure Column_Titles_Hide (Clist : in Gtk_CList);

   procedure Column_Titles_Passive (Clist : in Gtk_CList);

   procedure Column_Titles_Show (Clist : in Gtk_CList);

   procedure Freeze (Clist : in Gtk_CList);

   function Get_Cell_Type
      (Clist  : in Gtk_CList;
       Row    : in Gint;
       Column : in Gint)
       return      Gtk_Cell_Type;

   function Get_Clist_Window (Clist : in Gtk_CList)
                              return Gdk.Window.Gdk_Window;

   function Get_Column_Button (Clist  : in Gtk_CList;
                               Column : in Gint)
                               return Gtk.Button.Gtk_Button;
   --  This function does not exist in C. It returns the button at the top
   --  of the column

   procedure Get_Pixmap
     (Clist    : in Gtk_CList;
      Row      : in Gint;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap'Class;
      Is_Valid : out Boolean);
   --  The result is meaningful only if Is_Valid is True

   procedure  Get_Pixtext
      (Clist    : in Gtk_CList;
       Row      : in Gint;
       Column   : in Gint;
       Spacing  : in out Guint8;
       Pixmap   : in out Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask     : in out Gdk.Bitmap.Gdk_Bitmap'Class;
       Is_Valid : out Boolean);
   --  The result is not meaningful if Is_Valid is false
   --  The only way to get the string is to use Get_Text (See below)

   function Get_Selection (Widget : in Gtk_CList) return Gint_List.Glist;

   procedure Get_Selection_Info
      (Clist    : in Gtk_CList;
       X        : in Gint;
       Y        : in Gint;
       Row      : out Gint;
       Column   : out Gint;
       Is_Valid : out Boolean);
   --  The result is valid only if Is_Valid is true

   function Get_Text
     (Clist    : in Gtk_CList;
      Row      : in Gint;
      Column   : in Gint) return String;
   --  If there was a problem, a null-length string is returned.
   --  The problem might appear in case the row or the column are
   --  invalid, or if the cell does not contain any text.

   procedure Gtk_New (Widget : out Gtk_CList; Columns : in Gint);

   procedure Gtk_New
      (Widget  : out Gtk_CList;
       Columns : in Gint;
       Titles  : in Line_Data);

   procedure Insert
      (Clist : in Gtk_CList;
       Row   : in Gint;
       Text  : in Line_Data);

   procedure Moveto
      (Clist     : in Gtk_CList;
       Row       : in Gint;
       Column    : in Gint;
       Row_Align : in Gfloat;
       Col_Align : in Gfloat);

   procedure Remove (Clist : in Gtk_CList; Row : in Gint);

   function Row_Is_Visible (Clist : in Gtk_CList; Row  : in Gint)
                            return      Gtk_Visibility;

   procedure Select_Row
      (Clist  : in Gtk_CList;
       Row    : in Gint;
       Column : in Gint);

   procedure Set_Background
      (Clist : in Gtk_CList;
       Row   : in Gint;
       Color : in Gdk.Color.Gdk_Color);

   procedure Set_Border (Clist : in Gtk_CList; Border : in Gtk_Shadow_Type);

   procedure Set_Column_Justification
      (Clist         : in Gtk_CList;
       Column        : in Gint;
       Justification : in Gtk_Justification);

   procedure Set_Column_Title
      (Clist  : in Gtk_CList;
       Column : in Gint;
       Title  : in String);

   procedure Set_Column_Widget
      (Clist  : in Gtk_CList;
       Column : in Gint;
       Widget : in Gtk.Widget.Gtk_Widget'Class);

   procedure Set_Column_Width
      (Clist  : in Gtk_CList;
       Column : in Gint;
       Width  : in Gint);

   procedure Set_Foreground
      (Clist : in Gtk_CList;
       Row   : in Gint;
       Color : in Gdk.Color.Gdk_Color);

   procedure Set_Pixmap
      (Clist  : in Gtk_CList;
       Row    : in Gint;
       Column : in Gint;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);

   procedure Set_Pixtext
      (Clist   : in Gtk_CList;
       Row     : in Gint;
       Column  : in Gint;
       Text    : in String;
       Spacing : in Guint8;
       Pixmap  : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask    : in Gdk.Bitmap.Gdk_Bitmap'Class);

   procedure Set_Row_Height (Clist : in Gtk_CList; Height : in Gint);

   procedure Set_Selection_Mode (Clist : in Gtk_CList;
                                 Mode  : in Gtk_Selection_Mode);

   procedure Set_Shift
      (Clist      : in Gtk_CList;
       Row        : in Gint;
       Column     : in Gint;
       Vertical   : in Gint;
       Horizontal : in Gint);

   procedure Set_Text
      (Clist  : in Gtk_CList;
       Row    : in Gint;
       Column : in Gint;
       Text   : in String);

   procedure Thaw (Clist : in Gtk_CList);

   procedure Unselect_Row
      (Clist  : in Gtk_CList;
       Row    : in Gint;
       Column : in Gint);

   ---------------
   -- Row_Data --
   ---------------

   generic
      type Data_Type (<>) is private;
   package Row_Data is
      function Get (Object : in Gtk_CList; Row : in Gint) return Data_Type;

      procedure Set (Object : in Gtk_CList;
                     Row    : in Gint;
                     Data   : in Data_Type);
   end Row_Data;

   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability


private
   type Gtk_CList is new Gtk.Container.Gtk_Container with null record;

end Gtk.CList;
