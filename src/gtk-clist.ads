-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget;

with Interfaces.C.Strings;

package Gtk.Clist is

   type Gtk_Clist_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Clist is access all Gtk_Clist_Record'Class;

   type Line_Data is array (Gint range <>)
     of Interfaces.C.Strings.chars_ptr;

   procedure Free_Line_Data (Data : in out Line_Data);
   --  Free all the strings in Data

   function Append
     (Clist : access Gtk_Clist_Record;
      Text  : in Line_Data)
      return      Gint;
   --  Return the index of the row

   procedure Clear (Clist : access Gtk_Clist_Record);

   procedure Column_Title_Active (Clist : access Gtk_Clist_Record;
                                  Column : in Gint);

   procedure Column_Title_Passive (Clist : access Gtk_Clist_Record;
                                   Column : in Gint);

   procedure Column_Titles_Active (Clist : access Gtk_Clist_Record);

   procedure Column_Titles_Hide (Clist : access Gtk_Clist_Record);

   procedure Column_Titles_Passive (Clist : access Gtk_Clist_Record);

   procedure Column_Titles_Show (Clist : access Gtk_Clist_Record);

   function Columns_Autosize (Clist  : access Gtk_Clist_Record) return Gint;
   --  Returns the total width of all columns after autosize.

   procedure Freeze (Clist : access Gtk_Clist_Record);

   function Get_Cell_Type
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint)
      return      Gtk_Cell_Type;

   function Get_Cell_Style (Clist  : access Gtk_Clist_Record;
                            Row    : in     Gint;
                            Column : in     Gint)
                            return          Gtk.Style.Gtk_Style'Class;

   function Get_Clist_Window (Clist : access Gtk_Clist_Record)
                              return Gdk.Window.Gdk_Window;

   function Get_Column_Widget (Clist  : access Gtk_Clist_Record;
                               Column : in Gint)
                               return Gtk.Widget.Gtk_Widget;

   function Get_Column_Title
     (Clist  : access Gtk_Clist_Record; Column : in Gint)
      return String;

   function Get_Hadjustment (Clist  : access  Gtk_Clist_Record)
                             return           Gtk.Adjustment.Gtk_Adjustment;

   procedure Get_Pixmap
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap'Class;
      Is_Valid : out Boolean);
   --  The result is meaningful only if Is_Valid is True

   procedure  Get_Pixtext
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Spacing  : in out Guint8;
      Pixmap   : in out Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask     : in out Gdk.Bitmap.Gdk_Bitmap'Class;
      Is_Valid : out Boolean);
   --  The result is not meaningful if Is_Valid is false
   --  The only way to get the string is to use Get_Text (See below)

   function Get_Row_Style (Clist  : access Gtk_Clist_Record;
                           Row    : in     Gint)
                           return          Gtk.Style.Gtk_Style'Class;

   function Get_Selectable
     (Clist : access Gtk_Clist_Record; Row : Gint)
      return Boolean;

   function Get_Selection (Widget : access Gtk_Clist_Record)
                           return Gint_List.Glist;

   function Get_Selection_Mode (Clist : access Gtk_Clist_Record)
                                return Gtk_Selection_Mode;

   procedure Get_Selection_Info
     (Clist    : access Gtk_Clist_Record;
      X        : in Gint;
      Y        : in Gint;
      Row      : out Gint;
      Column   : out Gint;
      Is_Valid : out Boolean);
   --  The result is valid only if Is_Valid is true

   function Get_Text
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint) return String;
   --  If there was a problem, a null-length string is returned.
   --  The problem might appear in case the row or the column are
   --  invalid, or if the cell does not contain any text.

   function Get_Vadjustment (Clist  : access Gtk_Clist_Record)
                             return          Gtk.Adjustment.Gtk_Adjustment;

   procedure Gtk_New (Widget : out Gtk_Clist; Columns : in Gint);
   procedure Initialize (Widget : access Gtk_Clist_Record; Columns : in Gint);

   procedure Gtk_New
     (Widget  : out Gtk_Clist;
      Columns : in Gint;
      Titles  : in Line_Data);
   procedure Initialize
     (Widget  : access Gtk_Clist_Record;
      Columns : in Gint;
      Titles  : in Line_Data);

   procedure Insert
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Text  : in Line_Data);

   procedure Moveto
     (Clist     : access Gtk_Clist_Record;
      Row       : in Gint;
      Column    : in Gint;
      Row_Align : in Gfloat;
      Col_Align : in Gfloat);

   procedure Remove (Clist : access Gtk_Clist_Record; Row : in Gint);

   function Row_Is_Visible (Clist : access Gtk_Clist_Record; Row  : in Gint)
                            return      Gtk_Visibility;

   function Prepend (Clist : access Gtk_Clist_Record; Text  : in Line_Data)
                     return      Gint;

   procedure Row_Move (Clist      : access Gtk_Clist_Record;
                       Source_Row : in     Gint;
                       Dest_Row   : in     Gint);

   procedure Select_All (Clist : access Gtk_Clist_Record);

   procedure Select_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint);

   procedure Set_Background
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color);

   procedure Set_Button_Actions (Clist         : access Gtk_Clist_Record;
                                 Button        :        Guint;
                                 Button_Action :        Gtk_Button_Action);

   procedure Set_Cell_Style (Clist  : access Gtk_Clist_Record;
                             Row    : in Gint;
                             Column : in Gint;
                             Style  : in Gtk_Style'Class);

   procedure Set_Column_Auto_Resize
     (Clist       : access Gtk_Clist_Record;
      Column      : in Gint;
      Auto_Resize : in Boolean);

   procedure Set_Column_Justification
     (Clist         : access Gtk_Clist_Record;
      Column        : in Gint;
      Justification : in Gtk_Justification);

   procedure Set_Column_Min_Width
     (Clist : access Gtk_Clist_Record; Column : Gint; Min_Width : Gint);

   procedure Set_Column_Max_Width
     (Clist : access Gtk_Clist_Record; Column : Gint; Max_Width : Gint);

   procedure Set_Column_Resizeable
     (Clist    : access Gtk_Clist_Record;
      Column   : in Gint;
      Resizable : in Boolean);

   procedure Set_Column_Title
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Title  : in String);

   procedure Set_Column_Visibility
     (Clist   : access Gtk_Clist_Record;
      Column  : in Gint;
      Visible : in Boolean);

   procedure Set_Column_Widget
     (Clist  : access Gtk_Clist_Record;
      Column : in     Gint;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Column_Width
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Width  : in Gint);

   procedure Set_Foreground
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color);

   procedure Set_Hadjustment
      (Clist      : access Gtk_Clist_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Set_Pixmap
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class);

   procedure Set_Pixtext
     (Clist   : access Gtk_Clist_Record;
      Row     : in Gint;
      Column  : in Gint;
      Text    : in String;
      Spacing : in Guint8;
      Pixmap  : in Gdk.Pixmap.Gdk_Pixmap'Class;
      Mask    : in Gdk.Bitmap.Gdk_Bitmap'Class);

   procedure Set_Reorderable
     (Clist : access Gtk_Clist_Record; Reorderable : Boolean);

   procedure Set_Row_Height
     (Clist : access Gtk_Clist_Record; Height : Gint);

   procedure Set_Row_Style
     (Clist : access Gtk_Clist_Record; Row : Gint;
      Style : in Gtk_Style'Class);

   procedure Set_Selectable
     (Clist : access Gtk_Clist_Record; Row : Gint; Selectable : Boolean);

   procedure Set_Selection_Mode (Clist : access Gtk_Clist_Record;
                                 Mode  : in Gtk_Selection_Mode);

   procedure Set_Shadow_Type (Clist    : access Gtk_Clist_Record;
                              The_Type : in Gtk_Shadow_Type);

   procedure Set_Shift
     (Clist      : access Gtk_Clist_Record;
      Row        : in Gint;
      Column     : in Gint;
      Vertical   : in Gint;
      Horizontal : in Gint);

   procedure Set_Text
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Text   : in String);

   procedure Set_Use_Drag_Icons
     (Clist : access Gtk_Clist_Record; Use_Icons : Boolean);

   procedure Set_Vadjustment
     (Clist      : access Gtk_Clist_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Thaw (Clist : access Gtk_Clist_Record);

   function Optimal_Column_Width
     (Clist : access Gtk_Clist_Record; Column : Gint) return Gint;

   procedure Unselect_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint);

   procedure Undo_Selection (Clist  : access Gtk_Clist_Record);

   procedure Unselect_All (Clist : access Gtk_Clist_Record);

   ---------------
   -- Row_Data --
   ---------------

   generic
      type Data_Type (<>) is private;
   package Row_Data is
      function Get (Object : access Gtk_Clist_Record; Row : in Gint)
                    return Data_Type;

      procedure Set (Object : access Gtk_Clist_Record;
                     Row    : in Gint;
                     Data   : in Data_Type);
   end Row_Data;

   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability


private
   type Gtk_Clist_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Clist;
