-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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


with Gtk.Widget;

package Gtk.Editable is

   type Gtk_Editable is new Gtk.Widget.Gtk_Widget with private;

   procedure Changed (Editable : in Gtk_Editable'Class);
   procedure Claim_Selection
      (Editable : in Gtk_Editable'Class;
       Claim    : in Boolean;
       Time     : in Guint32);
   procedure Copy_Clipboard
      (Editable : in Gtk_Editable'Class;
       Time     : in Guint32);
   procedure Cut_Clipboard
      (Editable : in Gtk_Editable'Class;
       Time     : in Guint32);
   procedure Delete_Selection (Editable : in Gtk_Editable'Class);
   procedure Delete_Text
      (Editable  : in Gtk_Editable'Class;
       Start_Pos : in Gint;
       End_Pos   : in Gint);
   function Get_Chars
      (Editable  : in Gtk_Editable'Class;
       Start_Pos : in Gint;
       End_Pos   : in Gint)
       return         String;
   function Get_Clipboard_Text (Widget : in Gtk_Editable'Class)
                                return      String;
   function Get_Current_Pos (Widget : in Gtk_Editable'Class)
                             return      Guint;
   function Get_Editable (Widget : in Gtk_Editable'Class)
                          return      Boolean;
   function Get_Has_Selection (Widget : in Gtk_Editable'Class)
                               return      Boolean;
   function Get_Selection_End_Pos (Widget : in Gtk_Editable'Class)
                                   return      Guint;
   function Get_Selection_Start_Pos (Widget : in Gtk_Editable'Class)
                                     return      Guint;
   procedure Insert_Text
      (Editable        : in Gtk_Editable'Class;
       New_Text        : in String;
       New_Text_Length : in Gint;
       Position        : in out Gint);
   procedure Paste_Clipboard
      (Editable : in Gtk_Editable'Class;
       Time     : in Guint32);
   procedure Select_Region
      (Editable : in Gtk_Editable'Class;
       Start    : in Gint;
       The_End  : in Gint);

private
   type Gtk_Editable is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: Changed gtkeditable.h gtk_editable_changed
   --  mapping: Claim_Selection gtkeditable.h gtk_editable_claim_selection
   --  mapping: Copy_Clipboard gtkeditable.h gtk_editable_copy_clipboard
   --  mapping: Cut_Clipboard gtkeditable.h gtk_editable_cut_clipboard
   --  mapping: Delete_Selection gtkeditable.h gtk_editable_delete_selection
   --  mapping: Delete_Text gtkeditable.h gtk_editable_delete_text
   --  mapping: Get_Chars gtkeditable.h gtk_editable_get_chars
   --  mapping: Get_Clipboard_Text gtkeditable.h GtkEditable->clipboard_text
   --  mapping: Get_Current_Pos gtkeditable.h GtkEditable->current_pos
   --  mapping: Get_Editable gtkeditable.h GtkEditable->editable
   --  mapping: Get_Has_Selection gtkeditable.h GtkEditable->has_selection
   --  mapping: Get_Selection_End_Pos gtkeditable.h \
   --  mapping:    GtkEditable->selection_end_pos
   --  mapping: Get_Selection_Start_Pos gtkeditable.h \
   --  mapping:    GtkEditable->selection_start_pos
   --  mapping: NOT_IMPLEMENTED gtkeditable.h gtk_editable_get_type
   --  mapping: Insert_Text gtkeditable.h gtk_editable_insert_text
   --  mapping: Paste_Clipboard gtkeditable.h gtk_editable_paste_clipboard
   --  mapping: Select_Region gtkeditable.h gtk_editable_select_region
end Gtk.Editable;
