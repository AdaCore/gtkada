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

with Gtk.Widget;

package Gtk.Editable is

   type Gtk_Editable_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Editable is access all Gtk_Editable_Record'Class;

   procedure Changed (Editable : access Gtk_Editable_Record);
   procedure Claim_Selection
      (Editable : access Gtk_Editable_Record;
       Claim    : in Boolean;
       Time     : in Guint32);
   procedure Copy_Clipboard
      (Editable : access Gtk_Editable_Record;
       Time     : in Guint32);
   procedure Cut_Clipboard
      (Editable : access Gtk_Editable_Record;
       Time     : in Guint32);
   procedure Delete_Selection (Editable : access Gtk_Editable_Record);
   procedure Delete_Text
      (Editable  : access Gtk_Editable_Record;
       Start_Pos : in Gint;
       End_Pos   : in Gint);
   function Get_Chars
      (Editable  : access Gtk_Editable_Record;
       Start_Pos : in Gint;
       End_Pos   : in Gint)
       return         String;
   function Get_Clipboard_Text (Widget : access Gtk_Editable_Record)
                                return      String;
   function Get_Current_Pos (Widget : access Gtk_Editable_Record)
                             return      Guint;
   function Get_Editable (Widget : access Gtk_Editable_Record)
                          return      Boolean;
   procedure Set_Editable (Widget : access Gtk_Editable_Record;
                           Editable : boolean);
   function Get_Has_Selection (Widget : access Gtk_Editable_Record)
                               return      Boolean;
   function Get_Selection_End_Pos (Widget : access Gtk_Editable_Record)
                                   return      Guint;
   function Get_Selection_Start_Pos (Widget : access Gtk_Editable_Record)
                                     return      Guint;
   procedure Insert_Text
      (Editable        : access Gtk_Editable_Record;
       New_Text        : in String;
       New_Text_Length : in Gint;
       Position        : in out Gint);
   procedure Paste_Clipboard
      (Editable : access Gtk_Editable_Record;
       Time     : in Guint32);
   procedure Select_Region
      (Editable : access Gtk_Editable_Record;
       Start    : in Gint;
       The_End  : in Gint);

   procedure Set_Position (Editable : access Gtk_Editable_Record;
                           Position : Gint);

   function Get_Position (Editable : access Gtk_Editable_Record) return Gint;

private
   type Gtk_Editable_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

end Gtk.Editable;
