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
--         General Public License for more details.                  --
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


with Gdk.Color;
with Gdk.Font;
with Gtk.Adjustment;
with Gtk.Editable;

package Gtk.Text is

   type Gtk_Text is new Gtk.Editable.Gtk_Editable with private;

   function Backward_Delete
      (Text   : in Gtk_Text'Class;
       Nchars : in Guint)
       return      Gint;
   function Forward_Delete
      (Text   : in Gtk_Text'Class;
       Nchars : in Guint)
       return      Gint;
   procedure Freeze (Text : in Gtk_Text'Class);
   function Get_Gap_Position (Widget : in Gtk_Text'Class)
                              return      Guint;
   function Get_Gap_Size (Widget : in Gtk_Text'Class)
                          return      Guint;
   function Get_Hadj (Widget : in Gtk_Text'Class)
                      return Gtk.Adjustment.Gtk_Adjustment;
   function Get_Length (Text   : in Gtk_Text'Class)
                        return      Guint;
   function Get_Point (Text   : in Gtk_Text'Class)
                       return      Guint;
   function Get_Text (Widget : in Gtk_Text'Class)
                      return      String;
   function Get_Text_End (Widget : in Gtk_Text'Class)
                          return      Guint;
   procedure Gtk_New
      (Widget : out Gtk_Text;
       Hadj   : in Gtk.Adjustment.Gtk_Adjustment'Class
                   := Gtk.Adjustment.Null_Adjustment;
       Vadj   : in Gtk.Adjustment.Gtk_Adjustment'Class
                   := Gtk.Adjustment.Null_Adjustment);
   function Get_Vadj (Widget : in Gtk_Text'Class)
                      return Gtk.Adjustment.Gtk_Adjustment;
   procedure Insert
      (Text   : in Gtk_Text'Class;
       Font   : in Gdk.Font.Gdk_Font'Class;
       Fore   : in Gdk.Color.Gdk_Color'Class;
       Back   : in Gdk.Color.Gdk_Color'Class;
       Chars  : in String;
       Length : in Gint);
   procedure Set_Adjustments
      (Text : in Gtk_Text'Class;
       Hadj : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadj : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Set_Editable
      (Text     : in Gtk_Text'Class;
       Editable : in Boolean);
   procedure Set_Point
      (Text  : in Gtk_Text'Class;
       Index : in Guint);
   procedure Set_Word_Wrap
      (Text      : in Gtk_Text'Class;
       Word_Wrap : in Boolean);
   procedure Thaw (Text : in Gtk_Text'Class);

private
   type Gtk_Text is new Gtk.Editable.Gtk_Editable with null record;

end Gtk.Text;
