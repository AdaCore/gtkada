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

--  <c_version>1.2.6</c_version>

with Gdk.Color;
with Gdk.Font;
with Gtk.Object; use Gtk.Object;
with Gtk.Adjustment;
with Gtk.Editable;

package Gtk.Text is

   type Gtk_Text_Record is new Gtk.Editable.Gtk_Editable_Record with private;
   type Gtk_Text is access all Gtk_Text_Record'Class;

   function Backward_Delete (Text : access Gtk_Text_Record; Nchars : in Guint)
     return Gint;

   function Forward_Delete (Text : access Gtk_Text_Record; Nchars : in Guint)
     return Gint;

   procedure Freeze (Text : access Gtk_Text_Record);

   function Get_Gap_Position (Text : access Gtk_Text_Record) return Guint;

   function Get_Gap_Size (Text : access Gtk_Text_Record) return Guint;

   function Get_Hadj (Text : access Gtk_Text_Record)
     return Gtk.Adjustment.Gtk_Adjustment;

   function Get_Length (Text : access Gtk_Text_Record) return Guint;

   function Get_Point (Text : access Gtk_Text_Record) return Guint;

   function Get_Text (Text : access Gtk_Text_Record) return String;

   function Get_Text_End (Text : access Gtk_Text_Record) return Guint;

   procedure Gtk_New
     (Text : out Gtk_Text;
      Hadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);

   procedure Initialize
     (Text : access Gtk_Text_Record'Class;
      Hadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadj : in Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);

   function Get_Vadj (Text : access Gtk_Text_Record)
     return Gtk.Adjustment.Gtk_Adjustment;

   procedure Insert
     (Text   : access Gtk_Text_Record;
      Font   : in Gdk.Font.Gdk_Font'Class;
      Fore   : in Gdk.Color.Gdk_Color;
      Back   : in Gdk.Color.Gdk_Color;
      Chars  : in String;
      Length : in Gint);

   procedure Set_Adjustments
     (Text : access Gtk_Text_Record;
      Hadj : Gtk.Adjustment.Gtk_Adjustment;
      Vadj : Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Editable
     (Text : access Gtk_Text_Record; Editable : in Boolean);

   procedure Set_Line_Wrap
     (Text : access Gtk_Text_Record; Line_Wrap : in Boolean);

   procedure Set_Point (Text : access Gtk_Text_Record; Index : in Guint);

   procedure Set_Word_Wrap
     (Text : access Gtk_Text_Record; Word_Wrap : in Boolean);

   procedure Thaw (Text : access Gtk_Text_Record);

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Text : in out Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private
   type Gtk_Text_Record is new Gtk.Editable.Gtk_Editable_Record
     with null record;

end Gtk.Text;
