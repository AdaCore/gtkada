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

with Gtk.Editable;
with Gtk.Object;

package Gtk.GEntry is

   type Gtk_Entry_Record is new Gtk.Editable.Gtk_Editable_Record with private;
   type Gtk_Entry is access all Gtk_Entry_Record'Class;
   subtype Gtk_GEntry is Gtk_Entry;

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record;
      Text      : in String);
   function Get_Text (The_Entry : access Gtk_Entry_Record)
     return String;
   procedure Gtk_New (Widget : out Gtk_Entry;
                      Max    : in Guint16);
   procedure Gtk_New (Widget : out Gtk_Entry);
   procedure Initialize (Widget : access Gtk_Entry_Record;
                         Max    : in Guint16);
   procedure Initialize (Widget : access Gtk_Entry_Record);

   procedure Prepend_Text
     (The_Entry : access Gtk_Entry_Record;
      Text      : in String);
   procedure Select_Region
     (The_Entry : access Gtk_Entry_Record;
      Start     : in Gint;
      The_End   : in Gint);
   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record;
      Editable  : in Boolean);
   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record;
      Max       : in Guint16);
   procedure Set_Position
     (The_Entry : access Gtk_Entry_Record;
      Position  : in Gint);
   procedure Set_Text
     (The_Entry : access Gtk_Entry_Record;
      Text      : in String);
   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record;
      Visible   : in Boolean);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (The_Entry : in out Object.Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Entry_Record is new Gtk.Editable.Gtk_Editable_Record
     with null record;

end Gtk.GEntry;
