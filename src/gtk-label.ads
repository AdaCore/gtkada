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

with Gtk.Object; use Gtk.Object;
with Gtk.Enums;
with Gtk.Misc;

package Gtk.Label is

   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with private;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   procedure Gtk_New (Label :    out Gtk_Label;
                      Str   : in     String);
   procedure Initialize (Label : access Gtk_Label_Record'Class;
                         Str   : in     String);

   procedure Set_Text (Label : access Gtk_Label_Record;
                       Str   : in String);

   procedure Set_Justify (Label : access Gtk_Label_Record;
                          Jtype : in Enums.Gtk_Justification);

   procedure Set_Pattern (Label   : access Gtk_Label_Record;
                          Pattern : in String);

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record;
                            Wrap  : in Boolean);

   function Get (Label : access Gtk_Label_Record) return String;

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Label : in out Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with null record;

end Gtk.Label;
