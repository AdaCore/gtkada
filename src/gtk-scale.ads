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

with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object;
with Gtk.GRange;

package Gtk.Scale is

   type Gtk_Scale_Record is new Gtk.GRange.Gtk_Range_Record with private;
   type Gtk_Scale is access all Gtk_Scale_Record'Class;
   subtype Gtk_Hscale is Gtk_Scale;
   subtype Gtk_Vscale is Gtk_Scale;

   procedure Draw_Value (Scale : access Gtk_Scale_Record);
   procedure Gtk_New_Hscale
     (Scale      : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Gtk_New_Vscale
     (Scale      : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Hscale
     (Scale      : access Gtk_Scale_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Vscale
     (Scale      : access Gtk_Scale_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment);
   procedure Set_Digits
     (Scale      : access Gtk_Scale_Record;
      The_Digits : in Gint);
   procedure Set_Draw_Value
     (Scale      : access Gtk_Scale_Record;
      Draw_Value : in Boolean);
   procedure Set_Value_Pos
     (Scale : access Gtk_Scale_Record;
      Pos   : in Gtk_Position_Type);
   function Get_Value_Width (Scale  : access Gtk_Scale_Record) return Gint;

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (Scale : in out Object.Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Scale_Record is new Gtk.GRange.Gtk_Range_Record with null record;

end Gtk.Scale;
