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

with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Frame is

   type Gtk_Frame_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Frame is access all Gtk_Frame_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Frame;
                      Label  : in String := "");
   procedure Initialize (Widget : access Gtk_Frame_Record;
                         Label  : in String := "");
   procedure Set_Label
      (Frame : access Gtk_Frame_Record;
       Label : in String);
   procedure Set_Label_Align
      (Frame  : access Gtk_Frame_Record;
       Xalign : in Gfloat;
       Yalign : in Gfloat);
   procedure Set_Shadow_Type
      (Frame    : access Gtk_Frame_Record;
       The_Type : in Gtk_Shadow_Type);

private
   type Gtk_Frame_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

end Gtk.Frame;
