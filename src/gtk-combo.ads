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


with Gtk.GEntry;
with Gtk.List;
with Gtk.Box;
with Gtk.Item;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Object;

package Gtk.Combo is

   type Gtk_Combo_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Combo is access all Gtk_Combo_Record'Class;

   procedure Disable_Activate (Combo_Box : access Gtk_Combo_Record);

   function Get_Entry (Combo_Box : access Gtk_Combo_Record)
     return Gtk.GEntry.Gtk_Entry;
   procedure Set_Entry (Combo_Box : access Gtk_Combo_Record;
                        GEntry    : Gtk.GEntry.Gtk_Entry);
   --  Get or sets the entry fields for the combo box.


   function Get_List (Combo_Box : access Gtk_Combo_Record)
     return Gtk.List.Gtk_List;

   procedure Gtk_New (Combo_Box : out Gtk_Combo);
   procedure Initialize (Combo_Box : access Gtk_Combo_Record'Class);

   procedure Set_Case_Sensitive (Combo_Box : access Gtk_Combo_Record;
                                 Val : in Boolean);

   procedure Set_Item_String (Combo_Box  : access Gtk_Combo_Record;
                              Item       : in Gtk.Item.Gtk_Item;
                              Item_Value : in String);

   procedure Set_Popdown_Strings (Combo_Box : access Gtk_Combo_Record;
                                  Strings   : in String_List.Glist);

   procedure Set_Use_Arrows (Combo_Box : access Gtk_Combo_Record;
                             Val : in Boolean);

   procedure Set_Use_Arrows_Always (Combo_Box : access Gtk_Combo_Record;
                                    Val : in Boolean);

   procedure Set_Value_In_List (Combo_Box   : access Gtk_Combo_Record;
                                Val         : in Gint;
                                Ok_If_Empty : in Boolean);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (Combo_Box : in out Object.Gtk_Object; N : in Node_Ptr);

private

   type Gtk_Combo_Record is new Gtk.Box.Gtk_Box_Record with null record;

end Gtk.Combo;
