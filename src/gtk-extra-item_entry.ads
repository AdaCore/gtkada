-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

--  <description>
--  A Gtk_Item_Entry is a special kind of entry item used in a
--  Gtk_Sheet to edit the current cell.
--  It can be used independently, but you should rather use a more general
--  Gtk_Entry widget.
--  </description>
--  <c_version>gtk+extra 0.99.1</c_version>

with Gtk.Enums;
with Gtk.GEntry;

package Gtk.Extra.Item_Entry is

   type Gtk_IEntry_Record is new Gtk.GEntry.Gtk_Entry_Record with private;
   type Gtk_IEntry is access all Gtk_IEntry_Record'Class;

   procedure Gtk_New (Widget : out Gtk_IEntry;
                      Max    : in Guint16 := 0);
   --  Create a new entry item.
   --  By default, the maximal length depends only on the size of the widget.

   procedure Initialize (Widget : access Gtk_IEntry_Record'Class;
                         Max    : in Guint16);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Item_Entry.

   procedure Set_Justification
     (Item_Entry    : access Gtk_IEntry_Record;
      Justification : in Gtk.Enums.Gtk_Justification);
   --  Change the justification of the text in the entry.

   procedure Set_Text
     (Item_Entry    : access Gtk_IEntry_Record;
      Text          : in String;
      Justification : in Gtk.Enums.Gtk_Justification);
   --  Change the text in the entry.

   function Get_Justification (Item_Entry    : access Gtk_IEntry_Record)
                              return Gtk.Enums.Gtk_Justification;
   --  Return the current justification for the entry.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>
private
   type Gtk_IEntry_Record is new Gtk.GEntry.Gtk_Entry_Record with null record;
   pragma Import (C, Get_Type, "gtk_item_entry_get_type");
end Gtk.Extra.Item_Entry;
