-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Glib;
with Gtk.Object;
with Gtk.Text_Tag;

package Gtk.Text_Tag_Table is

   type Gtk_Text_Tag_Table_Record is
     new Gtk.Object.Gtk_Object_Record with private;
   type Gtk_Text_Tag_Table is access all Gtk_Text_Tag_Table_Record'Class;

   procedure Gtk_New (Table : out Gtk_Text_Tag_Table);

   procedure Initialize (Table : access Gtk_Text_Tag_Table_Record'Class);

   function Get_Type return Glib.GType;

   procedure Add
     (Table : access Gtk_Text_Tag_Table_Record;
      Tag   : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);

   procedure Remove
     (Table : access Gtk_Text_Tag_Table_Record;
      Tag   : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);
   --  ??? Check what happens when Tag not found.

   function Lookup
     (Table  : access Gtk_Text_Tag_Table_Record;
      Name   : String)
      return Gtk.Text_Tag.Gtk_Text_Tag;

   function Size (Table  : access Gtk_Text_Tag_Table_Record)
                  return Gint;

   generic
      type Data_Type (<>) is private;
   package Iterator is

      type Data_Type_Access is access all Data_Type;

      type Gtk_Text_Tag_Table_Proc is access
        procedure (Tag  : access Gtk_Text_Tag_Table_Record'Class;
                   Data : Data_Type_Access);

      procedure Foreach
        (Table : access Gtk_Text_Tag_Table_Record;
         Proc  : Gtk_Text_Tag_Table_Proc;
         Data  : Data_Type_Access);

   end Iterator;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "tag_changed"
   --    procedure Handler (Widget : access Gtk_Text_Tag_Table_Record'Class;
   --       Tag : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
   --       Size_Changed : Boolean);
   --
   --  - "tag_added"
   --    procedure Handler (Widget : access Gtk_Text_Tag_Table_Record'Class;
   --       Tag : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);
   --
   --  - "tag_removed"
   --    procedure Handler (Widget : access Gtk_Text_Tag_Table_Record'Class;
   --       Tag : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);
   --
   --  </signals>

private
   type Gtk_Text_Tag_Table_Record is
     new Gtk.Object.Gtk_Object_Record with null record;

   pragma Import (C, Get_Type, "gtk_text_tag_table_get_type");

end Gtk.Text_Tag_Table;
