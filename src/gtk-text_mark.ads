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

--  <description>
--  The Gtk_Text_Mark data type.
--  </description>
--  <c_version>1.3.4</c_version>

with Glib.Values;

package Gtk.Text_Mark is

   type Gtk_Text_Mark_Record is new GObject_Record with private;
   type Gtk_Text_Mark is access all Gtk_Text_Mark_Record'Class;

   --  ??? How are the marks destroyed? Is this automatic when the
   --  ??? associated buffer is destroyed? Is this automatic when
   --  ??? the text area where the mark is located is destroyed?
   --  ??? More investigation needed.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Label.

   procedure Set_Visible
     (Mark    : access Gtk_Text_Mark_Record;
      Setting : Boolean := True);
   --  Set the visibility of Mark.
   --  The insertion point is normally visible, i.e. you can see it as a
   --  vertical bar. Also, the text widget uses a visible mark to indicate
   --  where a drop will occur when dragging-and-dropping text. Most other
   --  marks are not visible.
   --  Marks are not visible by default.

   function Get_Visible
     (Mark : access Gtk_Text_Mark_Record) return Boolean;
   --  Return True if the mark is visible.
   --  i.e. a cursor is displayed for it.

   function Get_Name (Mark : access Gtk_Text_Mark_Record) return String;
   --  Return the mark name; Return "" for anonymous marks.

   function Get_Deleted (Mark : access Gtk_Text_Mark_Record) return Boolean;
   --  Returns True if the mark has been removed from its buffer with
   --  Gtk.Text_Buffer.Delete_Mark. Marks can't be used once deleted.

   --  function Get_Buffer (Mark   : access Gtk_Text_Mark_Record)
   --                       return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  ??? Can not be bound here to avoid circular dependency with
   --  ??? Gtk.Text_Buffer.
   --  Get the buffer this mark is located inside, or null if the mark is
   --  deleted.

   function Get_Left_Gravity
     (Mark : access Gtk_Text_Mark_Record) return Boolean;
   --  Return True if the mark has left gravity, False otherwise.

   -------------------------------
   -- Converting to/from GValue --
   -------------------------------

   procedure Set_Text_Mark
     (Val  : in out Glib.Values.GValue;
      Mark : access Gtk_Text_Mark_Record);
   --  Set the value of the given GValue to Mark.

   function Get_Text_Mark (Val  : Glib.Values.GValue) return Gtk_Text_Mark;
   --  Extract the iterator from the given GValue.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private

   type Gtk_Text_Mark_Record is new GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_text_mark_get_type");

end Gtk.Text_Mark;
