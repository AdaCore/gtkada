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

--  with Gdk.Pixbuf;
--  ??? Commented out for the moment, as Gdk.Pixbuf port to gtk2 not done yet
with Glib;
with Glib.GObjects;
with Gtk.Text_Child;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtk.Text_Tag_Table;

package Gtk.Text_Buffer is

   type Gtk_Text_Buffer_Record is
     new Glib.GObjects.GObject_Record with private;
   type Gtk_Text_Buffer is access all Gtk_Text_Buffer_Record'Class;

   procedure Gtk_New (Buffer : out Gtk_Text_Buffer);

   procedure Gtk_New
     (Buffer : out Gtk_Text_Buffer;
      Table  : access Gtk.Text_Tag_Table.Gtk_Text_Tag_Table_Record'Class);

   procedure Initialize (Buffer : access Gtk_Text_Buffer_Record'Class);

   procedure Initialize
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Table  : access Gtk.Text_Tag_Table.Gtk_Text_Tag_Table_Record'Class);

   function Get_Type return Glib.GType;

   function Get_Line_Count (Buffer : access Gtk_Text_Buffer_Record)
                            return Gint;

   function Get_Char_Count (Buffer : access Gtk_Text_Buffer_Record)
                            return Gint;

   function Get_Tag_Table (Buffer : access Gtk_Text_Buffer_Record)
                           return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;

   procedure Set_Text
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : String);

   procedure Insert
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : String);

   procedure Insert_At_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : String);

   procedure Insert_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text             : String;
      Default_Editable : Boolean;
      Result           : out Boolean);
   --  ??? Check the documentation. See if Default_Editable should have
   --  ??? a default value.

   function Insert_Interactive_At_Cursor
     (Buffer           : access Gtk_Text_Buffer_Record;
      Text             : String;
      Default_Editable : Boolean)
      return Boolean;
   --  ??? Same as for Insert_Interactive

   procedure Insert_Range
     (Buffer  : access Gtk_Text_Buffer_Record;
      Iter    : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Insert_Range_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Start            : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable : Boolean;
      Result           : out Boolean);
   --  ??? Same as for Insert_Interactive

   --  gtk_text_buffer_insert_with_tags not bound: variable number of arguments
   --  ??? Write to the Gtk+ team about this.

   --  gtk_text_buffer_insert_with_tags_by_name not bound: variable number
   --  of arguments
   --  ??? Write to the Gtk+ team about this.

   procedure Delete
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : in out Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Delete_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Start_Iter       : in out Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter         : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable : Boolean;
      Result           : out Boolean);

   function Get_Text
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False)
      return String;
   --  ??? Returns an allocated UTF-8 string, check that the UTF-8 does
   --  ??? not cause any problem.
   --  ??? Check for memory leak problem.

   function Get_Slice
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False)
      return String;
   --  ??? Same as for Get_Text.

   --  procedure Insert_Pixbuf
   --    (Buffer : access Gtk_Text_Buffer_Record;
   --     Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
   --     Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  ??? Commented out for the moment, as Gdk.Pixbuf not ported to gtk2 yet.

   procedure Insert_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);

   procedure Create_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result : out Gtk.Text_Child.Gtk_Text_Child_Anchor);

   function Create_Mark
     (Buffer       : access Gtk_Text_Buffer_Record;
      Mark_Name    : String := "";
      Where        : Gtk.Text_Iter.Gtk_Text_Iter;
      Left_Gravity : Boolean := True)
      return Gtk.Text_Mark.Gtk_Text_Mark;

   procedure Move_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Delete_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   function Get_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String)
      return Gtk.Text_Mark.Gtk_Text_Mark;

   procedure Move_Mark_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Delete_Mark_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String);

   function Get_Insert (Buffer : access Gtk_Text_Buffer_Record)
                        return Gtk.Text_Mark.Gtk_Text_Mark;

   function Get_Selection_Bound (Buffer : access Gtk_Text_Buffer_Record)
                                 return Gtk.Text_Mark.Gtk_Text_Mark;

   procedure Place_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Apply_Tag
     (Buffer  : access Gtk_Text_Buffer_Record;
      Tag     : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Remove_Tag
     (Buffer  : access Gtk_Text_Buffer_Record;
      Tag     : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Apply_Tag_By_Name
     (Buffer  : access Gtk_Text_Buffer_Record;
      Name    : String;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Remove_Tag_By_Name
     (Buffer  : access Gtk_Text_Buffer_Record;
      Name    : String;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter);

   --  gtk_text_buffer_create_tag not bound: variable number of arguments
   --  ??? Discuss this with the Gtk+ team.

   procedure Get_Iter_At_Line_Offset
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint;
      Char_Offset : Gint := 0);

   procedure Get_Iter_At_Line_Index
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint;
      Byte_Index  : Gint := 0);

   procedure Get_Iter_At_Offset
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Char_Offset : Gint);

   procedure Get_Iter_At_Line
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint);

   procedure Get_End_Iter
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Get_Bounds
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : out Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Get_Iter_At_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   procedure Get_Iter_At_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);

   function Get_Modified (Buffer : access Gtk_Text_Buffer_Record)
                          return Boolean;

   procedure Set_Modified
     (Buffer  : access Gtk_Text_Buffer_Record;
      Setting : Boolean := True);

   procedure Paste_Primary
     (Buffer            : access Gtk_Text_Buffer_Record;
      Override_Location : Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable  : Boolean);
   --  ??? Same as Get_Text. Find appropriate default value for
   --  ??? Default_Editable.

   procedure Cut_Clipboard
     (Buffer           : access Gtk_Text_Buffer_Record;
      Default_Editable : Boolean);
   --  ??? Same as Paste_Primary

   procedure Copy_Clipboard (Buffer : access Gtk_Text_Buffer_Record);

   procedure Paste_Clipboard
     (Buffer           : access Gtk_Text_Buffer_Record;
      Default_Editable : Boolean);
   --  ??? Same as Paste_Primary

   procedure Get_Selection_Bounds
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : out Gtk.Text_Iter.Gtk_Text_Iter;
      Result  : out Boolean);

   function Delete_Selection
     (Buffer           : access Gtk_Text_Buffer_Record;
      Interactive      : Boolean;
      Default_Editable : Boolean)
      return Boolean;
   --  ??? Same as Paste_Primary

   procedure Begin_User_Action (Buffer : access Gtk_Text_Buffer_Record);

   procedure End_User_Action (Buffer : access Gtk_Text_Buffer_Record);

   -------------
   -- Signals --
   -------------

   --  ??? The mode for the Gtk_Text_Iter parameters are probably
   --  ??? Incorrect. Check each of them, one day.

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "insert_text"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Pos : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       Text : String;
   --       Length : Gint);
   --
   --  - "insert_pixbuf"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Pos : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --
   --  - "insert_child_anchor"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Pos : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);
   --
   --  - "delete_range"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Start : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       The_End : access Gtk.Text_Iter.Gtk_Text_Iter);
   --
   --  - "changed"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  - "modified_changed"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  - "mark_set"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Location : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       Mark : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --
   --  - "mark_deleted"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Mark : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);
   --
   --  - "apply_tag"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Tag : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
   --       Start_Char : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       End_Char : access Gtk.Text_Iter.Gtk_Text_Iter);
   --
   --  - "remove_tag"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class;
   --       Tag : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
   --       Start_Char : access Gtk.Text_Iter.Gtk_Text_Iter;
   --       End_Char : access Gtk.Text_Iter.Gtk_Text_Iter);
   --
   --  - "begin_user_action"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  - "end_user_action"
   --    procedure Handler (Widget : access Gtk_Text_Buffer_Record'Class);
   --
   --  </signals>

private

   type Gtk_Text_Buffer_Record is new Glib.GObjects.GObject_Record with
     null record;

   pragma Import (C, Get_Type, "gtk_text_buffer_get_type");

end Gtk.Text_Buffer;
