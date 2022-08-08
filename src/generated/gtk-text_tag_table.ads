------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  You may wish to begin by reading the [text widget conceptual
--  overview][TextWidget] which gives an overview of all the objects and data
--  types related to the text widget and how they work together.
--
--  # GtkTextTagTables as GtkBuildable
--
--  The GtkTextTagTable implementation of the GtkBuildable interface supports
--  adding tags by specifying "tag" as the "type" attribute of a <child>
--  element.
--
--  An example of a UI definition fragment specifying tags: |[ <object
--  class="GtkTextTagTable"> <child type="tag"> <object class="GtkTextTag"/>
--  </child> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Text_Tag;  use Gtk.Text_Tag;

package Gtk.Text_Tag_Table is

   type Gtk_Text_Tag_Table_Record is new GObject_Record with null record;
   type Gtk_Text_Tag_Table is access all Gtk_Text_Tag_Table_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Text_Tag_Table_Foreach is access procedure
     (Tag : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);
   --  "tag": the Gtk.Text_Tag.Gtk_Text_Tag

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Table : out Gtk_Text_Tag_Table);
   procedure Initialize
      (Table : not null access Gtk_Text_Tag_Table_Record'Class);
   --  Creates a new Gtk.Text_Tag_Table.Gtk_Text_Tag_Table. The table contains
   --  no tags by default.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Text_Tag_Table_New return Gtk_Text_Tag_Table;
   --  Creates a new Gtk.Text_Tag_Table.Gtk_Text_Tag_Table. The table contains
   --  no tags by default.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_tag_table_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Table : not null access Gtk_Text_Tag_Table_Record;
       Tag   : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);
   --  Add a tag to the table. The tag is assigned the highest priority in the
   --  table.
   --  Tag must not be in a tag table already, and may not have the same name
   --  as an already-added tag.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag

   procedure Foreach
      (Table : not null access Gtk_Text_Tag_Table_Record;
       Func  : Gtk_Text_Tag_Table_Foreach);
   --  Calls Func on each tag in Table, with user data Data. Note that the
   --  table may not be modified while iterating over it (you can't add/remove
   --  tags).
   --  "func": a function to call on each tag

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Text_Tag_Table_Foreach is access procedure
        (Tag  : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
         Data : User_Data_Type);
      --  "tag": the Gtk.Text_Tag.Gtk_Text_Tag
      --  "data": data passed to Gtk.Text_Tag_Table.Foreach

      procedure Foreach
         (Table : not null access Gtk.Text_Tag_Table.Gtk_Text_Tag_Table_Record'Class;
          Func  : Gtk_Text_Tag_Table_Foreach;
          Data  : User_Data_Type);
      --  Calls Func on each tag in Table, with user data Data. Note that the
      --  table may not be modified while iterating over it (you can't
      --  add/remove tags).
      --  "func": a function to call on each tag
      --  "data": user data

   end Foreach_User_Data;

   function Get_Size
      (Table : not null access Gtk_Text_Tag_Table_Record) return Glib.Gint;
   --  Returns the size of the table (number of tags)

   function Lookup
      (Table : not null access Gtk_Text_Tag_Table_Record;
       Name  : UTF8_String) return Gtk.Text_Tag.Gtk_Text_Tag;
   --  Look up a named tag.
   --  "name": name of a tag

   procedure Remove
      (Table : not null access Gtk_Text_Tag_Table_Record;
       Tag   : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);
   --  Remove a tag from the table. If a Gtk.Text_Buffer.Gtk_Text_Buffer has
   --  Table as its tag table, the tag is removed from the buffer. The table's
   --  reference to the tag is removed, so the tag will end up destroyed if you
   --  don't have a reference to it.
   --  "tag": a Gtk.Text_Tag.Gtk_Text_Tag

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Text_Tag_Table_Gtk_Text_Tag_Void is not null access procedure
     (Self : access Gtk_Text_Tag_Table_Record'Class;
      Tag  : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);

   type Cb_GObject_Gtk_Text_Tag_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Tag  : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class);

   Signal_Tag_Added : constant Glib.Signal_Name := "tag-added";
   procedure On_Tag_Added
      (Self  : not null access Gtk_Text_Tag_Table_Record;
       Call  : Cb_Gtk_Text_Tag_Table_Gtk_Text_Tag_Void;
       After : Boolean := False);
   procedure On_Tag_Added
      (Self  : not null access Gtk_Text_Tag_Table_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Text_Tag_Table_Gtk_Text_Tag_Boolean_Void is not null access procedure
     (Self         : access Gtk_Text_Tag_Table_Record'Class;
      Tag          : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Size_Changed : Boolean);

   type Cb_GObject_Gtk_Text_Tag_Boolean_Void is not null access procedure
     (Self         : access Glib.Object.GObject_Record'Class;
      Tag          : not null access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Size_Changed : Boolean);

   Signal_Tag_Changed : constant Glib.Signal_Name := "tag-changed";
   procedure On_Tag_Changed
      (Self  : not null access Gtk_Text_Tag_Table_Record;
       Call  : Cb_Gtk_Text_Tag_Table_Gtk_Text_Tag_Boolean_Void;
       After : Boolean := False);
   procedure On_Tag_Changed
      (Self  : not null access Gtk_Text_Tag_Table_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:
   --    --  "tag": the changed tag.
   --    --  "size_changed": whether the change affects the
   --    --  Gtk.Text_View.Gtk_Text_View layout.

   Signal_Tag_Removed : constant Glib.Signal_Name := "tag-removed";
   procedure On_Tag_Removed
      (Self  : not null access Gtk_Text_Tag_Table_Record;
       Call  : Cb_Gtk_Text_Tag_Table_Gtk_Text_Tag_Void;
       After : Boolean := False);
   procedure On_Tag_Removed
      (Self  : not null access Gtk_Text_Tag_Table_Record;
       Call  : Cb_GObject_Gtk_Text_Tag_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Text_Tag_Table_Record, Gtk_Text_Tag_Table);
   function "+"
     (Widget : access Gtk_Text_Tag_Table_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Text_Tag_Table
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Text_Tag_Table;
