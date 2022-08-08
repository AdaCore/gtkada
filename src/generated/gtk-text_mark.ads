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
--  A Gtk.Text_Mark.Gtk_Text_Mark is like a bookmark in a text buffer; it
--  preserves a position in the text. You can convert the mark to an iterator
--  using Gtk.Text_Buffer.Get_Iter_At_Mark. Unlike iterators, marks remain
--  valid across buffer mutations, because their behavior is defined when text
--  is inserted or deleted. When text containing a mark is deleted, the mark
--  remains in the position originally occupied by the deleted text. When text
--  is inserted at a mark, a mark with "left gravity" will be moved to the
--  beginning of the newly-inserted text, and a mark with "right gravity" will
--  be moved to the end.
--
--  Note that "left" and "right" here refer to logical direction (left is the
--  toward the start of the buffer); in some languages such as Hebrew the
--  logically-leftmost text is not actually on the left when displayed.
--
--  Marks are reference counted, but the reference count only controls the
--  validity of the memory; marks can be deleted from the buffer at any time
--  with Gtk.Text_Buffer.Delete_Mark. Once deleted from the buffer, a mark is
--  essentially useless.
--
--  Marks optionally have names; these can be convenient to avoid passing the
--  Gtk.Text_Mark.Gtk_Text_Mark object around.
--
--  Marks are typically created using the Gtk.Text_Buffer.Create_Mark
--  function.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;

package Gtk.Text_Mark is

   type Gtk_Text_Mark_Record is new GObject_Record with null record;
   type Gtk_Text_Mark is access all Gtk_Text_Mark_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Mark         : out Gtk_Text_Mark;
       Name         : UTF8_String := "";
       Left_Gravity : Boolean);
   procedure Initialize
      (Mark         : not null access Gtk_Text_Mark_Record'Class;
       Name         : UTF8_String := "";
       Left_Gravity : Boolean);
   --  Creates a text mark. Add it to a buffer using Gtk.Text_Buffer.Add_Mark.
   --  If Name is null, the mark is anonymous; otherwise, the mark can be
   --  retrieved by name using Gtk.Text_Buffer.Get_Mark. If a mark has left
   --  gravity, and text is inserted at the mark's current location, the mark
   --  will be moved to the left of the newly-inserted text. If the mark has
   --  right gravity (Left_Gravity = False), the mark will end up on the right
   --  of newly-inserted text. The standard left-to-right cursor is a mark with
   --  right gravity (when you type, the cursor stays on the right side of the
   --  text you're typing).
   --  Since: gtk+ 2.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "name": mark name or null
   --  "left_gravity": whether the mark should have left gravity

   function Gtk_Text_Mark_New
      (Name         : UTF8_String := "";
       Left_Gravity : Boolean) return Gtk_Text_Mark;
   --  Creates a text mark. Add it to a buffer using Gtk.Text_Buffer.Add_Mark.
   --  If Name is null, the mark is anonymous; otherwise, the mark can be
   --  retrieved by name using Gtk.Text_Buffer.Get_Mark. If a mark has left
   --  gravity, and text is inserted at the mark's current location, the mark
   --  will be moved to the left of the newly-inserted text. If the mark has
   --  right gravity (Left_Gravity = False), the mark will end up on the right
   --  of newly-inserted text. The standard left-to-right cursor is a mark with
   --  right gravity (when you type, the cursor stays on the right side of the
   --  text you're typing).
   --  Since: gtk+ 2.12
   --  "name": mark name or null
   --  "left_gravity": whether the mark should have left gravity

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_mark_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Deleted
      (Mark : not null access Gtk_Text_Mark_Record) return Boolean;
   --  Returns True if the mark has been removed from its buffer with
   --  Gtk.Text_Buffer.Delete_Mark. See Gtk.Text_Buffer.Add_Mark for a way to
   --  add it to a buffer again.

   function Get_Left_Gravity
      (Mark : not null access Gtk_Text_Mark_Record) return Boolean;
   --  Determines whether the mark has left gravity.

   function Get_Name
      (Mark : not null access Gtk_Text_Mark_Record) return UTF8_String;
   --  Returns the mark name; returns NULL for anonymous marks.

   function Get_Visible
      (Mark : not null access Gtk_Text_Mark_Record) return Boolean;
   --  Returns True if the mark is visible (i.e. a cursor is displayed for
   --  it).

   procedure Set_Visible
      (Mark    : not null access Gtk_Text_Mark_Record;
       Setting : Boolean);
   --  Sets the visibility of Mark; the insertion point is normally visible,
   --  i.e. you can see it as a vertical bar. Also, the text widget uses a
   --  visible mark to indicate where a drop will occur when
   --  dragging-and-dropping text. Most other marks are not visible. Marks are
   --  not visible by default.
   --  "setting": visibility of mark

   ----------------------
   -- GtkAda additions --
   ----------------------

   -------------------------------
   -- Converting to/from GValue --
   -------------------------------

   procedure Set_Text_Mark
     (Val  : in out Glib.Values.GValue;
      Mark : access Gtk_Text_Mark_Record);
   function Get_Text_Mark (Val : Glib.Values.GValue) return Gtk_Text_Mark;
   --  Set the value of the given GValue to Mark.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Left_Gravity_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the mark has left gravity. When text is inserted at the mark's
   --  current location, if the mark has left gravity it will be moved to the
   --  left of the newly-inserted text, otherwise to the right.

   Name_Property : constant Glib.Properties.Property_String;
   --  The name of the mark or null if the mark is anonymous.

private
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Left_Gravity_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("left-gravity");
end Gtk.Text_Mark;
