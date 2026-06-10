------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Marks a spot in a `GtkTextBuffer` where child widgets can be "anchored".
--
--  The anchor can have multiple widgets anchored, to allow for multiple
--  views.

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Gtk.Text_Iter;
with Gtk.Widget;

package Gtk.Text_Child_Anchor is

   type Gtk_Text_Child_Anchor_Record is new GObject_Record with null record;
   type Gtk_Text_Child_Anchor is access all Gtk_Text_Child_Anchor_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Anchor : out Gtk_Text_Child_Anchor);
   procedure Initialize
      (Anchor : not null access Gtk_Text_Child_Anchor_Record'Class);
   --  Creates a new `GtkTextChildAnchor`.
   --  Usually you would then insert it into a `GtkTextBuffer` with
   --  [methodGtk.TextBuffer.insert_child_anchor]. To perform the creation and
   --  insertion in one step, use the convenience function
   --  [methodGtk.TextBuffer.create_child_anchor].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Text_Child_Anchor_New return Gtk_Text_Child_Anchor;
   --  Creates a new `GtkTextChildAnchor`.
   --  Usually you would then insert it into a `GtkTextBuffer` with
   --  [methodGtk.TextBuffer.insert_child_anchor]. To perform the creation and
   --  insertion in one step, use the convenience function
   --  [methodGtk.TextBuffer.create_child_anchor].

   procedure Gtk_New_With_Replacement
      (Anchor    : out Gtk_Text_Child_Anchor;
       Character : UTF8_String);
   procedure Initialize_With_Replacement
      (Anchor    : not null access Gtk_Text_Child_Anchor_Record'Class;
       Character : UTF8_String);
   --  Creates a new `GtkTextChildAnchor` with the given replacement
   --  character.
   --  Usually you would then insert it into a `GtkTextBuffer` with
   --  [methodGtk.TextBuffer.insert_child_anchor].
   --  Since: gtk+ 4.6
   --  Initialize_With_Replacement does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Character a replacement character

   function Gtk_Text_Child_Anchor_New_With_Replacement
      (Character : UTF8_String) return Gtk_Text_Child_Anchor;
   --  Creates a new `GtkTextChildAnchor` with the given replacement
   --  character.
   --  Usually you would then insert it into a `GtkTextBuffer` with
   --  [methodGtk.TextBuffer.insert_child_anchor].
   --  Since: gtk+ 4.6
   --  @param Character a replacement character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_child_anchor_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Deleted
      (Anchor : not null access Gtk_Text_Child_Anchor_Record) return Boolean;
   --  Determines whether a child anchor has been deleted from the buffer.
   --  Keep in mind that the child anchor will be unreferenced when removed
   --  from the buffer, so you need to hold your own reference (with
   --  g_object_ref) if you plan to use this function — otherwise all deleted
   --  child anchors will also be finalized.
   --  @return True if the child anchor has been deleted from its buffer

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Gtk_Widget_Array is
      array (Natural range <>) of Gtk.Widget.Gtk_Widget;

      function Get_Widgets
        (Anchor : not null access Gtk_Text_Child_Anchor_Record)
      return Gtk_Widget_Array;
      --  Gets a list of all widgets anchored at this child anchor.
      --  The order in which the widgets are returned is not defined.

      function Get_Child_Anchor
        (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Gtk_Text_Child_Anchor;
      --  If the location at Iter contains a child anchor, the anchor is
      --  returned (with no new reference count added). Otherwise, null is
      --  returned.

end Gtk.Text_Child_Anchor;
