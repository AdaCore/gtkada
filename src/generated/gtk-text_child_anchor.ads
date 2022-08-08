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
--  A Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor is a spot in the buffer
--  where child widgets can be "anchored" (inserted inline, as if they were
--  characters). The anchor can have multiple widgets anchored, to allow for
--  multiple views.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Gtk.Widget;  use Gtk.Widget;

package Gtk.Text_Child_Anchor is

   type Gtk_Text_Child_Anchor_Record is new GObject_Record with null record;
   type Gtk_Text_Child_Anchor is access all Gtk_Text_Child_Anchor_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Anchor : out Gtk_Text_Child_Anchor);
   procedure Initialize
      (Anchor : not null access Gtk_Text_Child_Anchor_Record'Class);
   --  Creates a new Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor. Usually you
   --  would then insert it into a Gtk.Text_Buffer.Gtk_Text_Buffer with
   --  Gtk.Text_Buffer.Insert_Child_Anchor. To perform the creation and
   --  insertion in one step, use the convenience function
   --  Gtk.Text_Buffer.Create_Child_Anchor.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Text_Child_Anchor_New return Gtk_Text_Child_Anchor;
   --  Creates a new Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor. Usually you
   --  would then insert it into a Gtk.Text_Buffer.Gtk_Text_Buffer with
   --  Gtk.Text_Buffer.Insert_Child_Anchor. To perform the creation and
   --  insertion in one step, use the convenience function
   --  Gtk.Text_Buffer.Create_Child_Anchor.

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

   function Get_Widgets
      (Anchor : not null access Gtk_Text_Child_Anchor_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Gets a list of all widgets anchored at this child anchor. The returned
   --  list should be freed with g_list_free.

end Gtk.Text_Child_Anchor;
