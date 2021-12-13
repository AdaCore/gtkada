------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
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
--  A GtkTextChildAnchor is a spot in the buffer where child widgets can be
--  "anchored" (inserted inline, as if they were characters). The anchor can
--  have multiple widgets anchored, to allow for multiple views.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Multiline Text Editor</group>

with Gtk.Widget;

package Gtk.Text_Child is

   type Gtk_Text_Child_Anchor_Record is new GObject_Record with private;
   type Gtk_Text_Child_Anchor is access all Gtk_Text_Child_Anchor_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Text_Child_Anchor);
   procedure Initialize (Widget : access Gtk_Text_Child_Anchor_Record'Class);
   --  Creates or initializes a Gtk_Text_Child_Anchor widget.
   --  Usually you would then insert it into a Gtk_Text_Buffer with
   --  Gtk.Text_Buffer.Insert_Child_Anchor.
   --  To perform the creation and insertion in one step, use the
   --  convenience function Gtk.Text_Buffer.Create_Child_Anchor.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Text_Child_Anchor.

   function Get_Widgets
     (Anchor : access Gtk_Text_Child_Anchor_Record)
      return Gtk.Widget.Widget_List.Glist;
   --  Return the list of widgets attached at anchor. The returned list should
   --  be freed by the caller.

   function Get_Deleted
     (Anchor : access Gtk_Text_Child_Anchor_Record) return Boolean;
   --  Determines whether a child anchor has been deleted from the buffer. Keep
   --  in mind that the child anchor will be unreferenced when removed from the
   --  buffer, so you need to hold your own reference (with Ref()) if you plan
   --  to use this function; otherwise all deleted child anchors will
   --  also be finalized.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Text_Child_Anchor_Record is new GObject_Record with null record;
   pragma Import (C, Get_Type, "gtk_text_child_anchor_get_type");
end Gtk.Text_Child;
