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

--  <c_version>1.3.4</c_version>

with Gtk; use Gtk;
with Gtk.Widget;

package Gtk.Text_Child is

   type Gtk_Text_Child_Anchor_Record is new GObject_Record with private;
   type Gtk_Text_Child_Anchor is access all Gtk_Text_Child_Anchor_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Text_Child_Anchor);
   --  Create a Gtk_Text_Child_Anchor widget.

   procedure Initialize (Widget : access Gtk_Text_Child_Anchor_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Text_Child_Anchor

   function Get_Widgets
     (Anchor : access Gtk_Text_Child_Anchor_Record)
      return Gtk.Widget.Widget_List.Glist;

   function Get_Deleted
     (Anchor : access Gtk_Text_Child_Anchor_Record) return Boolean;

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
