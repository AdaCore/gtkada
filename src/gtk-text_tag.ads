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
--  The Gtk_Text_Tag data type.
--  </description>
--  <c_version>1.3.4</c_version>

package Gtk.Text_Tag is

   type Gtk_Text_Tag_Record is new GObject_Record with private;
   type Gtk_Text_Tag is access all Gtk_Text_Tag_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Text_Tag; Name : String := "");
   --  Create a new Gtk_Text_Tag.

   procedure Initialize
     (Widget : access Gtk_Text_Tag_Record'Class;
      Name   : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   function Get_Priority (Tag : access Gtk_Text_Tag_Record) return Gint;
   --  Return the tag priority.

   procedure Set_Priority (Tag : access Gtk_Text_Tag_Record; Priority : Gint);
   --  Set the priority of a Gtk_Text_Tag.
   --  Valid priorities start at 0 and go to one less than Table_Size.
   --  Each tag in a table has a unique priority; setting the priority of one
   --  tag shifts the priorities of all the other tags in the table to maintain
   --  a unique priority for each tag. Higher priority tags "win" if two tags
   --  both set the same text attribute. When adding a tag to a tag table, it
   --  will be assigned the highest priority in the table by default; so
   --  normally the precedence of a set of tags is the order in which they were
   --  added to the table, or created with Gtk.Text_Buffer.Create_Tag, which
   --  adds the tag to the buffer's table automatically.

   --  function Event
   --    (Tag          : access Gtk_Text_Tag_Record;
   --     Event_Object : access Glib.Object.GObject_Record'Class;
   --     Event        : Gdk.Event.Gdk_Event;
   --     Iter         : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  ??? Can not be bound here. Circular dependency problem with
   --  Gtk_Text_Iter.
   --  Emit the "event" signal on Tag.
   --  Event_Object: object that received the event, such as a widget.
   --  Event: the event.
   --  Iter: location where the event was received.

   --  ??? Might need a binding for the GtkTextAttributes structure.
   --  ??? If needed, might be better off in a separate package.
   --  ??? Will need lots of accessors...

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "event"
   --    function Handler
   --      (Widget       : access Gtk_Text_Tag_Record'Class;
   --       Event_Object : out G_Object;
   --       Event        : Gdk.Event.Gdk_Event;
   --       Iter         : access Gtk.Text_Iter.Gtk_Text_Iter_Record'Class)
   --       return Gint;
   --
   --  </signals>

private

   type Gtk_Text_Tag_Record is new GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_text_tag_get_type");

end Gtk.Text_Tag;
