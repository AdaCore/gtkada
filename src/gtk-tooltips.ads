-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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
--
--  Tooltips are the small text windows that popup when the mouse rests over
--  a widget, and that provide a quick help for the user.
--
--  In GtkAda, all tooltips belong to a group (a Gtk_Tooltips). All the
--  individual tooltips in a group can be disabled or enabled at the same
--  time. Likewise, the colors and style of a tooltip can be set on a group
--  basis.
--
--  See the example at the end for how to change the default colors used
--  for tooltips.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Glib;
with Gtk.Object;
with Gtk.Widget;

package Gtk.Tooltips is

   type Gtk_Tooltips_Record is new Gtk.Object.Gtk_Object_Record with private;
   type Gtk_Tooltips is access all Gtk_Tooltips_Record'Class;

   --  <doc_ignore>
   type Tooltips_Data
     (Text_Length : Natural; Private_Length : Natural) is
   record
      Tooltips     : Gtk_Tooltips;    -- the group of the tooltip
      Widget       : Gtk.Widget.Gtk_Widget; -- the widget to which it applies
      Text         : UTF8_String (1 .. Text_Length); -- the text of the tooltip
      Text_Private : UTF8_String (1 .. Private_Length); -- the private text
   end record;
   --  </doc_ignore>

   procedure Gtk_New (Widget : out Gtk_Tooltips);
   --  Create a new group of tooltips.

   procedure Initialize (Widget : access Gtk_Tooltips_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Tooltips.

   procedure Enable (Tooltips : access Gtk_Tooltips_Record);
   --  Enable all the tooltips in the group.
   --  From now on, when the mouse rests over a widget for a short period of
   --  time, the help text is automatically displayed.

   procedure Disable (Tooltips : access Gtk_Tooltips_Record);
   --  Disable all the tooptips in the group.
   --  From now on, no tooltips in this group will appear, unless they are
   --  re-enabled.

   --  <doc_ignore>
   procedure Set_Delay
     (Tooltips : access Gtk_Tooltips_Record;
      Duration : Guint := 500);
   --  Set the delay between the user moving the mouse over a widget and the
   --  text appearing.
   --  Duration is in milli-seconds.
   --  pragma Deprecated (Set_Delay);
   --  </doc_ignore>

   procedure Set_Tip
     (Tooltips    : access Gtk_Tooltips_Record;
      Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tip_Text    : UTF8_String;
      Tip_Private : UTF8_String := "");
   --  Add a new tooltip to Widget.
   --  The message that appears in the tooltip is Tip_Text, and the tooltip
   --  belongs to the group Tooltips.
   --  Tip_Private contains more information, that can be displayed by a
   --  Gtk_Tips_Query widget through the "widget_selected" signal.
   --  In most cases, Tip_Private should simply keep its default empty value.

   function Get_Data
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Tooltips_Data;
   --  Return the tooltip data associated with the Widget.
   --  If there is none, the two text fields in the returned structure have
   --  a length 0.

   procedure Force_Window (Widget : access Gtk_Tooltips_Record);
   --  Make sure the window in which the tooltips will be displayed is
   --  created.
   --  This is useful if you want to modify some characteristics of that
   --  window.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Tooltips_Record is
     new Gtk.Object.Gtk_Object_Record with null record;

   pragma Import (C, Get_Type, "gtk_tooltips_get_type");
end Gtk.Tooltips;

--  <example>
--  <include>../examples/documentation/tooltips.adb</include>
--  </example>
