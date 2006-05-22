-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  A separator is a vertical or horizontal line that can be displayed between
--  widgets, to provide visual grouping of the widgets into meaningful groups.
--  It is for instance used in dialogs to isolate the actual contents of the
--  dialogs and the various buttons to acknowledge the dialog (OK, Cancel,...)
--  </description>
--  <c_version>2.8.17</c_version>

with Gtk.Widget;

package Gtk.Separator is

   type Gtk_Separator_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   subtype Gtk_Hseparator_Record is Gtk_Separator_Record;
   subtype Gtk_Vseparator_Record is Gtk_Separator_Record;

   type Gtk_Separator is access all Gtk_Separator_Record'Class;
   subtype Gtk_Hseparator is Gtk_Separator;
   subtype Gtk_Vseparator is Gtk_Separator;

   procedure Gtk_New_Hseparator (Separator : out Gtk_Separator);
   procedure Initialize_Hseparator
     (Separator : access Gtk_Separator_Record'Class);
   --  Creates or initializes a horizontal separator

   procedure Gtk_New_Vseparator (Separator : out Gtk_Separator);
   procedure Initialize_Vseparator
     (Separator : access Gtk_Separator_Record'Class);
   --  Creates or initializes a vertical separator

   function Get_Type return Gtk.Gtk_Type;
   function Hseparator_Get_Type return Gtk.Gtk_Type;
   function Vseparator_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Separator.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Separator_Record is new
     Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gtk_separator_get_type");
   pragma Import (C, Hseparator_Get_Type, "gtk_hseparator_get_type");
   pragma Import (C, Vseparator_Get_Type, "gtk_vseparator_get_type");
end Gtk.Separator;
