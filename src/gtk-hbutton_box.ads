-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  A Gtk_Hbutton_Box is a specific Gtk_Button_Box that organizes its
--  children horizontally.
--  The beginning of the box (when you add children with Gtk.Box.Pack_Start)
--  is on the left of the box. Its end (for Gtk.Box.Pack_End) is on the right.
--
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Button_Box;
with Gtk.Enums;

package Gtk.Hbutton_Box is

   type Gtk_Hbutton_Box_Record is new
     Gtk.Button_Box.Gtk_Button_Box_Record with private;
   type Gtk_Hbutton_Box is access all Gtk_Hbutton_Box_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Hbutton_Box);
   --  Create a new horizontal button box.

   procedure Initialize (Widget : access Gtk_Hbutton_Box_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_HButton_Box.

   procedure Set_Spacing_Default (Spacing : in Gint);
   --  Set the default spacing (space between two adjacent children).
   --  This is done for all the Hbutton_Boxes in your application. This can be
   --  overridden for a specific box by calling Gtk.Button_Box.Set_Spacing.
   --  pragma Deprecated (Set_Spacing_Default);

   function Get_Spacing_Default return Gint;
   --  Return the default spacing to use for all Hbutton_Boxes in your
   --  application that don't have a specific value.
   --  pragma Deprecated (Get_Spacing_Default);

   procedure Set_Layout_Default (Layout : Gtk.Enums.Gtk_Button_Box_Style);
   --  Set the the default layout to use for all the hbutton_boxes in your
   --  application that don't have a specific value set by
   --  Gtk.Button_Box.Set_Layout. The default value is Buttonbox_Edge.
   --  pragma Deprecated (Set_Layout_Default);

   function Get_Layout_Default return Gtk.Enums.Gtk_Button_Box_Style;
   --  Return the default layout to use for all the hbutton_boxes.
   --  pragma Deprecated (Get_Layout_Default);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Hbutton_Box_Record is new Gtk.Button_Box.Gtk_Button_Box_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_hbutton_box_get_type");
   pragma Import
     (C, Get_Spacing_Default, "gtk_hbutton_box_get_spacing_default");
   pragma Import
     (C, Set_Spacing_Default, "gtk_hbutton_box_set_spacing_default");
   pragma Import (C, Set_Layout_Default, "gtk_hbutton_box_set_layout_default");
end Gtk.Hbutton_Box;
