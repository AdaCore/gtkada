-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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
--  A Gtk_Vbutton_Box is a specific Gtk_Button_Box that organizes its children
--  vertically. The beginning of the box (when you add children with
--  Gtk.Box.Pack_Start) is on the top of the box. Its end (for
--  Gtk.Box.Pack_End) is on the bottom.
-- 
--  </description>
--  <group>Layout containers</group>

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Gtk.Button_Box; use Gtk.Button_Box;
with Gtk.Enums;      use Gtk.Enums;

package Gtk.Vbutton_Box is

   type Gtk_Vbutton_Box_Record is new Gtk_Button_Box_Record with null record;
   type Gtk_Vbutton_Box is access all Gtk_Vbutton_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Vbutton_Box);
   procedure Initialize (Self : access Gtk_Vbutton_Box_Record'Class);
   --  Creates a new vertical button box.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_vbutton_box_get_type");

   ---------------
   -- Functions --
   ---------------

   function Get_Layout_Default return Gtk.Enums.Gtk_Button_Box_Style;
   pragma Obsolescent;
   procedure Set_Layout_Default (Layout : Gtk.Enums.Gtk_Button_Box_Style);
   pragma Obsolescent;
   --  Sets a new layout mode that will be used by all button boxes.
   --  Deprecated since 2.0, Use gtk_button_box_set_layout() instead.
   --  "layout": a new Gtk.Enums.Gtk_Button_Box_Style.

   function Get_Spacing_Default return Gint;
   pragma Obsolescent;
   procedure Set_Spacing_Default (Spacing : Gint);
   pragma Obsolescent;
   --  Changes the default spacing that is placed between widgets in an
   --  vertical button box.
   --  Deprecated since 2.0, Use gtk_box_set_spacing() instead.
   --  "spacing": an integer value.

end Gtk.Vbutton_Box;
