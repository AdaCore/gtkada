-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000 Helix Code, Inc.                 --
--               Copyright (C) 2000-2001 ACT-Europe                  --
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

with Glib; use Glib;
with Gtk;
with Gtk.Box;

package Gnome.Paper_Selector is

   type Gnome_Paper_Selector_Record is new Gtk.Box.Gtk_Vbox_Record
     with private;
   type Gnome_Paper_Selector is access all Gnome_Paper_Selector_Record'Class;

   procedure Gnome_New (Paper_Selector : out Gnome_Paper_Selector);
   --  Create a new paper selector.

   procedure Initialize
     (Paper_Selector : access Gnome_Paper_Selector_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gnome_Paper_Selector.

   function Get_Width
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat;

   function Get_Height
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat;

   function Get_Left_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat;

   function Get_Right_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat;

   function Get_Top_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat;

   function Get_Bottom_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat;

   procedure Set_Name
     (Paper_Selector : access Gnome_Paper_Selector_Record;
      Name           : String);

   procedure Set_Width
     (Paper_Selector : access Gnome_Paper_Selector_Record;
      Width          : Gfloat);

   procedure Set_Height
     (Paper_Selector : access Gnome_Paper_Selector_Record;
      Height         : Gfloat);

private
   type Gnome_Paper_Selector_Record is new Gtk.Box.Gtk_Vbox_Record
     with null record;
   pragma Import (C, Get_Type, "gnome_paper_selector_get_type");
end Gnome.Paper_Selector;
