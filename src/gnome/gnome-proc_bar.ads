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

with Glib; use Glib;
with Gdk.Color;
with Gtk;
with Gtk.Box;
with Gtk.Widget;
with System;

package Gnome.Proc_Bar is

   type Gnome_Proc_Bar_Record is new Gtk.Box.Gtk_Hbox_Record with private;
   type Gnome_Proc_Bar is access all Gnome_Proc_Bar_Record'Class;

   type Function_Gint is access function return Gint;
   pragma Convention (C, Function_Gint);

   procedure Gnome_New
     (Widget : out Gnome_Proc_Bar;
      Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Colors : access Gdk.Color.Gdk_Color_Array;
      Cb     : Function_Gint);

   procedure Initialize
     (Widget : access Gnome_Proc_Bar_Record'Class;
      Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Colors : access Gdk.Color.Gdk_Color_Array;
      Cb     : Function_Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Orient
     (Pb       : access Gnome_Proc_Bar_Record;
      Vertical : Boolean);

   procedure Set_Values
     (Pb  : access Gnome_Proc_Bar_Record;
      Val : out Guint);

   procedure Start
     (Pb    : access Gnome_Proc_Bar_Record;
      Gtime : Gint;
      Data  : System.Address);

   procedure Stop (Pb : access Gnome_Proc_Bar_Record);

   procedure Update
     (Pb     : access Gnome_Proc_Bar_Record;
      Colors : access Gdk.Color.Gdk_Color_Array);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Proc_Bar_Record is new Gtk.Box.Gtk_Hbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_proc_bar_get_type");
end Gnome.Proc_Bar;
