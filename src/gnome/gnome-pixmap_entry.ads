------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Glib; use Glib;
with Gtk;
with Gtk.Box;
with Gtk.Widget;

package Gnome.Pixmap_Entry is

   type Gnome_Pixmap_Entry_Record is new Gtk.Box.Gtk_Vbox_Record with private;
   type Gnome_Pixmap_Entry is access all Gnome_Pixmap_Entry_Record'Class;

   procedure Gnome_New
     (Widget              : out Gnome_Pixmap_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String;
      Do_Preview          : Boolean);

   procedure Initialize
     (Widget              : access Gnome_Pixmap_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String;
      Do_Preview          : Boolean);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function File_Entry (Pentry : access Gnome_Pixmap_Entry_Record)
                        return Gtk.Widget.Gtk_Widget;

   function Gtk_Entry
     (Pentry : access Gnome_Pixmap_Entry_Record) return Gtk.Widget.Gtk_Widget;

   function Gnome_Entry
     (Pentry : access Gnome_Pixmap_Entry_Record) return Gtk.Widget.Gtk_Widget;

   function Get_Filename (Pentry : access Gnome_Pixmap_Entry_Record)
                          return String;

   procedure Set_Pixmap_Subdir
     (Pentry : access Gnome_Pixmap_Entry_Record;
      Subdir : String);

   procedure Set_Preview
     (Pentry     : access Gnome_Pixmap_Entry_Record;
      Do_Preview : Boolean);

   procedure Set_Preview_Size
     (Pentry    : access Gnome_Pixmap_Entry_Record;
      Preview_W : Gint;
      Preview_H : Gint);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Pixmap_Entry_Record is new
     Gtk.Box.Gtk_Vbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_pixmap_entry_get_type");
end Gnome.Pixmap_Entry;
