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
pragma Warnings (Off); --  Gtk.Combo is obsolescent
with Gtk.Combo;
pragma Warnings (On);
with Gtk.Widget;

package Gnome.GEntry is

   pragma Warnings (Off);  --  Gtk.Combo is obsolescent;
   type Gnome_GEntry_Record is new Gtk.Combo.Gtk_Combo_Record with private;
   pragma Warnings (On);
   type Gnome_GEntry is access all Gnome_GEntry_Record'Class;

   procedure Gnome_New (Widget     : out Gnome_GEntry;
                        History_Id : String);

   procedure Initialize (Widget     : access Gnome_GEntry_Record'Class;
                         History_Id : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Entry_Append_History
     (Gentry : access Gnome_GEntry_Record;
      Save   : Gint;
      Text   : String);

   function Entry_Gtk_Entry (Gentry : access Gnome_GEntry_Record)
                             return Gtk.Widget.Gtk_Widget;

   procedure Entry_Prepend_History
     (Gentry : access Gnome_GEntry_Record;
      Save   : Gint;
      Text   : String);

   procedure Entry_Set_History_Id
     (Gentry     : access Gnome_GEntry_Record;
      History_Id : String);

   procedure Entry_Set_Max_Saved
     (Gentry    : access Gnome_GEntry_Record;
      Max_Saved : Guint);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   pragma Warnings (Off);  --  Gtk.Combo is obsolescent;
   type Gnome_GEntry_Record is new Gtk.Combo.Gtk_Combo_Record with null record;
   pragma Warnings (On);

   pragma Import (C, Get_Type, "gnome_gentry_get_type");
end Gnome.GEntry;
