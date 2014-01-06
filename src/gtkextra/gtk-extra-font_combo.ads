------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

--  <description>
--  A Gtk_Font_Combo is a small toolbar used to select fonts.
--  This widget takes less real-estate on the screen than a
--  Gtk_Font_Selection widget, and thus can be kept permanently on the
--  screen.
--  This widget only works with postscript fonts (see Gtk.Extra.PsFont).
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>GtkExtra, additional widgets</group>

with Glib;
with Gtk.Toolbar;
with Gdk.Font;
with Pango.Font;

package Gtk.Extra.Font_Combo is

   type Gtk_Font_Combo_Record is new Gtk.Toolbar.Gtk_Toolbar_Record
     with private;
   type Gtk_Font_Combo is access all Gtk_Font_Combo_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Font_Combo);
   --  Create a new combo box

   procedure Initialize (Widget : access Gtk_Font_Combo_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Font_Combo.

   procedure Font_Combo_Select
     (Font_Combo : access Gtk_Font_Combo_Record;
      Family     : String;
      Bold       : Boolean;
      Italic     : Boolean;
      Height     : Gint);
   --  Selects a new font
   --  Family is the name of the postscript font.

   procedure Font_Combo_Select_Nth
     (Font_Combo : access Gtk_Font_Combo_Record;
      N          : Gint;
      Bold       : Boolean;
      Italic     : Boolean;
      Height     : Gint);
   --  Selects the nth font in the combo box.

   function Get_Font_Height
     (Font_Combo : access Gtk_Font_Combo_Record) return Glib.Gint;
   --  Return the height of the selected font

   function Get_GdkFont
     (Font_Combo : access Gtk_Font_Combo_Record) return Gdk.Font.Gdk_Font;
   --  Return the selected font.

   function Get_Font_Description
     (Font_Combo : access Gtk_Font_Combo_Record)
     return Pango.Font.Pango_Font_Description;
   --  Return the selected font.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --  procedure Handler (Combo : access Gtk_Font_Combo_Record'Class);
   --
   --  Emitted when a new font was selected by the user.
   --  </signals>

private
   type Gtk_Font_Combo_Record is new Gtk.Toolbar.Gtk_Toolbar_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_font_combo_get_type");

   --  Unbound:
   --     gtk_font_combo_get_psfont
end Gtk.Extra.Font_Combo;
