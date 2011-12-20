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

with Gtk;
with Gtk.Bin;

package Gnome.Druid_Page is

   type Gnome_Druid_Page_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gnome_Druid_Page is access all Gnome_Druid_Page_Record'Class;

   function Back (Druid_Page : access Gnome_Druid_Page_Record)
                  return Boolean;

   function Cancel (Druid_Page : access Gnome_Druid_Page_Record)
                    return Boolean;

   procedure Finish (Druid_Page : access Gnome_Druid_Page_Record);

   function Next (Druid_Page : access Gnome_Druid_Page_Record)
                  return Boolean;

   procedure Prepare (Druid_Page : access Gnome_Druid_Page_Record);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "next"
   --    function Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gboolean;
   --
   --  - "prepare"
   --    procedure Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class);
   --
   --  - "back"
   --    function Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gboolean;
   --
   --  - "finish"
   --    procedure Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class);
   --
   --  - "cancel"
   --    function Handler (Widget : access Gnome_Druid_Page_Record'Class;
   --       Druid : access Gtk.Widget.Gtk_Widget_Record'Class)
   --       return Gboolean;
   --
   --  </signals>

private
   type Gnome_Druid_Page_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

end Gnome.Druid_Page;
