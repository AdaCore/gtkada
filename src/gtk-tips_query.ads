------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

--  <c_version>2.8.17</c_version>
--  <group>Obsolescent widgets</group>
--  <doc_ignore>

with Gtk.Label;
with Gtk.Widget;

package Gtk.Tips_Query is
   pragma Obsolescent;

   type Gtk_Tips_Query_Record is new Gtk.Label.Gtk_Label_Record with private;
   type Gtk_Tips_Query is access all Gtk_Tips_Query_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Tips_Query);

   procedure Initialize (Widget : access Gtk_Tips_Query_Record'Class);

   procedure Set_Caller
     (Tips_Query : access Gtk_Tips_Query_Record;
      Caller     : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Labels
     (Tips_Query     : access Gtk_Tips_Query_Record;
      Label_Inactive : UTF8_String;
      Label_No_Tip   : UTF8_String);

   procedure Start_Query (Tips_Query : access Gtk_Tips_Query_Record);

   procedure Stop_Query (Tips_Query : access Gtk_Tips_Query_Record);

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
   --
   --  - "widget_selected"
   --  - "start_query"
   --  - "stop_query"
   --  - "widget_entered"
   --
   --  </signals>

   Signal_Widget_Selected : constant Glib.Signal_Name := "widget_selected";
   Signal_Start_Query     : constant Glib.Signal_Name := "start_query";
   Signal_Stop_Query      : constant Glib.Signal_Name := "stop_query";
   Signal_Widget_Entered  : constant Glib.Signal_Name := "widget_entered";

private
   type Gtk_Tips_Query_Record is new Gtk.Label.Gtk_Label_Record
     with null record;
end Gtk.Tips_Query;

--  This subprogram was never implemented, and the whole package is now
--  obsolescent
--  No binding: gtk_tips_query_get_type

--  </doc_ignore>
