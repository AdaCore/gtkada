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
with Gtk.Container;
with Gnome.Druid_Page;

package Gnome.Druid is

   type Gnome_Druid_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gnome_Druid is access all Gnome_Druid_Record'Class;

   procedure Gnome_New (Widget : out Gnome_Druid);

   procedure Initialize (Widget : access Gnome_Druid_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Append_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Insert_Page
     (Druid     : access Gnome_Druid_Record;
      Back_Page : access Gnome.Druid_Page.Gnome_Druid_Page_Record;
      Page      : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Prepend_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Set_Buttons_Sensitive
     (Druid            : access Gnome_Druid_Record;
      Back_Sensitive   : Boolean;
      Next_Sensitive   : Boolean;
      Cancel_Sensitive : Boolean);

   procedure Set_Page
     (Druid : access Gnome_Druid_Record;
      Page  : access Gnome.Druid_Page.Gnome_Druid_Page_Record);

   procedure Set_Show_Finish
     (Druid       : access Gnome_Druid_Record;
      Show_Finish : Boolean);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "cancel"
   --    procedure Handler (Widget : access Gnome_Druid_Record'Class);
   --
   --  </signals>

private
   type Gnome_Druid_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "gnome_druid_get_type");
end Gnome.Druid;
