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
with Gtk.Button;

package Gnome.HRef is

   type Gnome_HRef_Record is new Gtk.Button.Gtk_Button_Record with private;
   type Gnome_HRef is access all Gnome_HRef_Record'Class;

   procedure Gnome_New
     (Widget : out Gnome_HRef;
      Url    : String;
      Label  : String);

   procedure Initialize
     (Widget : access Gnome_HRef_Record'Class;
      Url    : String;
      Label  : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Get_Text (Href : access Gnome_HRef_Record) return String;

   function Get_Url (Href : access Gnome_HRef_Record) return String;

   procedure Set_Text
     (Href : access Gnome_HRef_Record;
      Text : String);

   procedure Set_Url
     (Href : access Gnome_HRef_Record;
      Url  : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_HRef_Record is new Gtk.Button.Gtk_Button_Record with null record;

   pragma Import (C, Get_Type, "gnome_href_get_type");
end Gnome.HRef;
