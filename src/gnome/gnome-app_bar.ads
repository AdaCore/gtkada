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

package Gnome.App_Bar is

   type Gnome_App_Bar_Record is new Gtk.Box.Gtk_Hbox_Record with private;
   type Gnome_App_Bar is access all Gnome_App_Bar_Record'Class;

   procedure Gnome_New
     (Widget        : out Gnome_App_Bar;
      Has_Progress  : Boolean;
      Has_Status    : Boolean;
      Interactivity : Gnome_Preferences_Type);

   procedure Initialize
     (Widget        : access Gnome_App_Bar_Record'Class;
      Has_Progress  : Boolean;
      Has_Status    : Boolean;
      Interactivity : Gnome_Preferences_Type);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Appbar_Clear_Prompt (Appbar : access Gnome_App_Bar_Record);

   procedure Appbar_Clear_Stack (Appbar : access Gnome_App_Bar_Record);

   function Appbar_Get_Response (Appbar : access Gnome_App_Bar_Record)
                                 return String;

   procedure Appbar_Pop (Appbar : access Gnome_App_Bar_Record);

   procedure Appbar_Push
     (Appbar : access Gnome_App_Bar_Record;
      Status : String);

   procedure Appbar_Refresh (Appbar : access Gnome_App_Bar_Record);

   procedure Appbar_Set_Default
     (Appbar         : access Gnome_App_Bar_Record;
      Default_Status : String);

   procedure Appbar_Set_Progress_Percentage
     (Appbar     : access Gnome_App_Bar_Record;
      Percentage : Gfloat);

   procedure Appbar_Set_Prompt
     (Appbar : access Gnome_App_Bar_Record;
      Prompt : String;
      Modal  : Boolean);

   procedure Appbar_Set_Status
     (Appbar : access Gnome_App_Bar_Record;
      Status : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "user_response"
   --    procedure Handler (Widget : access Gnome_App_Bar_Record'Class);
   --
   --  - "clear_prompt"
   --    procedure Handler (Widget : access Gnome_App_Bar_Record'Class);
   --
   --  </signals>

private
   type Gnome_App_Bar_Record is new Gtk.Box.Gtk_Hbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_appbar_get_Type");
end Gnome.App_Bar;
