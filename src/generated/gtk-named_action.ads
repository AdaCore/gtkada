------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Activates a named action.
--
--  See [methodGtk.WidgetClass.install_action] and
--  [methodGtk.Widget.insert_action_group] for ways to associate named actions
--  with widgets.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.Properties;     use Glib.Properties;
with Gtk.Shortcut_Action; use Gtk.Shortcut_Action;

package Gtk.Named_Action is

   type Gtk_Named_Action_Record is new Gtk_Shortcut_Action_Record with null record;
   type Gtk_Named_Action is access all Gtk_Named_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Named_Action; Name : UTF8_String);
   procedure Initialize
      (Self : not null access Gtk_Named_Action_Record'Class;
       Name : UTF8_String);
   --  Creates an action that when activated, activates the named action on
   --  the widget.
   --  It also passes the given arguments to it.
   --  See [methodGtk.Widget.insert_action_group] for how to add actions to
   --  widgets.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Name the detailed name of the action

   function Gtk_Named_Action_New
      (Name : UTF8_String) return Gtk_Named_Action;
   --  Creates an action that when activated, activates the named action on
   --  the widget.
   --  It also passes the given arguments to it.
   --  See [methodGtk.Widget.insert_action_group] for how to add actions to
   --  widgets.
   --  @param Name the detailed name of the action

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_named_action_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Action_Name
      (Self : not null access Gtk_Named_Action_Record) return UTF8_String;
   --  Returns the name of the action that will be activated.
   --  @return the name of the action to activate

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Action_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the action to activate.

private
   Action_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("action-name");
end Gtk.Named_Action;
