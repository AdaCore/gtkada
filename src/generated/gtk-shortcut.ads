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

--  Describes a keyboard shortcut.
--
--  It contains a description of how to trigger the shortcut via a
--  [classGtk.ShortcutTrigger] and a way to activate the shortcut on a widget
--  via a [classGtk.ShortcutAction].
--
--  The actual work is usually done via [classGtk.ShortcutController], which
--  decides if and when to activate a shortcut. Using that controller directly
--  however is rarely necessary as various higher level convenience APIs exist
--  on `GtkWidget`s that make it easier to use shortcuts in GTK.
--
--  `GtkShortcut` does provide functionality to make it easy for users to work
--  with shortcuts, either by providing informational strings for display
--  purposes or by allowing shortcuts to be configured.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Glib.Variant;         use Glib.Variant;
with Gtk.Shortcut_Action;  use Gtk.Shortcut_Action;
with Gtk.Shortcut_Trigger; use Gtk.Shortcut_Trigger;

package Gtk.Shortcut is

   type Gtk_Shortcut_Record is new GObject_Record with null record;
   type Gtk_Shortcut is access all Gtk_Shortcut_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gtk_Shortcut;
       Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Action  : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class);
   procedure Initialize
      (Self    : not null access Gtk_Shortcut_Record'Class;
       Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Action  : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class);
   --  Creates a new `GtkShortcut` that is triggered by Trigger and then
   --  activates Action.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Trigger The trigger that will trigger the shortcut
   --  @param Action The action that will be activated upon triggering

   function Gtk_Shortcut_New
      (Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Action  : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class)
       return Gtk_Shortcut;
   --  Creates a new `GtkShortcut` that is triggered by Trigger and then
   --  activates Action.
   --  @param Trigger The trigger that will trigger the shortcut
   --  @param Action The action that will be activated upon triggering

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_shortcut_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Action
      (Self : not null access Gtk_Shortcut_Record)
       return Gtk.Shortcut_Action.Gtk_Shortcut_Action;
   --  Gets the action that is activated by this shortcut.
   --  @return the action
   --  Return has transfer-ownership='none'

   procedure Set_Action
      (Self   : not null access Gtk_Shortcut_Record;
       Action : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class);
   --  Sets the new action for Self to be Action.
   --  Parameter Action has transfer-ownership='full'
   --  @param Action The new action. If the Action is null, the nothing action
   --  will be used.

   function Get_Arguments
      (Self : not null access Gtk_Shortcut_Record)
       return Glib.Variant.Gvariant;
   --  Gets the arguments that are passed when activating the shortcut.
   --  @return the arguments
   --  Return has transfer-ownership='none'

   procedure Set_Arguments
      (Self : not null access Gtk_Shortcut_Record;
       Args : Glib.Variant.Gvariant);
   --  Sets the arguments to pass when activating the shortcut.
   --  @param Args arguments to pass when activating Self

   function Get_Trigger
      (Self : not null access Gtk_Shortcut_Record)
       return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger;
   --  Gets the trigger used to trigger Self.
   --  @return the trigger used
   --  Return has transfer-ownership='none'

   procedure Set_Trigger
      (Self    : not null access Gtk_Shortcut_Record;
       Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class);
   --  Sets the new trigger for Self to be Trigger.
   --  Parameter Trigger has transfer-ownership='full'
   --  @param Trigger The new trigger. If the Trigger is null, the never
   --  trigger will be used.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Action_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Shortcut_Action.Gtk_Shortcut_Action
   --  The action that gets activated by this shortcut.

   Trigger_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger
   --  The trigger that triggers this shortcut.

private
   Trigger_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("trigger");
   Action_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("action");
end Gtk.Shortcut;
