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

--  Combines two shortcut triggers.
--
--  The `GtkAlternativeTrigger` triggers when either of the two trigger.
--
--  This can be cascaded to combine more than two triggers.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.Properties;      use Glib.Properties;
with Gtk.Shortcut_Trigger; use Gtk.Shortcut_Trigger;

package Gtk.Alternative_Trigger is

   type Gtk_Alternative_Trigger_Record is new Gtk_Shortcut_Trigger_Record with null record;
   type Gtk_Alternative_Trigger is access all Gtk_Alternative_Trigger_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Alternative_Trigger;
       First  : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Second : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Alternative_Trigger_Record'Class;
       First  : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Second : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class);
   --  Creates a `GtkShortcutTrigger` that will trigger whenever either of the
   --  two given triggers gets triggered.
   --  Note that nesting is allowed, so if you want more than two alternative,
   --  create a new alternative trigger for each option.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param First The first trigger that may trigger
   --  @param Second The second trigger that may trigger

   function Gtk_Alternative_Trigger_New
      (First  : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Second : not null access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class)
       return Gtk_Alternative_Trigger;
   --  Creates a `GtkShortcutTrigger` that will trigger whenever either of the
   --  two given triggers gets triggered.
   --  Note that nesting is allowed, so if you want more than two alternative,
   --  create a new alternative trigger for each option.
   --  @param First The first trigger that may trigger
   --  @param Second The second trigger that may trigger

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_alternative_trigger_get_type");

   -------------
   -- Methods --
   -------------

   function Get_First
      (Self : not null access Gtk_Alternative_Trigger_Record)
       return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger;
   --  Gets the first of the two alternative triggers that may trigger Self.
   --  [methodGtk.AlternativeTrigger.get_second] will return the other one.
   --  @return the first alternative trigger
   --  Return has transfer-ownership='none'

   function Get_Second
      (Self : not null access Gtk_Alternative_Trigger_Record)
       return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger;
   --  Gets the second of the two alternative triggers that may trigger Self.
   --  [methodGtk.AlternativeTrigger.get_first] will return the other one.
   --  @return the second alternative trigger
   --  Return has transfer-ownership='none'

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   First_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger
   --  The first `GtkShortcutTrigger` to check.

   Second_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger
   --  The second `GtkShortcutTrigger` to check.

private
   Second_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("second");
   First_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("first");
end Gtk.Alternative_Trigger;
