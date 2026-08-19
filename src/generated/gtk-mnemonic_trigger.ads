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

--  Triggers when a specific mnemonic is pressed.
--
--  Mnemonics require a *mnemonic modifier* (typically <kbd>Alt</kbd>) to be
--  pressed together with the mnemonic key.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                 use Glib;
with Glib.Properties;      use Glib.Properties;
with Gtk.Shortcut_Trigger; use Gtk.Shortcut_Trigger;

package Gtk.Mnemonic_Trigger is

   type Gtk_Mnemonic_Trigger_Record is new Gtk_Shortcut_Trigger_Record with null record;
   type Gtk_Mnemonic_Trigger is access all Gtk_Mnemonic_Trigger_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Mnemonic_Trigger; Keyval : Guint);
   procedure Initialize
      (Self   : not null access Gtk_Mnemonic_Trigger_Record'Class;
       Keyval : Guint);
   --  Creates a `GtkShortcutTrigger` that will trigger whenever the key with
   --  the given Keyval is pressed and mnemonics have been activated.
   --  Mnemonics are activated by calling code when a key event with the right
   --  modifiers is detected.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Keyval The keyval to trigger for

   function Gtk_Mnemonic_Trigger_New
      (Keyval : Guint) return Gtk_Mnemonic_Trigger;
   --  Creates a `GtkShortcutTrigger` that will trigger whenever the key with
   --  the given Keyval is pressed and mnemonics have been activated.
   --  Mnemonics are activated by calling code when a key event with the right
   --  modifiers is detected.
   --  @param Keyval The keyval to trigger for

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_mnemonic_trigger_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Keyval
      (Self : not null access Gtk_Mnemonic_Trigger_Record) return Guint;
   --  Gets the keyval that must be pressed to succeed triggering Self.
   --  @return the keyval

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Keyval_Property : constant Glib.Properties.Property_Uint;
   --  The key value for the trigger.

private
   Keyval_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("keyval");
end Gtk.Mnemonic_Trigger;
