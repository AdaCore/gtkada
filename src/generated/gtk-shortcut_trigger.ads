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

--  Tracks how a `GtkShortcut` can be activated.
--
--  To find out if a `GtkShortcutTrigger` triggers, you can call
--  [methodGtk.ShortcutTrigger.trigger] on a `GdkEvent`.
--
--  `GtkShortcutTriggers` contain functions that allow easy presentation to
--  end users as well as being printed for debugging.
--
--  All `GtkShortcutTriggers` are immutable, you can only specify their
--  properties during construction. If you want to change a trigger, you have
--  to replace it with a new one.

pragma Warnings (Off, "*is already use-visible*");
with Gdk;           use Gdk;
with Gdk.Display;
with Gdk.Event;     use Gdk.Event;
with Gdk.Key_Match; use Gdk.Key_Match;
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.String;   use Glib.String;

package Gtk.Shortcut_Trigger is

   type Gtk_Shortcut_Trigger_Record is new GObject_Record with null record;
   type Gtk_Shortcut_Trigger is access all Gtk_Shortcut_Trigger_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Shortcut_Trigger; String : UTF8_String);
   procedure Initialize
      (Self   : not null access Gtk_Shortcut_Trigger_Record'Class;
       String : UTF8_String);
   --  Tries to parse the given string into a trigger.
   --  On success, the parsed trigger is returned. When parsing failed, null
   --  is returned.
   --  The accepted strings are:
   --  - `never`, for `GtkNeverTrigger` - a string parsed by
   --  gtk_accelerator_parse, for a `GtkKeyvalTrigger`, e.g. `<Control>C` -
   --  underscore, followed by a single character, for `GtkMnemonicTrigger`,
   --  e.g. `_l` - two valid trigger strings, separated by a `|` character, for
   --  a `GtkAlternativeTrigger`: `<Control>q|<Control>w`
   --  Note that you will have to escape the `<` and `>` characters when
   --  specifying triggers in XML files, such as GtkBuilder ui files. Use `<`
   --  instead of `<` and `>` instead of `>`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param String the string to parse

   function Gtk_Shortcut_Trigger_Parse_String
      (String : UTF8_String) return Gtk_Shortcut_Trigger;
   --  Tries to parse the given string into a trigger.
   --  On success, the parsed trigger is returned. When parsing failed, null
   --  is returned.
   --  The accepted strings are:
   --  - `never`, for `GtkNeverTrigger` - a string parsed by
   --  gtk_accelerator_parse, for a `GtkKeyvalTrigger`, e.g. `<Control>C` -
   --  underscore, followed by a single character, for `GtkMnemonicTrigger`,
   --  e.g. `_l` - two valid trigger strings, separated by a `|` character, for
   --  a `GtkAlternativeTrigger`: `<Control>q|<Control>w`
   --  Note that you will have to escape the `<` and `>` characters when
   --  specifying triggers in XML files, such as GtkBuilder ui files. Use `<`
   --  instead of `<` and `>` instead of `>`.
   --  @param String the string to parse

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_shortcut_trigger_get_type");

   -------------
   -- Methods --
   -------------

   function Compare
      (Self     : not null access Gtk_Shortcut_Trigger_Record;
       Trigger2 : not null access Gtk_Shortcut_Trigger_Record'Class)
       return Glib.Gint;
   --  The types of Trigger1 and Trigger2 are `gconstpointer` only to allow
   --  use of this function as a `GCompareFunc`.
   --  They must each be a `GtkShortcutTrigger`.
   --  @param Trigger2 a `GtkShortcutTrigger`
   --  @return An integer less than, equal to, or greater than zero if
   --  Trigger1 is found, respectively, to be less than, to match, or be
   --  greater than Trigger2.

   function Equal
      (Self     : not null access Gtk_Shortcut_Trigger_Record;
       Trigger2 : not null access Gtk_Shortcut_Trigger_Record'Class)
       return Boolean;
   --  Checks if Trigger1 and Trigger2 trigger under the same conditions.
   --  The types of One and Two are `gconstpointer` only to allow use of this
   --  function with `GHashTable`. They must each be a `GtkShortcutTrigger`.
   --  @param Trigger2 a `GtkShortcutTrigger`
   --  @return True if Trigger1 and Trigger2 are equal

   function Hash
      (Self : not null access Gtk_Shortcut_Trigger_Record) return Guint;
   --  Generates a hash value for a `GtkShortcutTrigger`.
   --  The output of this function is guaranteed to be the same for a given
   --  value only per-process. It may change between different processor
   --  architectures or even different versions of GTK. Do not use this
   --  function as a basis for building protocols or file formats.
   --  The types of Trigger is `gconstpointer` only to allow use of this
   --  function with `GHashTable`. They must each be a `GtkShortcutTrigger`.
   --  @return a hash value corresponding to Trigger

   procedure Print
      (Self   : not null access Gtk_Shortcut_Trigger_Record;
       String : access Glib.String.Gstring);
   --  Prints the given trigger into a string for the developer. This is meant
   --  for debugging and logging.
   --  The form of the representation may change at any time and is not
   --  guaranteed to stay identical.
   --  @param String a `GString` to print into

   function Print_Label
      (Self    : not null access Gtk_Shortcut_Trigger_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       String  : access Glib.String.Gstring) return Boolean;
   --  Prints the given trigger into a string.
   --  This function is returning a translated string for presentation to end
   --  users for example in menu items or in help texts.
   --  The Display in use may influence the resulting string in various forms,
   --  such as resolving hardware keycodes or by causing display-specific
   --  modifier names.
   --  The form of the representation may change at any time and is not
   --  guaranteed to stay identical.
   --  @param Display `GdkDisplay` to print for
   --  @param String a `GString` to print into
   --  @return True if something was printed or False if the trigger did not
   --  have a textual representation suitable for end users.

   function To_Label
      (Self    : not null access Gtk_Shortcut_Trigger_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return UTF8_String;
   --  Gets textual representation for the given trigger.
   --  This function is returning a translated string for presentation to end
   --  users for example in menu items or in help texts.
   --  The Display in use may influence the resulting string in various forms,
   --  such as resolving hardware keycodes or by causing display-specific
   --  modifier names.
   --  The form of the representation may change at any time and is not
   --  guaranteed to stay identical.
   --  @param Display `GdkDisplay` to print for
   --  @return a new string

   function To_String
      (Self : not null access Gtk_Shortcut_Trigger_Record)
       return UTF8_String;
   --  Prints the given trigger into a human-readable string.
   --  This is a small wrapper around [methodGtk.ShortcutTrigger.print] to
   --  help when debugging.
   --  @return a new string

   function Trigger
      (Self             : not null access Gtk_Shortcut_Trigger_Record;
       Event            : Gdk.Event.Gdk_Event;
       Enable_Mnemonics : Boolean) return Gdk.Key_Match.Gdk_Key_Match;
   --  Checks if the given Event triggers Self.
   --  @param Event the event to check
   --  @param Enable_Mnemonics True if mnemonics should trigger. Usually the
   --  value of this property is determined by checking that the passed in
   --  Event is a Key event and has the right modifiers set.
   --  @return Whether the event triggered the shortcut

end Gtk.Shortcut_Trigger;
