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

--  Encodes an action that can be triggered by a keyboard shortcut.
--
--  `GtkShortcutActions` contain functions that allow easy presentation to end
--  users as well as being printed for debugging.
--
--  All `GtkShortcutActions` are immutable, you can only specify their
--  properties during construction. If you want to change a action, you have to
--  replace it with a new one. If you need to pass arguments to an action,
--  these are specified by the higher-level `GtkShortcut` object.
--
--  To activate a `GtkShortcutAction` manually,
--  [methodGtk.ShortcutAction.activate] can be called.
--
--  GTK provides various actions:
--
--  - [classGtk.MnemonicAction]: a shortcut action that calls
--  Gtk.Widget.Mnemonic_Activate - [classGtk.CallbackAction]: a shortcut action
--  that invokes a given callback - [classGtk.SignalAction]: a shortcut action
--  that emits a given signal - [classGtk.ActivateAction]: a shortcut action
--  that calls Gtk.Widget.Activate - [classGtk.NamedAction]: a shortcut action
--  that calls gtk_widget_activate_action - [classGtk.NothingAction]: a
--  shortcut action that does nothing

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.String;             use Glib.String;
with Glib.Variant;            use Glib.Variant;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Shortcut_Action is

   type Gtk_Shortcut_Action_Record is new GObject_Record with null record;
   type Gtk_Shortcut_Action is access all Gtk_Shortcut_Action_Record'Class;

   type Gtk_Shortcut_Action_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Shortcut_Action_Flags);
   --  Flags that can be passed to action activation.
   --
   --  More flags may be added in the future.

   Shortcut_Action_Exclusive : constant Gtk_Shortcut_Action_Flags := 1;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Shortcut_Action_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Shortcut_Action_Flags);
   type Property_Gtk_Shortcut_Action_Flags is new Gtk_Shortcut_Action_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Shortcut_Action; String : UTF8_String);
   procedure Initialize
      (Self   : not null access Gtk_Shortcut_Action_Record'Class;
       String : UTF8_String);
   --  Tries to parse the given string into an action.
   --  On success, the parsed action is returned. When parsing failed, null is
   --  returned.
   --  The accepted strings are:
   --  - `nothing`, for `GtkNothingAction` - `activate`, for
   --  `GtkActivateAction` - `mnemonic-activate`, for `GtkMnemonicAction` -
   --  `action(NAME)`, for a `GtkNamedAction` for the action named `NAME` -
   --  `signal(NAME)`, for a `GtkSignalAction` for the signal `NAME`
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param String the string to parse

   function Gtk_Shortcut_Action_Parse_String
      (String : UTF8_String) return Gtk_Shortcut_Action;
   --  Tries to parse the given string into an action.
   --  On success, the parsed action is returned. When parsing failed, null is
   --  returned.
   --  The accepted strings are:
   --  - `nothing`, for `GtkNothingAction` - `activate`, for
   --  `GtkActivateAction` - `mnemonic-activate`, for `GtkMnemonicAction` -
   --  `action(NAME)`, for a `GtkNamedAction` for the action named `NAME` -
   --  `signal(NAME)`, for a `GtkSignalAction` for the signal `NAME`
   --  @param String the string to parse

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_shortcut_action_get_type");

   -------------
   -- Methods --
   -------------

   function Activate
      (Self   : not null access Gtk_Shortcut_Action_Record;
       Flags  : Gtk_Shortcut_Action_Flags;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Args   : Glib.Variant.Gvariant) return Boolean;
   --  Activates the action on the Widget with the given Args.
   --  Note that some actions ignore the passed in Flags, Widget or Args.
   --  Activation of an action can fail for various reasons. If the action is
   --  not supported by the Widget, if the Args don't match the action or if
   --  the activation otherwise had no effect, False will be returned.
   --  @param Flags flags to activate with
   --  @param Widget Target of the activation
   --  @param Args arguments to pass
   --  @return True if this action was activated successfully

   procedure Print
      (Self   : not null access Gtk_Shortcut_Action_Record;
       String : access Glib.String.Gstring);
   --  Prints the given action into a string for the developer.
   --  This is meant for debugging and logging.
   --  The form of the representation may change at any time and is not
   --  guaranteed to stay identical.
   --  @param String a `GString` to print into

   function To_String
      (Self : not null access Gtk_Shortcut_Action_Record) return UTF8_String;
   --  Prints the given action into a human-readable string.
   --  This is a small wrapper around [methodGtk.ShortcutAction.print] to help
   --  when debugging.
   --  @return a new string

end Gtk.Shortcut_Action;
