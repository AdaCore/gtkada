------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

--  <description>
--  Gtk_Bindings provides a mechanism for configuring Gtk+ key bindings through
--  RC files. This eases key binding adjustments for application developers as
--  well as users and provides Gtk+ users or administrators with high key
--  binding configurability which requires no application or toolkit side
--  changes.
--
--  Installing a key binding
--  ========================
--
--  A resource file binding consists of a 'binding' definition and a match
--  statement to apply the binding to specific widget types. Details on the
--  matching mechanism are described under Pathnames and patterns. Inside the
--  binding definition, key combinations are bound to specific signal emissions
--  on the target widget. Key combinations are strings consisting of an
--  optional Gdk_Modifier_Type name and key names such as those defined in
--  Gdk.Types.Keysyms or returned from gdk_keyval_name(), they have to be
--  parsable by gtk_accelerator_parse(). Specifications of signal emissions
--  consist of a string identifying the signal name, and a list of signal
--  specific arguments in parenthesis. For example for binding Control and the
--  left or right cursor keys of a Gtk_Entry widget to the
--  Gtk_Entry::move-cursor signal, so movement occurs in 3 letter steps, the
--  following binding can be used:
--
--  binding "MoveCursor3" {
--     bind "<Control>Right" {
--       "move-cursor" (visual-positions, 3, 0)
--     }
--     bind "<Control>Left" {
--       "move-cursor" (visual-positions, -3, 0)
--     }
--  }
--  class "GtkEntry" binding "MoveCursor3"
--
--  Unbinding existing key bindings
--  ===============================
--
--  Gtk+ already defines a number of useful bindings for the widgets it
--  provides. Because custom bindings set up in RC files take precedence over
--  the default bindings shipped with Gtk+, overriding existing bindings as
--  demonstrated in Installing a key binding works as expected. The same
--  mechanism can not be used to "unbind" existing bindings, however.
--
--  binding "MoveCursor3" {
--     bind "<Control>Right" { }
--     bind "<Control>Left" { }
--  }
--  class "GtkEntry" binding "MoveCursor3"
--
--  The above example will not have the desired effect of causing
--  "<Control>Right" and "<Control>Left" key presses to be ignored by Gtk+.
--  Instead, it just causes any existing bindings from the bindings set
--  "MoveCursor3" to be deleted, so when "<Control>Right" or "<Control>Left"
--  are pressed, no binding for these keys is found in binding set
--  "MoveCursor3". Gtk+ will thus continue to search for matching key bindings,
--  and will eventually lookup and find the default Gtk+ bindings for entries
--  which implement word movement. To keep Gtk+ from activating its default
--  bindings, the "unbind" keyword can be used like this:
--
--  binding "MoveCursor3" {
--     unbind "<Control>Right"
--     unbind "<Control>Left"
--  }
--  class "GtkEntry" binding "MoveCursor3"
--
--  Now, Gtk+ will find a match when looking up "<Control>Right" and
--  "<Control>Left" key presses before it resorts to its default bindings, and
--  the match instructs it to abort ("unbind") the search, so the key presses
--  are not consumed by this widget. As usual, further processing of the key
--  presses, e.g. by an entries parent widget, is now possible.
--  </description>
--  <c_version>2.14</c_version>
--  <group>Configuration and Themes</group>

with Gdk.Event;
with Gdk.Types;
with Glib.Object;

package Gtk.Bindings is

   type Gtk_Binding_Set is new Glib.C_Proxy;
   --  A binding set maintains a list of activatable key bindings. A single
   --  binding set can match multiple types of widgets. Similar to styles,
   --  widgets can be mapped by widget name paths, widget class paths or widget
   --  class types. When a binding within a set is matched upon activation, an
   --  action signal is emitted on the target widget to carry out the actual
   --  activation.

   function Binding_Set_New (Set_Name : String) return Gtk_Binding_Set;
   --  Gtk+ maintains a global list of binding sets. Each binding set has a
   --  unique name which needs to be specified upon creation.

   function Binding_Set_By_Class
     (Object_Class : Glib.Object.GObject_Class) return Gtk_Binding_Set;
   --  This function returns the binding set named after the type name of the
   --  passed in class structure. New binding sets are created on demand by
   --  this function.

   function Binding_Set_Find (Set_Name : String) return Gtk_Binding_Set;
   --  Find a binding set by its globally unique name. The set_name can either
   --  be a name used for Binding_Set_New or the type name of a class
   --  used in Binding_Set_By_Class.

   function Activate
     (Object    : access Glib.Object.GObject_Record'Class;
      Keyval    : Guint;
      Modifiers : Gdk.Types.Gdk_Modifier_Type)
      return Boolean;
   --  Find a key binding matching keyval and modifiers and activate the
   --  binding on object.

   function Activate_Event
     (Object : access Glib.Object.GObject_Record;
      Event  : Gdk.Event.Gdk_Event_Key)
      return Boolean;
   --  Looks up key bindings for Object to find one matching
   --  Event, and if one was found, activate it.
   --  Return value: True if a matching key binding was found

   function Binding_Set_Activate
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Object      : access Glib.Object.GObject_Record'Class)
      return Boolean;
   --  Find a key binding matching keyval and modifiers within binding_set and
   --  activate the binding on object.

   procedure Binding_Entry_Skip
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type);
   --  Install a binding on Binding_Set which causes key lookups
   --  to be aborted, to prevent bindings from lower priority sets
   --  to be activated.
   --  Since: 2.12

   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : Glib.Signal_Name);
   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : Glib.Signal_Name;
      Arg1        : Gint);
   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : Glib.Signal_Name;
      Arg1        : Boolean);
   procedure Add_Signal
     (Binding_Set : Gtk_Binding_Set;
      Keyval      : Guint;
      Modifiers   : Gdk.Types.Gdk_Modifier_Type;
      Signal_Name : Glib.Signal_Name;
      Arg1        : Gint;
      Arg2        : Gint);
   --  Override or install a new key binding for keyval with modifiers on
   --  binding_set. When the binding is activated, signal_name will be emitted
   --  on the target widget, with the given arguments as argument.

private
   pragma Import (C, Binding_Set_By_Class, "gtk_binding_set_by_class");
   pragma Import (C, Binding_Entry_Skip,   "gtk_binding_entry_skip");
end Gtk.Bindings;

--  These are bound through our own C wrappers:
--  No binding: gtk_binding_entry_add_signal

--  These are internal gtk+ functions
--  No binding: gtk_binding_entry_add_signall
--  No binding: gtk_binding_entry_clear
--  No binding: gtk_binding_entry_remove
--  No binding: gtk_binding_parse_binding
--  No binding: gtk_binding_set_add_path
