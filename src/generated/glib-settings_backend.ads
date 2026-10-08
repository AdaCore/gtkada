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

--  The Glib.Settings_Backend.Gsettings_Backend interface defines a generic
--  interface for non-strictly-typed data that is stored in a hierarchy. To
--  implement an alternative storage backend for Glib.Settings.Gsettings, you
--  need to implement the Glib.Settings_Backend.Gsettings_Backend interface and
--  then make it implement the extension point
--  G_SETTINGS_BACKEND_EXTENSION_POINT_NAME.
--
--  The interface defines methods for reading and writing values, a method for
--  determining if writing of certain values will fail (lockdown) and a change
--  notification mechanism.
--
--  The semantics of the interface are very precisely defined and
--  implementations must carefully adhere to the expectations of callers that
--  are documented on each of the interface methods.
--
--  Some of the Glib.Settings_Backend.Gsettings_Backend functions accept or
--  return a Gtree.Gtree. These trees always have strings as keys and
--  Glib.Variant.Gvariant as values. g_settings_backend_create_tree is a
--  convenience function to create suitable trees.
--
--  The Glib.Settings_Backend.Gsettings_Backend API is exported to allow
--  third-party implementations, but does not carry the same stability
--  guarantees as the public GIO API. For this reason, you have to define the C
--  preprocessor symbol G_SETTINGS_ENABLE_BACKEND before including
--  `gio/gsettingsbackend.h`.
--
--  <group>GIO</group>
--  <gtkada_demo>create_settings.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings; use GNAT.Strings;
with Glib.Object;  use Glib.Object;

package Glib.Settings_Backend is

   type Gsettings_Backend_Record is new GObject_Record with null record;
   type Gsettings_Backend is access all Gsettings_Backend_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_settings_backend_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed
      (Self       : not null access Gsettings_Backend_Record;
       Key        : UTF8_String;
       Origin_Tag : System.Address);
   --  Signals that a single key has possibly changed. Backend implementations
   --  should call this if a key has possibly changed its value.
   --  Key must be a valid key (ie starting with a slash, not containing '//',
   --  and not ending with a slash).
   --  The implementation must call this function during any call to
   --  g_settings_backend_write, before the call returns (except in the case
   --  that no keys are actually changed and it cares to detect this fact). It
   --  may not rely on the existence of a mainloop for dispatching the signal
   --  later.
   --  The implementation may call this function at any other time it likes in
   --  response to other events (such as changes occurring outside of the
   --  program). These calls may originate from a mainloop or may originate in
   --  response to any other action (including from calls to
   --  g_settings_backend_write).
   --  In the case that this call is in response to a call to
   --  g_settings_backend_write then Origin_Tag must be set to the same value
   --  that was passed to that call.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key
   --  @param Origin_Tag the origin tag

   procedure Keys_Changed
      (Self       : not null access Gsettings_Backend_Record;
       Path       : UTF8_String;
       Items      : GNAT.Strings.String_List;
       Origin_Tag : System.Address);
   --  Signals that a list of keys have possibly changed. Backend
   --  implementations should call this if keys have possibly changed their
   --  values.
   --  Path must be a valid path (ie starting and ending with a slash and not
   --  containing '//'). Each string in Items must form a valid key name when
   --  Path is prefixed to it (ie: each item must not start or end with '/' and
   --  must not contain '//').
   --  The meaning of this signal is that any of the key names resulting from
   --  the contatenation of Path with each item in Items may have changed.
   --  The same rules for when notifications must occur apply as per
   --  Glib.Settings_Backend.Changed. These two calls can be used
   --  interchangeably if exactly one item has changed (although in that case
   --  Glib.Settings_Backend.Changed is definitely preferred).
   --  For efficiency reasons, the implementation should strive for Path to be
   --  as long as possible (ie: the longest common prefix of all of the keys
   --  that were changed) but this is not strictly required.
   --  Since: gtk+ 2.26
   --  @param Path the path containing the changes
   --  @param Items the null-terminated list of changed keys
   --  @param Origin_Tag the origin tag

   procedure Path_Changed
      (Self       : not null access Gsettings_Backend_Record;
       Path       : UTF8_String;
       Origin_Tag : System.Address);
   --  Signals that all keys below a given path may have possibly changed.
   --  Backend implementations should call this if an entire path of keys have
   --  possibly changed their values.
   --  Path must be a valid path (ie starting and ending with a slash and not
   --  containing '//').
   --  The meaning of this signal is that any of the key which has a name
   --  starting with Path may have changed.
   --  The same rules for when notifications must occur apply as per
   --  Glib.Settings_Backend.Changed. This call might be an appropriate
   --  reasponse to a 'reset' call but implementations are also free to
   --  explicitly list the keys that were affected by that call if they can
   --  easily do so.
   --  For efficiency reasons, the implementation should strive for Path to be
   --  as long as possible (ie: the longest common prefix of all of the keys
   --  that were changed) but this is not strictly required. As an example, if
   --  this function is called with the path of "/" then every single key in
   --  the application will be notified of a possible change.
   --  Since: gtk+ 2.26
   --  @param Path the path containing the changes
   --  @param Origin_Tag the origin tag

   procedure Path_Writable_Changed
      (Self : not null access Gsettings_Backend_Record;
       Path : UTF8_String);
   --  Signals that the writability of all keys below a given path may have
   --  changed.
   --  Since GSettings performs no locking operations for itself, this call
   --  will always be made in response to external events.
   --  Since: gtk+ 2.26
   --  @param Path the name of the path

   procedure Writable_Changed
      (Self : not null access Gsettings_Backend_Record;
       Key  : UTF8_String);
   --  Signals that the writability of a single key has possibly changed.
   --  Since GSettings performs no locking operations for itself, this call
   --  will always be made in response to external events.
   --  Since: gtk+ 2.26
   --  @param Key the name of the key

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Memory_New return Gsettings_Backend;
   --  Creates a private, nonpersistent back end. Unref when finished.

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gsettings_Backend;
   --  Returns the default Glib.Settings_Backend.Gsettings_Backend. It is
   --  possible to override the default by setting the `GSETTINGS_BACKEND`
   --  environment variable to the name of a settings backend.
   --  The user gets a reference to the backend.
   --  Since: gtk+ 2.28
   --  @return the default Glib.Settings_Backend.Gsettings_Backend, which will
   --  be a dummy (memory) settings backend if no other settings backend is
   --  available. Has transfer-ownership='full'.

end Glib.Settings_Backend;
