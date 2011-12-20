------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with System;

package Glib.G_Icon is

   type G_Icon is private;
   Null_G_Icon : constant G_Icon;

   function G_Icon_Get_Type return GType;

   function "=" (Icon1, Icon2 : G_Icon) return Boolean;
   --  Returns whether two icons are equal.

   function Hash (Icon : G_Icon) return Guint;
   --  Returns a Guint containing a hash for the Icon, suitable for
   --  use in a GHashTable or similar data structure.

   function To_String (Icon : G_Icon) return UTF8_String;
   --  Generates a textual representation of Icon that can be used for
   --  serialization such as when passing Icon to a different process or
   --  saving it to persistent storage. Use New_For_String to get Icon
   --  back from the returned string.
   --
   --  The encoding of the returned string is proprietary to G_Icon except
   --  in the following two cases:
   --
   --  If Icon is a GFileIcon, the returned string is a native path
   --  (such as "/path/to/my icon.png") without escaping if the GFile for
   --  Icon is a native file.  If the file is not native, the returned
   --  string is the result of g_file_get_uri() (such as
   --  "sftp://path/to/my%%20icon.png").
   --
   --  If Icon is a GThemedIcon with exactly one name, the encoding is
   --  simply the name (such as "network-server").

   procedure New_For_String
     (Widget : out G_Icon;
      Str    : String);
   --  Generate a G_Icon instance from Str. This function can fail if
   --  Str is not valid - see To_String for discussion.  Returns
   --  Null_G_Icon on failure.
   --
   --  If your application or library provides one or more G_Icon
   --  implementations you need to ensure that each GType is registered
   --  with the type system prior to calling New_For_String.

private

   type G_Icon is new System.Address;
   Null_G_Icon : constant G_Icon := G_Icon (System.Null_Address);

end Glib.G_Icon;
