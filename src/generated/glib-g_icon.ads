------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
--  Glib.G_Icon.G_Icon is a very minimal interface for icons. It provides
--  functions for checking the equality of two icons, hashing of icons and
--  serializing an icon to and from strings.
--
--  Glib.G_Icon.G_Icon does not provide the actual pixmap for the icon as this
--  is out of GIO's scope, however implementations of Glib.G_Icon.G_Icon may
--  contain the name of an icon (see Gthemed.Icon.Gthemed_Icon), or the path to
--  an icon (see Gloadable.Icon.Gloadable_Icon).
--
--  To obtain a hash of a Glib.G_Icon.G_Icon, see Glib.G_Icon.Hash.
--
--  To check if two GIcons are equal, see Glib.G_Icon.Equal.
--
--  For serializing a Glib.G_Icon.G_Icon, use Glib.G_Icon.To_String and
--  Glib.G_Icon.New_For_String.
--
--  If your application or library provides one or more Glib.G_Icon.G_Icon
--  implementations you need to ensure that each GType is registered with the
--  type system prior to calling Glib.G_Icon.New_For_String.
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Glib.Types; use Glib.Types;

package Glib.G_Icon is

   type G_Icon is new Glib.Types.GType_Interface;
   Null_G_Icon : constant G_Icon;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_icon_get_type");

   -------------
   -- Methods --
   -------------

   function Equal (Self : G_Icon; Icon2 : G_Icon) return Boolean;
   --  Checks if two icons are equal.
   --  "icon2": pointer to the second Glib.G_Icon.G_Icon.

   function To_String (Self : G_Icon) return UTF8_String;
   --  Generates a textual representation of Icon that can be used for
   --  serialization such as when passing Icon to a different process or saving
   --  it to persistent storage. Use Glib.G_Icon.New_For_String to get Icon
   --  back from the returned string.
   --  The encoding of the returned string is proprietary to
   --  Glib.G_Icon.G_Icon except in the following two cases
   --     * If Icon is a Gfile.Icon.Gfile_Icon, the returned string is a
   --  native path (such as '/path/to/my icon.png') without escaping if the
   --  Gfile.Gfile for Icon is a native file. If the file is not native, the
   --  returned string is the result of g_file_get_uri (such as
   --  'sftp://path/to/my%20icon.png').
   --     * If Icon is a Gthemed.Icon.Gthemed_Icon with exactly one name, the
   --  encoding is simply the name (such as 'network-server').
   --  Since: gtk+ 2.20

   ---------------
   -- Functions --
   ---------------

   function Hash (Icon : G_Icon) return Guint;
   pragma Import (C, Hash, "g_icon_hash");
   --  Gets a hash for an icon.
   --  "icon": gconstpointer to an icon object.

   function New_For_String (Str : UTF8_String) return G_Icon;
   --  Generate a Glib.G_Icon.G_Icon instance from Str. This function can fail
   --  if Str is not valid - see Glib.G_Icon.To_String for discussion.
   --  If your application or library provides one or more Glib.G_Icon.G_Icon
   --  implementations you need to ensure that each GType is registered with
   --  the type system prior to calling Glib.G_Icon.New_For_String.
   --  Since: gtk+ 2.20
   --  "str": A string obtained via Glib.G_Icon.To_String.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "G_Icon"

   function "+" (W : G_Icon) return G_Icon;
   pragma Inline ("+");

private

Null_G_Icon : constant G_Icon :=
   G_Icon (Glib.Types.Null_Interface);
end Glib.G_Icon;
