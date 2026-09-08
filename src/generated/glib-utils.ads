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


pragma Warnings (Off, "*is already use-visible*");

package Glib.Utils is

   Major_Version : constant Glib.Gint := 2;
   Micro_Version : constant Glib.Gint := 0;
   Minor_Version : constant Glib.Gint := 88;

   ---------------
   -- Functions --
   ---------------

   function Check_Version
      (Required_Major : Guint;
       Required_Minor : Guint;
       Required_Micro : Guint) return UTF8_String;
   --  Checks that the GLib library in use is compatible with the given
   --  version.
   --  Generally you would pass in the constants GLIB_MAJOR_VERSION,
   --  GLIB_MINOR_VERSION, GLIB_MICRO_VERSION as the three arguments to this
   --  function; that produces a check that the library in use is compatible
   --  with the version of GLib the application or module was compiled against.
   --  Compatibility is defined by two things: first the version of the
   --  running library is newer than the version
   --  `Required_Major.required_minor.Required_Micro`. Second the running
   --  library must be binary compatible with the version
   --  `Required_Major.Required_Minor.Required_Micro` (same major version.)
   --  Since: gtk+ 2.6
   --  @param Required_Major the required major version
   --  @param Required_Minor the required minor version
   --  @param Required_Micro the required micro version
   --  @return null if the GLib library is compatible with the given version,
   --  or a string describing the version mismatch. The returned string is
   --  owned by GLib and must not be modified or freed.

   function Get_Home_Dir return UTF8_String;
   --  Gets the current user's home directory.
   --  As with most UNIX tools, this function will return the value of the
   --  `HOME` environment variable if it is set to an existing absolute path
   --  name, falling back to the `passwd` file in the case that it is unset.
   --  If the path given in `HOME` is non-absolute, does not exist, or is not
   --  a directory, the result is undefined.
   --  Before version 2.36 this function would ignore the `HOME` environment
   --  variable, taking the value from the `passwd` database instead. This was
   --  changed to increase the compatibility of GLib with other programs (and
   --  the XDG basedir specification) and to increase testability of programs
   --  based on GLib (by making it easier to run them from test frameworks).
   --  If your program has a strong requirement for either the new or the old
   --  behaviour (and if you don't wish to increase your GLib dependency to
   --  ensure that the new behaviour is in effect) then you should either
   --  directly check the `HOME` environment variable yourself or unset it
   --  before calling any functions in GLib.
   --  @return the current user's home directory

end Glib.Utils;
