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
with Pango.Language; use Pango.Language;

package Gtk.Main is

   ---------------
   -- Functions --
   ---------------

   function Get_Major_Version return Guint;
   --  Returns the major version number of the GTK library.
   --  For example, in GTK version 3.1.5 this is 3.
   --  This function is in the library, so it represents the GTK library your
   --  code is running against. Contrast with the GTK_MAJOR_VERSION macro,
   --  which represents the major version of the GTK headers you have included
   --  when compiling your code.
   --  @return the major version number of the GTK library

   function Get_Minor_Version return Guint;
   --  Returns the minor version number of the GTK library.
   --  For example, in GTK version 3.1.5 this is 1.
   --  This function is in the library, so it represents the GTK library your
   --  code is are running against. Contrast with the GTK_MINOR_VERSION macro,
   --  which represents the minor version of the GTK headers you have included
   --  when compiling your code.
   --  @return the minor version number of the GTK library

   function Get_Micro_Version return Guint;
   --  Returns the micro version number of the GTK library.
   --  For example, in GTK version 3.1.5 this is 5.
   --  This function is in the library, so it represents the GTK library your
   --  code is are running against. Contrast with the GTK_MICRO_VERSION macro,
   --  which represents the micro version of the GTK headers you have included
   --  when compiling your code.
   --  @return the micro version number of the GTK library

   function Get_Binary_Age return Guint;
   --  Returns the binary age as passed to `libtool`.
   --  If `libtool` means nothing to you, don't worry about it.
   --  @return the binary age of the GTK library

   function Get_Interface_Age return Guint;
   --  Returns the interface age as passed to `libtool`.
   --  If `libtool` means nothing to you, don't worry about it.
   --  @return the interface age of the GTK library

   function Check_Version
      (Required_Major : Guint;
       Required_Minor : Guint;
       Required_Micro : Guint) return UTF8_String;
   --  Checks that the GTK library in use is compatible with the given
   --  version.
   --  Generally you would pass in the constants GTK_MAJOR_VERSION,
   --  GTK_MINOR_VERSION, GTK_MICRO_VERSION as the three arguments to this
   --  function; that produces a check that the library in use is compatible
   --  with the version of GTK the application or module was compiled against.
   --  Compatibility is defined by two things: first the version of the
   --  running library is newer than the version
   --  Required_Major.required_minor.Required_Micro. Second the running library
   --  must be binary compatible with the version
   --  Required_Major.required_minor.Required_Micro (same major version.)
   --  This function is primarily for GTK modules; the module can call this
   --  function to check that it wasn't loaded into an incompatible version of
   --  GTK. However, such a check isn't completely reliable, since the module
   --  may be linked against an old version of GTK and calling the old version
   --  of Gtk.Main.Check_Version, but still get loaded into an application
   --  using a newer version of GTK.
   --  @param Required_Major the required major version
   --  @param Required_Minor the required minor version
   --  @param Required_Micro the required micro version
   --  @return null if the GTK library is compatible with the given version,
   --  or a string describing the version mismatch. The returned string is
   --  owned by GTK and should not be modified or freed.

   procedure Init;
   --  Initializes GTK.
   --  This function must be called before using any other GTK functions in
   --  your GUI applications.
   --  It will initialize everything needed to operate the toolkit. In
   --  particular, it will open the default display (see
   --  [funcGdk.Display.get_default]).
   --  If you are using [classGtk.Application], you usually don't have to call
   --  this function; the [vfuncGio.Application.startup] handler does it for
   --  you. Though, if you are using `GApplication` methods that will be
   --  invoked before `startup`, such as `local_command_line`, you may need to
   --  initialize GTK explicitly.
   --  This function will terminate your program if it was unable to
   --  initialize the windowing system for some reason. If you want your
   --  program to fall back to a textual interface, call [funcGtk.init_check]
   --  instead.
   --  GTK calls `signal (SIGPIPE, SIG_IGN)` during initialization, to ignore
   --  SIGPIPE signals, since these are almost never wanted in graphical
   --  applications. If you do need to handle SIGPIPE for some reason, reset
   --  the handler after Gtk.Main.Init, but notice that other libraries (e.g.
   --  libdbus or gvfs) might do similar things.

   function Init_Check return Boolean;
   --  Initializes GTK.
   --  This function does the same work as [funcGtk.init] with only a single
   --  change: It does not terminate the program if the windowing system can't
   --  be initialized. Instead it returns false on failure.
   --  This way the application can fall back to some other means of
   --  communication with the user - for example a curses or command line
   --  interface.
   --  @return true if the windowing system has been successfully initialized,
   --  false otherwise

   procedure Disable_Setlocale;
   --  Prevents [funcGtk.init] and [funcGtk.init_check] from calling
   --  `setlocale`.
   --  You would want to use this function if you wanted to set the locale for
   --  your program to something other than the user's locale, or if you wanted
   --  to set different values for different locale categories.
   --  Most programs should not need to call this function.

   function Get_Default_Language return Pango.Language.Pango_Language;
   --  Returns the `PangoLanguage` for the default language currently in
   --  effect.
   --  Note that this can change over the life of an application.
   --  The default language is derived from the current locale. It determines,
   --  for example, whether GTK uses the right-to-left or left-to-right text
   --  direction.
   --  This function is equivalent to [funcPango.Language.get_default]. See
   --  that function for details.
   --  @return the default language

end Gtk.Main;
