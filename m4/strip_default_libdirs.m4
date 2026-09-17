##########################################################################
## Remove from a list of linker switches every -L option that designates a
## directory the toolchain already searches by default.
##   $1 = name of the shell variable holding the switches, modified in place
##
## This ensures gprbuild/gprinstall do not add these to project
## switches, which in turn would cause this to be baked into an 'rpath'
## directive, which might put the system path in front of the gtkada-provider
## libs for the end user.
##
##########################################################################

AC_DEFUN([AM_STRIP_DEFAULT_LIBDIRS],
[
   AC_MSG_CHECKING([for redundant -L switches in $1])

   # Canonicalise one directory.
   am_sdl_realpath ()
   {
      ( cd "$[]1" 2>/dev/null && pwd -P ) || :
   }

   # Run "gcc -print-search-dirs", canonicalize the entries, to find the
   # system-searched paths on the system.
   am_sdl_defaults=" "
   for am_sdl_dir in `LIBRARY_PATH= $CC -print-search-dirs 2>/dev/null | \
                      sed -n 's/^libraries: *=*//p' | tr ':' ' '`; do
      am_sdl_real=`am_sdl_realpath "$am_sdl_dir"`
      if test -n "$am_sdl_real"; then
         am_sdl_defaults="$am_sdl_defaults$am_sdl_real "
      fi
   done

   am_sdl_kept=""
   am_sdl_dropped=""
   for am_sdl_arg in $[]$1; do
      am_sdl_drop=no
      case $am_sdl_arg in
         # Only absolute directories are considered.  A relative -L cannot be
         # compared against the (absolute) default list, and is in any case
         # meant to be interpreted relative to the installed project file
         # rather than to the current directory. (See the mingw case in
         # AM_PATH_GTK).
         -L/*)
            am_sdl_dir=`expr "X$am_sdl_arg" : 'X-L\(.*\)'`
            am_sdl_real=`am_sdl_realpath "$am_sdl_dir"`
            if test -n "$am_sdl_real"; then
               case $am_sdl_defaults in
                  *" $am_sdl_real "*) am_sdl_drop=yes ;;
               esac
            fi
            ;;
      esac
      if test $am_sdl_drop = yes; then
         am_sdl_dropped="$am_sdl_dropped $am_sdl_arg"
      else
         am_sdl_kept="$am_sdl_kept $am_sdl_arg"
      fi
   done

   # Unquoted, so that the leading separator built up by the loop is
   # cleared, along with any other whitespace.
   $1=`echo $am_sdl_kept`

   if test -n "$am_sdl_dropped"; then
      AC_MSG_RESULT([dropped$am_sdl_dropped])
   else
      AC_MSG_RESULT([none])
   fi
])
