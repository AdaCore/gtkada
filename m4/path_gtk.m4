#############################################################
#
# Configure paths for GTK+
#
#############################################################


AC_DEFUN([AM_PATH_GTK],
[dnl AC_REQUIRE([AM_PATH_GTK_3_0])dnl
  AM_PATH_GTK_4_0([4.22.2])

  GTK_PREFIX=`$PKG_CONFIG gtk4 --variable=prefix`
  AC_SUBST(GTK_PREFIX)
  AM_TO_GPR($GTK_CFLAGS, GTK_CFLAGS_GPR)

  dnl 'gmodule-2.0' module is needed to link on Linux
  dnl 'fontconfig' module is needed to link on Windows

  PKG_CHECK_MODULES([GMODULE], "gmodule-2.0")
  PKG_CHECK_MODULES([FONTCONFIG], "fontconfig")

  GTK_LIBS="$GTK_LIBS $GMODULE_LIBS $FONTCONFIG_LIBS"

  dnl GTK_LIBS is pkg-config output, so it carries a -L for every directory
  dnl the .pc files mention, and some of those are system directories: GTK+ 4
  dnl requires vulkan, and when the GTK+ stack we build against ships no
  dnl vulkan.pc of its own, pkg-config falls back to the system one and we
  dnl inherit its "-L/usr/lib64".  Such a switch reaches gtkada.gpr's
  dnl Linker_Options, and from there gprbuild turns it into an
  dnl $ORIGIN-relative run path entry that diverts the linker to the system
  dnl libraries.  See strip_default_libdirs.m4 for the full story.  Do this
  dnl before the mingw adjustment below, so that the relative -L switches it
  dnl prepends are never even looked at.

  AM_STRIP_DEFAULT_LIBDIRS([GTK_LIBS])

  dnl On windows gtk will be embedded along with gtk distrib. In that
  dnl case we need to adjust switches so that gtkada.gpr packaged in
  dnl lib/gnat is usable

  case $build_os in
     *mingw*) GTK_LIBS="-L../../lib -L../../bin $GTK_LIBS";;
  esac

  AM_TO_GPR($GTK_LIBS, GTK_LIBS_GPR)
])
