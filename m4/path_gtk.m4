#############################################################
#
# Configure paths for GTK+
#
#############################################################


AC_DEFUN([AM_PATH_GTK],
[dnl AC_REQUIRE([AM_PATH_GTK_3_0])dnl
  AM_PATH_GTK_3_0([3.24.24])

  GTK_PREFIX=`$PKG_CONFIG gtk+-3.0 --variable=prefix`
  AC_SUBST(GTK_PREFIX)
  AM_TO_GPR($GTK_CFLAGS, GTK_CFLAGS_GPR)

  dnl 'gmodule-2.0' module is needed to link on Linux
  dnl 'fontconfig' module is needed to link on Windows

  PKG_CHECK_MODULES([GMODULE], "gmodule-2.0")
  PKG_CHECK_MODULES([FONTCONFIG], "fontconfig")

  GTK_LIBS="$GTK_LIBS $GMODULE_LIBS $FONTCONFIG_LIBS"

  dnl On windows gtk will be embedded along with gtk distrib. In that
  dnl case we need to adjust switches so that gtkada.gpr packaged in
  dnl lib/gnat is usable

  case $build_os in
     *mingw*) GTK_LIBS="-L../../lib -L../../bin $GTK_LIBS";;
  esac

  AM_TO_GPR($GTK_LIBS, GTK_LIBS_GPR)
])
