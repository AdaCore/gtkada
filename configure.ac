AC_INIT(gtkada,18.0w,report@adacore.com,gtkada)
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_TARGET
PKG_PROG_PKG_CONFIG

TARGET=""
if test x$host != x$target ; then
   TARGET=${target}
fi
AC_SUBST(TARGET)

AM_ADD_OS_SPECIFIC_FLAGS

# checking for local tools
AC_PROG_CC

case $build_os in
   *darwin*) AC_PROG_OBJC(clang gcc CC) ;;
   *) OBJC=""; AC_SUBST(OBJC) ;;
esac

AM_PATH_GNAT

CHECK_BUILD_TYPE     # are we in devel mode ?

# Gettext stuff
ALL_LINGUAS="en"
AM_GNU_GETTEXT([external])

# checking for gtk:
#  1. PKG_CHECK_MODULES: check for pkg-config modules, this provides useful
#     diagnostic when some error occured.
#  2. AM_PATH_GTK: check for low level binary interface of the libraries and
#     create necessary variables to substitute into .gpr files.
PKG_CHECK_MODULES([GTK], [gtk+-3.0])
AM_PATH_GTK

# Ignore user's choice of libdir, as well as configure's own default,
# because we want GtkAda to always install in /lib (backward compatibility)

libdir='${prefix}/lib'

AC_OUTPUT(Makefile shared.gpr po/Makefile docs/gtkada_rm/html/static/index.html)

AC_MSG_NOTICE([--------- Summary for Gtkada $PACKAGE_VERSION -----------------])
AC_MSG_NOTICE(AC_HELP_STRING(
              [Shared libraries:],[$CAN_BUILD_SHARED (default: $DEFAULT_LIBRARY_TYPE)]))
AC_MSG_NOTICE([--------------------------------------------])
