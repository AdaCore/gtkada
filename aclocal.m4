#############################################################
#
#  Adding some OS specific flags and parameters
#
############################################################

AC_DEFUN(AM_ADD_OS_SPECIFIC_FLAGS,
[
   SO_EXT=.so
   SO_OPTS=-Wl,-soname,
   TARGET_LFLAGS=
   DEFAULT_LIBRARY_TYPE=static
   NEED_OBJECTIVE_C=no

   AC_ARG_ENABLE(static,
     [AC_HELP_STRING(
        [--disable-static],
        [Disable building of static libraries.])
AC_HELP_STRING(
        [--enable-static],
        [Build static libraries (default).])],
     [BUILD_STATIC=$enableval
      if test $enableval = no; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [BUILD_STATIC=yes])

   AC_ARG_ENABLE(shared,
     [AC_HELP_STRING(
        [--disable-shared],
        [Disable building of shared libraries (default is to build them if supported on the target)])
AC_HELP_STRING(
        [--enable-shared],
        [Build shared libraries if supported on the target and
make them preselected in project files (static libraries are preselected by default])],
     [CAN_BUILD_SHARED=$enableval
      if test $enableval = yes; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [CAN_BUILD_SHARED=yes])

   BUILD_SHARED=$CAN_BUILD_SHARED

   case $build_os in
   aix*)
      BUILD_SHARED=no
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-bexpall,-berok
      TARGET_LFLAGS=-Wl,-bbigtoc
      SO_OPTS="-o "
      ;;
   hp*)
      SO_EXT=.sl
      SO_OPTS=-Wl,+h,
      BUILD_SHARED=no
      ;;
   *sysv4uw* | *sysv5uw*)
      SO_OPTS=-Wl,-h,
      BUILD_SHARED=no
      ;;
   *solaris*)
      SO_OPTS=-Wl,-h,
      ;;
   *irix*)
      ;;
   *osf*)
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-expect_unresolved,\*
      ;;
   *mingw*)
      if test x$CAN_BUILD_SHARED = xyes ; then
         BUILD_SHARED=yes
      fi
      SO_EXT=.dll
      ac_tmp_GNATDIR=`which gcc | sed 's,/gcc$,,'`
      ac_GNATDIR=`cygpath --mixed $ac_tmp_GNATDIR`
      count=`cd $ac_GNATDIR; ls libgnat-*.dll | wc -l`
      if test $count -gt 1 ; then
         echo "Too many libgnat.dll, in $ac_GNATDIR"
	 echo Found: `cd $ac_GNATDIR; ls libgnat-*.dll`
         exit 1
      fi
      ac_GNATLIB=`cd $ac_GNATDIR; ls libgnat-*.dll | sed 's,lib,,;s,.dll,,'`
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-L$ac_GNATDIR,-l$ac_GNATLIB
      ;;
   *darwin*)
      SO_EXT=.dylib
      NEED_OBJECTIVE_C=yes
      if test x$CAN_BUILD_SHARED = xyes ; then
         BUILD_SHARED=yes
      fi
      SO_OPTS="-Wl,-undefined,dynamic_lookup -dynamiclib -Wl,-dylib_install_name,"
      LDFLAGS="-Wl,-framework,Cocoa"
      TARGET_LFLAGS="-Wl,-framework,Cocoa"
      ;;
   # ??? The following case has been introduced because of an elaboration
   # problem with the GtkAda dynamic library and GPS (see E511-010). This
   # is a workaround, and shall be removed as soon as the bug is fixed.
   *linux*)
      case $build_cpu in
      *ia64*)
         BUILD_SHARED=no
         ;;
      esac
      ;;
   esac

  if test x$BUILD_SHARED = xno ; then
     DEFAULT_LIBRARY_TYPE=static
  fi

  AC_SUBST(DEFAULT_LIBRARY_TYPE)
  AC_SUBST(OS_SPECIFIC_LINK_OPTIONS)
  AC_SUBST(BUILD_STATIC)
  AC_SUBST(BUILD_SHARED)
  AC_SUBST(SO_EXT)
  AC_SUBST(SO_OPTS)
  AC_SUBST(TARGET_LFLAGS)
  AC_SUBST(NEED_OBJECTIVE_C)

]
)

#############################################################
#  Checking for build type
#############################################################

AC_DEFUN(CHECK_BUILD_TYPE,
[
    AC_ARG_ENABLE(build,
       [AC_HELP_STRING(
          [--enable-build=<type>],
          [Default build type for the library (Debug, Production)])],
       BUILD_TYPE=$enableval,
       BUILD_TYPE=Production)
   AC_SUBST(BUILD_TYPE)
]
)

##########################################################################
## Converts a list of space-separated words into a list suitable for
## inclusion in .gpr files
##   $1=the list
##   $2=exported name
##########################################################################

AC_DEFUN(AM_TO_GPR,
[
   value=[$1]

   # Special handling on darwin for gcc 4.5 and 4.7
   case "$build_os" in
      *darwin*)
         value=`echo $value | sed -e "s/-framework \([[^ ]]*\)/-Wl,-framework -Wl,\1/g"`
   esac

   output=$2
   result=""
   for v in $value; do
      if test "$result" != ""; then
         result="$result, "
      fi
      result="$result\"$v\""
   done
   $2=$result
   AC_SUBST($2)
])

#############################################################
#
#  Checking for Gnat
#
#############################################################

conftest_ok="conftest.ok"

AC_DEFUN(AM_PATH_GNAT,
[
   AC_PATH_PROG(GPRBUILD, gprbuild, no)

   if test x$GPRBUILD = xno ; then
      AC_MSG_ERROR(I could not find gprbuild. See the file 'INSTALL' for more details.)
   fi

   AC_MSG_CHECKING(that your gnat compiler works with a simple example)

   rm -f conftest.adb
   cat << EOF > conftest.adb
with Ada.Text_IO;

procedure Conftest is
   Conftest_Ok : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Create (File => Conftest_Ok,
                       Name => "$conftest_ok");
   Ada.Text_IO.Close (Conftest_Ok);
end Conftest;
EOF
   cat <<EOF > conftest.gpr
project Conftest is
   for Main use ("conftest.adb");
   for Source_Files use ("conftest.adb");
end Conftest;
EOF

   $GPRBUILD -q -P conftest.gpr > /dev/null

   if ( test ! -x conftest ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GPRBUILD test failed at compile time! Check your configuration.)
   fi

   ./conftest

   if ( test ! -f $conftest_ok ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GPRBUILD test failed at run time! Check your configuration.)
   fi

   AC_MSG_RESULT(yes)
])


#############################################################
#
#  Checking for gnatprep
#
#############################################################


AC_DEFUN(AM_PATH_GNATPREP,
[
   AC_PATH_PROG(GNATPREP, gnatprep, no)

   if test x$GNATPREP = xno ; then
      AC_MSG_ERROR(I could not find gnatprep. See the file 'INSTALL' for more details.)
   fi

])

#############################################################
#
#  Checking for Perl
#
#############################################################

AC_DEFUN(AM_PATH_PERL,
[
   AC_PATH_PROGS(PERL, perl5 perl)

   ### We don't really have any need for a specific version
   ### of perl for the moment, so we don't verify it.

])

#############################################################
#
# Configure paths for GTK+
# Input:
#    $1=minimal version of gtk+ needed
#
#############################################################

AC_DEFUN(AM_PATH_GTK,
[
  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  min_gtk_version=[$1]
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  GTK="gtk+-3.0"
  if test "$PKG_CONFIG" = "no" ; then
     AC_MSG_ERROR([pkg-config not found])
  else
    $PKG_CONFIG $GTK --atleast-version=$min_gtk_version
    if test $? != 0 ; then
       AC_MSG_ERROR([old version detected])
    fi
    
    GTK_PREFIX=`$PKG_CONFIG $GTK --variable=prefix`
    GTK_CFLAGS=`$PKG_CONFIG $GTK --cflags`
    GTK_LIBS=`$PKG_CONFIG $GTK gmodule-2.0 --libs`

    dnl force some explicit flags, like -framework Cocoa. On some machines,
    dnl these are added automatically by pkg-config, but not systematically.
    GTK_LIBS="$GTK_LIBS $LDFLAGS"

    dnl
    dnl Now check if the installed GTK is sufficiently new. (Also sanity
    dnl checks the results of pkg-config to some extent
    dnl
    ac_save_CFLAGS="$CFLAGS"
    ac_save_LIBS="$LIBS"
    CFLAGS="$CFLAGS $GTK_CFLAGS"
    LIBS="$LIBS $GTK_LIBS"
    rm -f conf.gtktest
    AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
int
main (int argc, char** argv)
{
  gtk_init(&argc, &argv);
  return 0;
}
],, no_gtk=yes)

    CFLAGS="$ac_save_CFLAGS"
    LIBS="$ac_save_LIBS"
  fi

  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
  else
     AC_MSG_ERROR(no)
  fi
  AC_SUBST(GTK_PREFIX)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  AM_TO_GPR($GTK_CFLAGS, GTK_CFLAGS_GPR)
  AM_TO_GPR($GTK_LIBS,  GTK_LIBS_GPR)
  rm -f conf.gtktest
])

#############################################################
#
#  Checking for openGL
#
#############################################################


AC_DEFUN(AM_CHECK_OPENGL,
[

   # checking for OpenGL libraries
   AC_ARG_WITH(GL,         [  --with-GL=value         Which OpenGL library to compile GtkAda with (auto,GL,GL32,MesaGL,,no)])
   AC_ARG_WITH(GL-prefix,  [  --with-GL-prefix=DIR    Prefix where GL/MesaGL is installed])

   if test "x$with_GL_prefix" = "x" ; then
      GL_LDOPTS=""
      GL_CFLAGS=""
   else
      GL_CFLAGS="-I$with_GL_prefix/include"
      case "${host}" in
         *64*)
            GL_LDOPTS="-L$with_GL_prefix/lib64"
            ;;
         *)
            GL_LDOPTS="-L$with_GL_prefix/lib"
            ;;
      esac
   fi

   saved_LIBS="$LIBS"

   if test "x$with_GL" != xno ; then
     AC_MSG_CHECKING([for OpenGL])
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lGLU -lGL"
     AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_GL=yes, have_GL=no)
     AC_MSG_RESULT($have_GL)

     AC_MSG_CHECKING([for GL32])
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lglu32 -lopengl32 -lgdi32"
     AC_TRY_LINK([
#include <GL/gl.h>
#include <windows.h>],
[ glBegin(0);
  CreateCompatibleDC(NULL); ], have_GL32=yes, have_GL32=no)
     AC_MSG_RESULT($have_GL32)

     AC_MSG_CHECKING([for Mesa])
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lMesaGLU -lMesaGL"
     AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_MesaGL=yes, have_MesaGL=no)
     AC_MSG_RESULT($have_MesaGL)

     if test "x$have_MesaGL" = "xno"; then
       AC_MSG_CHECKING([Mesa with pthreads])
       LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lMesaGLU -lMesaGL -lpthread"
       AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_MesaGL_pthread=yes, have_MesaGL_pthread=no)
       AC_MSG_RESULT($have_MesaGL_pthread)
     fi
   fi

   LIBS="$saved_LIBS"
   HAVE_OPENGL="False"

   case "x$with_GL" in
   x|xauto)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lGLU -lGL"
         HAVE_OPENGL="True"
      elif test "x$have_GL32" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lglu32 -lopengl32 -lgdi32"
         HAVE_OPENGL="True"
      elif test "x$have_MesaGL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL"
         HAVE_OPENGL="True"
      elif test "x$have_MesaGL_pthread" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL -lpthread"
         HAVE_OPENGL="True"
      fi
      ;;
   xGL)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lGLU -lGL"
         HAVE_OPENGL="True"
      else
         AC_MSG_ERROR([Missing OpenGL library])
      fi
      ;;
   xGL32)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lglu32 -lopengl32 -lgdi32"
         HAVE_OPENGL="True"
      else
         AC_MSG_ERROR([Missing Windows OpenGL library])
      fi
      ;;
   xMesaGL)
      if test "x$have_MesaGL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL"
         HAVE_OPENGL="True"
      elif test "x$have_MesaGL_pthread" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lMesaGLU -lMesaGL -lpthread"
         HAVE_OPENGL="True"
      else
         AC_MSG_ERROR([Missing Mesa library])
      fi
      ;;
   xno)
      ;;
   *)
      AC_MSG_ERROR([Unknown value for "--with-GL" option. Should be either auto, GL32, GL, MesaGL, no])
      ;;
   esac

   AC_SUBST(GL_LIBS)
   AC_SUBST(GL_CFLAGS)
   AC_SUBST(HAVE_OPENGL)
   AM_TO_GPR($GL_LIBS, GL_LIBS_GPR)
   AM_TO_GPR($GL_CFLAGS, GL_CFLAGS_GPR)
])

#############################################################
#
#  A small macro to create a file after preprocessing it using gnatprep
#
#############################################################


AC_DEFUN(AM_GNATPREP,
[
   echo "creating $1"
   $GNATPREP $1.in $1 config.defs
])


#############################################################
#
#  Macro to add for using GNU gettext
#
#############################################################


AC_DEFUN(AM_WITH_NLS,
  [AC_MSG_CHECKING([whether NLS is requested])
    dnl Default is enabled NLS
    AC_ARG_ENABLE(nls,
      [  --disable-nls           do not use Native Language Support],
      USE_NLS=$enableval, USE_NLS=yes)
    AC_MSG_RESULT($USE_NLS)
    AC_SUBST(USE_NLS)

    GETTEXT_INTL="False"
    HAVE_GETTEXT="False"

    dnl If we use NLS figure out what method
    if test "$USE_NLS" = "yes"; then
      AC_DEFINE(ENABLE_NLS)

      dnl Figure out whether gettext is available in the C or intl library.
      nls_cv_header_intl=
      nls_cv_header_libgt=

      AC_CACHE_CHECK([for gettext in libc], gt_cv_func_gettext_libc,
       [AC_TRY_LINK([extern int gettext(char*);], [return (int) gettext ("")],
	gt_cv_func_gettext_libc=yes, gt_cv_func_gettext_libc=no)])

      if test "$gt_cv_func_gettext_libc" != "yes"; then
        AC_CHECK_LIB(intl, bindtextdomain,
         [AC_CACHE_CHECK([for gettext in libintl],
           gt_cv_func_gettext_libintl,
           [AC_CHECK_LIB(intl, gettext,
              gt_cv_func_gettext_libintl=yes,
              gt_cv_func_gettext_libintl=no)],
	    gt_cv_func_gettext_libintl=no)])

	  if test "$gt_cv_func_gettext_libintl" = "yes"; then
            GETTEXT_INTL="True"
          fi
      fi

       if test "$gt_cv_func_gettext_libc" = "yes" \
         || test "$gt_cv_func_gettext_libintl" = "yes"; then
            HAVE_GETTEXT="True"
       fi
    fi

    dnl Make all variables we use known to autoconf.
    AC_SUBST(GETTEXT_INTL)
    AC_SUBST(HAVE_GETTEXT)
  ])

AC_DEFUN(AM_GNU_GETTEXT,
  [AM_WITH_NLS
  ])
