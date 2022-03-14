#############################################################
#
#  Adding some OS specific flags and parameters
#
############################################################

AC_DEFUN([AM_ADD_OS_SPECIFIC_FLAGS],
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

   AC_ARG_ENABLE(static_pic,
     [AC_HELP_STRING(
        [--disable-static-pic],
        [Disable building of static PIC libraries.])
AC_HELP_STRING(
        [--enable-static-pic],
        [Build static PIC libraries (default).])],
     [BUILD_STATIC_PIC=$enableval
      if test $enableval = yes; then
         DEFAULT_LIBRARY_TYPE=static-pic
      fi],
     [BUILD_STATIC_PIC=yes])

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
    if test x$BUILD_STATIC_PIC = xno ; then
       DEFAULT_LIBRARY_TYPE=static
    fi
  fi

  AC_SUBST(DEFAULT_LIBRARY_TYPE)
  AC_SUBST(OS_SPECIFIC_LINK_OPTIONS)
  AC_SUBST(BUILD_STATIC)
  AC_SUBST(BUILD_STATIC_PIC)
  AC_SUBST(BUILD_SHARED)
  AC_SUBST(SO_EXT)
  AC_SUBST(SO_OPTS)
  AC_SUBST(TARGET_LFLAGS)
  AC_SUBST(NEED_OBJECTIVE_C)

]
)
