#############################################################
#
#  Adding some OS specific flags and parameters
#
############################################################

AC_DEFUN(AM_ADD_OS_SPECIFIC_FLAGS,
[

   SO_EXT=.so
   SO_OPTS=-Wl,-soname,
   BUILD_SHARED=yes
   FPIC=-fPIC
   TARGET_LFLAGS=

   case $build_os in
   aix*)
      BUILD_SHARED=no
      FPIC=
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-bexpall,-berok
      TARGET_LFLAGS=-Wl,-bbigtoc
      ;;
   hp*)
      SO_EXT=.sl
      SO_OPTS=-Wl,+h,
      BUILD_SHARED=no
      FPIC=
      ;;
   *sysv4uw* | *sysv5uw*)
      SO_OPTS=-Wl,-h,
      BUILD_SHARED=no
      FPIC=
      ;;
   *solaris*)
      SO_OPTS=-Wl,-h,
      ;;
   *irix*)
      FPIC=
      ;;
   *osf*)
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-expect_unresolved,\*
      ;;
   *mingw*)
      BUILD_SHARED=no
      FPIC=
      ;;
   esac

  AC_SUBST(OS_SPECIFIC_LINK_OPTIONS)
  AC_SUBST(BUILD_SHARED)
  AC_SUBST(SO_EXT)
  AC_SUBST(SO_OPTS)
  AC_SUBST(FPIC)
  AC_SUBST(TARGET_LFLAGS)

]
)

#############################################################
#
#  Checking for Gnat
#
#############################################################

conftest_ok="conftest.ok"

AC_DEFUN(AM_PATH_GNAT,
[   
   AC_PATH_PROG(GNATMAKE, gnatmake, no)

   if test x$GNATMAKE = xno ; then
      AC_MSG_ERROR(I could not find gnatmake. See the file 'INSTALL' for more details.)
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

   $GNATMAKE conftest > /dev/null 2>&1

   if ( test ! -x conftest ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GNATMAKE test failed at compile time! Check your configuration.)
   fi

   ./conftest

   if ( test ! -f $conftest_ok ) then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR($GNATMAKE test failed at run time! Check your configuration.)
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
# Extracted from the aclocal.m4 file of gimp-1.0.0
#
#############################################################

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTK,
[dnl 
dnl Get the cflags and libraries from the pkg-config script
dnl
  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  min_gtk_version=ifelse([$1], ,1.3.0,$1)
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  GTK="gtk+-2.0"
  if test "$PKG_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_PREFIX=`$PKG_CONFIG $GTK --variable=prefix`
    GTK_CFLAGS=`$PKG_CONFIG $GTK --cflags`
    GTK_LIBS=`$PKG_CONFIG $GTK --libs`
    gtk_config_major_version=`$PKG_CONFIG $GTK --modversion | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$PKG_CONFIG $GTK --modversion | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$PKG_CONFIG $GTK --modversion | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gtktest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GTK_CFLAGS"
      LIBS="$LIBS $GTK_LIBS"
dnl
dnl Now check if the installed GTK is sufficiently new. (Also sanity
dnl checks the results of gtk-config to some extent
dnl
      rm -f conf.gtktest
      AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtktest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtk_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

  if ((gtk_major_version != $gtk_config_major_version) ||
      (gtk_minor_version != $gtk_config_minor_version) ||
      (gtk_micro_version != $gtk_config_micro_version))
    {
      printf("\n*** 'gtk-config --version' returned %d.%d.%d, but GTK+ (%d.%d.%d)\n", 
             $gtk_config_major_version, $gtk_config_minor_version, $gtk_config_micro_version,
             gtk_major_version, gtk_minor_version, gtk_micro_version);
      printf ("*** was found! If gtk-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK+. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
    } 
  else
    {
      if ((gtk_major_version > major) ||
        ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
        ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK+ (%d.%d.%d) was found.\n",
               gtk_major_version, gtk_minor_version, gtk_micro_version);
        printf("*** You need a version of GTK+ newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK+ is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
      }
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GTK_CONFIG" = "no" ; then
       echo "*** The pkg-config script could not be found"
       echo "*** If GTK was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path."
     else
       if test -f conf.gtktest ; then
        :
       else
          echo "*** Could not run GTK test program, checking why..."
          CFLAGS="$CFLAGS $GTK_CFLAGS"
          LIBS="$LIBS $GTK_LIBS"
          AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
],      [ return ((gtk_major_version) || (gtk_minor_version) || (gtk_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK or finding the wrong"
          echo "*** version of GTK. If it is not finding GTK, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"
          echo "***"
          echo "*** If you have a RedHat 5.0 system, you should remove the GTK package that"
          echo "*** came with the system with the command"
          echo "***"
          echo "***    rpm --erase --nodeps gtk gtk-devel" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK was incorrectly installed"
          echo "*** or that you have moved GTK since it was installed." ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_PREFIX)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  rm -f conf.gtktest
])

#############################################################
#
#  Checking for merge
#
#############################################################

file1="conftest1"
file2="conftest2"
file3="conftest3"
result_file="conftest.res"

AC_DEFUN(AM_PATH_MERGE,
[   
   ### Let's try to find a merge somewhere...

   MERGE_AVAIL=False
   AC_PATH_PROG(MERGE, merge, true)

   ### ... and see how it works
   if test x$MERGE != xtrue ; then
      cat > $file1 <<EOF
This is the first line
This is the second line
Last line
EOF

      cat > $file2 <<EOF
This is the first line
This is the second line
EOF

      cat > $file3 <<EOF
This is the first line
An inserted line
This is the second line
EOF

      cat > $result_file <<EOF
This is the first line
An inserted line
This is the second line
Last line
EOF

      AC_MSG_CHECKING(whether merge works correctly...)

      if $MERGE $file1 $file2 $file3 >/dev/null 2>&1 ; then
	 if cmp -s $file1 $result_file; then
	    MERGE_AVAIL=True
	    AC_MSG_RESULT(yes)
	 else
	    AC_MSG_RESULT(no)
	 fi
      else
	 AC_MSG_RESULT(no)
      fi
   fi

   AC_SUBST(MERGE_AVAIL)
   AC_SUBST(MERGE)
   rm -f $file1 $file2 $file3 $result_file
])

#############################################################
#
#  Checking for diff and patch
#
#############################################################

file_to_patch="conftest"
patch_file="conftest.dif"
result_file="conftest.res"

AC_DEFUN(AM_PATH_DIFF_AND_PATCH,
[   
   ### Let's try to find a diff somewhere...

   AC_PATH_PROG(DIFF, diff, true)

   ### ... and see how it works

   if test x$DIFF = xtrue ; then

      AC_MSG_WARN([-----------------------------------------------------------])
      AC_MSG_WARN([--  The diff utility has not been found! However, gate])
      AC_MSG_WARN([--  needs it to handle merges, so merges have been disabled])
      AC_MSG_WARN([--  (replaced by true)])
      AC_MSG_WARN([-----------------------------------------------------------])

   else

      cat > $file_to_patch <<EOF
This is the first line
This is the second line
EOF
      
      cat > $result_file <<EOF
This is the first line
An inserted line
This is the second line
EOF

      AC_MSG_CHECKING(for the correct diff option)

      if $DIFF -u $file_to_patch $file_to_patch > /dev/null 2>&1 ; then
         AC_MSG_RESULT(-u)
         DIFF="$DIFF -u"

      elif $DIFF -c $file_to_patch $file_to_patch > /dev/null 2>&1 ; then
         AC_MSG_RESULT(-c)
         DIFF="$DIFF -c"
      
      else
         AC_MSG_RESULT(** none **);
	 DIFF=true
	 AC_MSG_WARN([--------------------------------------------------------])
	 AC_MSG_WARN([--  diff does not accept the -u nor -c option, which is])
	 AC_MSG_WARN([--  needed by gate to perform merges, so merges have])
	 AC_MSG_WARN([--  been disabled])
	 AC_MSG_WARN([--------------------------------------------------------])
      fi

   fi

   ### if we've found a working diff, then DIFF should not be
   ### equal to true. So we can start looking for patch...

   if test "x$DIFF" != "xtrue" ; then

      AC_PATH_PROG(PATCH, patch, true)
 
      if test x$PATCH = xtrue ; then

	 AC_MSG_WARN([-----------------------------------------------------------])
	 AC_MSG_WARN([--  The patch utility has not been found! However, gate])
	 AC_MSG_WARN([--  needs it to handle merges, so merges have been disabled])
	 AC_MSG_WARN([--  (replaced by true)])
	 AC_MSG_WARN([-----------------------------------------------------------])

      else

	 AC_MSG_CHECKING(for the correct patch option)

	 cat > $patch_file <<EOF
--- $file_to_patch.ori Mon Mar  1 21:21:34 1999
+++ $file_to_patch     Mon Mar  1 21:22:04 1999
@@ -1,2 +1,3 @@
 This is the first line
+An inserted line
 This is the second line
EOF

	 if $PATCH -f < $patch_file  >/dev/null 2>&1 ; then
	    AC_MSG_RESULT(-f)
	    PATCH="$PATCH -f"

	 else
	    AC_MSG_RESULT(** none  **)
	    PATCH=true
	    AC_MSG_WARN([--------------------------------------------------------])
	    AC_MSG_WARN([--  A patch utility has been found. However, it does not])
	    AC_MSG_WARN([--  seem to work with the -f option. As gate needs])
	    AC_MSG_WARN([--  patch to perform merges, they have been disabled.])
	    AC_MSG_WARN([--------------------------------------------------------])

	 fi

      fi

   fi

   ###  End of tests, cleanup and string replacements...

   rm -f $file_to_patch
   rm -f $patch_file
   rm -f $result_file

   AC_SUBST(DIFF)
   AC_SUBST(PATCH)

])


#############################################################
#
#  Checking for openGL
#
#############################################################


AC_DEFUN(AM_CHECK_OPENGL,
[   

   # checking for OpenGL libraries
   AC_ARG_WITH(GL,         [  --with-GL=value         Which OpenGL library to compile GtkAda with (auto,GL,MesaGL,no)])
   AC_ARG_WITH(GL-prefix,  [  --with-GL-prefix=DIR    Prefix where GL/MesaGL is installed])
   

   if test "x$with_GL_prefix" = "x" ; then
      GL_LDOPTS=""
      GL_CFLAGS=""
   else
      GL_LDOPTS="-L$with_GL_prefix/lib"
      GL_CFLAGS="-I$with_GL_prefix/include"
   fi
   

   saved_LIBS="$LIBS"
 
   AC_MSG_CHECKING([for OpenGL])
   LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lGLU -lGL"
   AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_GL=yes, have_GL=no)
   AC_MSG_RESULT($have_GL)
   
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

   LIBS="$saved_LIBS"
   HAVE_OPENGL="False"

   case "x$with_GL" in
   x|xauto)
      if test "x$have_GL" = "xyes"; then
         GL_LIBS="$GL_LDOPTS -lGLU -lGL"
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
      AC_MSG_ERROR([Unknown value for "--with-GL" option. Should be either auto, GL, MesaGL, no])
      ;;
   esac

   if test "x$HAVE_OPENGL" = "xFalse"; then
      AC_MSG_RESULT([*** OpenGL support will not be integrated into GtkAda ***])
   fi


   AC_SUBST(GL_LIBS)
   AC_SUBST(GL_CFLAGS)
   AC_SUBST(HAVE_OPENGL)

])

#############################################################
#
#  Checking for gnome2
#
#############################################################

AC_DEFUN(AM_CHECK_GNOME,
[   
  if test "$PKG_CONFIG" = "no" ; then
    HAVE_GNOME="False"
    GNOME_CFLAGS=""
    GNOME_LIBS=""
    GNOME_STATIC_LIBS=""
  else
    GNOMEUI="libgnomeui-2.0"
    GNOME_PREFIX=`$PKG_CONFIG $GNOMEUI --variable=prefix`
    if test "x$GNOME_PREFIX" = "x"; then
      HAVE_GNOME="False"
    else
      HAVE_GNOME="True"
      GNOME_CFLAGS=`$PKG_CONFIG $GNOMEUI --cflags gnomeui`
      GNOME_LIBS=`$PKG_CONFIG $GNOMEUI --libs`
      GNOME_STATIC_LIBS="$GNOME_PREFIX/lib/libgnomeui-2.a $GNOME_PREFIX/lib/libgnome-2.a $GNOME_PREFIX/lib/libart_lgpl.a $GNOME_PREFIX/lib/lib/libpopt.a $GNOME_PREFIX/lib/libbonoboui-2.a" 
    fi
  fi

  AC_SUBST(GNOME_CFLAGS)
  AC_SUBST(GNOME_LIBS)
  AC_SUBST(GNOME_STATIC_LIBS)
  AC_SUBST(HAVE_GNOME)
])

#############################################################
#
#  Checking for libglade
#
#############################################################

AC_DEFUN(AM_CHECK_LIBGLADE,
[   
  AC_PATH_PROG(LIBGLADE_CONFIG, libglade-config, no)

  if test "$LIBGLADE_CONFIG" = "no" ; then
    HAVE_LIBGLADE="False"
    LIBGLADE_CFLAGS=""
    LIBGLADE_LIBS=""
    LIBGLADE_STATIC_LIBS=""
  else
    HAVE_LIBGLADE="True"
    LIBGLADE_PREFIX=`$LIBGLADE_CONFIG --prefix`
    LIBGLADE_CFLAGS=`$LIBGLADE_CONFIG --cflags`
    LIBGLADE_LIBS="-L$LIBGLADE_PREFIX/lib -lglade -lxml"
    LIBGLADE_STATIC_LIBS="$LIBGLADE_PREFIX/lib/libglade.a $LIBGLADE_PREFIX/lib/libxml.a"
  fi

  AC_SUBST(LIBGLADE_CFLAGS)
  AC_SUBST(LIBGLADE_LIBS)
  AC_SUBST(LIBGLADE_STATIC_LIBS)
  AC_SUBST(HAVE_LIBGLADE)
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

      AC_CHECK_HEADER(libintl.h,
	[AC_CACHE_CHECK([for gettext in libc], gt_cv_func_gettext_libc,
	  [AC_TRY_LINK([#include <libintl.h>], [return (int) gettext ("")],
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
	])
    fi

    dnl Make all variables we use known to autoconf.
    AC_SUBST(GETTEXT_INTL)
    AC_SUBST(HAVE_GETTEXT)
  ])

AC_DEFUN(AM_GNU_GETTEXT,
  [AM_WITH_NLS
  ])
