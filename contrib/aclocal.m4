#############################################################
#
#  Adding some OS specific flags to the compiler
#
############################################################

AC_DEFUN(AM_ADD_OS_SPECIFIC_FLAGS,
[

   case $build_os in
   aix*)
      OS_SPECIFIC_LINK_OPTIONS=-Wl,-bexpall,-berok
      ;;
   esac

  AC_SUBST(OS_SPECIFIC_LINK_OPTIONS)

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
dnl Get the cflags and libraries from the gtk-config script
dnl
AC_ARG_WITH(gtk-prefix,[  --with-gtk-prefix=PFX   Prefix where GTK is installed (optional)],
            gtk_config_prefix="$withval", gtk_config_prefix="")
AC_ARG_WITH(gtk-exec-prefix,[  --with-gtk-exec-prefix=PFX Exec prefix where GTK is installed (optional)],
            gtk_config_exec_prefix="$withval", gtk_config_exec_prefix="")
AC_ARG_ENABLE(gtktest, [  --disable-gtktest       Do not try to compile and run a test GTK program],
		    , enable_gtktest=yes)

  if test x$gtk_config_exec_prefix != x ; then
     gtk_config_args="$gtk_config_args --exec-prefix=$gtk_config_exec_prefix"
     if test x${GTK_CONFIG+set} != xset ; then
        GTK_CONFIG=$gtk_config_exec_prefix/bin/gtk-config
     fi
  fi
  if test x$gtk_config_prefix != x ; then
     gtk_config_args="$gtk_config_args --prefix=$gtk_config_prefix"
     if test x${GTK_CONFIG+set} != xset ; then
        GTK_CONFIG=$gtk_config_prefix/bin/gtk-config
     fi
  fi

  AC_PATH_PROG(GTK_CONFIG, gtk-config, no)
  min_gtk_version=ifelse([$1], ,1.1.15,$1)
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  if test "$GTK_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_PREFIX=`$GTK_CONFIG $gtk_config_args --prefix`
    GTK_CFLAGS=`$GTK_CONFIG $gtk_config_args --cflags`
    GTK_LIBS=`$GTK_CONFIG $gtk_config_args --libs`
    gtk_config_major_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$GTK_CONFIG $gtk_config_args --version | \
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
      printf("*** If gtk-config was wrong, set the environment variable GTK_CONFIG\n");
      printf("*** to point to the correct copy of gtk-config, and remove the file config.cache\n");
      printf("*** before re-running configure\n");
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
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the gtk-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GTK+, but you can also set the GTK_CONFIG environment to point to the\n");
        printf("*** correct copy of gtk-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
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
       echo "*** The gtk-config script installed by GTK could not be found"
       echo "*** If GTK was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GTK_CONFIG environment variable to the"
       echo "*** full path to gtk-config."
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
          echo "*** or that you have moved GTK since it was installed. In the latter case, you"
          echo "*** may want to edit the gtk-config script: $GTK_CONFIG" ])
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
   LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lGL"
   AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_GL=yes, have_GL=no)
   AC_MSG_RESULT($have_GL)
   
   AC_MSG_CHECKING([for Mesa])
   LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lMesaGL"
   AC_TRY_LINK( ,[ char glBegin(); glBegin(); ], have_MesaGL=yes, have_MesaGL=no)
   AC_MSG_RESULT($have_MesaGL)
    
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
         GL_LIBS="$GL_LDOPTS -lGLU -lGL"
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

   if test "x$HAVE_OPENGL"="xFalse"; then
      AC_MSG_RESULT([*** OpenGL support will not be integrated into GtkAda ***])
   fi


   AC_SUBST(GL_LIBS)
   AC_SUBST(GL_CFLAGS)
   AC_SUBST(HAVE_OPENGL)

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
