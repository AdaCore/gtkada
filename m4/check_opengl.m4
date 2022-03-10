#############################################################
#
#  Checking for openGL
#
#############################################################


AC_DEFUN([AM_CHECK_OPENGL],
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
     LIBS="$saved_LIBS $GTK_LIBS $GL_LDOPTS -lGLU -lGL -lm -lX11"
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
         GL_LIBS="$GL_LDOPTS -lGLU -lGL -lm -lX11"
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
         GL_LIBS="$GL_LDOPTS -lGLU -lGL -lm -lX11"
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
