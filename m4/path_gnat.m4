#############################################################
#
#  Checking for Gnat
#
#############################################################

conftest_ok="conftest.ok"

AC_DEFUN([AM_PATH_GNAT],
[
   AC_PATH_PROG(GPRBUILD, gprbuild, no)
   AC_PATH_PROG(GPRINSTALL, gprinstall, no)

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
   rm -f auto.cgpr b__conftest.*

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
