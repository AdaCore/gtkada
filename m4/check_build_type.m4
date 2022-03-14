#############################################################
#  Checking for build type
#############################################################

AC_DEFUN([CHECK_BUILD_TYPE],
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
