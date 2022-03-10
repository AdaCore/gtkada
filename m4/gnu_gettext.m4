#############################################################
#
#  Macro to add for using GNU gettext
#
#############################################################


AC_DEFUN([AM_WITH_NLS],
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

AC_DEFUN([AM_GNU_GETTEXT],
  [AM_WITH_NLS
  ])
