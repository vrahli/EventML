#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT(EventML, 0.2, rahli@cs.cornell.edu)

# Checks for programs.

AC_PROG_MAKE_SET

AC_CHECK_PROG(HAVE_MLLEX, mllex, yes, no)
if test x"$HAVE_MLLEX" = "xno" ; then
   AC_MSG_ERROR([ mllex is required to compile EventML. ])
fi

AC_CHECK_PROG(HAVE_MLYACC, mlyacc, yes, no)
if test x"$HAVE_MLYACC" = "xno" ; then
   AC_MSG_ERROR([ mlyacc is required to compile EventML. ])
fi

AC_CHECK_PROG(HAVE_MLTON, mlton, yes, no)
if test x"$HAVE_MLTON" = "xno" ; then
   AC_MSG_ERROR([ mlton is required to compile EventML. ])
fi

# AC_CHECK_PROG(HAVE_LATEX, latex, yes, no)
# if test x"$HAVE_LATEX" = "xno" ; then
#    AC_MSG_ERROR([ latex is required to generate EventML's documentation. ])
# fi

# AC_CHECK_PROG(HAVE_RUBBER, rubber, yes, no)
# if test x"$HAVE_RUBBER" = "xno" ; then
#    AC_MSG_ERROR([ rubber is required to generate the EventML's documentation. ])
# fi

# Checks for libraries.

AC_CHECK_LIB(gmp, __gmpz_init, ,
 [AC_MSG_ERROR([ Cannot find the GMP library, please install libgmp. ])])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
