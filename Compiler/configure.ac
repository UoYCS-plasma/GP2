#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([gp2], [1.0])
#LT_PREREQ([2.2])
#LT_INIT
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_PROG_CC
AM_PROG_LEX
AC_PROG_YACC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks for libraries. To use the result, add LIBS = @LIBS@ to the appropriate 
# Makefile.in and $(LIBS) to the C compiler call.
# AC_SEARCH_LIBS([g_hash_table_new_full], [glib])
PKG_CHECK_MODULES([GLIB], [glib-2.0])

# Checks for header files.
AC_CHECK_HEADERS([glib.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([getcwd mkdir strdup])

# Indicates the name of a subdirectory in which the alocal utility can find
# all project-local M4 macro files. Might not be necessary for GP2.
# AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile
                 inc/Makefile
                 lib/Makefile
                 src/Makefile])

AC_OUTPUT

echo "
   ($PACKAGE_NAME) version $PACKAGE_VERSION
   Prefix........: $prefix
   Debug Build...: $debug
   C Compiler....: $CC $CFLAGS
   Linker........: $LD $LDFLAGS $LIBS
   Flex..........: $LEX
   Bison.........: $YACC
"
