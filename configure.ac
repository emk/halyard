# Process this file with autoconf to produce a configure script.
AC_INIT([Common/TVersion.h])
AM_INIT_AUTOMAKE(halyard, 0.0)

# Turn on debugging, and fail if we encounter any warnings.
CFLAGS="$CFLAGS -g -Wall -Werror"
CXXFLAGS="$CXXFLAGS -g -Wall -Werror"

# Set up some compiler options for Unix.
#AC_DEFINE(TEST_TYPOGRAPHY)
#AC_DEFINE(HAVE_EYE_OF_GNOME)

# Libraries we need to build Common:
#              In tree  MacPorts  Ubuntu 7.10
#   boost      1.31.0   1.34.1    "
#   freetype2  2.3.4    2.3.5     "
#   libxml2    2.4.30   2.6.31    2.6.30
#   plt        360      371       360 (do we have dev headers here?)
#   sqlite     3.0.3    3.5.7     3.4.2
#
# On the Mac, run:
#   sudo port install boost freetype libxml2 mzscheme sqlite3
# If you get errors on Leopard, Google for the error messages.

# Figure out how to link against FreeType 2.
#CFLAGS="$CFLAGS `freetype-config --cflags`"
#CXXFLAGS="$CXXFLAGS `freetype-config --cflags`"
#FREETYPE_LDADD="`freetype-config --libs`"
#AC_SUBST(FREETYPE_LDADD)


#==========================================================================
# Checks for Programs
#==========================================================================

AC_PROG_RANLIB
AC_PROG_CXX
AC_PROG_CC

#==========================================================================
# Checks for Libraries
#==========================================================================


#==========================================================================
# Checks for Header Files
#==========================================================================

#==========================================================================
# Checks for Typedefs, Structures, and Compiler Characteristics
#==========================================================================

AC_C_CONST
AC_C_INLINE


#==========================================================================
# Checks for library functions.
#==========================================================================

AC_CHECK_FUNCS([strstr])


#==========================================================================
# Output
#==========================================================================

# Output our results.
AC_OUTPUT([Makefile libs/Makefile libs/sha1/Makefile Common/Makefile])
