#####
#
# SYNOPSIS
#
#   AX_LIB_MZSCHEME(GC, [MINIMUM-VERSION])
#
# DESCRIPTION
#
#   Test for a particular version of the mszcheme library
#
#   This macro takes one required argument, specifying which garbage
#   collector to use.  Possible values are "cgc" (for the old conservative
#   garbage collector) or "3m" (for the new precise garbage collector).
#   The second argument is optional, and specifies the minimum acceptable
#   version of PLT Scheme.  It defaults to "360".
#
#   If no intallation prefix is given, we search the usual directories.
#
#   This macro calls:
#
#     AC_SUBST(MZSCHEME_CFLAGS)
#     AC_SUBST(MZSCHEME_LDFLAGS)
#     AC_DEFINE([HAVE_MZSCHEME], ...)
#
#   And sets:
#
#     HAVE_MZSCHEME
#
#   This macro is adapted from Mateusz Loskot <mateusz@loskot.net>'s
#   ax_lib_sqlite3.m4 macro.
#
# LAST MODIFICATION
#
#   2008-04-09
#
# COPYLEFT
#
#   Copyright (c) 2008 Mateusz Loskot <mateusz@loskot.net>
#     Original ax_lib_sqlite3.m4 macro.
#   Copyright (c) 2008 Trustees of Dartmouth College
#     Contact Eric Kidd <eric.m.kidd@dartmouth.edu>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

AC_DEFUN([AX_LIB_MZSCHEME],
[
    AC_ARG_WITH([mzscheme],
        AC_HELP_STRING(
            [--with-mzscheme=@<:@DIR@:>@],
            [use mzscheme library @<:@default=yes@:>@, optionally specifying where to find it]
        ),
        [
        if test "$withval" = "no"; then
            WANT_MZSCHEME="no"
        elif test "$withval" = "yes"; then
            WANT_MZSCHEME="yes"
            ac_mzscheme_path=""
        else
            WANT_MZSCHEME="yes"
            ac_mzscheme_path="$withval"
        fi
        ],
        [WANT_MZSCHEME="yes"]
    )

    MZSCHEME_CFLAGS=""
    MZSCHEME_LDFLAGS=""
    MZSCHEME_VERSION=""

    if test "x$WANT_MZSCHEME" = "xyes"; then

        ac_mzscheme_header="schvers.h"

        mzscheme_gc_req=ifelse([$1], [], [cgc], [$1])
        mzscheme_version_req=ifelse([$2], [], [360], [$2])

        AC_MSG_CHECKING(for mzscheme library ($mzscheme_gc_req) >= $mzscheme_version_req)

        if test $mzscheme_gc_req = cgc; then
            ac_mzscheme_want_precise_gc=0
        elif test $mzscheme_gc_req = 3m; then
            ac_mzscheme_want_precise_gc=1
        else 
            AC_MSG_ERROR(Unknown GC type \"$mzscheme_gc_req\")
        fi

        if test x"$ac_mzscheme_path" = x""; then
            for ac_mzscheme_path_tmp in /usr /usr/local /opt /opt/local ; do
                if test -f "$ac_mzscheme_path_tmp/include/$ac_mzscheme_header" \
                    && test -r "$ac_mzscheme_path_tmp/include/$ac_mzscheme_header"; then
                    ac_mzscheme_path=$ac_mzscheme_path_tmp
                    break;
                fi
            done
        fi
        ac_mzscheme_cppflags="-I$ac_mzscheme_path/include"

        if test -d "$ac_mzscheme_path/lib/PLT_MzScheme.framework"; then
            ac_mzscheme_libdir="-F$ac_mzscheme_path/lib"
            # Link against the framework and hope it defaults to the right GC.
            ac_mzscheme_libs="-framework PLT_MzScheme"
        elif test -d "$ac_mzscheme_path/Library/Frameworks/PLT_MzScheme.framework"; then
            ac_mzscheme_libdir="-F$ac_mzscheme_path/Library/Frameworks"
            ac_mzscheme_libs="-framework PLT_MzScheme"
        else
            ac_mzscheme_libdir="-L$ac_mzscheme_path/lib"
            if test $ac_mzscheme_want_precise_gc = 1; then
                ac_mzscheme_libs="-lmzscheme -lmzgc"
            else
                ac_mzscheme_libs="-lmzscheme3m"
            fi
        fi
        ac_mzscheme_ldflags="$ac_mzscheme_libdir $ac_mzscheme_libs"

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS $ac_mzscheme_cppflags"
        saved_LDFLAGS="$LDFLAGS"
        LDFLAGS="$LDFLAGS $ac_mzscheme_ldflags"
        
        AC_LANG_PUSH([C])
        AC_LINK_IFELSE(
        [
            AC_LANG_PROGRAM([[@%:@include <scheme.h>]],
            [[
/* Make sure we have the right GC. */
#if (MZSCHEME_VERSION_MAJOR >= $mzscheme_version_req)
#   if ($ac_mzscheme_want_precise_gc)
        MZ_GC_DECL_REG(1);
        scheme_set_stack_base(&__gc_var_stack__, 1);
#   elif defined(MZ_PRECISE_GC)
#error "Want to build with cgc, found 3m"
#   else
        scheme_set_stack_base(NULL, 0);
#   endif
#else
#   error "mzscheme version is too old"
#endif
            ]])
        ],
        [
            AC_MSG_RESULT([$ac_mzscheme_ldflags])
            HAVE_MZSCHEME=yes
        ],
        [
            AC_MSG_RESULT([not found])
            HAVE_MZSCHEME=no
        ])
        AC_LANG_POP([C])

        CPPFLAGS="$saved_CPPFLAGS"
        LDFLAGS="$saved_LDFLAGS"

        if test "$HAVE_MZSCHEME" = "yes"; then
            MZSCHEME_CFLAGS="$ac_mzscheme_cppflags"
            MZSCHEME_LDFLAGS="$ac_mzscheme_ldflags"

            AC_SUBST(MZSCHEME_CFLAGS)
            AC_SUBST(MZSCHEME_LDFLAGS)
            AC_DEFINE([HAVE_MZSCHEME], [], [Have the mzscheme library])
        fi
    fi
])
