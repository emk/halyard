# LIBRARY_MISSING_IF(test, library_name)
AC_DEFUN([LIBRARY_MISSING_IF],
[
    if $1; then
       AC_MSG_ERROR([Please install $2 and re-run configure.  Thank you!]) 
    fi
])
