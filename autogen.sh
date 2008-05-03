#!/bin/sh
# Run this to generate all the initial makefiles, etc.
#
# This script is adapted from the gnome-common module, and is copyright by
# various Gnome contributors.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

# If your system uses older versions of these tools, please feel free to
# edit these version numbers, test the result carefully, and send us a
# patch.  Thank you!
REQUIRED_AUTOMAKE_VERSION=1.10
REQUIRED_AUTOCONF_VERSION=2.61
REQUIRED_PKG_CONFIG_VERSION=0.22

# Look for our own m4 macro directory.
ACLOCAL_FLAGS="-I $srcdir/m4 $ACLOCAL_FLAGS"

# Check for at least one m4 macro from each directory.
REQUIRED_M4MACROS="ax_lib_sqlite3.m4 wxwin.m4"

# We don't actually want to run ./configure, since the Gnome scripts try to
# pass --enable-maintainer-mode, which is obsolete.
NOCONFIGURE=1

PKG_NAME="Halyard"

(test -f $srcdir/configure.ac \
&& test -f $srcdir/Halyard.sln
) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

. $srcdir/tools/gnome-autogen.sh

echo
echo "You may now run $srcdir/configure with any arguments you need."