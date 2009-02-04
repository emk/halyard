# @BEGIN_LICENSE
#
# Halyard - Multimedia authoring and playback system
# Copyright 1993-2009 Trustees of Dartmouth College
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# @END_LICENSE

# Stage 2 of the bootstrap loader for the Halyard Ruby environment.  At
# this point, we expect to have $HALYARD_SCRIPT and $HALYARD_RUNTIME
# variables.

# Find directories with Ruby-related files.
$HALYARD_RUBY_DIRS =
  Dir["#{$HALYARD_SCRIPT}/ruby",
      "#{$HALYARD_SCRIPT}/collects/**/_halyard/ruby",
      "#{$HALYARD_RUNTIME}/ruby"]

# Add each directory of Ruby-related files to our load path.
for dir in $HALYARD_RUBY_DIRS
  $LOAD_PATH << "#{dir}/lib"
  # TODO: We might want to set up  "#{dir}/gems", too.
end
