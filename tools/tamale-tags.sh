#!/bin/bash
#
# Generates Emacs TAGS files for quickly finding definitions of things.
#
# To use this script on Windows, you need to install Cygwin, add the Emacs
# 'bin' directory to your path, and cd to the top-level folder of the
# Tamale program you wish to generate tags for.

REGEX='(\(def[^ \t]*\|card\|group\|sequence\|element\)[ \t]+(?\([^ \t)]+\)/\2'
etags --language=none \
  --ignore-case-regex="/$REGEX/" Scripts/start.ss \
  --ignore-case-regex="/  $REGEX/" \
  `find Runtime/5L -name \*.ss` \
  `find Scripts \! \( -name start.ss -prune \) -a -name \*.ss`
