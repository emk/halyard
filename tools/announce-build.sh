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

#!/bin/sh

# Announce a build of the engine on the mailing list, IRC, freshmeat, and
# anywhere else relevant.

# In order to use this successfully, you need to have git configured to
# be able to use imap-send to push an email into your drafts folder.  You
# will then be able to edit the email as you see fit, and send it out.

usage () { 
cat <<EOF >&2
usage: announce-build.sh [--dry-run] [-v|--verbose] vX.Y.Z [DESTINATION ...]    

OPTIONS
    --dry-run
        Print announcements, don't send them.

    -v, --verbose
        Print information about what we're doing.

    vX.Y.Z
        Version to announce.

    DESTINATION ...
        One or more of 'email', 'freshmeat', or 'irc'.  If a
        destination is specified, only send announcements to the
        specified destinations.
EOF

    exit 1 
}

dry_run= version= vernum= flags= email= freshmeat= irc=

while [ $# != 0 ]; do
    case $1 in
        --dry-run) dry_run=true; flags+=" --dry-run" ;;
        -v | --verbose) verbose=true; flags+=" --verbose" ;;
        v*) version=$1; vernum=$(expr $version : 'v\(.*\)'); shift; break ;;
    esac
    shift
done

if [ -z "$version" ]; then usage; fi

while [ $# != 0 ]; do
    case $1 in
        email) email=true ;;
        freshmeat) freshmeat=true ;;
        irc) irc=true ;;
        *) usage ;;
    esac
    shift
done

if [ -z "$email$freshmeat$irc" ]; then
    email=true; freshmeat=true; irc=true
fi

send_email () {
    msgfile="announce-halyard-${version}.mbox"
    email=$(git config --get user.email)
    date=$(date)
    
    cat <<EOF >$msgfile
From $email $date
From: $(git config --get user.name) <$email>
Subject: ANNOUNCE: Halyard ${vernum}
To: halyard-dev@iml.dartmouth.edu
Date: $date

We are pleased to announce Halyard version ${vernum}.

Halyard is a scriptable multimedia engine, with support for video,
audio, graphics, and user interaction. It currently runs on Windows,
and has a preliminary Macintosh port. Halyard is based on PLT Scheme,
wxWidgets and the Quake 2 Engine. It includes a rudimentary IDE based
on Scintilla.

You may download Halyard at:

http://iml.dartmouth.edu/halyard/dist/halyard-${vernum}/

$(git log --pretty="format:%s%n%n%b" "${version}^..${version}")
EOF

    if [ "$dry_run" = "true" -o "$verbose" = "true" ]; then
        cat $msgfile
    fi

    if [ "$dry_run" != "true" ]; then
        # Put our email in our drafts folder via IMAP.  If this
        # succeeds, we can delete the temporary file; otherwise, leave
        # it around so we can extract the message and send it
        # manually.
        git imap-send <$msgfile
    fi
    
    rm $msgfile
}

send_freshmeat () {
    ./tools/announce-build-freshmeat.rb $flags $version
}

send_irc () {
    ./tools/irc-announce.rb $flags irc.freenode.net 7000 <<EOF
/identify $(git config --get announcebot.password)
/join #halyard
/gsub-topic /Latest: [0-9.]+(-[a-zA-Z._-]*[0-9])?/ Latest: $vernum
Halyard $vernum has been released! See http://iml.dartmouth.edu/halyard/dist/Release-Notes.txt for details.
/part
EOF
}

if [ "$email" = "true" ]; then send_email; fi
if [ "$freshmeat" = "true" ]; then send_freshmeat; fi
if [ "$irc" = "true" ]; then send_irc; fi