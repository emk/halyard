#!/bin/sh

# Announce a build of the engine on the mailing list, IRC, freshmeat, and
# anywhere else relevant.

# In order to use this successfully, you need to have git configured to
# be able to use imap-send to push an email into your drafts folder.  You
# will then be able to edit the email as you see fit, and send it out.

usage () { 
    echo "usage: $0 [--dry-run] vX.Y.Z"
    echo "    announce release of version X.Y.Z"
    echo ""
    echo "    --dry-run     print announcements, don't send them"
    exit 1 
}

dry_run=0
case $1 in
    --dry-run) dry_run=1; shift ;;
esac

version=$1
case "$version" in
    "") usage ;;
esac
vernum=$(expr $version : 'v\(.*\)') || exit $?

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

if (( $dry_run )); then
    cat $msgfile && rm $msgfile
    ./tools/announce-build-freshmeat.rb --dry-run $version
else
    # Put our email in our drafts folder via IMAP.  If this succeeds, we can
    # delete the temporary file; otherwise, leave it around so we can extract
    # the message and send it manually.
    git imap-send <$msgfile && rm $msgfile
    ./tools/announce-build-freshmeat.rb $version
fi

