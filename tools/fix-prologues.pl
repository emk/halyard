#!/usr/bin/perl
#
# Usage: fix-prologues.pl FILENAME...
#
# Fix license boilerplate and Emacs modelines in the specified files,
# making backups.  On DOS, you may need to set PERLIO=crlf before running
# this script.

#==========================================================================
#  Configuration Options
#==========================================================================

my $modeline = <<'__EOD__';
-*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
__EOD__

my $copyright_and_license = <<'__EOD__';
Tamale - Multimedia authoring and playback system
Copyright 1993-2005 Trustees of Dartmouth College

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
__EOD__


#==========================================================================
#  Actual Code
#==========================================================================

# For each file on the command line.
foreach my $filename (@ARGV) {
    # If a backup file exists, delete it.
    unlink("$filename.bak");

    # Rename our input file as a backup.
    rename($filename, "$filename.bak")
        or die "Can't rename $filename to backup, stopping";

    # Open our files.
    open(INPUT, "<$filename.bak")
        or die "Can't open $filename.bak for reading, stopping";
    open(OUTPUT, ">$filename")
        or die "Can't open $filename for writing, stopping";

    # Snarf the whole file into memory for easy processing.
    my @lines = <INPUT>;

    # Remove any existing modeline.
    if ($lines[0] =~ m{^// -\*- Mode:}) {
        shift @lines;
    }

    # Print a new modeline.
    print OUTPUT "// $modeline";

    # Remove any existing license block.
    if ($lines[0] =~ m{^// \@BEGIN_LICENSE}) {
        shift @lines;
        while (!($lines[0] =~ m{^// \@END_LICENSE})) {
            die "Can't find end of license block, stopping"
                if (!($lines[0] =~ m{^//}));
            shift @lines;
        }
        shift @lines;
    }
    
    # Print a new license block.
    print OUTPUT <<'__EOD__';
// @BEGIN_LICENSE
//
__EOD__

    foreach (split(/\n/, $copyright_and_license)) {
        print OUTPUT "// $_\n";
    }
    
    print OUTPUT <<'__EOD__';
//
// @END_LICENSE
__EOD__

    # Print a line of whitespace (if necessary).
    my $first = $lines[0];
    if (!($first =~ m{^[ \t]*\r?$})) {
        print OUTPUT "\n";
    }
        
    # Print the remaining lines.
    foreach (@lines) {
        print OUTPUT $_;
    }

    # Close the files.
    close(INPUT)
        or die "Can't finish reading from $filename.bak, stopping";
    close(OUTPUT)
        or die "Can't finish writing to $filename, stopping";
}
