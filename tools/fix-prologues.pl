#!/usr/bin/perl
#
# Usage: fix-prologues.pl FILENAME...
#
# Fix license boilerplate and Emacs modelines in the specified files,
# making backups.  On DOS, you may need to set PERLIO=crlf before running
# this script.
#
# When updating the licenses (usually when updating the year), we should
# probably run this script on the following files:
# 
#   tools/find-halyard-sources | PERLIO=lf xargs tools/fix-prologues.pl
#
# The following files should be excluded:
#
#   git checkout Common/test/scripts    # Can't have prologues here.
#   git checkout runtime/templates      # Public domain

use Cwd;

#==========================================================================
#  Configuration Options
#==========================================================================

my $modeline = <<'__EOD__';
-*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
__EOD__

my $halyard_program_copyright = <<'__EOD__';
Halyard - Multimedia authoring and playback system
Copyright 1993-2009 Trustees of Dartmouth College
__EOD__

my $mizzen_program_copyright = <<'__EOD__';
Mizzen - Scheme object system
Copyright 2006-2009 Trustees of Dartmouth College
__EOD__

my $gpl_license = <<'__EOD__';
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

my $lgpl_license = <<'__EOD__';
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
__EOD__


#==========================================================================
#  Actual Code
#==========================================================================

my $cwd = cwd();

# For each file on the command line.
foreach my $filename (@ARGV) {
    # Determine what kind of headers we should be adding
    $filename =~ m{\.([^.]+)$};
    my $file_type = $1;
    my $full_filename = "$cwd/$filename";
    # Are we in the mizzen collects directory?
    my $mizzen_file = ($full_filename =~ m{/collects/mizzen/});
    
    # Setup our comment characters and licenses
    my $comment_prefix;
    my $license;
    my $want_modeline;
    my $program_copyright;
    if ($file_type eq 'ss') {
        $comment_prefix = ';;';
        $license = $lgpl_license;
        $want_modeline = 0;
    } elsif ($file_type eq 'rb' || $file_type eq 'rake' || 
             $filename =~ /Rakefile$/) {
        $comment_prefix = '#';
        $license = $lgpl_license;
        $want_modeline = 0;
    } else {
        $comment_prefix = '//';
        $license = $gpl_license;
        $want_modeline = 1;
    }
    if ($mizzen_file) {
        $program_copyright = $mizzen_program_copyright;
    } else {
        $program_copyright = $halyard_program_copyright;
    }
    my $copyright_and_license = "$program_copyright\n$license";

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

    if ($want_modeline) {
        # Remove any existing modeline.
        if ($lines[0] =~ m{^// -\*- Mode:}) {
            shift @lines;
        }
        
        # Print a new modeline.
        print OUTPUT "// $modeline";
    }

    # Remove any existing license block.
    if ($lines[0] =~ m{^$comment_prefix \@BEGIN_LICENSE}) {
        shift @lines;
        while (!($lines[0] =~ m{^$comment_prefix \@END_LICENSE})) {
            die "Can't find end of license block, stopping"
                if (!($lines[0] =~ m{^$comment_prefix}));
            shift @lines;
        }
        shift @lines;
    }
    
    # Print a new license block.
    print OUTPUT <<"__EOD__";
$comment_prefix \@BEGIN_LICENSE
$comment_prefix
__EOD__

    foreach (split(/\n/, $copyright_and_license)) {
        print OUTPUT "$comment_prefix $_\n";
    }
    
    print OUTPUT <<"__EOD__";
$comment_prefix
$comment_prefix \@END_LICENSE
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
