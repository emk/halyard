#!/usr/bin/perl
#
# This is pretty much a one-time script to strip useless lines from our
# comments.  (An earlier version converted old-style comments to Doxygen
# format.)  Use at your own risk.

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

    # Clean up the comments.
    my $in_comment = 0;
    my @lines;
    while (<INPUT>) {
        if (m,^\s+////+\s*\n?$,) {
            $in_comment = 1;
        } elsif (m,^\s*///,) {
            if ($in_comment) {
                push @lines, $_;
            } else {
                print OUTPUT $_;
            }
        } else {
            $in_comment = 0;
            my $last = pop @lines;
            print OUTPUT join('', @lines);
            if ($last =~ m,[^ \t\n/],) {
                print OUTPUT $last;
            }
            print OUTPUT $_;
            @lines = ();
        }
    }

    # Close the files.
    close(INPUT)
        or die "Can't finish reading from $filename.bak, stopping";
    close(OUTPUT)
        or die "Can't finish writing to $filename, stopping";
}

