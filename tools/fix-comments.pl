#!/usr/bin/perl
#
# This is pretty much a one-time script to convert our comments to Doxygen
# format.  Use at your own risk.

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
    while (<INPUT>) {
        chomp;
        if (m,^\s*////+\s*$,) {
            $in_comment = 1;
        } elsif (m,^\s*//[ \t], or m,^\s*//$,) {
            if ($in_comment) {
                s,^(\s*)//,\1///,;
                s,\[out\]\s*return\s*-\s*,\\return  ,;
                s,\[in\]\s*(\w+)\s*-\s*,\\param \1  ,;
                s,\[out\]\s*(\w+)\s*-\s*,\\param \1  (out) ,;
                s,\[in/out\]\s*(\w+)\s*-\s*,\\param \1  (in/out) ,;
            }
        } else {
            $in_comment = 0;
        }
        print OUTPUT "$_\n";
    }

    # Close the files.
    close(INPUT)
        or die "Can't finish reading from $filename.bak, stopping";
    close(OUTPUT)
        or die "Can't finish writing to $filename, stopping";
}

