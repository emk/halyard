#!/usr/bin/perl

# Parse our arguments.
if (@ARGV < 2) {
    print STDERR "Usage: diff-extract.pl patchfile.diff filename...\n";
    exit 1;
}
my ($patchfile, @files_to_extract) = @ARGV;

# Make a hash table of filenames to extract.
my %files;
foreach (@files_to_extract) {
    $files{$_} = 1;
}

# Extract the appropriate lines from the patch.
open(DIFF, "<$patchfile") or die "Can't open $patchfile, stopping";
my $current_file = "";
foreach (<DIFF>) {
    chomp;

    # See if we need to update our current filename.
    if (/^--- ([^\r\t\n]+)/) {
        $current_file = $1;
        print "Index: $current_file\n"
            if (defined $files{$current_file});
    } elsif (/^Index: / || /^diff / || /^Only /) {
        $current_file = "";
    }

    # Output this line only if it belongs to one of the specified files.
    print "$_\n"
        if (defined $files{$current_file});
}
close(DIFF) or die "Can't close $patchfile, stopping";
