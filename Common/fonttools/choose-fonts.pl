#!/usr/bin/perl
# Display all the PostScript/TT fonts in a given directory, and prompt the
# user as to which should be copied to the current directory.  Requires
# the 'ftview' application from FreeType 2.
#
# There isn't much chance of this running anywhere but Unix, and even then,
# you need to install ftview.

use warnings;
use strict;

# Parse our arguments.
unless (@ARGV == 1) {
    print STDERR "Usage: choose-fonts.pl <directory>\n";
    exit 1;
}
my ($dirname) = @ARGV;

# Read the directory contents.
opendir(DIR, $dirname)
    or die "Can't open directory $dirname, stopping";
my @files = readdir(DIR);
closedir(DIR);

# Examine each file.
foreach my $file (@files) {
    # Don't look at files we don't know how to open.
    next unless $file =~ /.(pfb|ttf)$/; 

    # Don't look at files that already exist in the current directory.
    next if -e $file;

    # Ask the user about the file.
    system("ftview", "16", "$dirname/$file");
    print "$file (y/N)? ";
    my $response = <STDIN>;
    next unless $response =~ /^[Yy]([Ee][Ss])?$/;

    # Copy the file.
    system("cp", "$dirname/$file", ".")
	and die "Copy of file failed, stopping";

    # If the metrics exist, copy them, too.
    my $metrics = $file;
    $metrics =~ s/\.[A-Za-z]+$/\.afm/;
    if (-e "$dirname/$metrics") {
	system("cp", "$dirname/$metrics", ".")
	    and die "Copy of metrics failed, stopping";
    }
}
