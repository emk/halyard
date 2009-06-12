#!/usr/bin/perl
#
# Attempt to convert braces to the following style:
#
#   void foo() {
#       if (x) {
#           bar();
#       } else {
#           baz();
#       }
#   }
#
# We're fairly conservative about collapsing braces onto the preceding line,
# and leave alone code like the following:
#
#   void foo(int arg1,
#            int arg2)
#   {
#       // ...
#   }
#
# This is an extremely dodgy and ugly tool, so please use it at your own
# risk, and only when absolutely necessary.

use strict;
use warnings;

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

    # Routines for handling lines.
    my $close_brace_line = undef;
    my $line = undef;
    sub print_any_close_brace_line() {
        if (defined($close_brace_line)) {
            print OUTPUT $close_brace_line;
            $close_brace_line = undef;
        }
    }
    sub print_any_line() {
        if (defined($line)) {
            print OUTPUT $line;
            $line = undef;
        }
    }
    sub maybe_combine($) {
        my ($open_brace_line) = @_; 
        if (defined($line)) {
            my $close_brace = '';
            my $open_brace = '';
            if (defined($close_brace_line)) {
                $close_brace = '} ';
            }
            if (defined($open_brace_line)) {
                $open_brace = ' {';
            }
            $close_brace_line = undef;
            $line =~ s/^(\s*)(.*\S)(\s*)$/$1$close_brace$2$open_brace\n/;
            print_any_line();
        } else {
            print_any_close_brace_line();
            print OUTPUT $open_brace_line if (defined($open_brace_line));
        }
    }

    # Process each line.
    while (<INPUT>) {
        if (/^\s+}\s*\n/) {
            die "Unexpected close brace after: $line in $filename" if defined($line);
            print_any_close_brace_line();
            $close_brace_line = $_;
        } elsif (defined($close_brace_line) && /^\s+(else|catch|while|until)/) {
            die "Unexpected line: $_" if defined($line);
            $line = $_;
        } elsif (/^([a-zA-Z]|\s+(if|for|while|do|try|switch|class|struct|enum|union)\b)/) {
            print_any_close_brace_line();
            print_any_line();
            $line = $_;
        } elsif (/^\s*{\s*\n/) {
            maybe_combine($_);
        } else {
            maybe_combine(undef);
            print OUTPUT $_;
        }
    }

    # Close the files.
    close(INPUT)
        or die "Can't finish reading from $filename.bak, stopping";
    close(OUTPUT)
        or die "Can't finish writing to $filename, stopping";
}

