#!/usr/bin/perl

foreach (<>) {
    if (/^--- ([^\r\t\n]+)/) {
        print "$1\n";
    }
}
