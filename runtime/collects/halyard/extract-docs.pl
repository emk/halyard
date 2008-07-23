#!/usr/bin/perl
#==========================================================================
#  Automatic Comment->Documentation Converter
#==========================================================================
#  This script parses specially-formatted Scheme comments and automatically
#  generates searchable DrScheme help-desk documentation from them.
#
#  For more information on the various help-file formats, see the
#  "help collection" in DrScheme's help desk.

use strict;
use warnings;
use File::Path;

my @API_files = qw(5L-Kernel.ss 5L-API.ss 5L.ss);


#==========================================================================
#  Data Structure
#==========================================================================
#  We represent our manual as a tree of nodes.  Each node has a type and
#  zero or more properties.

my @sections;
my $current_section;

# We represent a node with a hash table.
sub node_new ($$) {
    my ($type, $id) = @_;
    {nodetype => $type, id => $id};
}

# Set a property directly.  Each property may only be set once.
sub node_set_property ($$$) {
    my ($node, $prop, $value) = @_;
    die "Already set $prop of $node->{'nodetype'} $node->{'id'}"
	if defined $node->{$prop};
    $node->{$prop} = $value;
}

# Treat a property as a list, and append a new item to it.
sub node_add_to_property ($$$) {
    my ($node, $prop, $value) = @_;
    $node->{$prop} = [] unless defined $node->{$prop};
    push @{$node->{$prop}}, $value;
}

# See if a node has the specified property.
sub node_has_property ($$) {
    my ($node, $prop) = @_;
    $node->{$prop};
}

# Fetch a property value.
sub node_get_property ($$;$) {
    my ($node, $prop, $default) = @_;
    if (defined $node->{$prop}) {
	$node->{$prop};
    } elsif (defined $default) {
	$default;
    } else {
	die "Require $prop for $node->{'nodetype'} $node->{'id'}";
    }
}


#==========================================================================
#  Support Code
#==========================================================================

# Convert a Perl string to a properly escaped Scheme string.
sub quote_string ($) {
    my ($str) = @_;
    $str =~ s/\"/\\\"/g;
    "\"$str\"";
}

# Register a keyword in our manual.
sub register_keyword ($$$$$) {
    my ($keyword, $display, $html_file, $html_label, $page_title) =
	map { quote_string($_) } @_;
    print KEYWORDS "($keyword $display $html_file $html_label $page_title)\n";
}

# Register a title as a series of keywords.
sub register_title ($$$) {
    my ($title, $html_file, $html_label) = @_;

    # Break the title up into single, lower-case words.
    my $keystr = $title;
    $keystr =~ tr/A-Z/a-z/;
    $keystr =~ tr/a-z0-9/ /cs;
    $keystr =~ s/^ //;
    $keystr =~ s/ $//;
    my @keywords = split(/ /, $keystr);

    # Register each keyword appropriately.
    foreach my $key (@keywords) {
	register_keyword($key, $title, $html_file, $html_label, $title);
    }
}

# Clean up a comment line to produce reasonable text.
sub clean_line ($) {
    my ($line) = @_;
    chomp $line;
    $line =~ s/^\s*;+\s*//;
    $line =~ s/\s*$//;
    #print STDERR "$line\n";
    $line;
}

# Generate a unique ID code.
my $counter = 0;
sub make_id () {
    $counter++;
    "a$counter";
}

# Rip apart a trailing (define ...) line (or other kind of definition)
# and use the information it contains to flesh out out a documentation node.
# In particular, we're trying to get reasonable values for 'deftype' and
# 'defname'.
sub process_define ($$) {
    my ($node, $line) = @_;

    my $deftype;
    my $defname;
    if (/\(define\s+([^()[:space:]]+)/) {
	$deftype = 'variable';
	($defname) = /\(define\s+([^()[:space:]]+)/;
    } elsif (/\(define\s+\(([^()[:space:]]+)/) {
	$deftype = 'function';
	($defname) = /\(define\s+\(([^()[:space:]]+)/;
    } elsif (/\(define-syntax\s+\(?([^()[:space:]]+)/) {
	$deftype = 'syntax';
	($defname) = /\(define-syntax\s+\(?([^()[:space:]]+)/;
    } elsif (/\(define-engine-variable\s+([^()[:space:]]+)\s+([^()[:space:]]+)\s+([^()[:space:]]+)\s*\)/) {
	my ($name, $engine_name, $type) =
	    /\(define-engine-variable\s+([^()[:space:]]+)\s+([^()[:space:]]+)\s+([^()[:space:]]+)\s*\)/;

	$deftype = 'variable';
	$defname = $name;
	node_set_property($node, 'engine_name', $engine_name);
	node_set_property($node, 'type', $type);
    } else {
	die "Unable to understand definition '$line', stopping";
    }

    node_set_property($node, 'deftype', $deftype);
    node_set_property($node, 'defname', $defname);
}

# Process all of our regular keys in an appropriate fashion.
sub process_key ($$@) {
    my ($node, $key, @lines) = @_;
    
    my $text = join(' ', @lines);
    if ($key eq 'return') {
	my ($type, $description) =
	    ($text =~ /^(\S+)\s+(.+)$/)
	    or die "Bad \@return: '$text', stopping";
	my $return_node = node_new('return', make_id());
	node_set_property($return_node, 'type', $type);
	node_set_property($return_node, 'description', $description);
        node_set_property($node, 'return', $return_node);
    } elsif ($key eq 'param' || $key eq 'opt' || $key eq 'key') {
	my ($type, $name, $description) =
	    ($text =~ /^(\S+)\s+(\S+)\s+(.+)$/)
	    or die "Bad parameter: '$text', stopping";
	my $param_node = node_new('param', make_id());
	node_set_property($param_node, 'paramtype', $key);
	node_set_property($param_node, 'type', $type);
	node_set_property($param_node, 'name', $name);
	node_set_property($param_node, 'description', $description);
        node_add_to_property($node, 'param', $param_node);
    } elsif ($key eq 'xref' || $key eq 'legacy') {
	my @names = split(/ /, $text);
	for my $name (@names) {
	    node_add_to_property($node, $key, $name);
	}
    } else {
	node_set_property($node, $key, $text);
    }
}


#==========================================================================
#  Source Parsing
#==========================================================================
#  First, we need to dig through the source code and build our tree of
#  nodes.

foreach my $file (@API_files) {
    open(SOURCE, "<$file") or die "Can't open $file: $!";

    # Iterate through our source file, line by line.
    local $_;
    $_ = <SOURCE>;
    COMMENT: while (1) {
	last COMMENT unless $_;

	# Skip everything which isn't a three-colon comment.
	unless (/^\s*;;;([^;]|$)/) {
	    $_ = <SOURCE>;			     
	    next COMMENT;
	}

	# We want to suck up this entire comment and any related
	# information into a node.
	my $node;

	# Determine how to process the top part of the comment.
	my $need_trailing_define = 0;
	if (/^\s*;;;=/) {
	    # We have a section heading.  Create a node to represent the
	    # section, and register it appropriately.
	    my $title = clean_line <SOURCE>; # Get the title information.
	    <SOURCE>; # Skip second ==== comment.
	    $node = node_new('section', make_id());
	    node_set_property($node, 'title', $title);
	    $current_section = $node;
	    push @sections, $current_section;
	    $_ = <SOURCE>;
	} elsif (/^\s*;;;\s+\@define\s+/) {
	    # We have a free-floating comment with no associated definition
	    # in the source.  This typically happens for structure members
	    # and for definitions imported from other modules.
	    $node = node_new('definition', make_id());
	    node_add_to_property($current_section, 'definitions', $node);
	    my $info = clean_line $_;
	    my ($type, $name) = ($info =~ /^\@define\s+(\S+)\s+(\S+)$/);
	    die "Malformed definition ($info), stopping"
		unless defined $name;
	    node_set_property($node, 'deftype', $type);
	    node_set_property($node, 'defname', $name);
	    $_ = <SOURCE>;
	} else {
	    # We have a definition block without a @define, so we'll need
	    # to parse the Scheme declaration immediately following the
	    # comment to extract the definition type, name and possibly
	    # some other information.
	    $node = node_new('definition', make_id());
	    node_add_to_property($current_section, 'definitions', $node);
	    $need_trailing_define = 1;
	}

	# Extract and process the section body.
	DESCRIPTION: while (1) {
	    last DESCRIPTION if (!/^\s*;;;([^;]|$)/ || /^\s*;;;\s*\@/);
	    node_add_to_property($node, 'text', clean_line $_);
	    $_ = <SOURCE>;
	}

	# Process any keys of the form @keyname, storing up the key and
	# any lines up to (but not including) the next key.
	KEY: while (1) {
	    last KEY unless (/^\s*;;;\s*\@/);

	    # Extract and separate the key from the line.
	    $_ = clean_line($_);
	    my ($key_name, $key_value) = /^\@(\S+)\s+(\S.*)$/;
	    defined $key_value or die "Malformed \@key line: '$_', stopping";
	    my @lines = ($key_value);
	    $_ = <SOURCE>;

	    # Process any continuation lines.
	    LINE: while (1) {
		last LINE if (!/^\s*;;;([^;]|$)/ || /^\s*;;;\s*\@/);
		push @lines, clean_line($_);
		$_ = <SOURCE>;
	    }

	    # Figure out how to store the property.
	    process_key($node, $key_name, @lines);
	}

	# If we're at the end, handle any trailing define and punt.
        if ($need_trailing_define) {
	    process_define($node, $_);
	    $_ = <SOURCE>;
	}
	next COMMENT;

        $_ = <SOURCE>;
    }

    close(SOURCE) or die "Can't close $file: $!";
}


#==========================================================================
#  Main Script
#==========================================================================

(rmtree("manual") or die "Can't delete manual") if -d "manual";
die "manual is not a directory" if -e "manual";
mkdir("manual", "755") or die "Can't create directory manual: $!";

# Open up our various output files.
open(INDEX, ">manual/index.htm") or die "Can't open index.htm: $!";
open(CONTENT, ">manual/content.html") or die "Can't open content.html: $!";
open(KEYWORDS, ">manual/keywords") or die "Can't open keywords file: $!";

print INDEX <<"__EOD__";
<html>
  <head>
    <title>5L Multimedia Programming Language</title>
  </head>
  <body>
    <h1>5L Multimedia Programming Language</h1>
    <ol>
__EOD__

print CONTENT <<"__EOD__";
<html>
  <head>
    <title>5L Multimedia Programming Language</title>
  </head>
  <body>
    <h1>5L Multimedia Programming Language</h1>
    <p><a href="index.htm">Index</a></p>
__EOD__

print KEYWORDS "(\n";

sub print_text (@) {
    my @lines = @_;

    # Print the text as a series of paragraphs.
    print CONTENT "<p>";
    foreach my $line (@lines) {
	if ($line eq '') {
	    print CONTENT "</p>\n\n<p>";
	} else {
	    print CONTENT "$line\n";
	}
    }
    print CONTENT "</p>";
}

foreach my $section (@sections) {

    # Spit out the section header.
    my $title = node_get_property($section, 'title');
    my $target = node_get_property($section, 'id');;
    register_title($title, "content.html", $target);
    print INDEX "<li><a href='content.html#$target'>$title</a></li>\n";
    print CONTENT "<h2><a name='$target'>$title</a></h2>\n\n";

    # Extract and process the section body.
    print_text(@{node_get_property($section, 'text', [])});

    # Extract and process the definitions in this section.
    my @definitions = @{node_get_property($section, 'definitions', [])};
    foreach my $definition (@definitions) {
	my $deftype = node_get_property($definition, 'deftype');
	my $defname = node_get_property($definition, 'defname');
	print CONTENT <<"__EOD__";
<p><strong><code style="color: green;">$defname</code></strong> [$deftype]</p>
<blockquote>
__EOD__


	# Get our parameters, etc.
	my @params;
	my $r;
	if ($deftype eq 'function' || $deftype eq 'syntax') {
	    @params = @{node_get_property($definition, 'param', [])};
	    if (node_has_property($definition, 'return')) {
		$r = node_get_property($definition, 'return');
	    }
	}

	# Variable stuff.
        if (node_has_property($definition, 'type')) {
	    print CONTENT ("<p><strong>Type:</strong> <em>", 
			   node_get_property($definition, 'type'),
			   "</em></p>\n\n");
	}

	# Macro stuff.
        if (node_has_property($definition, 'syntax')) {
	    my $syntax = node_get_property($definition, 'syntax');
	    print CONTENT "<p><strong>Synopsis:</strong> <code>$syntax</code></p>\n\n";
	}

	# Function stuff.
	if ($deftype eq 'function') {
	    my $name = node_get_property($definition, 'defname');
	    print CONTENT "<p><strong>Synopsis:</strong> <code>($name";
	    my $last_type = 'param';
	    for my $p (@params) {
		my $p_type = node_get_property($p, 'paramtype');
		if ($p_type ne $last_type) {
		    print CONTENT " &$p_type";
		    $last_type = $p_type;
		}
		print CONTENT " ", node_get_property($p, 'name');
	    }
	    print CONTENT ")</code>";
	    if (defined $r) {
		print CONTENT " => ";
		print CONTENT "<em>", node_get_property($r, 'type'), "</em>";
	    }
	    print CONTENT "</p>\n\n";
	}

        print_text(@{node_get_property($definition, 'text', [])});

	# Summarize parameters.
	if ($deftype eq 'function' || $deftype eq 'syntax') {
	    if (@params) {
		print CONTENT "<p><strong>Parameters:</strong></p>\n\n<ul>\n";
		for my $p (@params) {
		    print CONTENT "<li>";
		    my $p_type = node_get_property($p, 'paramtype');
		    print CONTENT "&$p_type " if ($p_type ne 'param');
		    print CONTENT ("<code>",
				   node_get_property($p, 'name'), "</code>: ",
				   node_get_property($p, 'description'),
				   " <em>(", node_get_property($p, 'type'),
				   ")</em></li>\n");
				   
		}
		print CONTENT "</ul>\n\n";
	    }
	    if (defined $r) {
		print CONTENT ("<p><strong>Result:</strong> ",
			       node_get_property($r, 'description'),
			       " <em>(", node_get_property($r, 'type'),
			       ")</em></p>\n\n");
			       
	    }
	}


	print CONTENT <<"__EOD__";
</blockquote>
__EOD__
    }
}

print KEYWORDS ")\n";

print CONTENT <<"__EOD__";
  </body>
</html>
__EOD__

print INDEX <<"__EOD__";
    </ol>
  </body>
</html>
__EOD__

# Close all our output files.
close(KEYWORDS) or die "Can't close keywords file: $!";
close(CONTENT) or die "Can't close content.html: $!";
close(INDEX) or die "Can't close index.htm: $!";
