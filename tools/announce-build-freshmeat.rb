#!/usr/bin/env ruby
# @BEGIN_LICENSE
#
# Halyard - Multimedia authoring and playback system
# Copyright 1993-2009 Trustees of Dartmouth College
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# @END_LICENSE

# == Synopsis
#
# announce-build-freshmeat.rb: Post a release to freshmeat, using the
# XML-RPC API
#
# This requires that your freshmeat username and password be stored in
# your git config, under the keys freshmeat.username and
# freshmeat.password.
#
# == Usage
#
# announce-build-freshmeat.rb [--dry-run] [--verbose] vX.Y.Z
# 
# OPTIONS
#     --dry-run
#         Print announcement, don't send it.
#
#     -v, --verbose
#         Print release info we are sending.
#
#     vX.Y.Z
#         Version to announce.

require 'xmlrpc/client'
require 'getoptlong'
require 'rdoc/usage'

opts = GetoptLong.new(
  ['--help', '-h', GetoptLong::NO_ARGUMENT],
  ['--verbose', '-v', GetoptLong::NO_ARGUMENT],
  ['--dry-run', GetoptLong::NO_ARGUMENT]
)

opts.each do |opt,arg|
  case opt
    when '--help'
      RDoc::usage
    when '--verbose'
      $verbose=true
    when '--dry-run'
      $dry_run=true
  end
end

version = ARGV[0] or RDoc::usage(1)
version_number = version.sub(/^v/, '')
project_url = 'http://iml.dartmouth.edu/halyard'
tarball_url = "#{project_url}/dist/halyard-#{version_number}/" + 
  "halyard-#{version_number}.tar.gz"

username = `git config --get freshmeat.username`.chomp
password = `git config --get freshmeat.password`.chomp

def print_error_and_exit error
  STDERR.puts "ERROR: #{error.faultCode} - #{error.faultString}"
  exit 1
end

def print_hash hash; hash.each { |key, val| puts "    #{key} => #{val}"}; end

def call_server method, params
  if $verbose || $dry_run
    puts "calling #{method}:"
    print_hash params
  end
  
  if $dry_run
    if method == 'login'
      result = { 'SID' => '0000000000' }
    else 
      result = { }
    end
  else
    ok, result = $server.call2(method, params)
    print_error_and_exit result unless ok
  end

  if $verbose || $dry_run
    puts "==> "
    print_hash result
    puts ""
  end

  return result
end

# API described here: http://freshmeat.net/faq/view/49/
$server = XMLRPC::Client.new2('http://freshmeat.net/xmlrpc/')
login = call_server('login', :username => username, :password => password)

# Our session ID, for future commands
sid = login['SID']

# Pull out the commit message for this release, without the subject (which
# is generally just the version number and date).
message = `git log --pretty="format:%b" "#{version}^..#{version}"`
short_message = ""

message.each_line do |line|
  # Stop once we get to the shortlog
  break if line =~ /^[a-zA-Z ]*\([0-9]*\):/
  short_message += line
end

if short_message.length > 600
  # Trim long messages down and add an elipsis.
  short_message = short_message[0..596] + "..."
end

short_message = short_message.strip

call_server('publish_release',
            'SID' => sid,
            :project_name => 'halyard',
            :branch_name => 'Default',
            :version => version_number,
            :changes => short_message,
            # Minor feature enhancements; I don't want to pick every time,
            # and this seems like a reasonable default.
            :release_focus => 4, 
            :url_tgz => tarball_url)

call_server('logout', :SID => sid)
