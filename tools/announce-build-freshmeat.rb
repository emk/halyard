#!/usr/bin/ruby

# Post a release to freshmeat, using the XML-RPC API described here:
# http://freshmeat.net/faq/view/49/
#
# This requires that your freshmeat username and password be stored in
# your git config, under the keys freshmeat.username and
# freshmeat.password.  Yes, this is an abusive hack.

require 'xmlrpc/client'

def usage
  $stderr.puts "usage: #{$0} [--dry-run] vX.Y.Z"
  $stderr.puts "    Send release announcment about version vX.Y.Z to Freshmeat."
  $stderr.puts ""
  $stderr.puts "    --dry-run      print announcement, don't send it"
  exit 1
end

dry_run = ARGV.include?('--dry-run') && ARGV.delete('--dry-run')
version = ARGV[0] or usage
version_number = version.sub(/^v/, '')
project_url = 'http://iml.dartmouth.edu/halyard'
tarball_url = "#{project_url}/dist/halyard-#{version_number}/" + 
  "halyard-#{version_number}.tar.gz"

username = `git config --get freshmeat.username`.chomp
password = `git config --get freshmeat.password`.chomp

def print_error_and_exit error
  puts "ERROR: #{error.faultCode} - #{error.faultString}"
  exit 1
end

server = XMLRPC::Client.new2('http://freshmeat.net/xmlrpc/')
ok, login = server.call2('login', 
                         :username => username,
                         :password => password)
print_error_and_exit login unless ok

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

release = { 'SID' => sid,
            :project_name => 'halyard',
            :branch_name => 'Default',
            :version => version_number,
            :changes => short_message,
            # Minor feature enhancements; I don't want to pick every time,
            # and this seems like a reasonable default.
            :release_focus => 4, 
            :url_tgz => tarball_url }
if dry_run
  release.each { |key, val| puts "#{key} => #{val}"}
else
  ok, response = server.call2('publish_release', release)
  print_error_and_exit response unless ok
end

ok, logout = server.call2('logout', :SID => sid)
print_error_and_exit logout unless ok
