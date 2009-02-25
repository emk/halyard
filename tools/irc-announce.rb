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

#!/usr/bin/env ruby
# == Synopsis
# 
# irc_announce.rb: Connect to an IRC server, send some commands read
# from standard input, and quit.  Keeps track of channel topics and
# allows regexp replacement of the channel topic, by asking ChanServ
# to set the topic for us.
# 
# == Usage
# 
# irc_announce.rb [OPTION] SERVER PORT
# 
# OPTIONS
#
#   -h, --help
#       Print usage instructions.
#
#   -v, --verbose
#       Print out all commands sent to the server and responses from it.
# 
#   --nick NICK
#       Connect using this nickname.  Defaults to announce-bot.
#
#   --dry-run
#       Don't actually connect to the server, just print out the
#       commands that we would send.  Will simply print out the regexp
#       and replacement for /gsub-topic, since we won't have an initail
#       topic to work from.
#
# == Commands
# 
# irc_announce.rb supports a very limited set of commands, read from
# standard input.  Any text prefixed with / is treated as a command;
# raw text is sent to the last channel joined.
#
#   /identify PASSWORD
#       Identify with NickServ, supplying the given password.
#
#   /join #CHANNEL
#       Join the named channel, and update our current channel to
#       refer to it.
#
#   /part [MESSAGE]
#       Leave a channel, with the given message (or a random one if no
#       message is supplied), and set our current channel to the most
#       recently joined channel still active.
#
#   /msg NAME TEXT
#       Send a message to the named entity (channel or
#       person). Implemented for convenience for those familiar with
#       most other IRC clients.
#
#   /gsub-topic /REGEXP/ REPLACEMENT
#       Do a global search and replace for the given regular
#       expression in the channel topic for the current channel (the
#       most recent channel joined), replacing each occurance with
#       REPLACEMENT, and set the topic to the result.  \1, \2, etc.,
#       may be used to refer to subexpression matches.
#
#   /COMMAND [ARGS]
#       Pass COMMAND [ARGS] verbatim to the server.  The user is 
#       responsible for formatting all arguments correctly for the
#       IRC protoocol.
#
#   TEXT
#       Send a message consiting of TEXT to the current channel, as
#       defined by the last /join command.

require 'socket'
require 'getoptlong'
require 'rdoc/usage'

opts = GetoptLong.new(
  ['--help', '-h', GetoptLong::NO_ARGUMENT],
  ['--verbose', '-v', GetoptLong::NO_ARGUMENT],
  ['--nick', GetoptLong::REQUIRED_ARGUMENT],
  ['--dry-run', GetoptLong::NO_ARGUMENT]
)

$nickname = 'announce-bot'
$verbose = false
$dry_run = false

opts.each do |opt,arg|
  case opt
    when '--help'
      RDoc::usage
    when '--verbose'
      $verbose = true
    when '--dry-run'
      $dry_run = true
    when '--nick'
      $nickname = arg
  end
end

if ARGV.length != 2
  STDERR.puts "Expected SERVER PORT, got: #{ARGV}"
  RDoc::usage 1, "Usage"
end

$host = ARGV[0]
$port = ARGV[1].to_i

if $port == 0
  STDERR.puts "Please provide a non-zero port number"
  RDoc::usage 2, "Usage"
end

if $dry_run
  # If we're doing a dry run, we won't ever try to write to a file, and
  # we shouldn't be reading anything from it either, so /dev/null should
  # suit our purposes.
  $server = File.new("/dev/null", "r")
else
  $server = TCPSocket::new($host, $port)
end

def send_command command
  puts " >>> #{command}" if ($verbose || $dry_run)
  $server.puts command unless $dry_run
end

# Log into our IRC server.
# TODO - deal with nick collisions.
send_command "NICK #{$nickname}"
send_command "USER #{ENV['USER']} 0 * :Announcement Bot"

# Our current state.

# Mapping from channels to topics, based on /join response and TOPIC commands
# receive.
$topics = { }
# Stack of joined channels; $channel is pushed on /join, popped on /part.
$channels = []
# The active channel; used for /part and /gsub-topic.
$channel = nil
# Buffer of data from the server that we haven't yet parsed.
$buffer = ""
# Should we exit on the next loop iteration?
$done = false
# Are we connected in yet? We can't send any further commands until we are.
$connected = false

# Pick a random quote for parting a channel
$quotes = 
  ['Hasta la vista, baby.',
   'Daisy, Daisy...',
   'Bite my shiny metal ass!',
   'Number Five is alive!',
   'See you later, Navigator.',
   'I suggest a new strategy, R2: let the Wookiee win.',
   'Klaatu barada nikto.',
   'EXTERMINATE! EXTERMINATE!',
   'I am fully functional, programmed in multiple techniques.',
   'Sorry, Miss. I was giving myself an oil job.',
   'I see the eigenvalue in thine eye, I hear the tender tensor in thy sigh.',
   'Danger, Will Robinson!',
   'Here I am, brain the size of a planet, and they ask me to take you to the bridge.']
def random_quote
  $quotes[rand($quotes.length)]
end

# Keep reading input from the server until a condition is reached.
# The condition may be a string or regular expression to match against
# incoming lines, or a block that may check incoming lines or state.
def expect text=nil, timeout=10
  return if $dry_run

  stop = Time.now + timeout
  line = nil

  until (line && text && line.index(text) || 
         block_given? && yield(line) || 
         Time.now > stop)
    line = receive_data(stop-Time.now)
  end
end

# Read a user command from STDIN and process it, sending the appropriate
# IRC commands to the server.
def execute_command
  command = STDIN.gets
  unless command
    $done = true
    send_command "QUIT :End of file"
    expect /.*/, 2
    return
  end

  command.chomp!

  case command 
    when /^\/join +(#[^ \r\n]+)/
      $channels.delete $channel
      $channels.push $channel
      $channel = $1
      send_command "JOIN #{$channel}"
      expect /JOIN/i
    when /^\/part( +([^ ].*))?$/
      send_command "PART #{$channel} :#{$2 ? $2 : random_quote}"
      $channel = $channels.pop
      expect /PART/i
    when /^\/msg ([^ ]+) (.*)$/
      send_command "PRIVMSG #{$1} :#{$2}"
    when /^\/gsub-topic +\/([^\/]+)\/ (.+)$/
      unless $channel
        STDERR.puts "ERROR: Cannot set topic without a channel"
        RDoc::usage 3, "Commands"
      end
      re = Regexp.new($1)
      if $dry_run
        new_topic = "/#{$1}/ #{$2}"
      else
        expect { $topics[$channel] } # Wait until we have a topic.
        new_topic = $topics[$channel].gsub(re, $2)
      end
      send_command "PRIVMSG ChanServ :topic #{$channel} #{new_topic}"
      expect /TOPIC/i
    when /^\/gsub-topic/
      STDERR.puts "Error: invalid arguments to /gsub-topic"
      RDoc::usage 3, "Commands"
    when /^\/identify +(.*)$/
      send_command "PRIVMSG NickServ :IDENTIFY #{$1}"
      expect "You are now identified"
    when /^\/(.*)/
      send_command $1
    else
      send_command "PRIVMSG #{$channel} :#{command}"
  end
end

# Receive data from the server, split it into lines, and process each
# line.
def receive_data timeout=nil
  # If we're doing a dry run, then fake connecting and return immediately.
  if $dry_run
    $connected = true
    return
  end
  
  if timeout
    r, w, e = select([$server], nil, nil, timeout)
    return unless r == [$server]
  end

  process_response $server.gets.chomp
end

# Act on any reponses we get from the server.  We always print the
# response to standard out if the --verbose flag is set.  We respond
# to PINGs, so the server won't drop our connection.  And we use TOPIC
# commands, as well as the 332 response to joining a channel, to keep
# track of the topic for each channel so we can update it rather than
# overwriting it entirely.  We also set our $connected flag based on
# receiving a 001 response.
def process_response response
  puts response if $verbose

  case response
    when /^:[^ ]+ +001.*/
      $connected = true
    when /^PING (.*)/
      send_command "PONG #{$1}"
    when /^:[^ ]+ +332 +[^ ]+ +(#[^ ]+) +:(.*)$/
      $topics[$1] = $2
    when /^:[^ ]+ +TOPIC +(#[^ ]+) +:(.*)/i
      $topics[$1] = $2
  end

  response
end

# Don't start processing any commands until we have heard from the server
# that we have connected.
expect { $connected }

# Our main loop.  We select on our socket and STDIN for reading, then
# loop through the resulting list of ready I/O, and dispatch to the
# appropraite function to deal with input from that source.
# TODO - We may eventually need a command queue, so we can send commands
# to the server slowly enough to avoid being kicked for flooding.
while (!$done)
  r, w, e = select([$server, STDIN], nil, nil)
  exit 0 unless r
  
  for read in r
    case read
      when $server
        receive_data
      when STDIN
        execute_command
      else
        STDERR.puts "Unexpected result from select(): #{read}"
        exit 6
    end
  end
end
