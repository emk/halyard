#!/usr/bin/ruby 
# Usage: find-copyrights.rb [DIR]
# Find all the copyright strings that appear in text files within the
# specified directory.

require 'find'
require 'fileutils'

dir = ARGV[0] || "."

def binary? file
  data = File.read(file, 256)
  return true if data.nil? || data.empty?
  
  # If it contains "copyright", we assume it isn't binary.
  return false if data =~ /copyright/i

  # If it contains a zero byte, we assume it's binary.
  return true if data =~ /\0/
  
  # Otherwise, treat it as ASCII.
  false
end

copyrights = Hash.new {|h, k| h[k] = [] }

Find.find dir do |f|
  next unless File.file?(f)
  next if f =~ %r"/.svn/"
  next if binary?(f)
  STDERR.puts "Scanning <#{f}>"
  File.read(f).each_line do |line|
    next unless line =~ /copyright.*\d\d\d\d/i
    line.chomp!
    line.strip!
    line.sub!(%r"^[*/;#]+\s*", '')
    copyrights[line] << f
  end
end

copyrights.keys.each do |copyright|
  puts copyright
  files = copyrights[copyright]
  next if files.length > 5 # Too many, we don't care.
  files.each {|f| puts "  in #{f}" }
end
