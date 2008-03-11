# Build support.  Run 'rake --tasks' for instructions.  -*- Ruby -*-
require 'tools/visual_studio_dot_net'
require 'tools/code_signing'
require 'fileutils'

#==========================================================================
#  Support Routines
#==========================================================================

#  Windows applications have multiple build configurations.  The most
#  common are "Debug" and "Release".
class Configuration
  attr_reader :name

  # Create a configuration using _name_.
  def initialize name
    @name = name
    define_tasks
  end

  # Construct a configuration-specific name for _task_.
  #   Configuration.new("Debug").task_name(:build) # Returns :build_debug
  def task_name task
    "#{task}_#{name.downcase}".to_sym
  end

  private

  # Return the file suffix we traditionally append to *.exe and *.dll file
  # names in this configuration.
  def suffix
    case name
    when "Debug" then "_d"
    when "Release" then ""
    else raise "Don't know suffix for configuration <#{name}>"
    end
  end
  
  # Define all the build tasks needed by this configuration.
  def define_tasks
    desc "Clean the #{name} configuration"
    task task_name(:clean) do |t|
      VisualStudioDotNet.clean "Halyard.sln", name
    end
    
    desc "Build the #{name} configuration"
    task task_name(:build) do |t|
      VisualStudioDotNet.build "Halyard.sln", name
    end
    
    desc "Run unit tests for #{name} configuration"
    task task_name(:test) => task_name(:build) do |t|
      FileUtils.cd("test") { sh "../Win32/Bin/CommonTest#{suffix}" }
    end
  end
end


#==========================================================================
#  Rake Tasks
#==========================================================================

# We currently support two project configurations; more can be added by
# editing the list below.
CONFIGURATIONS = %w(Debug Release).map {|name| Configuration.new(name) }

# Get _task_'s name for all configurations.
def task_names task
  CONFIGURATIONS.map {|config| config.task_name(task) }
end

task :default => :test

desc "Clean all configurations"
task :clean => task_names(:clean)

desc "Build all configurations"
task :build => task_names(:build)

desc "Run unit tests for all configurations"
task :test => task_names(:test)

desc "Tag the current HEAD revision (rake tag AS=0.0.63)"
task :tag do |t|
  # This will fail if the tag contents are weird enough.
  tag = ENV['AS']
  tag =~ /^\S+$/ or raise "Invalid tag <#{tag}>"
  SVN_URL = 'svn+ssh://orson.hitchcock.org/var/lib/svn/main/public/halyard'
  msg = "Tagging trunk as #{tag}"
  sh "svn cp -m '#{msg}' -r HEAD #{SVN_URL}/trunk #{SVN_URL}/tags/#{tag}"
end

desc "Sign *.exe and *.dll files (USB key required)"
CodeSigning::Task.new do |t|
  t.files = Dir['Win32/Bin/*.exe'] + Dir['Win32/Bin/*.dll']
  t.description['Win32/Bin/UpdateInstaller.exe'] = "Update Installer"
  t.default_description = "Halyard multimedia engine"
  t.description_url = "http://iml.dartmouth.edu/halyard/"
  t.key_file = 'iml_authenticode_key'
end
