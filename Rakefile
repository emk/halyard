# Build support.  Run 'rake --tasks' for instructions.  -*- Ruby -*-
require 'tools/visual_studio_dot_net'
require 'tools/code_signing'
require 'fileutils'
require 'find'

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
      FileUtils.cd("Common/test") do
        sh("../../runtime/CommonTest#{suffix}")
      end
      FileUtils.cd("test") do
        sh("../runtime/Halyard#{suffix}", "-e", "(command-line-test-driver)",
           ".")
      end
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

desc "Sign *.exe and *.dll files (USB key required)"
CodeSigning::Task.new do |t|
  t.files = Dir['Win32/Bin/*.exe'] + Dir['Win32/Bin/*.dll']
  t.description['Win32/Bin/UpdateInstaller.exe'] = "Update Installer"
  t.default_description = "Halyard multimedia engine"
  t.description_url = "http://iml.dartmouth.edu/halyard/"
  t.key_file = 'iml_authenticode_key'
end

desc "Build libraries in libs/"
task :libs do
  VisualStudioDotNet.build "Halyard.sln", "Libraries"  
end

desc "Clean *.zo files and compiled/ directories"
task :clean_scheme do
  %w(libs/plt/collects test/Runtime test/Scripts).each do |root|
    Find.find(root) do |path|
      if File.basename(path) == "compiled" && File.directory?(path)
        rm_rf path
      end
    end
  end

  # These directories were removed in version 0.5.4.  When you update using
  # git, the files in these directories will be delete, but the empty
  # directories will be left around.  Unfortunately, the empty directories
  # will crash mzscheme.
  obsolete_runtime_dirs = %w(compiler config errortrace mzlib net planet
                             setup srfi swindle syntax xml)
  obsolete_runtime_dirs.each do |dirname|
    dir = "test/Runtime/#{dirname}"
    next unless File.directory?(dir)
    Find.find(dir) do |path|
      if File.file?(path)
        raise "Can't delete #{dir}: contains #{path}"
      end
    end
    rm_rf(dir)
  end
end
