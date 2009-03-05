# -*- Mode: Ruby; -*-
require 'find'

namespace :halyard do
  desc 'Delete generated files'
  task :clean do
    script_dirs = ["#{$HALYARD_RUNTIME}/plt", "#{$HALYARD_RUNTIME}/collects",
                   "collects", "scripts"]
    to_delete = Dir["temp", "config/cached.conf",
                    *script_dirs.map {|p| "#{p}/**/compiled" }]
    to_delete.each {|f| rm_rf f }
  end

  desc 'Compile *.ss files and generate cached.conf'
  task :build do
    halyard_command "(exit-script)"
  end

  desc 'Run all test cards under tests/'
  task :test do
    halyard_command "(command-line-test-driver)"
  end

  desc 'Jump to each card in the script'
  task :jump_each do
    halyard_command "(jump-to-each-card)"
  end

  namespace :jump_each do
    desc 'Jump to each card in the script and test it'
    task :test do
      halyard_command "(jump-to-each-card :planner 'test)"
    end
  end

  desc 'Update support files to the lastest versions'
  task :update_tools do
    # Build a list of files in our template directory.
    template_dir = "#{$HALYARD_RUNTIME}/templates/update_tools"
    files = []
    cd template_dir do
      Find.find "." do |path|
        next if File.basename(path) =~ /^\./ or path =~ /~$/
        next if File.directory?(path)
        files << path.sub(/^\.\//, '')
      end
    end

    # Copy each file to the current script.
    files.each do |path|
      puts "Updating #{path}"
      mkdir_p File.dirname(path)
      cp "#{template_dir}/#{path}", path
    end
  end
end
