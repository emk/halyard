# -*- Mode: Ruby; -*-
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
end
