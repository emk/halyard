# -*- Mode: Ruby; -*-
require 'find'

namespace :halyard do
  # What directory should we freeze into?
  FREEZE_DIR="engine/win32"

  # Don't freeze any file with a regex matching this path.
  IGNORE=[/\.(zo|dep)$/, /\/compiled\//, /\*~$/, /\/\.(git|svn)\//]

  # Library collections to bundle into our distribution.
  PLT_COLLECTS = %w(compiler config errortrace mzlib net planet setup srfi
                    swindle syntax xml)

  def abort_if_using_frozen_runtime
    if $HALYARD_RUNTIME == "#{$HALYARD_SCRIPT}/#{FREEZE_DIR}"
      STDERR.puts "Cannot delete #{FREEZE_DIR} without specifying alternate HALYARD_RUNTIME."
      exit 1
    end
  end

  def cp_r_freeze source, glob, dest=''
    files = nil
    cd source do
      files = Dir[glob].reject do |path|
        !File.file?(path) || IGNORE.any? {|regex| path =~ regex }
      end
    end
    files.each do |f|
      full_dest = "#{FREEZE_DIR}/#{dest}/#{File.dirname(f)}"
      mkdir_p(full_dest)
      cp "#{source}/#{f}", full_dest
    end
  end

  desc "Copy the Halyard runtime to #{FREEZE_DIR}"
  task :freeze => :unfreeze do
    puts "Freezing Halyard from #{$HALYARD_RUNTIME} to #{FREEZE_DIR}"
    mkdir_p "engine/win32"
    Dir["#{$HALYARD_RUNTIME}/*.{exe,dll,pdb,txt}"].each do |f|
      next if f =~ /CommonTest/
      cp f, FREEZE_DIR
    end
    cp_r_freeze $HALYARD_RUNTIME, "*/**/*"

    # A special case for halyard/test.  The value "./../runtime" is a
    # specific value set by config/boot.rb when it discovers that we're in
    # the Halyard source tree.
    if $HALYARD_RUNTIME == "./../runtime"
      puts "Freezing extra files because we're in halyard/test"
      cp "../LICENSE.txt", FREEZE_DIR
      mkdir "#{FREEZE_DIR}/plt"
      PLT_COLLECTS.each do |collect|
        cp_r_freeze("../libs/plt/collects", "#{collect}/**/*", 'plt')
      end
    end
  end

  desc "Delete the Halyard runtime from #{FREEZE_DIR}"
  task :unfreeze do
    abort_if_using_frozen_runtime
    puts "Deleting #{FREEZE_DIR}"
    rm_rf FREEZE_DIR
  end
end
