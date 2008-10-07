# This is a bootstrap loader for our Ruby environment, roughly inspired by
# Rails' config/boot.rb.  Its job is to find the Halyard runtime directory,
# and continue loading from there.  Do not modify this file: It may be
# overridden by future versions of Halyard.

$HALYARD_SCRIPT = File.dirname(File.dirname(__FILE__))
[ENV['HALYARD_RUNTIME'],
 "#{$HALYARD_SCRIPT}/engine/win32",
 # For people who keep Halyard's source tree as a git submodule.
 "#{$HALYARD_SCRIPT}/engine/src/runtime",
 # For running in-tree from the halyard/test project.
 "#{$HALYARD_SCRIPT}/../runtime"].each do |path|
  if !path.nil? && File.exists?("#{path}/ruby/lib")
    $HALYARD_RUNTIME = path
    break
  end
end

if $HALYARD_RUNTIME
  require "#{$HALYARD_RUNTIME}/ruby/lib/halyard/boot"
else
  raise "Can't find Halyard Ruby libraries"
end

