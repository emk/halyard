$HALYARD_SCRIPT  = "#{File.dirname(File.dirname(__FILE__))}/test"
$HALYARD_RUNTIME = "#{File.dirname(File.dirname(__FILE__))}/runtime"
require "#{$HALYARD_RUNTIME}/ruby/lib/halyard/boot"
require 'halyard/rake_util'
include Halyard::RakeUtil
