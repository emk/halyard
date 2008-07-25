# Stage 2 of the bootstrap loader for the Halyard Ruby environment.  At
# this point, we expect to have $HALYARD_SCRIPT and $HALYARD_RUNTIME
# variables.

# Find directories with Ruby-related files.
$HALYARD_RUBY_DIRS =
  Dir["#{$HALYARD_SCRIPT}/ruby",
      "#{$HALYARD_SCRIPT}/collects/**/_halyard/ruby",
      "#{$HALYARD_RUNTIME}/ruby"]

# Add each directory of Ruby-related files to our load path.
for dir in $HALYARD_RUBY_DIRS
  $LOAD_PATH << "#{dir}/lib"
  # TODO: We might want to set up  "#{dir}/gems", too.
end
