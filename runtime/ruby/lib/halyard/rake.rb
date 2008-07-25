# Load Rake tasks from a variety of different directories in our tree.
# I'm not sure if config/tasks is actually a good place to put stuff; Rails
# uses lib/tasks.
for dir in $HALYARD_RUBY_DIRS
  for task in Dir["#{dir}/tasks/*.rake"]
    load task
  end
end

