# -*- Mode: Ruby; -*-

# Pass an optional start card name to jump_each.  This is useful for
# resuming a test run after an error.
def jump_each_start_arg
  if ENV['START'].nil?
    ''
  else
    " :start #{ENV['START']}"
  end
end

namespace :halyard do
  desc 'Jump to each card in the script'
  task :jump_each do
    halyard_command "(jump-to-each-card#{jump_each_start_arg})"
  end

  namespace :jump_each do
    desc 'Jump to each card in the script and test it'
    task :test do
      halyard_command "(jump-to-each-card :planner 'test#{jump_each_start_arg})"
    end
  end
end
