namespace :halyard do
  desc "Build an installer for this program"
  task :installer => ['halyard:freeze', 'halyard:build'] do
    inno_setup_5 'config/windows-installer.iss'
  end
end
