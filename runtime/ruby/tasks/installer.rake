require 'halyard/installer_tools'

namespace :halyard do
  desc "Build an installer for this program"
  task :installer => ['halyard:freeze', 'halyard:build',
                      'temp/windows-installer-final.iss'] do
    inno_setup_5 'temp/windows-installer-final.iss'
  end
end

installer_iss_deps =
  ['config/windows-installer.iss'] +
  Halyard::InstallerTools.windows_installer_dependencies

file 'temp/windows-installer-final.iss' => installer_iss_deps do |t|
  mkdir_p(File.dirname(t.name))
  puts t.prerequisites
  File.open(t.name, 'w') do |f|
    input = File.read(t.prerequisites.first)
    f.write(Halyard::InstallerTools.make_installer_file(input))
  end
end
