require 'config/boot'
require 'buildscript/buildscript'
include Buildscript
require 'buildscript/inno_setup'
require 'buildscript/commands'

# Configuration options.
svn_url = 'svn+ssh://imlsrc.dartmouth.edu/var/lib/svn/main/'
quicktime_dir = '//mccay/Mccay/program/sources/Installers/QuickTime/'
program_unix_name = 'halyard-test'
release_dir = "//mccay/Mccay/Testing/#{program_unix_name}"


# Start the build.
start_build(:build_dir => "c:/build/#{program_unix_name}",
            :release_dir => release_dir,
            :signing_key => 'iml_authenticode_key')

# Which type of build are we? 
if release_build?
  update_url = "http://iml.dartmouth.edu/updates/#{program_unix_name}/"
  update_ssh_host = 'iml.dartmouth.edu'
else
  update_url = "http://mccay.hitchcock.org/updates/#{program_unix_name}/"
  update_ssh_host = 'mccay.hitchcock.org'
end

update_temp_path = '/home/iml_updater/tmp/'
update_path = "/home/iml_updater/www/updater/#{program_unix_name}/"
update_ssh_user = 'iml_updater'

# Get a clean source tree from subversion.
# This is not a section that you can enable or disable, because the cd command
# is required by the rest of the script, and doesn't do anything time 
# consuming when the build is dirty.
heading 'Check out the source code.'
unless dirty_build?
  # TODO: Check out of version control.
end
cd 'halyard/test'

# Prepare to build our installer.
# Also not a section you can disable because it's required by later sections.
heading 'Preparing to build an installer.'
iss_file = 'temp/windows-installer-final.iss'
run 'rake', iss_file
installer = 'Halyard_Test_Setup.exe'
release_installer_support_files iss_file, {:cd => 1}, quicktime_dir

# Compile the scripts
heading 'Compiling scheme scripts.', :name => :compile_scheme do
  compile_scheme_scripts
end

# Make MANIFEST files that describe the contents of our installer components.
heading 'Generating MANIFEST.* files.', :name => :manifest do
  generate_manifest_files iss_file, update_url
end

# Copy files to update server, and put them in the right places for us to
# update from. 
heading 'Copying files for updater', :name => :updater do 
  upload_files_for_updater(update_ssh_host, update_ssh_user, update_path,
                           update_temp_path, program_unix_name)
end

# Build our DVD installer.
heading 'Building DVD installer.', :name => :dvd do
  inno_setup_5 iss_file
  sign_file(installer, 'Halyard Test',
            'http://iml.dartmouth.edu/halyard/')
  release installer, :cd => 1
  release 'streaming', :filter => /\.(mov|mp3)$/, :cd => 1
end

# Automatically run our installers when we insert CD 1.
release 'Autorun.inf', :cd => 1, :cd_only => true, :hidden => true
release('local/branding/application.ico', 
        :cd => 1, :cd_only => true, :hidden => true)

# Finish the build.
finish_build_and_upload_files
