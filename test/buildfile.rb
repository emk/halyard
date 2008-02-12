require 'buildscript'
include Buildscript
require 'inno_setup'
require 'commands'

# Configuration options.
svn_url = 'svn+ssh://imlcvs.hitchcock.org/var/lib/svn/main/'
quicktime_dir =
  '//Mccay/Mccay/program/archive/program_sources/Installers/QuickTime6'
release_dir = '//mccay/Mccay/Testing/TamaleTest'

update_temp_path = '/home/iml_updater/tmp/'
update_path = '/home/iml_updater/www/updater/tamale_test/'

# Start the build.
start_build(:build_dir => 'c:/build/tamale_test',
            :release_dir => release_dir,
            :signing_key => 'iml_authenticode_key')

# Which type of build are we? 
if release_build?
  update_url = 'http://iml.dartmouth.edu/updates/tamale_test/'
  update_ssh_host = 'iml.dartmouth.edu'
else
  update_url = 'http://mccay.hitchcock.org/updates/tamale_test/'
  update_ssh_host = 'mccay.hitchcock.org'
end

update_ssh_user = 'iml_updater'

# Get a clean source tree from SVN.
# This is not a section that you can enable or disable, because the cd command
# is required by the rest of the script, and doesn't do anything time 
# consuming when the build is dirty.
heading 'Check out the source code.'
unless dirty_build?
  svn_co_and_tag 'tamale_test', "#{svn_url}/programs/tamale_test", release_id
end
cd 'tamale_test'

# Prepare to build our installer.
# Also not a section you can disable because it's required by later sections.
heading 'Preparing to build an installer.'
iss_file = 'windows-installer.iss'
release_installer_support_files iss_file, {:cd => 1}, quicktime_dir

# Compile the scripts
heading 'Compiling scheme scripts.', :name => :compile_scheme do
  compile_scheme_scripts
end

# Make MANIFEST files that describe the contents of our installer components.
heading 'Generating MANIFEST.* files.', :name => :manifest do
  generate_manifest_files iss_file, update_url
end

# Upload update-related files to the update server.
heading 'Copying files for updater', :name => :updater do 
  upload_files_for_updater(update_ssh_host, update_ssh_user, update_path,
                           update_temp_path, 'tamale_test')
end

# Build our DVD/web installer.
heading 'Building DVD/web installer.', :name => :dvd do
  inno_setup_5 iss_file
  sign_file('Tamale_Test_Setup.exe', "Tamale Test Setup",
            "http://iml.dartmouth.edu/tamale")
  release 'Tamale_Test_Setup.exe', :cd => 1

  # We only want to release streamable media files.
  release 'Media', :filter => /\.(mov|mp3)$/, :cd => 1
end

# Build our multisegment CD installer.
#heading 'Building CD installer.', :name => :cd do
#  inno_setup_5 iss_file, :define => {'CD_INSTALLER' => 1}
#  cd 'cd_installer' do
#    sign_file('Tamale_Test_Setup.exe', "Tamale Test Setup",
#              "http://iml.dartmouth.edu/tamale")
#    release 'Tamale_Test_Setup.exe', :cd => 1, :cd_only => true
#    # Release each of the binary archives, one to a CD.
#    Dir['*-*.bin'].each do |binfile|
#      binfile =~ /-(\d+)\.bin$/
#      release binfile, :cd => $1.to_i, :cd_only => true
#    end
#  end
#end

# Automatically run our installers when we insert the DVD.
release 'Autorun.inf', :cd => 1, :cd_only => true, :hidden => true
release 'Graphics/script/application.ico', :cd => 1, :cd_only => true,
  :hidden => true

# Finish the build.
finish_build_and_upload_files
