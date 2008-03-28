require 'buildscript'
include Buildscript
require 'commands'

svn_url = 'https://imlsrc.dartmouth.edu/svn'
release_dir = '//mccay/Mccay/Testing/Halyard'
build_dir = 'c:/build/halyard'

version = ARGV.pop
unless version && /^[0-9.]*$/ =~ version
  raise "Error: bad version number: #{version}"
end

halyard_base_url = "#{svn_url}/public/halyard"
tag_url = "#{halyard_base_url}/tags/#{version}"
bin_url = "#{svn_url}/builds/halyard/#{version}"

src_dir = "halyard-#{version}"
bin_dir = "halyard-bin-#{version}"

release_binaries = 
  %w(libmzgc2.dll libmzgc2_d.dll libmzsch2.dll libmzsch2_d.dll
     Tamale.exe Tamale.pdb Tamale_d.exe Tamale_d.pdb
     wxref_gl.dll wxref_soft.dll)

web_host = 'iml.dartmouth.edu'
web_ssh_user = 'iml_updater'
web_path = '/var/www/halyard/dist'

start_build(:build_dir => build_dir,
            :release_dir => release_dir,
            :signing_key => 'iml_authenticode_key')

heading 'Check out the source code.', :name => :checkout do
  svn :export, tag_url, src_dir
end

heading 'Building and testing engine.', :name => :build do
  cd src_dir do |d|
    run 'rake', 'test'
    # TODO - optionally sign the binaries
  end
end

heading 'Tagging Runtime and binaries in Subversion.', :name => :tag_binaries do
  svn :mkdir, '-m', "Creating directory for release #{version}.", bin_url
  svn :co, bin_url, "#{bin_dir}-svn"
  cd "#{bin_dir}-svn" do |d|
    svn :cp, "#{tag_url}/Common/Runtime", "."
    svn :cp, "#{tag_url}/Common/Fonts", "."
    svn :cp, "#{tag_url}/LICENSE.txt", "."
    release_binaries.each do |file|
      cp "#{build_dir}/#{src_dir}/Win32/Bin/#{file}", file
      svn :add, file
    end
    svn :ci, '-m', "Tagging binaries for release #{version}."
  end
end

# Finish the build.
finish_build_and_upload_files
