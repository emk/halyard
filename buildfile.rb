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
test_dir = "halyard-test-#{version}"
bin_dir = "halyard-bin-#{version}"
mizzen_dir = "mizzen-#{version}"

release_binaries = 
  %w(libmzgc2.dll libmzgc2_d.dll libmzsch2.dll libmzsch2_d.dll
     Halyard.exe Halyard.pdb Halyard_d.exe Halyard_d.pdb
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

heading 'Make source tarball.', :name => :source_tarball do
  make_tarball src_dir
  # Copy out clean copy of halyard/test before doing rake test
  cp_r "#{src_dir}/test", test_dir
  # Also, an independent distribution of mizzen
  cp_r "#{src_dir}/test/Runtime/mizzen", mizzen_dir
end

heading 'Building and testing engine.', :name => :build do
  cd src_dir do |d|
    ENV['WXWIN'] = "#{build_dir}/#{src_dir}/libs/wxWidgets"
    run 'rake', 'test'
    # TODO - optionally sign the binaries
  end
end

heading 'Tagging Runtime and binaries in Subversion.', :name => :tag_binaries do
  svn :mkdir, '-m', "Creating directory for release #{version}.", bin_url
  svn :co, bin_url, "#{bin_dir}-svn"
  cd "#{bin_dir}-svn" do |d|
    svn :cp, "#{tag_url}/test/Runtime", "."
    svn :cp, "#{tag_url}/test/Fonts", "."
    svn :cp, "#{tag_url}/LICENSE.txt", "."
    release_binaries.each do |file|
      cp "#{build_dir}/#{src_dir}/Win32/Bin/#{file}", file
      svn :add, file
    end
    svn :ci, '-m', "Tagging binaries for release #{version}."
  end
  svn :export, bin_url, bin_dir
end

heading 'Releasing binaries to test project.', :name => :release_to_test do
  release_binaries.each do |file|
    cp "#{src_dir}/Win32/Bin/#{file}", "#{test_dir}/#{file}"
  end
  cp "#{src_dir}/LICENSE.txt", test_dir
end

heading 'Building tarballs.', :name => :build_tarballs do
  make_zipfile test_dir
  make_tarball bin_dir
  make_tarball mizzen_dir
end

heading 'Uploading tarballs to website.', :name => :upload do
  server = remote_host(web_host, :user => web_ssh_user)
  # TODO - we should base this on information in release_infos, but
  # that needs a bit of refactoring before it will be able to do what
  # we want.
  [src_dir, bin_dir, mizzen_dir].each do |file|
    # TODO - server.upload does rsync, while we only need scp. We might want
    # to distinguish them.
    server.upload "#{file}.tar.gz", web_path
  end
  server.upload "#{test_dir}.zip", web_path
  server.upload "#{src_dir}/Release-Notes.txt", web_path
end

# Finish the build.
finish_build_and_upload_files
