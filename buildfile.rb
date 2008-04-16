require 'tools/buildscript/buildscript'
include Buildscript
require 'tools/buildscript/commands'

svn_url = 'svn+ssh://imlsrc.dartmouth.edu/var/lib/svn/main'
git_url = 'git://imlsrc.dartmouth.edu'
release_dir = '//mccay/Mccay/Testing/Halyard'
build_dir = 'c:/build/halyard'

version = ARGV.pop
unless version && /^[0-9.]*$/ =~ version
  raise "Error: bad version number: #{version}"
end

halyard_url = "#{git_url}/halyard"
bin_url = "#{svn_url}/builds/halyard/#{version}"

src_dir = "halyard-#{version}"
test_dir = "halyard-test-#{version}"
mizzen_dir = "mizzen-#{version}"
bin_dir = "halyard-bin-#{version}"

all_archive = "halyard-all-#{version}.tar.gz"
src_archive = "halyard-#{version}.tar.gz"
libs_archive = "halyard-libs-#{version}.tar.gz"
media_archive = "halyard-media-#{version}.tar.gz"
mizzen_archive = "mizzen-#{version}.tar.gz"
test_archive = "halyard-test-#{version}.zip"

# Library collections to bundle into our distribution.
plt_collects = %w(compiler config errortrace mzlib net planet setup srfi
                  swindle syntax xml)

lib_dirs = %w(libs/boost libs/curl libs/freetype2 libs/libivorbisdec
             libs/libxml2 libs/plt libs/portaudio libs/quake2
             libs/sqlite libs/wxWidgets)
media_dir = "test/Media"

release_binaries = 
  %w(libmzsch3mxxxxxxx.dll Halyard.exe Halyard.pdb Halyard_d.exe Halyard_d.pdb
     wxref_gl.dll wxref_soft.dll)

web_host = 'iml.dartmouth.edu'
web_ssh_user = 'iml_updater'
web_path = '/var/www/halyard/dist'
web_halyard_dir = "#{web_path}/halyard-#{version}"
web_halyard_test_dir = "#{web_path}/halyard-test"
web_mizzen_dir = "#{web_path}/mizzen"

start_build(:build_dir => build_dir,
            :release_dir => release_dir,
            :signing_key => 'iml_authenticode_key')

heading 'Check out the source code.', :name => :checkout do
  git :clone, halyard_url, src_dir
  cd src_dir do |d|
    git :checkout, "v#{version}"
    git :submodule, 'init'
    git :submodule, 'update'
  end
end

# Build copies of all of our tarballs before we do any builds or testing,
# so we will have clean trees.  make_tarball and make_tarball_from_files
# will automatically ignore any .git and .gitignore files.
heading 'Build source tarballs.', :name => :source_tarball do
  # Build our halyard-all-x.y.z.tar.gz archive
  make_tarball src_dir, :filename => all_archive

  # Build our normal source archive, halyard-x.y.z.tar.gz, ecluding
  # our libraries and media directory.
  excludes = lib_dirs + [media_dir]
  make_tarball src_dir, :filename => src_archive, :exclude => excludes

  # Build our libs tarball.
  lib_dirs_full = lib_dirs.map {|d| "#{src_dir}/#{d}"}
  make_tarball_from_files lib_dirs_full, :filename => libs_archive

  # Build our media tarball.
  make_tarball "#{src_dir}/#{media_dir}", :filename => media_archive

  # Build our mizzen tarball
  cp_r "#{src_dir}/test/Runtime/mizzen", mizzen_dir
  make_tarball mizzen_dir, :filename => mizzen_archive

  # Copy our PLT collections into the Runtime directory.
  plt_collects.each do |name|
    cp_r "#{src_dir}/libs/plt/collects/#{name}", "#{src_dir}/test/Runtime"
  end

  # Copy out clean copy of halyard/test before doing rake test; we will
  # build the zipfile for this after building the engine and copying
  # it in.
  cp_r "#{src_dir}/test", test_dir
end

heading 'Building and testing engine.', :name => :build do
  cd src_dir do |d|
    run 'rake', 'libs'
    run 'rake', 'test'
    # TODO - optionally sign the binaries
  end
end

heading 'Tagging Runtime and binaries in Subversion.', :name => :tag_binaries do
  if dirty_build?
    mkdir "#{bin_dir}-svn"
  else
    svn :mkdir, '-m', "Creating directory for release #{version}.", bin_url
    svn :co, bin_url, "#{bin_dir}-svn"
  end
  cd "#{bin_dir}-svn" do |d|
    full_src_dir = "#{build_dir}/#{src_dir}"

    cp_r "#{full_src_dir}/test/Runtime", "."
    svn :add, "Runtime" unless dirty_build?
    cp_r "#{full_src_dir}/test/Fonts", "."
    svn :add, "Fonts" unless dirty_build?
    cp "#{full_src_dir}/LICENSE.txt", "."
    svn :add, "LICENSE.txt" unless dirty_build?

    release_binaries.each do |file|
      cp "#{full_src_dir}/Win32/Bin/#{file}", file
      svn :add, file unless dirty_build?
    end
    svn :ci, '-m', "Tagging binaries for release #{version}." unless dirty_build?
  end
end

heading 'Releasing binaries to test project.', :name => :release_to_test do
  release_binaries.each do |file|
    cp "#{src_dir}/Win32/Bin/#{file}", "#{test_dir}/#{file}"
  end
  cp "#{src_dir}/LICENSE.txt", test_dir
end

heading 'Building Halyard Test ZIP archive.', :name => :build_test_zip do
  make_zipfile test_dir, :filename => test_archive
end

heading 'Uploading tarballs to website.', :name => :upload do
  server = remote_host(web_host, :user => web_ssh_user)

  server.run 'mkdir', '-p', web_halyard_dir

  # TODO - we should base this on information in release_infos, but
  # that needs a bit of refactoring before it will be able to do what
  # we want.
  # TODO - server.upload does rsync, while we only need scp. We might want
  # to distinguish them.

  [all_archive, src_archive, libs_archive, media_archive].each do |file|
    server.upload file, web_halyard_dir
  end
  server.upload test_archive, web_halyard_test_dir
  server.upload mizzen_archive, web_mizzen_dir
  server.upload "#{src_dir}/Release-Notes.txt", web_path
end

# Finish the build.
finish_build_and_upload_files
