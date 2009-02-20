# The buildscript library is designed for turning Halyard programs (hosted
# in Svubversion) into DVDs.  Here, we abuse it terribly by turning a Win32
# application (hosted in git) into tarballs and a series of Subversion
# commits.  This script would be much nicer if we actually modified
# buildscript to better support the things we're asking it to do here.

# Things you will need to install or setup to make this work:
#   - The packages described in tools/buildscript/README.txt
#   - Cygwin packages: git, autoconf, automake, pkg-config, subversion, zip
#   - Password-free login to any SVN and SSH servers mentioned below

# Add our Halyard-related libraries to our $LOAD_PATH.
$LOAD_PATH << "#{File.dirname(__FILE__)}/runtime/ruby/lib"

require 'buildscript/buildscript'
include Buildscript
require 'buildscript/commands'
require 'find'

svn_url = 'svn+ssh://imlsrc.dartmouth.edu/var/lib/svn/main'
git_url = 'git://imlsrc.dartmouth.edu'
release_dir = 'c:/release/halyard'
build_dir = 'c:/build/halyard'

# This quick-and-dirty argument check isn't technically correct, because we
# may have other arguments parsed by buildscript.  But it catches the most
# common incorrect command lines.
if ARGV.length == 0 || ARGV.last[0] == "-"[0]
  STDERR.puts 'Usage: buildfile.rb <version>'
  STDERR.puts 'Version should resolve to a git commit, e.g., v0.5.4'
  exit 1
end

# Get our version number.
commit = ARGV.pop
# Versions may contain "-rcN", "-preN", etc., but if they do, they must end
# with a number.  This prevents us from releasing tarballs of things like
# "v0.4-stable".
if commit =~ /^v[0-9.]+(-.*[0-9])?$/
  # Here, $untagged_build is a global variable so that we can access it
  # from inside the function for_release?, below.
  $untagged_build = false
  version = commit.sub(/^v/, '')
elsif commit =~ /^origin\//
  # If specify a version of the form origin/foo, then we want to make an
  # untagged build of branch foo.
  $untagged_build = true
  version = commit.sub(/^origin\//, '')
else
  # If we don't have a version of the form "v0.5.1", "v2.5-rc2", etc., then
  # don't actually upload the results of this build anywhere.
  $untagged_build = true
  version = commit
end

# Is this a build we intend to release, or just a test build?
def for_release?
  !(dirty_build? || $untagged_build)
end

halyard_url = "#{git_url}/halyard"

# These are the places in SVN and Git where we push our binaries for
# inclusion in our programs.
svn_bin_url = "#{svn_url}/builds/halyard/#{version}"
git_bin_url = "#{git_url}/halyard-bin-0.5"

# These correspond to the tarballs we would like to release.
src_dir = "halyard-#{version}"
test_dir = "halyard-test-#{version}"
mizzen_dir = "mizzen-#{version}"

# Directory that should be released to Subversion and Git as our binaries
# module.
$release_files_dir = "#{build_dir}/#{src_dir}/test/engine/win32"

# Find all of the top-level files and directories to be released.
def release_files
  Dir["#{$release_files_dir}/*"]
end

svn_bin_dir = "#{src_dir}-svn"
git_bin_dir = "#{src_dir}-git"

all_archive = "halyard-all-#{version}.tar.gz"
src_archive = "halyard-#{version}.tar.gz"
libs_archive = "halyard-libs-#{version}.tar.gz"
media_archive = "halyard-media-#{version}.tar.gz"
mizzen_archive = "mizzen-#{version}.tar.gz"
test_archive = "halyard-test-#{version}.zip"

lib_dirs = %w(libs/boost libs/curl libs/freetype2 libs/libivorbisdec
             libs/libxml2 libs/plt libs/portaudio libs/quake2
             libs/sqlite libs/wxWidgets)
media_dir = "test/streaming/media"

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
  # Clone the git repo without doing a checkout, so we don't get hosed
  # by issues of case renames between HEAD and the version we're building
  # from.
  git :clone, '--no-checkout', halyard_url, src_dir unless dirty_build?
  cd src_dir do |d|
    git :checkout, commit
    git :submodule, 'init'
    git :submodule, 'update'
  end
end

# Generate configure and Makefile.in for use on Unix platforms.  This
# requires autoconf, automake, and pkg-config.
heading "Run autogen.sh.", :name => :autogen do
  cd(src_dir) { run "./autogen.sh" }
end

# Build copies of all of our tarballs before we do any builds or testing,
# so we will have clean trees.  make_tarball and make_tarball_from_files
# will automatically ignore any .git and .gitignore files.
heading 'Build source tarballs.', :name => :source_tarball do
  # Build our halyard-all-x.y.z.tar.gz archive
  make_tarball src_dir, :filename => all_archive

  # Build our normal source archive, halyard-x.y.z.tar.gz, excluding
  # our libraries and media directory.
  excludes = lib_dirs + [media_dir]
  make_tarball src_dir, :filename => src_archive, :exclude => excludes

  # Build our libs tarball.
  lib_dirs_full = lib_dirs.map {|d| "#{src_dir}/#{d}"}
  make_tarball_from_files lib_dirs_full, :filename => libs_archive

  # Build our media tarball.
  make_tarball "#{src_dir}/#{media_dir}", :filename => media_archive

  # Build our mizzen tarball
  cp_r "#{src_dir}/runtime/collects/mizzen", mizzen_dir
  make_tarball mizzen_dir, :filename => mizzen_archive

  # Copy out clean copy of halyard/test before doing rake test; we will
  # build the zipfile for this after building the engine and copying
  # it in.
  rm_rf test_dir if dirty_build?
  cp_r "#{src_dir}/test", test_dir
end

heading 'Building and testing engine.', :name => :build do
  cd src_dir do |d|
    # We need a copy of gpgv.exe to run our updater tests.  Grab it from our
    # internal server.
    mkdir_p 'test/binaries'
    svn :export, "#{svn_url}/tools/crypto/gpgv.exe", 'test/binaries/gpgv.exe'
    run 'chmod', '+x', 'test/binaries/gpgv.exe'
    run 'rake', 'libs'
    run 'rake', 'test'
    # TODO - optionally sign the binaries

    # Freeze our runtime into test/engine/win32, which we can then use
    # as a basis for releasing to SVN, Git, and halyard-test.
    cd 'test' do |d|
      run 'rake', 'halyard:freeze'
    end
  end
end

heading 'Tagging runtime and binaries in Subversion.', :name => :push_svn do
  if for_release?
    svn :mkdir, '-m', "Creating directory for release #{version}.", svn_bin_url
    svn :co, svn_bin_url, svn_bin_dir
  else
    rm_rf svn_bin_dir if dirty_build?
    mkdir svn_bin_dir
  end
  cd svn_bin_dir do |d|
    release_files.each do |path|
      cp_r path, "."
      svn :add, File.basename(path) if for_release?
    end

    # Set up svn:ignore properties on the Runtime directories.
    %w(plt collects).each do |scheme_dir|
      Find.find scheme_dir do |path|
        next unless File.directory?(path)
        next if path =~ /\/\.svn$/
        next if path =~ /\/\.svn\//
        svn :propset, "svn:ignore", "compiled", path if for_release?
      end
    end

    svn :ci, '-m', "Tagging binaries for release #{version}." if for_release?
  end
end

heading 'Tagging runtime and binaries in Git.', :name => :push_git do
  rm_rf git_bin_dir if dirty_build?
  git :clone, git_bin_url, git_bin_dir
  cd git_bin_dir do |d|
    release_files.each do |path|
      cp_r path, "."
      git :add, File.basename(path)
    end

    git :commit, '-m', "Pushing binaries for release #{version}."
    git :tag, "v#{version}"
    git :push if for_release?
    git :push, '--tags' if for_release?
  end
end

heading 'Releasing binaries to test project.', :name => :release_to_test do
  cp "#{src_dir}/LICENSE.txt", test_dir
  mkdir_p "#{test_dir}/engine"
  cp_r $release_files_dir, "#{test_dir}/engine/win32"
end

heading 'Building Halyard Test ZIP archive.', :name => :build_test_zip do
  make_zipfile test_dir, :filename => test_archive
end

heading 'Uploading tarballs to website.', :name => :upload do
  if for_release?
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
end

# Finish the build.
finish_build_and_upload_files
