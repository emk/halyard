# @BEGIN_LICENSE
#
# Halyard - Multimedia authoring and playback system
# Copyright 1993-2009 Trustees of Dartmouth College
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# @END_LICENSE

require 'test/unit'
require 'fileutils'
require 'pathname'

$foo_digest = "855426068ee8939df6bce2c2c4b1e7346532a133"
$null_digest = "da39a3ee5e6b4b0d3255bfef95601890afd80709"

class UpdateInstallerTest < Test::Unit::TestCase
  class << self
    attr_accessor :fixture_dir

    def suite
      if name == "UpdateInstallerTest"
        return Test::Unit::TestSuite.new(name)
      else
        super
      end
    end
  end

  def setup
    @olddir = Dir.getwd
    FileUtils.rm_rf "fixture-temp"
    FileUtils.cp_r self.class.fixture_dir, "fixture-temp"
    FileUtils.cd "fixture-temp"
  end

  def teardown
    FileUtils.cd @olddir
    FileUtils.rm_rf "fixture-temp"
  end

  def assert_exists file
    assert File.exists?(file), "#{file} does not exist"
  end

  def assert_file_equals contents, file
    pn = Pathname.new(file)
    assert_equal contents, pn.read
  end
  
  def assert_files_equal file1, file2
    pn = Pathname.new(file1)
    assert_file_equals pn.read, file2
  end

  def exe_path
    "../../../runtime/UpdateInstaller.exe"
  end

  def run_exe *args
    system exe_path, *args
  end
end

class UpdateInstallerSimpleTest < UpdateInstallerTest
  @fixture_dir = "fixture"

  def test_exe_exists
    assert_exists exe_path
  end

  def test_run_cpp_tests
    assert system("../test/Debug/UpdateInstallerTest.exe")
  end

  def test_install
    download_mtime = 
      File.mtime("Updates/pool/#{$foo_digest}")
    sleep 2
    assert run_exe(".", ".")
    assert !File.exists?("UPDATE.LCK")
    assert_exists "sub/quux.txt"
    assert_file_equals "", "sub/quux.txt"
    assert_file_equals "foo\r\n", "foo.txt"
    assert_file_equals "foo\r\n", "sub/foo.txt"
    assert_files_equal "Updates/release.spec", "release.spec"
    assert_files_equal("Updates/manifests/update/MANIFEST.base", 
                       "MANIFEST.base")
    assert_files_equal("Updates/manifests/update/MANIFEST.sub", 
                       "MANIFEST.sub")
    assert_file_equals <<EOF, "Updates/temp/log"
Checking if install is possible.\r
Install is possible; beginning install.\r
Update installed successfully. Relaunching.\r
EOF
    assert_operator download_mtime, :<, File.mtime("foo.txt")
  end

  def assert_fails_gracefully
    assert !run_exe(".", ".")
    assert_file_equals "", "foo.txt"
    assert_file_equals "", "sub/foo.txt"
    assert !File.exists?("sub/quux.txt")
    assert_file_equals <<EOF, "Updates/temp/log"
Checking if install is possible.\r
Update is impossible; relaunching.\r
EOF
  end

  def test_failed_install
    FileUtils.rm "Updates/pool/#{$foo_digest}"
    assert_fails_gracefully
  end

  def test_no_permission
    FileUtils.chmod 0400, "foo.txt"
    assert_fails_gracefully 
  end

  def test_uninstall_removes_lock_file
    File.open("UPDATE.LCK", 'w') {|f| }
    assert run_exe("--uninstall", ".")
    assert !File.exists?("UPDATE.LCK")
    assert !File.exists?("Updates/temp/log")
  end

  # This test case passes, but only with manual interaction.  For
  # simplicity, I've disabled it for now.  Feel free to re-enable.
  #def test_should_not_install_to_directory_without_release_spec
  #  FileUtils.rm "release.spec"
  #  assert !system(EXE_PATH, ".", ".")
  #end
end

class FixtureBuilder
  def initialize
    @saved = { }
  end

  def fullname name
    Dir.getwd + "/" + name
  end

  def dir name, &block
    FileUtils.mkdir_p name
    if block_given?
      FileUtils.cd name do |d|
        yield self
      end
    end
    fullname name
  end

  def file name, contents
    FileUtils.mkdir_p File.dirname(name)
    File.open(name, "w") { |f| f << contents }
    fullname name
  end

  def copy source, dest
    FileUtils.cp_r source, dest
    fullname dest
  end

  def []=(key, name)
    @saved[key] = name
  end
  
  def [](key)
    @saved[key]
  end
end

Build = Struct.new(:spec, :manifests)

class UpdaterFixtureBuilder < FixtureBuilder
  def initialize
    super
    @builds = { }
  end

  def pool_file contents
    name = Digest::SHA1.hexdigest(contents)
    file name, contents
  end

  def build_manifest component, files
    name = "MANIFEST.#{component}"
    File.open(name, "w") do |f|
      files.each do |path|
        f << manifest_entry(path)
      end
    end
    fullname name
  end

  def build_release_spec build_id
    name = "release.spec"
    File.open(name, "w") do |f|
      f << "Build: " << build_id << "\n\n"
      manifests = Dir["MANIFEST.*"]
      manifests.each do |manifest|
        f << manifest_entry(manifest)
      end
    end
    fullname name
  end

  def create_build dirname, id, components
    dir dirname do |fb|
      yield fb if block_given?
      manifests = []
      components.each do |name, files| 
        manifests.push(fb.build_manifest(name, files))
      end
      release_spec = fb.build_release_spec id
      @builds[id] = Build.new(release_spec, manifests)
    end
  end

  def create_download dirname, build
    dir "#{dirname}/Updates" do |fb|
      fb.copy @builds[build].spec, "release.spec"
      fb.dir "pool" do |fb|
        yield fb
      end
      fb.dir "manifests/#{build}" do |fb|
        @builds[build].manifests.each do |manifest|
          fb.copy manifest, "."
        end
      end
    end
  end

  private

  def manifest_entry path
    digest = Digest::SHA1.hexdigest(IO.read(path))
    size = File.size(path)
    "#{digest} #{size} #{path}\n"
  end
end

class UpdateInstallerSmarterUpdateTest < UpdateInstallerTest
  COPIED_TEXT = "This file should be copied"
  MOVED_TEXT = "This file should be moved"
  OVERWRITTEN_TEXT = "Here are the new contents of this file"
  MULTIPLE_LOCATIONS_TEXT = "This file should be installed in multiple locations"

  # Override the superclass that uses an existing fixture dir, and
  # simply generate the fixture dir that we need.
  def setup
    @olddir = Dir.getwd
    FileUtils.rm_rf "fixture-temp"
    UpdaterFixtureBuilder.new.dir "fixture-temp" do |fb|
      fb.create_build("installed-program", "build-A", 
                      :component => %w[copied deleted 
                                       moved overwritten]) do |fb|
        fb.file "copied", COPIED_TEXT
        fb.file "deleted", "This file should be deleted"
        fb.file "moved", MOVED_TEXT
        fb.file "overwritten", "This file should be overwritten"
      end
      fb.create_build("new-version", "build-B",
                      :component => %w[copied from-copied from-moved
                                       overwritten multiple-locations-1
                                       multiple-locations-2
                                       multiple-locations-3]) do |fb|
        fb.file "copied", COPIED_TEXT
        fb.file "from-copied", COPIED_TEXT
        fb.file "from-moved", MOVED_TEXT
        fb.file "overwritten", OVERWRITTEN_TEXT
        %w[1 2 3].each do |i|
          fb.file "multiple-locations-#{i}", MULTIPLE_LOCATIONS_TEXT
        end
      end
      fb.create_download "download-dir", "build-B" do |fb|
        fb.pool_file OVERWRITTEN_TEXT
        fb.pool_file MULTIPLE_LOCATIONS_TEXT
      end
    end
    FileUtils.cd "fixture-temp"

    run_exe "download-dir", "installed-program"
  end

  def test_copy_files_from_tree
    assert_file_equals COPIED_TEXT, "installed-program/copied"
    assert_file_equals COPIED_TEXT, "installed-program/from-copied"
  end

  def test_move_files_in_tree
    assert !File.exists?("installed-program/moved")
    assert_file_equals MOVED_TEXT, "installed-program/from-moved"
  end

  def test_delete_files_from_tree
    assert !File.exists?("installed-program/deleted")
  end

  def test_overwrite_file
    assert_file_equals OVERWRITTEN_TEXT, "installed-program/overwritten"
  end

  def test_pool_cleared
    %w[1 2 3].each do |i|
      assert_file_equals(MULTIPLE_LOCATIONS_TEXT, 
                         "installed-program/multiple-locations-#{i}")
    end
    assert_equal [], Dir["download-dir/Updates/pool/*"]
  end
end

class UpdateInstallerCleanupTest < UpdateInstallerTest
  def setup
    @olddir = Dir.getwd
    FileUtils.rm_rf "fixture-temp"
    UpdaterFixtureBuilder.new.dir "fixture-temp" do |fb|
      fb.create_build("installed-program", "build-A",
                      :component => %w[Scripts/constant Scripts/changed
                                       top-level deleted]) do |fb|
        fb.file "top-level", "some text"
        fb.file "deleted", "this should be deleted"

        fb.dir "Scripts" do |fb|
          fb.file "constant", "constant"
          fb.file "changed", "old text"  
        end
        
        # These are some extra files which need to be cleaned up
        fb.file "scripts/extra.ss", '(display "Hello, world!")'
        fb.file "collects/somedir/extra.zo", "stuff"
        fb.file "engine/win32/collects/extra.dep", "dep"
        fb.file "engine/win32/plt/another-extra.ss", "(* 3 4)"
      end
      fb.create_build("new-version", "build-B",
                      :component => %w[scripts/constant scripts/changed
                                       top-level new]) do |fb|
        fb.file "top-level", "some text"
        fb.file "new", "this is new"

        # Note that the case has changed on the scripts directory
        fb.dir "scripts" do |fb|
          fb.file "constant", "constant"
          fb.file "changed", "new text"
        end
      end
      fb.create_download "download-dir", "build-B" do |fb|
        fb.pool_file "new text"
        fb.pool_file "this is new"
      end
    end

    FileUtils.cd "fixture-temp"
  end

  def test_cleanup_extra_files
    assert File.exists?("installed-program/Scripts/extra.ss")
    assert File.exists?("installed-program/collects/somedir/extra.zo")
    assert run_exe("download-dir", "installed-program")

    # Check with both capitalizations so we work regardless of whether
    # or not the case change has happened and whether or not we're on
    # a case insensitive file system.
    assert !File.exists?("installed-program/scripts/extra.ss")
    assert !File.exists?("installed-program/Scripts/extra.ss")
    assert !File.exists?("installed-program/collects/somedir/extra.zo")
    assert !File.exists?("installed-program/engine/win32/collects/extra.dep")
    assert !File.exists?("installed-program/engine/win32/plt/another-extra.ss")
    assert !File.exists?("installed-program/collects/somedir")

    # Make sure we didn't delete any files that should still exist
    assert(File.exists?("installed-program/scripts/constant") ||
           File.exists?("installed-program/Scripts/constant"))
    assert(File.exists?("installed-program/scripts/changed") ||
           File.exists?("installed-program/Scripts/changed"))
  end

  def test_cleanup_works_if_unexpected_files_in_non_empty_directory
    # The scripts directory should continue to have files in it after
    # the update, so we don't care if it contains any innocuous extra
    # files.
    File.open("installed-program/Scripts/important-document.doc", 'w') {|f| }
    assert run_exe("download-dir", "installed-program")

    assert File.exists?("installed-program/Scripts/important-document.doc")
    assert !File.exists?("installed-program/deleted")
    assert File.exists?("installed-program/new")    
  end

  def test_cleanup_fails_if_unexpected_files_in_directory_which_should_be_empty
    # The somedir directory should be deleted upon cleanup, as it
    # does not exist within the updated tree.  But if we put an unexpected
    # file in here, the updater should not delete it.
    File.open("installed-program/collects/somedir/important.doc", 'w') {|f| }
    assert !run_exe("download-dir", "installed-program")

    assert File.exists?("installed-program/collects/somedir/important.doc")
    assert File.exists?("installed-program/deleted")
    assert !File.exists?("installed-program/new")
  end
end
