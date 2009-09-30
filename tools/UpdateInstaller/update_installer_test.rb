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
    FileUtils.rm_rf "fixture-temp"
    FileUtils.cp_r self.class.fixture_dir, "fixture-temp"
    FileUtils.cd "fixture-temp"
  end

  def teardown
    FileUtils.cd ".."
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

  def dir name, &block
    FileUtils.mkdir_p name
    if block_given?
      FileUtils.cd name do |d|
        yield self
      end
    end
    name
  end

  def file name, contents
    File.open(name, "w") { |f| f << contents }
    name
  end

  def pool_file contents
    name = Digest::SHA1.hexdigest(contents)
    file name, contents
  end

  def copy source, dest
    FileUtils.cp_r source, dest
    dest
  end

  def build_manifest component, files
    name = "MANIFEST.#{component}"
    File.open(name, "w") do |f|
      files.each do |path|
        f << manifest_entry(path)
      end
    end
    name
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
    name
  end

  def []=(key, name)
    @saved[key] = Dir.getwd + "/" + name
  end
  
  def [](key)
    @saved[key]
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
    FileUtils.rm_rf "fixture-temp"
    FixtureBuilder.new.dir "fixture-temp" do |fb|
      fb.dir "installed-program" do |fb|
        fb.file "copied", COPIED_TEXT
        fb.file "deleted", "This file should be deleted"
        fb.file "moved", MOVED_TEXT
        fb.file "overwritten", "This file should be overwritten"
        fb.build_manifest("component", %w[copied deleted moved overwritten])
        fb.build_release_spec "build-A"
      end
      fb.dir "new-version" do |fb|
        fb.file "copied", COPIED_TEXT
        fb.file "from-copied", COPIED_TEXT
        fb.file "from-moved", MOVED_TEXT
        fb.file "overwritten", OVERWRITTEN_TEXT
        %w[1 2 3].each do |i|
          fb.file "multiple-locations-#{i}", MULTIPLE_LOCATIONS_TEXT
        end
        fb["manifest"] = fb.build_manifest("component", 
                                           %w[copied from-copied from-moved
                                              overwritten multiple-locations-1
                                              multiple-locations-2
                                              multiple-locations-3])
        fb["release.spec"] = fb.build_release_spec "build-B"
      end
      fb.dir "download-dir/Updates" do |fb|
        fb.copy fb["release.spec"], "release.spec"
        fb.dir "pool" do |fb|
          fb.pool_file OVERWRITTEN_TEXT
          fb.pool_file MULTIPLE_LOCATIONS_TEXT
        end
        fb.dir "manifests/build-B" do |fb|
          fb.copy fb["manifest"], "MANIFEST.component"
        end
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
