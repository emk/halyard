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

class UpdateInstallerTest < Test::Unit::TestCase
  EXE_PATH="../../../runtime/UpdateInstaller.exe"

  def setup
    FileUtils.rm_rf "fixture-temp"
    FileUtils.cp_r "fixture", "fixture-temp"
    FileUtils.cd "fixture-temp"
  end

  def teardown
    FileUtils.cd ".."
    FileUtils.rm_rf "fixture-temp"
  end

  def test_exe_exists
    assert_exists EXE_PATH
  end

  def test_run_cpp_tests
    assert system("../test/Debug/UpdateInstallerTest.exe")
  end

  def test_install
    download_mtime = 
      File.mtime("Updates/pool/855426068ee8939df6bce2c2c4b1e7346532a133")
    sleep 2
    assert system(EXE_PATH, ".", ".")
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
    assert !system(EXE_PATH, ".", ".")
    assert_file_equals "", "foo.txt"
    assert_file_equals "", "sub/foo.txt"
    assert !File.exists?("sub/quux.txt")
    assert_file_equals <<EOF, "Updates/temp/log"
Checking if install is possible.\r
Update is impossible; relaunching.\r
EOF
  end

  def test_failed_install
    FileUtils.rm "Updates/pool/855426068ee8939df6bce2c2c4b1e7346532a133"
    assert_fails_gracefully
  end

  def test_no_permission
    FileUtils.chmod 0400, "foo.txt"
    assert_fails_gracefully 
  end

  def test_uninstall_removes_lock_file
    File.open("UPDATE.LCK", 'w') {|f| }
    assert system(EXE_PATH, "--uninstall", ".")
    assert !File.exists?("UPDATE.LCK")
    assert !File.exists?("Updates/temp/log")
  end

  # This test case passes, but only with manual interaction.  For
  # simplicity, I've disabled it for now.  Feel free to re-enable.
  #def test_should_not_install_to_directory_without_release_spec
  #  FileUtils.rm "release.spec"
  #  assert !system(EXE_PATH, ".", ".")
  #end

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
end
