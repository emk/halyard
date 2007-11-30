require 'test/unit'
require 'fileutils'
require 'pathname'

class UpdateInstallerTest < Test::Unit::TestCase
  EXE_PATH="../../../Win32/Bin/UpdateInstaller.exe"

  def setup
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
    assert_file_equals <<EOF, "Updates/temp/log"
Uninstall completed.\r
EOF
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
end
