require 'test/unit'
require 'fileutils'
require 'pathname'

class UpdateInstallerTest < Test::Unit::TestCase
  EXE_PATH="../Release/UpdateInstaller.exe"

  def test_exe_exists
    assert_exists EXE_PATH
  end

  def test_install
    assert system(EXE_PATH, ".")
    assert_exists "sub/quux.txt"
    assert_file_equals "", "sub/quux.txt"
    assert_file_equals "foo\r\n", "foo.txt"
    assert_file_equals "foo\r\n", "sub/foo.txt"
    assert_files_equal "Updates/release.spec", "release.spec"
    assert_files_equal("Updates/manifests/update/MANIFEST.base", 
                       "MANIFEST.base")
    assert_files_equal("Updates/manifests/update/MANIFEST.sub", 
                       "MANIFEST.sub")
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
