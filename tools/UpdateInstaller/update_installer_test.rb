require 'test/unit'

class UpdateInstallerTest < Test::Unit::TestCase
  EXE_PATH="Release/UpdateInstaller.exe"

  def test_exe_exists
    assert File.exists?(EXE_PATH), "#{EXE_PATH} does not exist"
  end
end
