require File.join(File.dirname(__FILE__), 'test_helper')
require 'halyard/installer_tools'

class InstallerToolsTest < Test::Unit::TestCase
  FILE_1 = <<EOD
This is the prologue.

[Files]
This is the files section.

[Tasks]  
This is the tasks section.
EOD

  FILE_2 = <<EOD
More prologue.

[Files]
More files.

[Code]
Code.
EOD

  MERGED = <<EOD
This is the prologue.

More prologue.

[Code]
Code.
[Files]
This is the files section.

More files.

[Tasks]
This is the tasks section.
EOD


  def test_should_parse_string_into_sections
    parsed = Halyard::InstallerTools.parse_sections(FILE_1)
    assert_equal "This is the prologue.\n\n", parsed['__prologue']
    assert_equal "This is the files section.\n\n", parsed['Files']
    assert_equal "This is the tasks section.\n", parsed['Tasks']
  end

  def test_should_merge_two_files_by_section
    assert_equal MERGED, Halyard::InstallerTools.merge_sections(FILE_1, FILE_2)
  end

  def test_all_dependencies_should_exist
    Halyard::InstallerTools.windows_installer_dependencies.each do |file|
      puts file
      assert File.exists?(file)
    end
  end

  def test_should_make_installer_file
    output = Halyard::InstallerTools.make_installer_file(FILE_1)
    assert_match /This is the files section/, output
  end
end
