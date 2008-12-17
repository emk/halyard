module Halyard
  module InstallerTools

    # Break +string+ into sections and store each in a hash table.
    def self.parse_sections string
      sections = {}
      current_section = '__prologue'
      string.each_line do |line|
        line.chomp!
        if line =~ /\A\[([^\]]+)\]\s*\z/
          current_section = $1
        else
          sections[current_section] ||= ''
          sections[current_section] += "#{line}\n"
        end
      end
      sections
    end

    # Combine +string1+ and +string2+ into a single string.
    def self.merge_sections string1, string2
      sections1 = parse_sections(string1)
      sections2 = parse_sections(string2)
      sections2.each do |section, content|
        sections1[section] ||= ''
        sections1[section] += content
      end
      prologue = sections1.delete('__prologue') || ''
      prologue + sections1.keys.sort.map do |section|
        "[#{section}]\n" + sections1[section]
      end.join
    end

    # Return a list of all the files used to construct an *.iss file.  Used
    # in rake dependencies.
    def self.windows_installer_dependencies
      [File.join(File.dirname(__FILE__), 'windows-installer.iss')]
    end

    # Merge +string+ with the standard installer template.
    def self.make_installer_file string
      template = File.read(windows_installer_dependencies.first)
      merge_sections(template, string)
    end
  end
end
