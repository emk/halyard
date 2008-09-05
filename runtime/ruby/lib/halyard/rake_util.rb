module Halyard
  # Functions that can be used from Rake tasks.
  module RakeUtil
    # Run Halyard in command-line mode with the specified command.  Has
    # support for Halyard's fake STDERR on platforms where the real one
    # won't work for a GUI application.
    def halyard_command command, opt = {}
      opt[:halyard] ||= "#{$HALYARD_RUNTIME}/Halyard_d.exe"
      cd $HALYARD_SCRIPT do
        begin
          rm_f "temp/output.txt"
          sh(opt[:halyard], '-c', command, ".")
        ensure
          if File.exists?("temp/output.txt")
            STDERR.print(File.read("temp/output.txt"))
            STDERR.flush
            rm_f "temp/output.txt"
          end
        end
      end
    end
  end
end
