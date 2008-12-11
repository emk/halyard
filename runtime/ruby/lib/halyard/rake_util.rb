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

    # Build an installer using Inno Setup 5, which must be installed in
    # the default location.  For best results, run this from the directory
    # containing your application.
    #   inno_setup_4 'myprogram.iss'
    def inno_setup_5 iss_file, options={}
      defines = (options[:define] || {}).map {|var,value| "-d#{var}=#{value}" }
      sh 'c:/Program Files/Inno Setup 5/iscc', iss_file, *defines
    end
  end
end
