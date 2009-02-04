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
