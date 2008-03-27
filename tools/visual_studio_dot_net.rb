# Helper routines for running <code>devenv</code>, the command-line
# interface to Microsoft Visual Studio.NET.
#
#   VisualStudioDotNet.build "MyProgram.sln", "Debug"
#   VisualStudioDotNet.clean "MyProgram.sln", "Release"
module VisualStudioDotNet
  # Peform the specified _action_ on _project_ in _configuration_.
  def self.perform_action action, project, configuration
    # "/build", etc., is a command-line switch, not a path.
    sh "devenv.com", project, "/#{action}", configuration
  end

  # Build _project_ in _configuration_.
  def self.build project, configuration
    perform_action :build, project, configuration
  end

  # Clean _project_ in _configuration_.
  def self.clean project, configuration
    perform_action :clean, project, configuration
  end
end
