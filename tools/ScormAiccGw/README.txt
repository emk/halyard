Scorm Gateway
=============

This gateway is designed to connect a SCORM-based LMS with a standalone
Windows application using the AICC AGR-006 file formats (but not, for
security reasons, the standard AGR-006 launch convention).

Directory Layout
----------------

  README.txt   - This documentation.
  TODO.txt     - List of things to do.
  Rakefile     - Like a makefile, but based on Rake.
  control/     - The source code for our ActiveX control.
  packaging/   - Tools for making our control automatically downloadble.
  fake_course/ - A fake course for use by the test harness.
  sco/         - A skeleton for a SCO-based course using this control.
                 Currently very incomplete.

Getting Started
---------------

First, you'll need Cygwin:

  http://www.cygwin.com/

Make sure your Cygwin installation uses DOS line endings, not Unix ones!
If this is inccorectly configured, you may have problems with *.dsw and
*.dsp files, and you may need to uninstall and reinstall Cygwin to fix
them.

At a minimum, you'll need to install the Cygwin packages related to Ruby.
If any other packages are required, please edit this file to mention them.

Second, you'll need RubyGems.  Get the latest version from here:

  http://rubyforge.org/frs/?group_id=126

To install RubyGems, you'll need to type something like:

  $ unzip rubygems-0.8.11.zip
  $ cd rubygems-0.8.11
  $ ruby setup.rb

Third, you'll need Rake.  You can get this using RubyGems:

  $ gem install rake

Now you're ready to go!  From within the ScormAiccGw directory, type:

  $ rake --tasks

This will print out a list of available Rake tasks (which are defined in
the Rakefile).

For more information on Rake and Ruby, see:

  http://docs.rubyrake.org/  (documentation for Rake)
  http://www.ruby-doc.org/   (the Programming Ruby book is especially good)

Debugging
---------

See this site for instructions on debugging ActiveX controls:

  http://support.microsoft.com/kb/q167715/

