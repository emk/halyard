//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TVersion.h : Version strings.
//

#define VERSION_MAJOR_NUM	3
#define VERSION_MINOR_NUM	03
#define VERSION_REV_BIG		02
#define VERSION_REV_SMALL	06

#define VERSION_STRING	"5L 3.3.2.6 (Development, FiveL_3_2_2_emk_typography_merge)"
#define SHORT_NAME		"5L"


/*
 $Log$
 Revision 1.9.2.7  2002/05/01 03:27:02  emk
 3.3.2.6 - First Windows engine with (textaa ...) command.

 - Implemented a primitive, slow Image::DrawPixMap command that uses
 ::GetPixel and ::SetPixel to do alpha blending (shudder).  Strangely
 enough, it's about as fast as the somewhat optimized Mac routines.
 Anyone got a good GDI book?

 - Fixed several assertion failures.

 Known problems:

 - Occasional assertion failure on exit.  The reference-counting on
 TIndexFile claims it's getting dereferenced too many times.  This is
 an old bug; all the TBTree and TBNode classes are pretty dodgy.

 - Assertion failure on "Special Variables" screen in 5Ltest.  This is
 caused by overlong lines.

 Revision 1.9.2.6  2002/04/30 07:57:24  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.9.2.5  2002/04/29 06:29:58  emk
 3.3.2.4 - Contains first set of performance tweaks, and fixes problem with assertion failures after reload and on CME screens.

 Revision 1.9.2.4  2002/04/26 11:31:01  emk
 3.3.2.3 - Fixed highlight shadow color to default to regular shadow color (instead of highlight color, which is obviously wrong).

 Revision 1.9.2.3  2002/04/26 08:51:21  emk
 3.3.2.2 - First experimental engine with (textaa ...) and (defstyle ...) commands.

 Changes:

   - Ported new TEncoding template class to the Mac.

   - Updated TStyleSheet to provide bug-for-bug compatibility with the way backslashed escaped sequences are processed.

 Revision 1.9.2.2  2002/04/23 11:29:47  emk
 Prepended "VERSION_" to the version-related preprocessor defines, because this way is (1) nicer and (2) matches the Mac engine's preprocessor defines.

 Revision 1.9.2.1  2002/04/19 11:20:13  emk
 Start of the heavy typography merging work.  I'm doing this on a branch
 so I don't cause problems for any of the other developers.

 Alpha-blend text colors.

 Merged Mac and Windows versions of several files into the Common directory.
 Not all of these work on Mac and/or Windows yet, but they're getting there.
 Primary sources for the merged code are:

   Win/FiveL/LVersion.h -> Common/TVersion.h
   Win/FiveL/LStream.h -> Common/TStream.h
   Mac/Source/CStream.cp -> Common/TStream.cpp
   Mac/Source/CStreamTests.cp -> Common/TStreamTests.cpp

 TStream changes:

   * The TStream code now uses a callback to variable values.  This will
     probably go away once Variable and CVariable get merged.
   * Input operators for std::string and GraphicTools::Color.

 Isolated Windows-specific code in TLogger.*, in preparation for a big merge.

   * Added a portable function to set up logging.
   * Fixed the logging code to use the portable FileSystem library.
   * Made FatalError actually quit the application.

 Turned off the FiveL namespace on FIVEL_PLATFORM_OTHER, so we can debug
 with GDB, which has a few minor but painful namespace issues.

 TString changes:

   * Made sure we can convert from std::string to a TString.
   * Added some more assertions.
   * Fixed bug in various operator= methods which would allow the string's
     internal data pointer to be NULL.
   * Changed operator[] and operator() arguments to be 'int' instead of
     'int32' to avoid nasty compiler warnings.

 Typography::Style changes:

   * Added a "ShadowOffset" field that specifies the offset of the
     drop shadow.
   * Added an operator== for testing.
   * Added a ToggleFaceStyle method for toggling specified face style bits.

 Typography::StyledText changes:

   * Added a method to append a single character.

 Other Typography changes:

   * Made FaceStyle an int, not an enum, so we can do bit math with it.
   * Added assertions to made sure you can't extract a StyledText iterator
     until you've called EndConstruction.

 Revision 1.9  2002/04/19 10:21:52  hyjin
 Added support for a movie controller in 5L applications, and deleted some buggy pre-roll code that appeared to be causing crashes.  We're not a hundred percent sure all the crashing problems are fixed, but things seem to be working very well.  Please test this extensively!

 Set global variable _bShowMC to see the movie controller (case insensitive).

 Changes by Yijin, reviewed by Eric Kidd.

 Revision 1.8  2002/04/19 06:02:57  emk
 Merged in MD5-replacement changes from 3.2.0.4.

 This code will become 3.3.1.  PLEASE USE THIS VERSION IN PREFERENCE TO THE 3.3.0
 OR OLDER BINARIES, WHICH THE FSF's LEGAL COUNCIL HAS ASKED US NOT TO SHIP.

 Revision 1.7  2002/03/29 11:14:42  emk
 Final Win32 fixes for 3.3.0 development release.

 Revision 1.6  2002/02/27 13:21:12  tvw
 Bug #613 - Changed calculation of _INCR_Y to include descenders
 (part or letter that goes below baseline).

 Revision 1.5  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.4  2002/02/19 11:41:38  tvw
 Merged from branch FiveL_3_1_1_Stabilization

 Revision 1.3.2.1  2002/02/19 10:20:23  tvw
 Stable build v3.2.0 (based on v3.1.1)

 Revision 1.3  2002/01/24 19:22:41  tvw
 Fixed bug (#531) in -D command-line option causing
 system registry read error.

 Revision 1.2  2002/01/23 20:39:20  tvw
 A group of changes to support a new stable build.

 (1) Only a single instance of the FiveL executable may run.

 (2) New command-line option "-D" used to lookup the installation directory in the system registry.
     Note: Underscores will be parsed as spaces(" ").
     Ex: FiveL -D HIV_Prevention_Counseling

 (3) Slow down the flash on buttpcx so it can be seen on
     fast machines.  A 200 mS pause was added.

 (4) Several bugfixes to prevent possible crashes when error
     conditions occur.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.14  2000/08/08 19:03:41  chuck
 no message

 Revision 1.13  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.12  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.11  2000/02/02 15:15:32  chuck
 no message

 Revision 1.10  2000/01/04 13:32:56  chuck
 New cursors

 Revision 1.9  1999/12/16 17:17:36  chuck
 no message

 Revision 1.8  1999/11/16 13:46:32  chuck
 no message

 Revision 1.7  1999/11/04 14:18:50  chuck
 2.00 Build 10

 Revision 1.6  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.5  1999/10/27 19:42:40  chuck
 Better cursor management

 Revision 1.4  1999/10/22 20:29:09  chuck
 New cursor management.

 Revision 1.3  1999/09/28 15:14:08  chuck
 no message

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
