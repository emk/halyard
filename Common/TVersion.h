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
// LVersion.h : Version strings.
//

#define MAJOR_NUM		3
#define MINOR_NUM		02
#define REV_BIG			00
#define REV_SMALL		03

#define BUILD_NUM		1

#define VERSION_STRING	"5L for Win32 3.2.0.3"
#define SHORT_NAME		"5L"	


/*
 $Log$
 Revision 1.3.2.4  2002/04/09 13:53:45  emk
 Mouse-click during text entry now works the same as typing RETURN.

 This fixes a common usability problem with our program's login screens--
 people don't read the "Press ENTER or TAB to continue" message, and
 click around blindy.  This is a very tiny UI tweak.

 This code is tagged in CVS as FiveL_3_2_0_3.

 Revision 1.3.2.3  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.


 SOME MERGED BITS:

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

 Revision 1.3.2.2  2002/02/26 14:29:30  tvw
 Bugs #497 and #613

 Quick fix to change the delimiter for underlining text.
 The pipe character ('|') is now used for underlining.

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
