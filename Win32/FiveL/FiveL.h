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

/*-----------------------------------------------------------------

LIBRARY
    FiveL

OVERVIEW
	5L is a scripting language developed by the Interactive Media Lab
	at Dartmouth Medical School. The language has a basic set of commands
	that allow the scriptor to control both the media elements needed in
	the program and the logical structure needed to support interactivity.

FIVEL MACRO DEFINITIONS
	#define FIVEL_TIMER_MSECS<br>

AUTHOR
    Chuck Officer<br>
	Sean Sharp<br>
	Yijin He

-----------------------------------------------------------------*/

#if !defined (_FiveL_h_)
#define _FiveL_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "resource.h"
#include "TVersion.h"
#include "LCommon.h"

#define FIVEL_TIMER_MSECS		100

//////////
// Supports the redoscript command, allowing the script developer to recompile
// scripts in real-time
//
// [in] inCardName - the name of the Card to jump to after recompiling the script
//
void	ReDoScript(TString &inCardName);

//////////
// Supports switching between scripts on exit.  All scripts should be specified 
// in the config file.
//
// [in] inScript - the script number to switch to (as defined in config file)
//
void	SwitchScripts(int32 inScript);

//////////
// Shutdown the 5L application.
//
// [in_optional] Toss - If true, calls a cleanup routine that tells each manager
//			            to toss everything in its index (default true)
//
void	ShutDown(bool Toss = true);

//////////
// Set the "global" variables that the engine maintains. These should not be 
// confused with "special" variables that the variable manager intercepts
// and maintains.
//
void	SetGlobals(void);

//////////
// Put the 5L application in the background (i.e. minimize)
//
void	PutInBackground(void);

//////////
// Called when the 5L application returns to the foreground as the current application
//
void	PutInForeground(void);


#endif // _FiveL_h_

/*
 $Log$
 Revision 1.4  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.2.1  2002/04/30 07:57:31  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.3  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

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

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/08/08 19:03:40  chuck
 no message

 Revision 1.4  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
