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
// THeader.h : Includes headers required by specific platforms.
//

#if !defined (_THeader_h_)
#define _THeader_h_


//=========================================================================
//  Win32 Configuration
//=========================================================================

#if defined(WIN32)

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <windows.h>
#include <crtdbg.h>

// QTML and QuickTime
#include <ConditionalMacros.h>
#include <MacTypes.h>
#include <QuickDraw.h>
#include <MacMemory.h>
#include <QTML.h>
#include <Movies.h>
#include <FixMath.h>
#include <Scrap.h>


//=========================================================================
//  Macintosh Configuration
//=========================================================================

#elif defined(macintosh)

// MacOS section
#define MACOS
#include "TCommon.h"
#include <string.h>	


//=========================================================================
//  Other Platform Configuration
//=========================================================================
//  We provide primitive support for building the Common/ module on
//  non-Macintosh, non-Windows platforms.  There isn't any very good reason
//  for this right now, except perhaps writing small command-line utilties
//  that use 5L's data structures.
//
//  We may eventually want to handle all non-Mac, non-Windows platforms
//  with autoconf.

#else

#include <string.h>	
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#endif


#endif // _THeader_h_


/*
 $Log$
 Revision 1.2  2002/02/27 16:38:21  emk
 Cross-platform code merge!

 * Merged back in support for the Macintosh platform.  This is an ongoing
   process, and we'll need to do more work.

 * Separated out platform specific configuration with big block comments.

 * Merged in a few changes from KBTree which appeared to fix bugs.

 * Merged in IntToString, UIntToString, DoubleToString changes from the
   Macintosh engine, and improved the error handling.  NOTE: doubles now
   print using "%f" (the way the Mac engine always did it).  This means
   that "tstr = 0.0" will set 'tstr' to "0.000000", not "0" (as it
   did in the Win32 engine).

 This code might not build on Windows.  As soon as I hear from ssharp
 that he's updated the project files, I'll test it myself.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
