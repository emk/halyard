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
// KHeader.h : 
//

#if !defined (_KHeader_h_)
#define _KHeader_h_

// WIN32 section
#ifdef WIN32
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

#endif // WIN32

// MacOS section
#if defined (macintosh)
#define MACOS
#include "KCommon.h"
#include <string.h>	

#endif	// macintosh

#endif // _KHeader_h_

/*
 $Log$
 Revision 1.2  2002/02/04 21:35:05  hamon
 More code warrior 7 updates that weren't checked in previously.

 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
