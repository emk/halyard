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
// StdAfx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined (_StdAfx_h_)
#define _StdAfx_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers

// Use LFileBundle instead of LFileList
#define	 USE_BUNDLE

// Windows Header Files:
#include <windows.h>
#include <wingdi.h>
#include <winuser.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>

// QTML and QuickTime
#include <ConditionalMacros.h>
#include <MacTypes.h>
#include <QuickDraw.h>
#include <MacMemory.h>
#include <QTML.h>
#include <Movies.h>
#include <scrap.h>

// Common library
//#include "TCommon.h"
//#include "TObject.h"
//#include "TPoint.h"
//#include "TRect.h"
//#include "TRandom.h"
//#include "TString.h"
//#include "TArray.h"
//#include "TBTree.h"

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // _StdAfx_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  2000/02/02 15:15:33  chuck
 no message

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
