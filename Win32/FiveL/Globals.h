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
// Globals.h : Declare all globals in FiveL.
//

#if !defined (_Globals_h_)
#define _Globals_h_

#include "Debug.h"

#include "View.h"
#include "Card.h"
#include "Macro.h"
#include "Header.h"
#include "Variable.h"
#include "TLogger.h"
#include "Video.h"
#include "Audio.h"
#include "LCursor.h"

#if defined USE_BUNDLE 
	#include "LFileBundle.h"
#else
	#include "LFiles.h"
#endif

#include "Config.h"
#include "SysInfo.h"
#include "LPicture.h"
#include "LPalette.h"
#include "LFont.h"
#include "LTouchZone.h"
#include "LCommandKey.h"
#include "Input.h"
#include "Index.h"
#include "LHttp.h"
#include "LBrowser.h"

//////////
// Global Screen View
//
extern View					*gView;

//////////
// Global Card Manager
//
extern CardManager			gCardManager;

//////////
// Global Macro Manager
//
extern MacroManager			gMacroManager;

//////////
// Global Header Manager
//
extern HeaderManager		gHeaderManager;

//////////
// Global Variable Manager
//
extern VariableManager		gVariableManager;

//////////
// Global TouchZone Manager
//
extern LTouchZoneManager	gTouchZoneManager;    

//////////
// Global Video Manager
//
extern VideoManager			gVideoManager;

//////////
// Global Audio Manager
//
extern AudioManager			gAudioManager; 

//////////
// Global Cursor Manager
//
extern LCursorManager		gCursorManager;

//////////
// Global Logger
//
extern TLogger				gLog;

//////////
// Global Missing Media Logger
//
extern TLogger				gMissingMediaLog;

//////////
// Global File Manager
//
#if defined USE_BUNDLE
	extern LFileBundle			gFileManager;
//////////
// Global File Manager
//
#else
	extern LFileList			gFileManager;
#endif

//////////
// Global Config Manager
//
extern ConfigManager		gConfigManager;

//////////
// Global System Info
//
extern SysInfo				gSysInfo; 

//////////
// Global Picture Manager
//
extern LPictureManager		gPictureManager;

//////////
// Global Palette Manager
//
extern LPaletteManager		gPaletteManager;

//////////
// Global Font Manager
//
extern LFontManager			gFontManager;

//////////
// Global CommandKey Manager
//
extern LCommandKeyManager	gCommandKeyManager;

//////////
// Global Input Manager
//
extern InputManager			gInputManager;

//////////
// Defines a complete drawing environment for color graphics operations in Quicktime.
//
extern CGrafPtr				gGrafPtr;

//////////
// Quicktime graphics world pointer.
//
extern GWorldPtr			gDummyGWorldPtr;

//////////
// Virtual screen used for FiveL
//
extern TRect				gScreenRect;

//////////
// Global IndexFile Manager
//
extern IndexFileManager		gIndexFileManager;

//////////
// Global URL Checker
//
extern LHttp				gHttpTool;

//////////
// Global Web Browser
//
extern LBrowser				gBrowserTool;

//////////
// Global Debug Logger
//
extern TLogger				gDebugLog;

//////////
// Global handle to Win32 window
//
extern HWND 			hwndApp;

//////////
// Global handle to Win32 application instance
//
extern HINSTANCE		hAppInst;

//////////
// Horizontal screen resolution of the virtual screen (see gScreenRect)
//
extern int H_SCREEN;

//////////
// Vertical screen resolution of the virtual screen (see gScreenRect)
//
extern int V_SCREEN;

#endif // _Globals_h_

/*
 $Log$
 Revision 1.1.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

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
