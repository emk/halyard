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
#include "TVariable.h"
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
#include "LHttp.h"
#include "LBrowser.h"

//////////
// Global Screen View
//
extern View					*gView;

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
// Global URL Checker
//
extern LHttp				gHttpTool;

//////////
// Global Web Browser
//
extern LBrowser				gBrowserTool;

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
 Revision 1.4  2002/06/20 16:32:54  emk
 Merged the 'FiveL_3_3_4_refactor_lang_1' branch back into the trunk.  This
 branch contained the following enhancements:

   * Most of the communication between the interpreter and the
     engine now goes through the interfaces defined in
     TInterpreter.h and TPrimitive.h.  Among other things, this
     refactoring makes will make it easier to (1) change the interpreter
     from 5L to Scheme and (2) add portable primitives that work
     the same on both platforms.
   * A new system for handling callbacks.

 I also slipped in the following, unrelated enhancements:

   * MacOS X fixes.  Classic Mac5L once again runs under OS X, and
     there is a new, not-yet-ready-for-prime-time Carbonized build.
   * Bug fixes from the "Fix for 3.4" list.

 Revision 1.3.6.2  2002/06/05 20:42:38  emk
 3.3.4.2 - Broke Win5L dependencies on TIndex file by moving various pieces
 of code into TWin5LInterpreter.  Windows 5L now accesses the interpreter
 through a well-defined API.  Changes:

   * Removed many direct and indirect #includes of TIndex.h.
   * Added a TInterpreter method ReloadScript, which can be called by the
     higher-level ReDoScript command.
   * Checked in some files which should have been included in the 3.3.4.1
     checkin--these files contain the initial refactorings of Card and Macro
     callsites to go through the TInterpreter interface.

 Up next: Refactor various Do* methods out of Card and into a procedural
 database.

 Revision 1.3.6.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.3  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.2.1  2002/04/30 07:57:31  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

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
