// -*- Mode: C++; tab-width: 4; -*-
/////////////////////////////////////////////////////////////////////////////////////////
//
//		(c) Copyright 1999, Trustees of Dartmouth College. All rights reserved
//			Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//			$Date$
//			$Revision$
//
//
/////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////
//
//	CMac5LApp.cp		   
//
//		Lifted from a Dashboard App for PowerPlant from MetroWerks.
//
//

#include "THeader.h"

#include <UStandardDialogs.h>
#include <Palettes.h>

#include <iostream.h>

#include "TLogger.h"

#include "CConfig.h"
#include "CMac5LApp.h"

#include "CPlayerView.h"
#include "CMoviePlayer.h"
#include "CBackWindow.h"

#include "gamma.h"

#include "TVariable.h"
#include "lang/old5l/CFiles.h"
#include "CCursor.h"
#include "TStartup.h"
#include "TVersion.h"
#include "lang/old5l/TMac5LInterpreter.h"
#include "CHeader.h"
#include "TStyleSheet.h"
#include "TMacPrimitives.h"
#include "TQTPrimitives.h"
#include "CModule.h"

//
// constants
//
const PP::ResIDT	WIND_Mac5L		= 200;
const PP::ResIDT	WIND_Mac5L_d	= 202;

//
// globals
//
bool				gInFront = true;			// is the app window in front?
bool				gPrintToFile = true;		// to print info string
#ifdef DEBUG
bool				gFullScreen = false;		
bool				gHideMenuBar = false;		// to hide menu bar
bool				gPrintDebug = true;
#else
bool				gFullScreen = true;			// use the full screen
bool				gHideMenuBar = true;		// to hide menu bar
bool				gPrintDebug = false;		// normally off
#endif
bool				gDoShiftScript = false;		// by default, start from beginning
PP::LGrowZone		*gGrowZone = NULL;

BEGIN_NAMESPACE_FIVEL

TInterpreterManager *gInterpreterManager;
bool				gHaveLegacyInterpreterManager = false;
CFileList			gFileManager;
CCursorManager		gCursorManager;
CPaletteManager		gPaletteManager;
CPictureManager		gPictureManager;

CPlayerView			*gPlayerView;
CMoviePlayer		gMovieManager;
WindowPtr			gWindow;

CMac5LApp			*gTheApp;

END_NAMESPACE_FIVEL

USING_NAMESPACE_FIVEL
using namespace PowerPlant;

//Handle				clickSound = NULL;

// ===========================================================================
//		¥ Main Program
// ===========================================================================

int FiveL::FiveLmain()
{
	FIVEL_SET_STACK_BASE();

	try
	{
		// Set Debugging options
#ifdef DEBUG
		SetDebugThrow_(debugAction_Nothing);
		SetDebugSignal_(debugAction_Nothing);
		//SetDebugThrow_(debugAction_SourceDebugger);
		//SetDebugSignal_(debugAction_SourceDebugger);
#else
		SetDebugThrow_(debugAction_Nothing);
		SetDebugSignal_(debugAction_Nothing);
#endif

		InitializeHeap(3); // Initialize Memory Manager
		                   // Parameter is number of Master Pointer
						   //   blocks to allocate
	
		// Initialize standard Toolbox managers
#if TARGET_API_MAC_CARBON
		UQDGlobals::InitializeToolbox();
#else
		UQDGlobals::InitializeToolbox(&qd);
#endif
		TLogger::MarkToolboxAsInitialized();
	
		// Install a GrowZone function to catch low memory situations.
		// Parameter is size of reserve memory block to allocate. The first
		// time the GrowZone function is called, there will be at least
		// this much memory left (so you'll have enough memory to alert the
		// user or finish what you are doing).
		gGrowZone = new LGrowZone(100000);

		// cbo_debug
		//if (ProfilerInit(collectDetailed, bestTimeBase, 100, 10))
		//{
		//	gLog.Caution("Couldn't start the profiler");
		//}
		//ProfilerSetStatus(false);
	
		// Check our configuration.
		theConfig = new CConfig;
	
		// if we don't have a good config, don't run
		if (theConfig->CheckConfig())
		{
			gTheApp = new CMac5LApp;	// create instance of application
			
			//gTheApp->CheckMemory();
			
			gTheApp->InitializeInterpreter();
			ASSERT(TInterpreterManager::HaveInstance());
			TInterpreterManager::GetInstance()->Run();
			delete gInterpreterManager;
			gInterpreterManager = NULL;

			//gTheApp->MaxMemory();
		}
		
		if (gTheApp != NULL)
			delete gTheApp;
	}
	catch (std::exception &e)
	{
		gLog.Error("Unexpected internal error: %s.", e.what());
    }
    catch (...)
    {
		gLog.Error("Unexpected, unknown internal error.");
	}

	return (0);
}


// ===========================================================================
//		¥ CMac5LApp Class
// ===========================================================================

void CMac5LApp::EmergencyUnfade()
{
	DoGFade(true, 5, false);
}

FiveL::TString CMac5LApp::ReadSpecialVariable_curcard()
{
	return TInterpreter::GetInstance()->CurCardName();
}

FiveL::TString CMac5LApp::ReadSpecialVariable_prevcard()
{
	return TInterpreter::GetInstance()->PrevCardName();
}

void CMac5LApp::MacIdleProc()
{
	gTheApp->ProcessEvents();
}


// ---------------------------------------------------------------------------
//		¥ CMac5LApp
// ---------------------------------------------------------------------------
//	Constructor

CMac5LApp::CMac5LApp()
{
	RGBColor	rgbBlack = {0, 0, 0};
	KeyMap 		keys;
	Rect		screenBounds;
	bool		centerOnScreen;

	// If the Shift key is down when starting up, do the shift script.
	::GetKeys(keys);
	if ((keys[1] & 1) != 0)
		gDoShiftScript = true;

	// Setup the Gamma tools
	SetupGammaTools();
	
	// Register classes for objects created from 'PPob' resources
	// For PowerPlant classes, you can copy the necessary RegisterClass
	// calls from PPobClasses.cp
	//
	// For your own classes, you must use the same four-character ID as
	// you specify in the 'PPob' resource (or in Constructor).
	// PowerPlant reserves all ID's composed entirely of lower case
	// letters.
	//
	// The convention in PowerPlant is to define a local enumerated
	// constant called class_ID for the four-character ID of each class.

	RegisterClass_(LWindow);
	RegisterClass_(LEditField);
	RegisterClass_(CPlayerView);
	RegisterClass_(CBackWindow);
	
	// Initialize the modules.
	gModMan = new CModuleManager;

	// Initialize our portable Common library.
	InitializeCommonCode();
	
	// Install our Macintosh-specific primitive functions.
	RegisterMacPrimitives();
	RegisterQuickTimePrimitives();

	// Register our platform-specific special variables.
	gVariableManager.RegisterSpecialVariable("_system",
		&ReadSpecialVariable_system);
	gVariableManager.RegisterSpecialVariable("_curcard",
		&ReadSpecialVariable_curcard);
	gVariableManager.RegisterSpecialVariable("_prevcard",
		&ReadSpecialVariable_prevcard);
	gVariableManager.RegisterSpecialVariable("_eof",		
		&CFileList::ReadSpecialVariable_eof);

	// Fade the screen out.
	if (gFullScreen and gHideMenuBar)
		DoGFade(false, 5, false);
	
	// Create and show our window
#ifndef DEBUG
	mDisplayWindow = CBackWindow::CreateWindow(WIND_Mac5L, this);
#else
	mDisplayWindow = CBackWindow::CreateWindow(WIND_Mac5L_d, this);
#endif
	
	// Set our global window pointer.
	gWindow = ::GetWindowFromPort(mDisplayWindow->GetMacPort());
		
	// Make this a subcommander of the main window.
	gPlayerView = (CPlayerView *) mDisplayWindow->FindPaneByID(210);
				
	gPaletteManager.Init();
		
	gCursorManager.Init();
	gCursorManager.ChangeCursor(ARROW_CURSOR);
	gCursorManager.ShowCursor();

	// Set our sleep time.
	mSleepTime = 0;
	
	screenBounds = theConfig->GetScreenRect();
	mScreenRect = screenBounds;
	mBitDepth = theConfig->GetBitDepth();

	centerOnScreen = true;			// by default

#ifdef DEBUG
	if ((not gFullScreen) 
		and (screenBounds.bottom >= 550) 
		and (screenBounds.right >= 750))
	{
		centerOnScreen = false;
	
		screenBounds.top += ::GetMBarHeight() + 50;
		screenBounds.left = 50;
		screenBounds.right = screenBounds.left + gScreenWidth;
		screenBounds.bottom = screenBounds.top + gScreenHeight;
	}
#endif
	
	// set the display window to cover the screen	
	mDisplayWindow->DoSetBounds(screenBounds);
	
	// Ensure all our sub-views are 640x480
	gPlayerView->ResizeFrameTo(gScreenWidth, gScreenHeight, FALSE);
	gPlayerView->CenterInView();
	
	AddAttachment(gPlayerView);								// to get events

	// pick up our click sound
//	clickSound = Get1Resource('snd ', 1001);
//	if (clickSound != NULL)
//		SetResAttrs(clickSound, resLocked);
	
	if (gHideMenuBar)
		::HideMenuBar();
	else
		::ShowMenuBar();

	mDisplayWindow->Show();

	::BringToFront(gWindow);
	gInFront = true;
		 	
	// set up initial values for "global" variables
	SetGlobals();

	// Register a function to unfade the screen before displaying
	// engine errors.
	// XXX - We should do this much sooner, but DoGFade has some
	// nasty dependencies on gView when unfading.  This coupling
	// should be broken.
	TLogger::RegisterErrorPrepFunction(&CMac5LApp::EmergencyUnfade);
	
	// Fade back in (once the window has drawn itself).
	if (gFullScreen and gHideMenuBar)
		mDisplayWindow->FadeAfterDraw();

	// Run our local re-implementation of the first half of LApplication::Run.
	PrepareToRun();
}


// ---------------------------------------------------------------------------
//		¥ ~CMac5LApp
// ---------------------------------------------------------------------------
//	Destructor

CMac5LApp::~CMac5LApp()
{
	DisposeGammaTools();
}

void CMac5LApp::HandleAppleEvent(
	const AppleEvent	&inAppleEvent,
	AppleEvent			&outAEReply,
	AEDesc				&outResult,
	long				inAENumber)
{
	switch (inAENumber) 
	{
		case ae_OpenApp:
			StartUp();
			break;
			
		case ae_OpenDoc:
			DoAEOpenDoc(inAppleEvent, outAEReply, inAENumber);
			break;
			
		case ae_Quit:
			DoQuit();
			break;
			
		default:
			LModelObject::HandleAppleEvent(inAppleEvent, outAEReply, outResult, inAENumber);
			break;
	}
}

void CMac5LApp::DoAEOpenDoc(
	const AppleEvent	&inAppleEvent,
	AppleEvent&			/* outAEReply */,
	long				/* inAENumber */)
{
	// Ignore the OpenDoc event--we no longer open drag & dropped scripts.
}

//
//	StartUp - We have started without a file to open.
//
void CMac5LApp::StartUp(void)
{
	// Ignore the Apple Event--startup is now handled by the
	// TInterpreterManager.
}

//
//	CleanUp - Toss everything.
//
void CMac5LApp::CleanUp(void)
{
	// XXX - We used to kill the interpreter here, but that's now
	// the job of the TInterpreterManager.  Should we ask it to
	// do something on our behalf, or is this routine just
	// useless cruft in the brave new world of interpreter managers?
	gVariableManager.RemoveAll();		// toss all variables
	gPaletteManager.RemoveAll();		// toss all palettes
	gPictureManager.RemoveAll();		// toss all pictures
}

//
//	EventResume - We are overriding LEventDispatcher's EventResume function.
//	We want to HideMenuBar and then call LEventDispatcher's function.
//
void CMac5LApp::EventResume(const EventRecord& inMacEvent)
{
	if (gHideMenuBar)
		::HideMenuBar();
	LApplication::EventResume(inMacEvent);	
}

//
//	EventSuspend - We are overriding LEventDispatcher's EventSuspend function.
//	We want to ShowMenuBar and then call LEventDispatcher's function.
//
void CMac5LApp::EventSuspend(const EventRecord& inMacEvent)		
{
 
	::ShowMenuBar();
	LApplication::EventSuspend(inMacEvent);
}

Boolean CMac5LApp::AttemptQuitSelf(SInt32 /* inSaveOption */)
{
	if (mScriptRunning)
	{
		gPlayerView->KillScript();
	}
	
	mScriptRunning = false;
	
	DoGFade(true, 0, false);	// make sure we aren't faded out
	::ShowMenuBar();			// make sure we have the menu
	::ShowCursor();				// make sure the cursor is still there
		
	// cbo_debug
	//ProfilerDump((unsigned char *) "\pdump.out");
	//ProfilerTerm();
	
	return (true);
}

//
//	GetScriptFile
//
bool CMac5LApp::GetScriptFile(FSSpec *scriptSpec)
{
	// XXX - This *should* be the way to call this under Carbon and/or
	// the regular MacOS, but it hasn't been tested.
	if (PP_StandardDialogs::AskOpenOneFile('TEXT', *scriptSpec))
		return TRUE;
	else
		return FALSE;
}

//
//	QuitScript - Stop running a script.
//
void CMac5LApp::QuitScript(void)
{
	DoQuit();
}

//
//  This code is adapted from PowerPlant's LApplication::Run method, and
//  may need to be updated if that function changes.  This snippet should
//  be short enough to fall under fair use exemptions to copyright law.
//
void CMac5LApp::PrepareToRun()
{
	MakeMenuBar();
	MakeModelDirector();
	Initialize();

	ForceTargetSwitch(this);
	UCursor::Normalize();
	UpdateMenus();
	mState = programState_ProcessingEvents;
}

void CMac5LApp::ProcessEvents()
{
	EventRecord ignored;
    bool is_null_event;

	// Continue to process events as long as (1) PowerPlant wants us
	// to process them and (2) there are non-null events available
	// to process.  It's important to drain this queue if we want the
	// application to be responsive.
	if (mState == programState_ProcessingEvents)
	{
		do
		{
			is_null_event = !::EventAvail(everyEvent, &ignored);
			ProcessNextEvent();
		} while (mState == programState_ProcessingEvents && !is_null_event);
	}

	// If LApplication thinks we're no longer processing events,
	// tell our TInterpreterManager to shut things down.
	if (mState != programState_ProcessingEvents)
		TInterpreterManager::GetInstance()->RequestQuitApplication();
}

//
//	InitializeInterpreter - Find an interpreter to run and get it
//      set up correctly.
//
void CMac5LApp::InitializeInterpreter()
{
	ASSERT(!TInterpreterManager::HaveInstance());

	// set mSleepTime again, just to be sure
	mSleepTime = 0;

	try
	{
		// Create our interpreter manager and get it ready to run.
		gInterpreterManager = MaybeGetSchemeInterpreterManager(&MacIdleProc);
		if (!gInterpreterManager)
		{
			gHaveLegacyInterpreterManager = true;
			gInterpreterManager = new TMac5LInterpreterManager(&MacIdleProc);
		}
		mScriptRunning = true;
		gVariableManager.SetString("_lpstat", "0");
		gVariableManager.SetString("_lpactive", "0");
		gVariableManager.SetString("_movieplaying", "0");
		gPlayerView->Activate();
	}
	catch (...)
	{
		if (gInterpreterManager)
		{
			delete gInterpreterManager;
			gInterpreterManager = NULL;
		}
		throw;
	}
}

#ifdef DEBUG

//
//	ReDoScript - Read in the current script from disc and start again.
//
void CMac5LApp::ReDoScript(const char *curCard)
{
	mScriptRunning = false;

	TInterpreter::GetInstance()->KillCurrentCard();
	
	if (gMovieManager.Playing())
		gMovieManager.Kill();
		
	gPlayerView->KillTZones();

	gDebugLog.Log("Killing everything for redoscript");
	
	gStyleSheetManager.RemoveAll();
	gHeaderManager.RemoveAll();

	// XXX - Does this potentially destroy an object in our call chain?
	if (!gHaveLegacyInterpreterManager)
		gPlayerView->DeleteAllKeyBinds();

	TInterpreterManager::GetInstance()->RequestReloadScript(curCard);
}

//
//	ObeyCommand - 
//
Boolean CMac5LApp::ObeyCommand(CommandT inCommand, void *ioParam)
{
	if (inCommand == cmd_Revert)
	{
		if (TInterpreterManager::HaveInstance() &&
			TInterpreterManager::GetInstance()->FailedToLoad())
		{
			gInterpreterManager->RequestRetryLoadScript();
			return (true);
		}
	}
	
	return (LApplication::ObeyCommand(inCommand, ioParam));
}

//
//	FindCommandStatus -
//
void CMac5LApp::FindCommandStatus(CommandT inCommand,
	Boolean& outEnabled, Boolean& outUsesMark,
	UInt16& outMark, Str255 outName)
{
	if (inCommand == cmd_Revert)
	{
		outUsesMark = false;
		
		if (TInterpreterManager::HaveInstance() &&
			TInterpreterManager::GetInstance()->FailedToLoad())
			outEnabled = true;
		else
			outEnabled = false;
			
		return;
	}
	
	LApplication::FindCommandStatus(inCommand, outEnabled, 
									outUsesMark, outMark, outName);
}

#endif

static long		numTimesChecked = 0;
static long		maxFreeMem = 0;
static long		maxTempFreeMem = 0;
static long		minFreeMem = 0;
static long		minTempFreeMem = 0;

void CMac5LApp::CheckMemory(void)
{
	long			mem;
	long			memUsed;

	numTimesChecked++;			
	mem = ::FreeMem();
	if (mem > maxFreeMem)
		maxFreeMem = mem;
	memUsed = maxFreeMem - mem;
	if (minFreeMem == 0)
		minFreeMem = mem;
	else if (mem < minFreeMem)
		minFreeMem = mem;
	//gLog.Log("FreeMem used: %d", memUsed);
	
	mem = ::TempFreeMem();
	if (mem > maxTempFreeMem)
		maxTempFreeMem = mem;
	memUsed = maxTempFreeMem - mem;
	if (minTempFreeMem == 0)
		minTempFreeMem = mem;
	else if (mem < minTempFreeMem)
		minTempFreeMem = mem;
	//gLog.Log("TempFreeMem used: %d", memUsed);
}

void CMac5LApp::DumpMemory(void)
{
	CheckMemory();
	
	long		mem = 0;
	
	mem = ::FreeMem();
	gLog.Log("FreeMem: %d", mem);
	mem = 0;
	mem = ::TempFreeMem();
	gLog.Log("TempFreeMem: %d", mem);
	mem = gPictureManager.CacheSize();
	gLog.Log("Resource cache: %d", mem);
}

void CMac5LApp::MaxMemory(void)
{
	gLog.Log("Memory statistics: number of times checked: %d", numTimesChecked);
	gLog.Log("Lowest number returned from FreeMem: %d", minFreeMem);
	gLog.Log("Highest number returned from FreeMem: %d", maxFreeMem);
	gLog.Log("Difference: %d", maxFreeMem - minFreeMem);
	gLog.Log("Lowest number returned from TempFreeMem: %d", minTempFreeMem);
	gLog.Log("Highest number returned from TempFreeMem: %d", maxTempFreeMem);
	gLog.Log("Difference: %d", maxTempFreeMem - minTempFreeMem);
}

void CMac5LApp::SetGlobals(void)
{
	// set up initial values for "global" variables
	gVariableManager.SetString("_NoCheckDisc", "0");		// check for discs by default
	gVariableManager.SetString("_locked", "0");				// start off unlocked
	gVariableManager.SetString("_graphpal", "NULL");		// something dumb in graphpal
	gVariableManager.SetString("_faded", "0");				// we aren't faded
	gVariableManager.SetString("_error", "0");
	gVariableManager.SetString("_lpstat", "0");
	gVariableManager.SetString("_lpactive", "0");
	gVariableManager.SetString("_movieplaying", "0");
	gVariableManager.SetLong("_resx", mScreenRect.right);
	gVariableManager.SetLong("_resy", mScreenRect.bottom);
	gVariableManager.SetLong("_bitdepth", mBitDepth);
	
#ifdef DEBUG
	gVariableManager.SetString("_debug", "1");
#else
	gVariableManager.SetString("_debug", "0");
#endif

	// set the engine build variables
	gVariableManager.SetString("_enginebuildstr", VERSION_STRING);
	
	int32	buildNum;
	
	buildNum = 10000 * VERSION_MAJOR_NUM;
	buildNum += (100 * VERSION_MINOR_NUM);
	buildNum += VERSION_REV_BIG;
	gVariableManager.SetLong("_enginebuild", buildNum);
}

/* 
$Log$
Revision 1.26  2002/10/28 17:13:35  emk
Removed some 640x480 constants from the code.

Revision 1.25  2002/10/03 19:53:14  emk
3.5.6 - 3 Oct 2002 - emk

  * Refactored CHeader in the same fashion as Header and TStyleSheet,
    which should allow headers (and therefore input) to be used from
    Scheme.  This may also fix a redoscript problem in old5l.
  * Merged "cursor during video" fix from 3.4.2.
  * Built new Windows engines with the MzScheme 202 code.  You'll
    need to get the newest version of the appropriate libmzsch and
    libmzgc DLLs.

Revision 1.24  2002/10/03 02:47:00  emk
3.5.5 - 1 Oct 2002 - emk

Preliminary MzScheme support for Macintosh engine.  DO NOT BUILD THE
NON-CARBON POWERPC BUILD!  WHEN RUN UNDER OS X'S OS 9 EMULATOR, IT WILL
CORRUPT YOUR OS 9 SYSTEM FILE.

For legal reasons, this Macintosh engine SHOULD NOT BE DISTRIBUTED outside
of IML until we do our open source release.  It's statically-linked against
the LGPL'd MzSchemeLib, which would require us to jump through some
annoying hoops if we wanted to ship proprietary binaries.

User-visible changes:

  * This engine supports both legacy 5L and Scheme.
  * No PowerPC build.
  * No more start scripts.  Move everything into your main script.
  * No more switching between scripts.
  * CD ejection and fades are gone, thanks to the fact this build is
    Carbon-only.

Engine changes:

  * Created CodeWarrior project for MzSchemeLib, to avoid the pay-per-view
    cage match between MPW StdCLib and Metrowerks Standard Library over
    who's going to link with what.
  * Fixed MzScheme's call to PBGetCatInfo to work under Carbon.
  * Added code to explicitly set Boehm GC stack base on the Macintosh.
    Boehm can't figure this out without help, and will crash on startup.
  * Namespace fixes to make things compile on the Mac.
  * Made a mess of the MzSchemeLib MPW build infrastructure trying to get
    things to work.  I'm checking in my hacked-up version because (1) it
    marginally works and (2) we might need it again someday.
  * Removed a number of unused files with licensing problems, and sorted
    the rest of the problematic files into categories.
  * Changed event-handling model to give the TInterpreterManager object control,
    instead of the traditional PowerPlant::LApplication::Run method.  This
    requires us to roll our own version of Run and do some tricky event
    handling in CMac5LApp::MacIdleProc.  But it also means the Mac engine
    now supports arbitrary interpreters.  This change has lots of
    consequences, most of them complicated--and it may affect stability.
  * CPlayerView is now more careful about checking whether a TInterpreter
    object actually exists before calling methods on it.
  * Renamed MakeNewIndex methods to ProcessTopLevelForm.
  * Moved all old5l-related files into Mac/Source/lang/old5l.
  * Moved interpreter-managing code into TMac5LInterpreter.{h,cpp}.
  * Moved file-related primitives into TMac5LPrimitives.cpp.
  * Shift-scripts are gone.
  * Ripped out support for switching scripts.
  * Reimplemented script reloading, exiting, etc., using the
    TInterpreterManager interface.
  * Lots of changes to the Macintosh project files.
  * Updated Windows interpreter to newest MzScheme release.
  * Fixed the Linux build.

Headers (and therefore text input) are only available to the old 5L
language.  This will change soon.

Revision 1.23  2002/07/24 01:17:32  emk
3.3.18 - 23 July 2002 - emk

  * Forward-ported QtComponentVersion to the Mac (bug #1054).
  * Fixed redoscript/keybind race condition (bug #1036).
  * Added a top-level try/catch block on the Mac (bug #955).
  * Added error checking in Mac BROWSE (bug #1070).
  * Enlarged our growzone a bit, and made sure it's working.
    This will help us determine if we have memory usage problems.

Revision 1.22  2002/06/20 21:02:26  emk
3.3.7 - Debug log updates, and error message if 5L is run without
Mac5L.config or other support files.

Revision 1.21  2002/06/20 16:32:57  emk
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

Revision 1.20.6.4  2002/06/19 22:51:32  emk
3.3.4.11 - Refactored Mac code to move primitives from CCard.{h,cpp} to
TMacPrimitives.{h,cpp}, and break most of the remaining dependencies on
the 5L interpreter.

Language changes: LOADPICK, RVAR and RNODE are gone.  I've also disabled
the Mac PAUSE command until Douglas tells me how it should work.

Testing: Please beat *very* hard on this build, and pay special attention
to WAIT, NAP, TIMEOUT, and similar commands.

Next up: I plan to merge this branch into HEAD tomorrow.

Revision 1.20.6.3  2002/06/18 21:57:02  emk
3.3.4.8 - Added (BODY ...) command on Mac, fixed arguments of BUTTPCX, TOUCH,
and KEYBIND to match Win32 engine, and refactored Mac engine to more-or-less
respect the TInterpreter interface.

Things to test: REDOSCRIPT, redo-REDOSCRIPT (feed REDOSCRIPT a bogus script,
try to fix it, then run REDOSCRIPT again), TOUCH, BUTTPCX, ORIGIN.

Some low-level details:

  - Added a KillCurrentCard method to the TInterpreter interface.  This
    works a lot like Pause, but it cannot be resumed.
  - Added a rough-cut TMac5LInterpreter class (with some methods stubbed
    out, because they are not needed on the Mac--we should look at
    this API in detail and formalize it sometime after 3.4).
  - Modified CTouchZone to take TCallback objects.
  - Modified CPlayerView's keybinding support to take TCallback objects
    (and to use a std::map instead of a PowerPlant list class).
  - Began to separate special forms (IF, BODY, EXIT, RETURN) from other
    commands.
  - Moved ReadSpecialVariable_* methods out of CCard and into CMac5LApp.
  - Made sure CMac5LApp::mReDoReDo got initialized to false.
  - Merged OpenScript and OpenScriptAgain into one function.

Revision 1.20.6.2  2002/06/15 01:06:55  emk
3.3.4.7 - Carbonization of Mac build, support for running non-Carbonized build
in MacOS X's OS 9 emulator, and basic support for 5L.prefs on the Mac.  The
Carbon build isn't yet ready for prime time--see BugHunt for details--but it
is good enough to use for engine development.

* Language changes

  - CHECKDISC is gone; use CHECKVOL instead.
  - EJECT is disabled in the Carbon build, because Carbon has no way to
    identify CD drives reliably.  EJECT still works in the regular build.
  - Gamma fades are ignored in the Carbon build.
  - KEYBINDs must now be accessed with the Command key only--not Option.

* Things to test

Please be hugely brutal to 5L; this is a big update.

  - 8-bit systems, palettes, ORIGIN, EJECT on the non-Carbon build.

* Internal changes

  - TException class (and all subclasses) now take a __FILE__ and __LINE__
    parameter.  This is ugly, but it allows me to debug 5L exceptions even
    without a working debugger (under the OS 9 emulator, for example).
  - FileSystem::Path::(DoesExist|IsRegularFile|IsDirectory) now rely on
    native MacOS File Manager calls instead of the broken MSL stat()
    function (which fails in the OS 9 emulator).
  - The ImlUnit test harness flushes its output more often.
  - Many data structure accessors (and such functions as c2pstr) have been
    replaced by their Carbon equivalents.
  - We now use PowerPlant accessors to get at the QuickDraw globals.
  - We now use PowerPlant calls in place of ValidRect and InvalRect.
  - Some very nasty code which set the palettes of our offscreen GWorlds
    has been removed (offscreen GWorlds have CLUTs, not palettes!).
    The various drawing commands now use gPaletteManager to map indexes
    to RGBColor values, and RGBForeColor to set the color--no more calls
    to ::PmForeColor on offscreen GWorlds, thank you!
  - The CMenuUtil code (which used low-memory system globals to hide
    and show the menu bar) has been removed entirely and replaced by
    calls to HideMenuBar and ShowMenuBar (which are present in 8.5 and
    Carbon).  This is much simpler, nicer, more portable and safer.
  - A bunch of code which had been disabled with #ifdefs has been
    removed entirely.  This mostly related to palettes and an obsolete
    version of the fade code which used GWorlds.
  - Code which used ROM-based KCHR resources to map option keys back to
    their unmodified key caps has been removed.  This means KEYBINDs
    can only be accessed using the Command key.
  - We assume Carbon systems always support the HFS file system (duh).
  - We use PowerPlant glue to access either StandardFile or Navigation
    Services, under OS 8/9 and Carbon, respectively.
  - Some old subroutines in CModuleManager appeared to have been
    snarfed from More Files, an old Mac utility library.  These have
    been moved into MoreFiles.{h,cpp}.

* Known Carbon Problems

Fades, ejecting CD-ROMs and playing QuickTime movies are all broken in
the Carbon build.  Douglas has found a problem with ORIGIN.  It looks
like we should continue to ship the OS 9 build for use with MacOS X,
at least for next few months.

Revision 1.20.6.1  2002/06/13 14:11:40  emk
Basic fixes to make updated Common library build on the Macintosh.

Revision 1.20  2002/05/15 11:05:27  emk
3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
is available, and several subsystems have been refactored.  For more
detailed descriptions, see the CVS branch.

The merged Mac code hasn't been built yet; I'll take care of that next.


Revision 1.19  2002/05/03 10:37:01  hamon
Deleted #ifdef DEBUG lines surrounding gDebugLog.Log messages

Changes by Elizabeth.

Revision 1.18.2.5  2002/05/15 09:09:57  emk
Added code to unfading the screen before displaying errors (this is needed to play nicely with the Mac gamma fader).

Migrated the Mac (buttpcx ...) to the new anti-aliased typography library.

The TBTree destructor is still a broken nightmare, especially on FatalError's forced shutdowns.  Expect FiveL to do something childish immediately after fatal errors and assertion failures.

Revision 1.18.2.4  2002/04/25 11:34:25  emk
Added a (defstyle ...) top-level form, and a (textaa ...) command for drawing anti-aliased text.

Revision 1.18.2.3  2002/04/24 04:34:53  emk
Added static member functions to CCard, FFiles, CMac5LApp to implement various 5L special variables that depend on unmerged and/or non-portable code.

Revision 1.18.2.2  2002/04/23 11:36:07  emk
More merge-related fixes.

1) Removed all code which treats scripts as FSSpecs, and replaced it with code that treats scripts as filenames, minus the path and extension.  This mirrors how TFileIndex wants to work.  A side effect: Dragging scripts to the engine will no longer work (if it ever did).

2) Use TVersion to get the version number.

3) KLogger -> TLogger changes.

Revision 1.18.2.1  2002/04/22 13:20:08  emk
Major Mac cleanups:

- We use subprojects to recursively build 5L and its support libraries.  This means no more opening up four different projects, etc., just to do a build.

- Search & replaced class names for merged classes.  This doesn't quite work or build yet, but I'm working on a branch, so that's OK.

- Other Mac build fixes.

Tomorrow: Get things running again, make a test binary, and continue merging.

Revision 1.18  2002/04/19 09:46:30  emk
Changes to make the Macintosh version build again for the 3.3.1 release, and to allow Elizabeth to work on the Macintosh without any trouble.

Revision 1.17  2002/03/28 20:22:29  emk
FreeType2 build fixes for the Mac:

  - Added a CodeWarrior project file with the correct extension, no text/binary corruption, and the options we want.

  - Disabled support for loading FOND-format Macintosh fonts, because the FreeType2 code relies on a MacOS X function: FSOpenResourceFile.

Revision 1.16  2002/03/11 17:51:47  hamon
Added new version numbers ( 2.02 b3) to files. Release notes lists additions in this version.
Changes by Elizabeth, okayed by Eric.

Revision 1.15  2002/03/06 16:29:28  hamon
Added a new file main.cp. main.cp has the function main, and it calls 5LMain in CMac5LApp.
 (Previously main() was in CMac5LApp and it has been renamed to 5LMain ). 
This allows the test suite to have its own call to main
  without conflicts because two functions are named main. 

Changes by Eric and Elizabeth, okayed by Eric

Revision 1.14  2002/03/04 15:41:49  hamon
Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.

Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.

Changes by Elizabeth and Eric, okayed by Eric.

Revision 1.13  2002/02/26 12:42:19  hamon
Made changes in order to show and hide the menu bar correctly when launching  the internet browser and returning to the program.

We overrode EventResume and EventSuspend functions of LEventDispatcher in CMac5LApp to do this and then call the inherited version.

We also deleted an unnecessary call to HideMenuBar found in CPlayerView.

We added ShowMenuBar to the DoBrowse command in CCard.cp. We cannot rely on EventSuspend to do this correctly - see comment in code.

Changes by Elizabeth Hamon, okayed by Eric.

Revision 1.12  2002/02/04 20:04:28  hamon
Updated Macintosh engine to compile under CodeWarrior 7.0 (Professional) and tested it lightly.  This will give us a base for future development and testing.

* Changed IntN types to SIntN types.
* Added some new libraries.
* Renamed LTimerTask to CTimerTask to avoid conflict with built-in Metrowerks class.
* Included C <string.h> header as needed.  This also affects one file in the Common directory, which we'll need to merge into Stable/ later on.
* We no longer init TString with 'nil', because of function overloading errors.  We use "", the empty string, instead.  We *think* this is a safe change, but the TString code is pretty iffy.
* Replaced a call to 'max', which can no longer be found in the system headers, with an explicit if statement.

Changes by Elizabeth Hamon with help from Eric Kidd.  Code reviewed by Eric.

Revision 1.11  2000/06/15 13:03:07  chuck
2.01 b4

Revision 1.10  2000/05/11 12:56:10  chuck
v 2.01 b1

Revision 1.9  2000/02/01 16:50:49  chuck
Fix cursors on overlapping touch zones.

Revision 1.8  1999/12/16 17:30:56  chuck
no message

Revision 1.7  1999/11/16 13:45:31  chuck
no message

Revision 1.6  1999/10/22 20:42:27  chuck
New cursor management

Revision 1.5  1999/10/21 17:21:05  chuck
Mac5L 2.00 b3

Revision 1.4  1999/10/14 11:59:59  chuck
no message

Revision 1.3  1999/10/07 12:37:53  chuck
Initial revision

*/
