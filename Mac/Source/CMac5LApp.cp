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

#include <Palettes.h>

#include <iostream.h>

#include "KLogger.h"

#include "CConfig.h"
#include "CMac5LApp.h"
#include "CModule.h"

#include "CPlayerView.h"
#include "CMoviePlayer.h"
#include "CBackWindow.h"

#include "gamma.h"
#include "CMenuUtil.h"

#include "CCard.h"
#include "CMacroManager.h"
#include "CHeader.h"
#include "CVariable.h"
#include "CFiles.h"
#include "CIndex.h"
#include "CCursor.h"


// build stuff
#define VERSION_STRING	"5L for MacOS 2.02, build 3"
#define VERSION_MAJOR_NUM	2
#define VERSION_MINOR_NUM	02
#define VERSION_BUILD_NUM	03

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

CMenuUtil			gMenuUtil;

BEGIN_NAMESPACE_FIVEL

CFileList			gFileManager;
CCardManager		gCardManager;
CMacroManager		gMacroManager;
CHeaderManager		gHeaderManager;

CVariableManager	gVariableManager;
CCursorManager		gCursorManager;
CIndexFileManager	gIndexFileManager;
CPaletteManager		gPaletteManager;
CPictureManager		gPictureManager;

CPlayerView			*gPlayerView;
CMoviePlayer		gMovieManager;
WindowPtr			gWindow;

KLogger				gLog;
KLogger				gMissingMediaLog;
#ifdef DEBUG
KLogger				gDebugLog;
#endif

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

	InitializeHeap(3);				// Initialize Memory Manager
									// Parameter is number of Master Pointer
									//   blocks to allocate
	
									// Initialize standard Toolbox managers
	UQDGlobals::InitializeToolbox(&qd);
	
	new LGrowZone(20000);			// Install a GrowZone function to catch
									//    low memory situations.
									//    Parameter is size of reserve memory
									//    block to allocate. The first time
									//    the GrowZone function is called,
									//    there will be at least this much
									//    memory left (so you'll have enough
									//    memory to alert the user or finish
									//    what you are doing).

	// cbo_debug
	//if (ProfilerInit(collectDetailed, bestTimeBase, 100, 10))
	//{
	//	gLog.Caution("Couldn't start the profiler");
	//}
	//ProfilerSetStatus(false);
	
	// Check our configuration.
	theConfig = new CConfig;
	
	if (theConfig->CheckConfig())	// if we don't have a good config, don't run
	{
		gTheApp = new CMac5LApp;	// create instance of application

		//gTheApp->CheckMemory();

		gTheApp->Run();				//   class and run it
		
		//gTheApp->MaxMemory();
	}
	
	if (gTheApp != NULL)
		delete gTheApp;
	
	return (0);
}


// ===========================================================================
//		¥ CMac5LApp Class
// ===========================================================================

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

	FiveL::TString	homeDir = gModMan->GetMasterPath();
	
	// Open the log file and append to it.
	gLog.Init(homeDir.GetString(), "5L", true, true);
	
	// Initialize the MissingMedia log file but don't open it.
	gMissingMediaLog.Init(homeDir.GetString(), "MissingMedia", false, true);
	
#ifdef DEBUG
	gDebugLog.Init(homeDir.GetString(), "Debug");
	gDebugLog.TimeStamp();
#endif

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
	gWindow = (WindowPtr) mDisplayWindow->GetMacPort();
		
	// Make this a subcommander of the main window.
	gPlayerView = (CPlayerView *) mDisplayWindow->FindPaneByID(210);
				
	// cbo_fix - if we really need this, figure out a new way to do it
	// Set the window's default palette
	//CTabHandle	theCTabHand;
	//theCTabHand = ::GetCTable(128);
	
	//if (theCTabHand != nil)
	//{
	//	DoNewPalette(theCTabHand);
	//	gPlayerView->DoNewPalette(theCTabHand);
	//	CheckPalette();
	//}
	// cbo_fix - this should be the equivalent of the above stuff
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
	
		screenBounds.top += ::LMGetMBarHeight() + 50;
		screenBounds.left = 50;
		screenBounds.right = screenBounds.left + 640;
		screenBounds.bottom = screenBounds.top + 480;
	}
#endif
	
	// set the display window to cover the screen	
	mDisplayWindow->DoSetBounds(screenBounds);
	
	// Ensure all our sub-views are 640x480
	gPlayerView->ResizeFrameTo(640, 480, FALSE);
	gPlayerView->CenterInView();
	
	AddAttachment(gPlayerView);								// to get events

	// pick up our click sound
//	clickSound = Get1Resource('snd ', 1001);
//	if (clickSound != NULL)
//		SetResAttrs(clickSound, resLocked);
	
	if (gHideMenuBar)
		gMenuUtil.HideMenuBar();

	mDisplayWindow->Show();

	::BringToFront(gWindow);
	gInFront = true;
		 	
	// set up initial values for "global" variables
	SetGlobals();
	
	// Fade back in (once the window has drawn itself).
	if (gFullScreen and gHideMenuBar)
		mDisplayWindow->FadeAfterDraw();
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
	AEDescList	docList;
	OSErr		err = AEGetParamDesc(&inAppleEvent, keyDirectObject,
							typeWildCard, &docList);
	if (err != noErr) 
		Throw_(err);
	
	SInt32	numDocs;
	err = AECountItems(&docList, &numDocs);
	if (err != noErr) 
		Throw_(err);
	
	// Loop through all items in the list
	// Extract descriptor for the document
	// Coerce descriptor data into a FSSpec
	// Tell Program object to open document
		
	for (SInt32 i = 1; i <= numDocs; i++) 
	{
		AEKeyword	theKey;
		DescType	theType;
		FSSpec		theFileSpec;
		Size		theSize;
		int32		theModule;
		
		err = AEGetNthPtr(&docList, i, typeFSS, &theKey, &theType,
							(Ptr) &theFileSpec, sizeof(FSSpec), &theSize);
		if (err != noErr) 
			Throw_(err);
		
		// If we are already running a script, ignore everything.
		if (not mScriptRunning)
		{
			if (gModMan->HaveModules())
			{
				// what do we do here? they have a config file but we have
				// dragged a file on the icon??
				
				// see if this file corresponds to one of our modules
				theModule = gModMan->FindScript(&theFileSpec);
				if (theModule != -1)
				{
					gModMan->LoadModule(theModule);
				}
				else
				{
					gModMan->ResetPaths(&theFileSpec);
					OpenScript(&theFileSpec);
				}
			}
			else
				OpenScript(&theFileSpec);
		}
	}
	
	AEDisposeDesc(&docList);
}

//
//	DoExit - Do the exit processing. If passed 0, quit the application.
//		Otherwise, asssume it is the module number to jump to.
//
void CMac5LApp::DoExit(int16 inSide)
{
	FiveL::TString theCurPal;
	Rect		theRect = {0, 0, 480, 640};
	int32		theCheckDisc;				// want to keep this value
	bool		goodModule = false;
	bool		reloadPal = false;

	if (mScriptRunning)
	{
		gPlayerView->KillScript();

		mScriptRunning = false;
		
		// save the value of _NoCheckDisc and _graphpal
		theCheckDisc = gVariableManager.GetLong("_NoCheckDisc");
		if (gPaletteManager.GetCurrentPalette() != NULL)
		{
			theCurPal = gVariableManager.GetString("_GraphPal");
			reloadPal = true;
		}
		
		CleanUp();
				
		// reset our "global" variables
		SetGlobals();
		
		// reload the current palette - it is already set so we don't
		// 	have to do anything else
		if (reloadPal)
		{
			gVariableManager.SetString("_GraphPal", theCurPal.GetString());
		}
		else
			gVariableManager.SetString("_GraphPal", "NULL");	
	}

	if ((inSide > 0) and (gModMan->HaveModules()))
		goodModule = gModMan->LoadModule(inSide);
	
	DoGFade(true, 0, false);				// make sure we aren't faded out
	
	if (not goodModule)
	{
		// if we are quitting - draw black
		gPlayerView->ColorCard(0);
		gPlayerView->Draw(nil);
		
		DoQuit();
	}
}	

//
//	StartUp - We have started without a file to open. If there is a 
//			config file (we read it while initializing gModMan) jump
//			to the first module. Otherwise, ask the user for a script.
//
void CMac5LApp::StartUp(void)
{
#ifdef DEBUG
	FSSpec	scrSpec;
#endif
	bool	stayRunning = true;
	
	if (gModMan->HaveModules())
	{
		// We have a config file, start the first module.
		// If shift key was down start with -1 (shift script).
		
		if (gDoShiftScript)
			stayRunning = gModMan->LoadModule(-2);
		else
			stayRunning = gModMan->LoadModule(-1);
	}
#ifdef DEBUG
	// We can ask for a script. Only for debugging??
	else if (GetScriptFile(&scrSpec))
		stayRunning = OpenScript(&scrSpec);
	else
		stayRunning = false;
#else
	else
	{
		gLog.Caution("Mac5L does not have a configuration file <Mac5L.config>");
		stayRunning = false;
	}
#endif

	if (not stayRunning)
		DoQuit();
}

//
//	CleanUp - Toss everything.
//
void CMac5LApp::CleanUp(void)
{
	gHeaderManager.RemoveAll();			// toss all headers
	gMacroManager.RemoveAll();			// toss all macros
	gCardManager.RemoveAll();			// toss all cards
	gVariableManager.RemoveAll();		// toss all variables
	gPaletteManager.RemoveAll();		// toss all palettes
	gPictureManager.RemoveAll();		// toss all pictures
	gIndexFileManager.RemoveAll();		// toss all the script files	
}

//
//	EventResume - We are overriding LEventDispatcher's EventResume function.
//	We want to HideMenuBar and then call LEventDispatcher's function.
//
void CMac5LApp::EventResume(const EventRecord& inMacEvent)
{
	if (gHideMenuBar)
		gMenuUtil.HideMenuBar();
	LApplication::EventResume(inMacEvent);	
}

//
//	EventSuspend - We are overriding LEventDispatcher's EventSuspend function.
//	We want to ShowMenuBar and then call LEventDispatcher's function.
//
void CMac5LApp::EventSuspend(const EventRecord& inMacEvent)		
{
 
	gMenuUtil.ShowMenuBar();
	LApplication::EventSuspend(inMacEvent);
}

Boolean CMac5LApp::AttemptQuitSelf(SInt32 /* inSaveOption */)
{
	if (mScriptRunning)
	{
		gPlayerView->KillScript();
		CleanUp();
	}
	
	mScriptRunning = false;
	
	DoGFade(true, 0, false);			// make sure we aren't faded out
	gMenuUtil.ShowMenuBar();	// make sure we have the menu
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
	StandardFileReply	sfReply;
	SFTypeList			sfTypes;
	
	sfTypes[0] = 'TEXT';
	sfTypes[1] = 0;
	
	StandardGetFile(NULL, 1, sfTypes, &sfReply);
	
	if (sfReply.sfGood)
	{
		*scriptSpec = sfReply.sfFile;
		return (TRUE);
	}
	return (FALSE); 
}

//
//	QuitScript - Stop running a script.
//
void CMac5LApp::QuitScript(void)
{
	DoQuit();
}

//
//	OpenScript - Open the given script file, open and read the index
//					file and start the script running.
//
bool  CMac5LApp::OpenScript(FSSpec *scriptSpec)
{
	bool	retValue = false;
		
	// set mSleepTime again, just to be sure
	mSleepTime = 0;

	if (gIndexFileManager.NewIndex(scriptSpec))	
	{
		mScriptRunning = true;
		
		gPlayerView->Activate();
		gCardManager.JumpToCardByName("Start", FALSE);

		retValue = true;
	}
	
	return (retValue);
}

#ifdef OLD_PALETTE_STUFF
void CMac5LApp::NewColorTable(CPalette *inPal, bool inGraphics)
{
	CPalette		*theOldPal;
	CTabHandle		theCTab;
			
	if (inGraphics)
	{
		// if we are trying to set the palette to what it already is
		// don't do anything
		if (inPal == mGraphicsPal)
			return;
			
		theOldPal = mGraphicsPal;
		mGraphicsPal = inPal;
	}	
	else
	{
		theOldPal = mMoviePal;
		mMoviePal = inPal;
	}
	
	theCTab = inPal->GetCTab();
	
	DoNewPalette(theCTab);
	
	if (inGraphics)
		gPlayerView->DoNewPalette(theCTab);
	else
		CheckPalette();			// do it now - we are going to show a movie
	
	if (theOldPal != nil)
		theOldPal->Purge();		// clear it from memory (unless it is locked)
}
		
void CMac5LApp::DoNewPalette(CTabHandle inCTab)
{
	PaletteHandle	thePalHand;
	PaletteHandle	theOldPalHand;

	// now make the new palette
	//thePalHand = ::NewPalette(256, inCTab,  pmTolerant + pmExplicit, 0);
	thePalHand = ::NewPalette((**inCTab).ctSize+1, inCTab, pmTolerant + pmExplicit, 0);
	
	theOldPalHand = ::GetPalette(gWindow);
	
	::NSetPalette(gWindow, thePalHand, pmNoUpdates);	// was pmAllUpdates
	
	mHaveNewPal = true;
	mCurPal = thePalHand;
	
	if (theOldPalHand != nil)
		::DisposePalette(theOldPalHand);
}

void CMac5LApp::CheckPalette(void)
{		
	if (mHaveNewPal)
	{
		mHaveNewPal = false;
		::ActivatePalette(gWindow);
	}
}
	
//
//	RestorePalette
//
void CMac5LApp::RestorePalette(void)
{
	CTabHandle		theCTab;
	
	if (mGraphicsPal != NULL)
	{
#ifdef DEBUG
		//gDebugLog.Log("restoring graphics palette to <%s>", mGraphicsPal->key.GetString());
#endif

		theCTab = mGraphicsPal->GetCTab();

		if (theCTab == NULL)
		{
#ifdef DEBUG
			//gDebugLog.Log("trying to restore palette, got dodo");
#endif
			mGraphicsPal->Load();		// reload the palette
			theCTab = mGraphicsPal->GetCTab();
			
			if (theCTab == NULL)
			{
#ifdef DEBUG
			//	gDebugLog.Log("still trying to restore, couldn't get graphics palette");
#endif
				return;
			}
		}

		DoNewPalette(theCTab);

		CheckPalette();				// will always be faded out for this
	}
}
#endif // OLD_PALETTE_STUFF

#ifdef DEBUG

//
//	ReDoScript - Read in the current script from disc and start again.
//
void CMac5LApp::ReDoScript(const char *curCard)
{
	FSSpec			scriptSpec;
	FiveL::TString	scriptString;
	bool			thing;

	mScriptRunning = false;

	scriptString = gModMan->GetCurScript();	
	
	gCardManager.CurCardKill();
	
	if (gMovieManager.Playing())
		gMovieManager.Kill();
		
	gPlayerView->KillTZones();

	gDebugLog.Log("Killing everything for redoscript");
			
	//
	// don't kill the variable tree as we want the variables to have their current values
	//
	gHeaderManager.RemoveAll();
	gMacroManager.RemoveAll();
	gCardManager.RemoveAll();
	gIndexFileManager.RemoveAll();
		
	theConfig->FillScriptSpec(&scriptSpec, scriptString.GetString());
	
	thing = OpenScriptAgain(&scriptSpec, curCard);

	if (not thing)
	{
		if (::CautionAlert(2003, nil) == 1)
		{
			// hang around
			gPlayerView->Deactivate();
			
			// keep the card name around
			mReDoCard = curCard;
			mReDoReDo = true;
			
			thing = true;
		}
	}
	
	if (not thing)
	{
		gLog.Caution("Couldn't restart <%s>", curCard);
		CleanUp();
		DoQuit();
	}
}

//
//	ObeyCommand - 
//
Boolean CMac5LApp::ObeyCommand(CommandT inCommand, void *ioParam)
{
	if (inCommand == cmd_Revert)
	{
		if (mReDoReDo)
		{
			ReDoReDoScript();
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
		
		if (mReDoReDo)
			outEnabled = true;
		else
			outEnabled = false;
			
		return;
	}
	
	return (LApplication::FindCommandStatus(inCommand, outEnabled, 
		outUsesMark, outMark, outName));
}

//
//	ReDoReDoScript - 
//
void CMac5LApp::ReDoReDoScript(void)
{
	FSSpec	scriptSpec;
	FiveL::TString	scriptString;
	bool	thing = false;
	
	if (mReDoReDo)
	{
		mReDoReDo = false;
		gPlayerView->Activate();
		
		scriptString = gModMan->GetCurScript();	
		theConfig->FillScriptSpec(&scriptSpec, scriptString.GetString());
	
		thing = OpenScriptAgain(&scriptSpec, mReDoCard.GetString());
		
		if (not thing)
		{
			if (::CautionAlert(2003, nil) == 1)
			{
				// do it again
				gPlayerView->Deactivate();
				mReDoReDo = true;
				thing = true;
			}
		}
		
		if (not thing)
		{
			gLog.Caution("Couldn't restart <%s>", mReDoCard.GetString());
			CleanUp();
			DoQuit();
		}
	}
}

//
//	OpenScriptAgain
//
bool CMac5LApp::OpenScriptAgain(FSSpec *scriptSpec, const char *jumpCard)
{
	bool	retValue = false;
		
	// set mSleepTime again, just to be sure
	mSleepTime = 0;

#ifdef DEBUG
	gDebugLog.Log("reinit everything for redoscript");
#endif

	if (gIndexFileManager.NewIndex(scriptSpec))		
	{
		mScriptRunning = true;
		
		gVariableManager.SetString("_lpstat", "0");
		gVariableManager.SetString("_lpactive", "0");
		gVariableManager.SetString("_movieplaying", "0");
		
		gPlayerView->Activate();
		gCardManager.JumpToCardByName(jumpCard, FALSE);

		retValue = true;
	}
	else
	{
		// error in the script, toss all cards headers and macros
		gHeaderManager.RemoveAll();
		gMacroManager.RemoveAll();
		gCardManager.RemoveAll();
		gIndexFileManager.RemoveAll();
	}
	
	return (retValue);
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
	buildNum += VERSION_BUILD_NUM;
	gVariableManager.SetLong("_enginebuild", buildNum);
}

/* 
$Log$
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