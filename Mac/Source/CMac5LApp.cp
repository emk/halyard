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

#include "debug.h"

#include <Palettes.h>

//#ifdef DEBUG_5L
#include <LSIOUXAttachment.h>
#include <SIOUX.h>
#include <iostream.h>
//#endif

#include "Mac5L.h"
#include "CConfig.h"
#include "CMac5LApp.h"
#include "CModule.h"

#include "CPlayerView.h"
#include "CMoviePlayer.h"

#include "gamma.h"
#include "CMenuUtil.h"

#include "CCard.h"
#include "CMacroManager.h"
#include "CHeader.h"
#include "CVariable.h"
#include "CFiles.h"
#include "CPrefs.h"
#include "CIndex.h"

// cbo_debug - take this out when don't need it anymore
//#include <profiler.h>

//
// constants
//
const ResIDT		WIND_Mac5L		= 200;

//
// globals
//
bool				gInFront = true;			// is the app window in front?
#ifdef DEBUG_5L
bool				gFullScreen = false;		
bool				gPrint = false;				// for SIOUX log window
bool				gPrintToFile = true;		// to print info string 
bool				gHideMenuBar = false;		// to hide menu bar
#else
bool				gFullScreen = true;			// use the full screen
bool				gHideMenuBar = true;		// to hide menu bar
#endif
bool				gDoShiftScript = false;		// by default, start from beginning

CPreferences        *gPrefs = NULL;

CFileList			gFileManager;
CCardManager		gCardManager;
CMacroManager		gMacroManager;
CHeaderManager		gHeaderManager;
CVariableManager	gVariableManager;
CMenuUtil			gMenuUtil;

CPlayerView			*gPlayerView;
CMoviePlayer		gMovieManager;
WindowPtr			gWindow;
CursHandle			gHandCursor = NULL;

CMac5LApp			*gTheApp;

//Handle				clickSound = NULL;

// ===========================================================================
//		¥ Main Program
// ===========================================================================

int main()
{
									// Set Debugging options
#ifdef DEBUG_5L
	SetDebugThrow_(debugAction_SourceDebugger);
	SetDebugSignal_(debugAction_SourceDebugger);
#else
	SetDebugThrow_(debugAction_Alert);
	SetDebugSignal_(debugAction_Alert);
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
	//	prcaution("Couldn't start the profiler");
	//}
	//ProfilerSetStatus(false);
	
	// Check our configuration.
	theConfig = new CConfig;
	
	if (theConfig->CheckConfig())	// if we don't have a good config, don't run
	{
		gTheApp = new CMac5LApp;	// create instance of application
		gTheApp->Run();				//   class and run it
	}
	
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
	Rect		theFrame;
	bool		centerOnScreen;

	mHaveNewPal = false;
	
	// If the Shift key is down when starting up, do the shift script.
	::GetKeys(keys);
	if ((keys[1] & 1) != 0)
		gDoShiftScript = true;

	// Setup the Gamma tools
	SetupGammaTools();
	
	// Fade the screen out.
	if (gFullScreen and gHideMenuBar)
		DoGFade(false, 5, false);
	
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
	
	// Initialize the modules.
	gModMan = new CModuleManager;

#ifdef DEBUG_5L	
	// Open the debug log file. This HAS to be after new CModuleManager.
	if (gPrintToFile)
		open_debug_file();
#endif
	
	// Create and show our window
	mDisplayWindow = LWindow::CreateWindow(WIND_Mac5L, this);
	
	// Set our global window pointer.
	gWindow = (WindowPtr) mDisplayWindow->GetMacPort();
		
	// Set background color to black
	mDisplayWindow->SetForeAndBackColors(&kRGB_White, &kRGB_Black);

	// Make this a subcommander of the main window.
	gPlayerView = (CPlayerView *) mDisplayWindow->FindPaneByID(210);
			
	// Set the window's default palette
	CTabHandle	theCTabHand;
	
	mGraphicsPal = NULL;
	mMoviePal = NULL;
	mCurPal = NULL;
	theCTabHand = ::GetCTable(128);
	
	if (theCTabHand != nil)
	{
		DoNewPalette(theCTabHand);
		gPlayerView->DoNewPalette(theCTabHand);
		CheckPalette();
	}
	
	// Get the hand cursor for touch zones.
	gHandCursor = ::GetCursor(128);
		
	// Set our sleep time.
	mSleepTime = 0;
	
	screenBounds = theConfig->GetScreenRect();

	centerOnScreen = true;			// by default

#ifdef DEBUG_5L
	if ((not gFullScreen) 
		and (screenBounds.bottom >= 550) 
		and (screenBounds.right >= 750))
	{
		centerOnScreen = false;
	
		screenBounds.top += ::LMGetMBarHeight() + 50;
		screenBounds.left = 50;
		screenBounds.right = 640;
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

#ifdef DEBUG_5L	
	if (gPrint)
	{
		SIOUXSettings.toppixel = screenBounds.bottom + 25;	
		SIOUXSettings.leftpixel = 0;
		SIOUXSettings.columns = 80;
		SIOUXSettings.rows = 15;
		
		AddAttachment(new LSIOUXAttachment);
				
		cout << "Mac5L starting up\r";	// this will get it to appear
	}
#endif
	
	if (gHideMenuBar)
		gMenuUtil.HideMenuBar();

	mDisplayWindow->Show();

	::BringToFront(gWindow);
	gInFront = true;
		
	// Blast the background up and valid the whole thing.
	mDisplayWindow->CalcLocalFrameRect(theFrame);
	::FillRect(&theFrame, &qd.black);
	mDisplayWindow->ValidPortRect(&theFrame);
 
	// set up initial values for "global" variables
	gVariableManager.SetString("_NoCheckDisc", "0");		// check for discs by default
	gVariableManager.SetString("_locked", "0");				// start off unlocked
	gVariableManager.SetString("_graphpal", "NULL");		// something dumb in graphpal
	gVariableManager.SetString("_faded", "0");				// we aren't faded
	gVariableManager.SetString("_error", "0");
	gVariableManager.SetString("_lpstat", "0");
	gVariableManager.SetString("_lpactive", "0");
	gVariableManager.SetString("_movieplaying", "0");
	
#ifdef DEBUG_5L
	gVariableManager.SetString("_debug", "1");
#else
	gVariableManager.SetString("_debug", "0");
#endif
	
	// Fade back in.
	if (gFullScreen and gHideMenuBar)
		DoGFade(true, 5, false);
}


// ---------------------------------------------------------------------------
//		¥ ~CMac5LApp
// ---------------------------------------------------------------------------
//	Destructor

CMac5LApp::~CMac5LApp()
{
	DisposeGammaTools();

#ifdef DEBUG_5L	
	if (gPrintToFile)
		close_debug_file();
#endif
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
	
	Int32	numDocs;
	err = AECountItems(&docList, &numDocs);
	if (err != noErr) 
		Throw_(err);
	
	// Loop through all items in the list
	// Extract descriptor for the document
	// Coerce descriptor data into a FSSpec
	// Tell Program object to open document
		
	for (Int32 i = 1; i <= numDocs; i++) 
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
	CString		theCurPal;
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
		if (mGraphicsPal != NULL)
		{
			theCurPal = gVariableManager.GetString("_graphpal");
			reloadPal = true;
		}
		
		KillIndex();						// kills the script file
		
		gHeaderManager.ZapTree();			// toss all headers
		gMacroManager.ZapTree();			// toss all macros
		gCardManager.ZapTree();				// toss all cards
		gVariableManager.ZapTree();			// toss all variables
		
		KillResTree();						// toss all resources (pictures)

		mGraphicsPal = NULL;
		mMoviePal = NULL;
		mCurPal = NULL;

		// cbo - reload the current palette - it is already set so we don't
		// 	have to do anything else
		if (reloadPal)
			mGraphicsPal = GetPalette(theCurPal.GetString());
		
		if (mGraphicsPal == NULL)
			gVariableManager.SetString("_graphpal", "NULL");
		else
			gVariableManager.SetString("_graphpal", theCurPal.GetString());
			
		// recreate our "global" variables
		gVariableManager.SetLong("_NoCheckDisc", theCheckDisc);	// reset this variable
		gVariableManager.SetString("_locked", "0");				// start off unlocked
		gVariableManager.SetString("_faded", "0");				// and not faded
		gVariableManager.SetString("_lpstat", "0");
		gVariableManager.SetString("_lpactive", "0");
		gVariableManager.SetString("_movieplaying", "0");
#ifdef DEBUG_5L
		gVariableManager.SetString("_debug", "1");
#else
		gVariableManager.SetString("_debug", "0");
#endif
		
		// cbo - don't know why we were doing this as KillScript() drew black
		// anyway - but now we want to leave up the last graphic so we
		// really don't need this
		//if (gModMan->HaveModules())
		//{
		//	gPlayerView->ColorCard(0);
		//	
		//	gPlayerView->Draw(nil);			// draw whatever it is we did
		//}
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
#ifdef DEBUG_5L
	FSSpec	scrSpec;
#endif
	bool	stayRunning = true;
	
	if (gModMan->HaveModules())
	{
		// We have a config file, start the first module.
		// If shift key was down start with -1 (shift script).
		
		if (gDoShiftScript)
			gModMan->LoadModule(-2);
		else
			gModMan->LoadModule(-1);
	}
#ifdef DEBUG_5L
	// We can ask for a script. Only for debugging??
	else if (GetScriptFile(&scrSpec))
		stayRunning = OpenScript(&scrSpec);
	else
		stayRunning = false;
#else
	else
	{
		prcaution("Mac5L does not have a configuration file <Mac5L.config>");
		stayRunning = false;
	}
#endif

	if (not stayRunning)
		DoQuit();
}

Boolean CMac5LApp::AttemptQuitSelf(Int32 /* inSaveOption */)
{
	if (mScriptRunning)
		gPlayerView->KillScript();
	mScriptRunning = FALSE;
	
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
	FSSpec	indexSpec;
	char	*strPtr;
	char	*indexName;
	Int16	nameLen;
	bool	retValue = false;

	indexSpec = *scriptSpec;
	
	// null terminate the name so we can use it as a c-string
	nameLen = indexSpec.name[0];
	indexSpec.name[nameLen+1] = '\0';
	indexName = (char *) &(indexSpec.name[1]);
	
	// set mSleepTime again, just to be sure
	mSleepTime = 0;

	strPtr = strstr(indexName, ".");
	if (strPtr != NULL)
		strcpy(strPtr, ".idx");
	else
		strcat(indexName, ".idx");
	
	if (InitIndex(scriptSpec, &indexSpec, FALSE))		// start the ball rolling
	{
		mScriptRunning = TRUE;
		
		gPlayerView->Activate();
		gCardManager.JumpToCardByName("Start", FALSE);

		retValue = true;
	}
	
	return (retValue);
}

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
	
	// cbo_mem
	// cbo - only purge if we don't want the resource tree to manage memory	
	// for us
	//if (theOldPal != nil)
	//	theOldPal->Purge();		// clear it from memory (unless it is locked)
}
		
		
void CMac5LApp::DoNewPalette(CTabHandle inCTab)
{
	PaletteHandle	thePalHand;
	PaletteHandle	theOldPalHand;

	// now make the new palette
	thePalHand = ::NewPalette(256, inCTab,  pmTolerant + pmExplicit, 0);
	
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
#ifdef DEBUG_5L
		prinfo("restoring graphics palette to <%s>", mGraphicsPal->key.GetString());
#endif

		theCTab = mGraphicsPal->GetCTab();

		if (theCTab == NULL)
		{
#ifdef DEBUG_5L
			prinfo("trying to restore palette, got dodo");
#endif
			mGraphicsPal->Load();		// reload the palette
			theCTab = mGraphicsPal->GetCTab();
			
			if (theCTab == NULL)
			{
#ifdef DEBUG_5L
				prinfo("still trying to restore, couldn't get graphics palette");
#endif
				return;
			}
		}

		DoNewPalette(theCTab);

		CheckPalette();				// will always be faded out for this
	}
}

#ifdef DEBUG_5L

//
//	ReDoScript - Read in the current script from disc and start again.
//
void CMac5LApp::ReDoScript(char *curCard)
{
	FSSpec	scriptSpec;
	CString	scriptString;
	bool	thing;

	mScriptRunning = false;
	
	gCardManager.CurCardKill();
	
	if (gMovieManager.Playing())
		gMovieManager.Kill();
		
	gPlayerView->KillTZones();

	prinfo("Killing everything for redoscript");
		
	KillIndex();
	
	//
	// don't kill the variable tree as we want the variables to have their current values
	//
	gHeaderManager.ZapTree();
	gMacroManager.ZapTree();
	gCardManager.ZapTree();
		
	gModMan->GetCurScript(scriptString);
	theConfig->FillScriptSpec(&scriptSpec, (char *) scriptString);
	
	thing = OpenScriptAgain(&scriptSpec, curCard);
	
	if (not thing)
	{
		prcaution("Couldn't restart <%s>", curCard);
		DoQuit();
	}
}
	
//
//	OpenScriptAgain
//
bool CMac5LApp::OpenScriptAgain(FSSpec *scriptSpec, char *jumpCard)
{
	FSSpec	indexSpec;
	char	*strPtr;
	char	*indexName;
	Int16	nameLen;
	bool	retValue = false;
	
	indexSpec = *scriptSpec;
	
	// null terminate the name so we can use it as a c-string
	nameLen = indexSpec.name[0];
	indexSpec.name[nameLen+1] = '\0';
	indexName = (char *) &(indexSpec.name[1]);
	
	// set mSleepTime again, just to be sure
	mSleepTime = 0;

	strPtr = strstr(indexName, ".");
	if (strPtr != NULL)
		strcpy(strPtr, ".idx");
	else
		strcat(indexName, ".idx");

	prinfo("reinit everything for redoscript");
		
	if (InitIndex(scriptSpec, &indexSpec, FALSE))		// start the ball rolling
	{
		mScriptRunning = TRUE;
		
		gVariableManager.SetString("_lpstat", "0");
		gVariableManager.SetString("_lpactive", "0");
		gVariableManager.SetString("_movieplaying", "0");
		
		gPlayerView->Activate();
		gCardManager.JumpToCardByName(jumpCard, FALSE);

		retValue = true;
	}
	
	return (retValue);
}
#endif

/* 
$Log$
Revision 1.3  1999/10/07 12:37:53  chuck
Initial revision

*/