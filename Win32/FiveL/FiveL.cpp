// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
// FiveL.cpp : 
//


#include "stdafx.h"
#include "resource.h"
#include "Debug.h"

#include "TStartup.h"
#include "TDeveloperPrefs.h"
#include "LUtil.h"
#include "Graphics.h"
#include "TVariable.h"
#include "Video.h"
#include "View.h"
#include "Audio.h"
#include "Config.h"
#include "SysInfo.h"
#include "TLogger.h"
#include "LCursor.h"
#include "FiveL.h"
#include "LPicture.h"
#include "LPalette.h"
#include "LFont.h"
#include "LTouchZone.h"
#include "LCommandKey.h"
#include "Input.h"
#include "Gestalt.h"
#include "LHttp.h"
#include "LBrowser.h"
#include "SingleInstance.h"
#include "TQTMovie.h"
#include "TWin5LInterpreter.h"
#include "TWinPrimitives.h"
#include "TQTPrimitives.h"

#if defined USE_BUNDLE
	#include "LFileBundle.h"
#else
	#include "LFiles.h"
#endif

#define MAX_LOADSTRING 100

// Resolution
int H_SCREEN = 640;
int V_SCREEN = 480;

// Timer stuff
#define FIVEL_TIMER				21
UINT	gTimer;						// our global timer		

// Global Variables:
TCHAR				szTitle[MAX_LOADSTRING];			// The title bar text
TCHAR				szWindowClass[MAX_LOADSTRING];		// The title bar text
TCHAR				szChildClass[MAX_LOADSTRING];

HINSTANCE			hAppInst;
HWND     			hwndApp;
HWND				hBackgroundWnd;
WINDOWPLACEMENT		hwndAppSettings;
WINDOWPLACEMENT		hBackgroundWndSettings;
RECT				gWinRect;
RECT				gBackgroundWinRect;
bool				gInFront = false;
bool				gInBackground = false;
bool				gForceRedraw = false;
bool				gPalNeedsReset = false; 
TRect				gScreenRect;	// our virtual screen rect (in screen coordinates)
int					gHorizRes;
int					gVertRes;

TWin5LInterpreter   *gWin5LInterpreter;
View				*gView = NULL;
LTouchZoneManager	gTouchZoneManager;     
SysInfo				gSysInfo; 
LCursorManager		gCursorManager; 
VideoManager		gVideoManager;
AudioManager		gAudioManager;
AudioManager		gBgAudioManager;

#if defined USE_BUNDLE
	LFileBundle			gFileManager;
#else
	LFileList           gFileManager;
#endif

ConfigManager		gConfigManager;
LPictureManager		gPictureManager;
LPaletteManager		gPaletteManager;
LFontManager		gFontManager;
LCommandKeyManager	gCommandKeyManager;
InputManager		gInputManager;
CGrafPtr			gGrafPtr = NULL;
LHttp				gHttpTool;
LBrowser			gBrowserTool;

// The one and only CLimitSingleInstance object
SingleInstance g_SingleInstanceObj(TEXT("{03C53E8D-4B46-428c-B812-DBAC5F1A6378}"));

// Foward declarations of functions included in this code module:
int					RealWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
								LPSTR lpCmdLine, int nCmdShow);
bool				InitInstance(HINSTANCE, int);
void				DeInitInstance(void);
bool				InitApplication(HINSTANCE hInstance);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM) throw ();
LRESULT				RealWndProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK	ChildWndProc(HWND, UINT, UINT, LPARAM) throw ();
LRESULT				RealChildWndProc(HWND, UINT, UINT, LPARAM);
LRESULT CALLBACK	About(HWND, UINT, WPARAM, LPARAM) throw ();
LRESULT				RealAbout(HWND, UINT, WPARAM, LPARAM);
void				StartTimer(void);
void				StopTimer(void);
bool				CheckSystem(void);
void				DumpStats(void);

// SpecialVariableFunctions for getting platform-specific special
// variables.
static TString ReadSpecialVariable_system();
static TString ReadSpecialVariable_curcard();
static TString ReadSpecialVariable_prevcard();
static TString ReadSpecialVariable_eof();

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
	throw ()
{
	CATCH_ALL_EXCEPTIONS_AND_RETURN(RealWinMain(hInstance, hPrevInstance,
												lpCmdLine, nCmdShow));
	return false;
}

int RealWinMain(HINSTANCE hInstance,
				HINSTANCE hPrevInstance,
				LPSTR     lpCmdLine,
				int       nCmdShow)
{
	HWND    	hwndPrev;
	MSG			msg;

	if (hPrevInstance)           /* Other instances of app running? */
    {
        /* Find the current instance of the app and bring to top */
        if (hwndPrev = ::FindWindow(szWindowClass, szTitle))
        {
            if (IsIconic(hwndPrev))
                ::ShowWindow(hwndPrev, SW_SHOWNORMAL);
            else
                ::BringWindowToTop(hwndPrev);
        }
        /* exit now the current instance is visible */
        return (false);
    }

	// Initialize the Common library.
	InitializeCommonCode();

	// Initialize global strings
	LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadString(hInstance, IDC_FIVEL, szWindowClass, MAX_LOADSTRING);
	LoadString(hInstance, IDC_FIVEL_CHILD, szChildClass, MAX_LOADSTRING);

	// Process the command line, configuration file, and user prefs.
	if (not gConfigManager.Init(lpCmdLine))
		return (false);

	// Initialize the application.
	if (not InitApplication(hInstance))
		return (false);

	// Make system checks.
	if (not CheckSystem())
		return (false);	

	// Register our platform-specific special variables.
	gVariableManager.RegisterSpecialVariable("_system",
		&ReadSpecialVariable_system);
	gVariableManager.RegisterSpecialVariable("_curcard",
		&ReadSpecialVariable_curcard);
	gVariableManager.RegisterSpecialVariable("_prevcard",
		&ReadSpecialVariable_prevcard);
	gVariableManager.RegisterSpecialVariable("_eof",		
		&ReadSpecialVariable_eof);

#if defined USE_BUNDLE
	if (not gFileManager.Init())
		return(false);
#endif

	// Initialise the cursor manager.
	gCursorManager.Init(hInstance);

	gView = new View;
    if (gView == NULL)
    	return (false);

	// Perform application initialization:
	if (not InitInstance (hInstance, nCmdShow)) 
		return (false);

	gView->BlackScreen();

	// Initialize the interpreter.
	try
	{
		RegisterWindowsPrimitives();
		RegisterQuickTimePrimitives();
		gWin5LInterpreter = new TWin5LInterpreter(gConfigManager.CurScript());
	}
	catch (...)
	{
		DeInitInstance();		
    	return (false);
	}

	// initialize the URL checker
	gHttpTool.Init();

	// initialize the browser launcher
	gBrowserTool.Init();
 
	gDebugLog.Log("Bit Depth: %d", gView->BitDepth());
	gDebugLog.Log("Resolution: %d x %d", gHorizRes, gVertRes);

	// jump to the start card
	TInterpreter::GetInstance()->JumpToCardByName("start"); 
            
	// start our idle loop timer
	StartTimer();
	
	::SetFocus(hwndApp);

	// Main message loop:
	while (::GetMessage(&msg, NULL, 0, 0)) 
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	// Clean everything up.
	ShutDown(true);

	return (msg.wParam);
}

//
//  InitApplication()
//
bool InitApplication(HINSTANCE hInstance)
{
	WNDCLASSEX		wcex;
	DWORD			errNum;
	char			errString[100];

	wcex.cbSize = sizeof(WNDCLASSEX); 

	wcex.style			= CS_HREDRAW | CS_VREDRAW | CS_BYTEALIGNWINDOW;
	wcex.lpfnWndProc	= (WNDPROC) WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;

	wcex.hCursor		= NULL;
	wcex.hbrBackground	= NULL;
	wcex.lpszClassName	= szWindowClass;

	if (gDeveloperPrefs.GetPref(MODE) == MODE_WINDOW)
	{
		wcex.hIcon			= LoadIcon(hInstance, (LPCTSTR)IDI_FIVEL);
		wcex.lpszMenuName	= (LPCSTR)IDC_FIVEL;
		wcex.hIconSm		= LoadIcon(wcex.hInstance, (LPCTSTR)IDI_SMALL);
	}
	else	// fullscreen mode
	{
		wcex.hIcon			= NULL;
		wcex.lpszMenuName	= NULL;
		wcex.hIconSm		= NULL;
	}

	if (::RegisterClassEx(&wcex) == 0)
	{
		errNum = ::GetLastError();
		//gLog.Error("Could not register class, error returned <%d>", errNum);
		sprintf(errString, "InitApplication error, cannot register class.  ErrorID = %d", errNum);
		AlertMsg(errString, true);
		return (false);
	}

	wcex.style			= CS_HREDRAW | CS_VREDRAW | CS_BYTEALIGNWINDOW;
	wcex.lpfnWndProc	= (WNDPROC) ChildWndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon			= NULL;
	wcex.hCursor		= NULL;
	wcex.hbrBackground	= (HBRUSH) ::GetStockObject(BLACK_BRUSH);
	wcex.lpszMenuName	= NULL;
	wcex.lpszClassName	= szChildClass;
	wcex.hIconSm		= NULL;

	if (::RegisterClassEx(&wcex) == 0)
	{
		errNum = ::GetLastError();
		//gLog.Error("Could not register child class, error returned <%d>", errNum);
		sprintf(errString, "Could not register child class.  ErrorID = %d", errNum);
		AlertMsg(errString, true);
		return (false);
	}

	return (true);
}

//
//	InitInstance
//
bool InitInstance(HINSTANCE hInstance, int nCmdShow)
{
    // first make sure we are the only instance running
	if (gDeveloperPrefs.GetPref(MULTIPLE_INSTANCES) == MULTIPLE_INSTANCES_NO)
	{
		if (g_SingleInstanceObj.IsAnotherInstanceRunning())
			return FALSE;
	}
	
	HWND        hWnd;
    POINT		cursorPos;
    RECT        Rectgl, cRectgl; 
    DWORD		win_style; 
    int			h_start;
    int			v_start;
    int			h_size;
    int			v_size;
    
	HDC hDC = ::GetDC(NULL);
	gHorizRes = ::GetDeviceCaps(hDC, HORZRES);
	gVertRes = ::GetDeviceCaps(hDC, VERTRES);
    ::ReleaseDC(NULL, hDC);

    Rectgl.left = 0;
    Rectgl.top = 0;
    Rectgl.right = H_SCREEN;
    Rectgl.bottom = V_SCREEN;
 
	//  black background:
    cRectgl.left = 0;
    cRectgl.top = 0;
    cRectgl.right = gHorizRes;
    cRectgl.bottom = gVertRes;

	// Initialize QuickTime - We used to use the kInitializeQTMLUseGDIFlag
	//      as we had seen flashes on startup of certain movies on certain
	//		platforms (Dell laptops and Win98).  But that flag forces use
	//      of GDI, which breaks VP 3.2.1.3 under the QT6 public preview.
 	try
 	{
 		TQTMovie::InitializeMovies();
 	}
 	catch (...)
	{
		// QuickTime is not installed
		gLog.Error("QuickTime is not installed. Please install QuickTime before running this program.");
		return (false);
	}

	// Get the QuickTime version number
	long	qtVersion;

	if (Gestalt(gestaltQuickTimeVersion, &qtVersion) == noErr)
	{
		// put the version number into a variable
		long	realVersion = qtVersion >> 16;

		//gVariableManager.SetLong("_QuickTimeVersion", realVersion);

		// the quicktime version is hex encoded
		char	theBuffer[64];

		sprintf(theBuffer, "%X", realVersion);

		gVariableManager.SetString("_QuickTimeVersion", theBuffer);
	}

	// Try to find out whether QuickTime is using DirectDraw.  This is
	// unscientific, and not based on any documentation, but it seems
	// to work.  We need to know this so we can warn the user that
	// QT 6 and VP 3.2.1.3 don't play nicely together if QuickTime
	// is using GDI (which is the most common alternative to DirectDraw).
	void *lpDDObject = NULL;
	if (::QTGetDDObject(&lpDDObject) == noErr && lpDDObject != NULL)
		gVariableManager.SetLong("_QuickTimeHasDDObject", 1);
	else
		gVariableManager.SetLong("_QuickTimeHasDDObject", 0);

	// Create and show app window:    
	if (gDeveloperPrefs.GetPref(MODE) == MODE_WINDOW)
	{ 
		if ((gHorizRes > 650) and (gVertRes > 500))
		{
			win_style = WS_CAPTION;
			::AdjustWindowRect(&Rectgl, WS_CAPTION, true); 	// find window size based on desired client area (Rectgl)
		}
		else
		{
			win_style = WS_POPUP;
			::AdjustWindowRect(&Rectgl, WS_POPUP, false);
		}
	}
	else
	{
		win_style = WS_POPUP;
		::AdjustWindowRect(&Rectgl, WS_POPUP, false);
	}
 
	gWinRect.top = (gVertRes / 2) - (V_SCREEN / 2);
	gWinRect.left = (gHorizRes / 2) - (H_SCREEN / 2);
	gWinRect.bottom = Rectgl.bottom - Rectgl.top;
	gWinRect.right = Rectgl.right - Rectgl.left;

    h_start = (gHorizRes / 2) - (H_SCREEN / 2);
    v_start = (gVertRes / 2) - (V_SCREEN / 2);

	h_size = Rectgl.right - Rectgl.left;
	v_size = Rectgl.bottom - Rectgl.top;
    
    hWnd = ::CreateWindow(
        szWindowClass,
        szTitle,
        win_style, 
        gWinRect.left,
        gWinRect.top,
        gWinRect.right,
        gWinRect.bottom,
        NULL,
        NULL,
        hInstance,
        NULL);

	if (hWnd == NULL)
	{
		DWORD	lastError = ::GetLastError();

		gLog.Error("Could not create main window, error <%ld>", lastError);
		return (false);
	}

	if (gDeveloperPrefs.GetPref(MODE) == MODE_FULLSCREEN)
		SetWindowPos(hWnd, /*HWND_TOP*/ HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE);
	else
		SetWindowPos(hWnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE);
	    
    gCursorManager.ChangeCursor(NO_CURSOR);

	// black full-screen background. Doesn't look good with blueramp.
	if (gDeveloperPrefs.GetPref(MODE) == MODE_FULLSCREEN)
	{
		AdjustWindowRect(&cRectgl, WS_POPUP, false); // find window size based on desired client area (Rectgl)

		gBackgroundWinRect.top = 0;
		gBackgroundWinRect.left = 0;
		gBackgroundWinRect.bottom = cRectgl.right - cRectgl.left;
		gBackgroundWinRect.right = cRectgl.bottom - cRectgl.top;

		hBackgroundWnd = CreateWindow(
			szChildClass,
			szTitle,
			WS_POPUP | WS_DISABLED,
			CW_USEDEFAULT,
			CW_USEDEFAULT,
			cRectgl.right - cRectgl.left,
			cRectgl.bottom - cRectgl.top,
			NULL,
			NULL,
			hInstance,
			NULL);
		SetWindowPos(hBackgroundWnd, hWnd, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE); 
		ShowWindow(hBackgroundWnd, SW_SHOW);
		UpdateWindow(hBackgroundWnd);
	}

    ::ShowWindow(hWnd, SW_SHOW);
    ::UpdateWindow(hWnd);

	if (gDeveloperPrefs.GetPref(MODE) == MODE_FULLSCREEN)
	{	
		// set our cursor position to be the center of our virtual screen
		cursorPos.x = gHorizRes/2;
		cursorPos.y = gHorizRes/2;
		SetCursorPos(cursorPos.x, cursorPos.y);
		
		// now restrict cursor movement to our virtual screen
		RECT		scrRect;

		::GetClientRect(hWnd, &scrRect);
		gScreenRect.Set(scrRect);
		
		cursorPos.x = gScreenRect.Left();
		cursorPos.y = gScreenRect.Top();
		::ClientToScreen(hWnd, &cursorPos);
		gScreenRect.SetLeft(cursorPos.x);
		gScreenRect.SetTop(cursorPos.y);
		
		cursorPos.x = gScreenRect.Right();
		cursorPos.y = gScreenRect.Bottom();
		::ClientToScreen(hWnd, &cursorPos);
		gScreenRect.SetRight(cursorPos.x);
		gScreenRect.SetBottom(cursorPos.y);
	}
    
    hwndApp = hWnd; 

	gPaletteManager.Init();

	TQTMovie::RegisterWindowForMovies(hwndApp);
	gGrafPtr = TQTMovie::GetPortFromHWND(hwndApp);

	// Initialize the view  
	if (not gView->Init())
		return (false);
  
	SetGlobals();

    return (true);
}

//
//	DeInitInstance
//
void DeInitInstance(void)
{
	gPaletteManager.Reset();

	// get rid of dynamic objects
	if (gView != NULL)
		delete gView;

 	TQTMovie::UnregisterWindowForMovies(hwndApp);
 	TQTMovie::ShutDownMovies();
}

//
//  WndProc - 
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
	throw ()
{
	CATCH_ALL_EXCEPTIONS_AND_RETURN(RealWndProc(hWnd, message,
												wParam, lParam));
	return 0;
}

LRESULT RealWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	int				wmId, wmEvent;
	PAINTSTRUCT		ps;
	HDC				hDC;
	LONG			lRet = 0;
	LCommandKey		*theKey;
    static  int     xcoor, ycoor,TransColourIndex = 9, did_Init = 0;
    TPoint			cursorPos;
    LTouchZone      *theZone; 
    bool			activate = false;

	// QT message handling
	if ((gVideoManager.Playing()) and (gVideoManager.HandleEvent(hWnd, message, wParam, lParam)))
		return (0);
		
	if ((gAudioManager.Playing()) and (gAudioManager.HandleEvent(hWnd, message, wParam, lParam)))
		return (0);

	switch (message) 
	{
		case WM_CREATE:
			::SetFocus(hWnd);
            break;

		case WM_CLOSE:				
			break;

		case WM_COMMAND:
			wmId    = LOWORD(wParam); 
			wmEvent = HIWORD(wParam); 
			// Parse the menu selections:
			switch (wmId)
			{
				case IDM_ABOUT:
				   DialogBox(hAppInst, (LPCTSTR)IDD_ABOUTBOX, hWnd, (DLGPROC)About);
				   break;
				case IDM_EXIT:
				   DestroyWindow(hWnd);
				   break;
				default:
				   return DefWindowProc(hWnd, message, wParam, lParam);
			}
			break;

		case WM_PAINT:	
			hDC = ::BeginPaint(hWnd, &ps);

			if (gView != NULL)
			{				
				if (gInFront)
				{
					if (gVideoManager.FullScreen())
						gView->BlackScreen();
					else
						gView->Draw(gForceRedraw); 
				
					// cbo_test - why aren't we calling UpdateMovie
					if (gVideoManager.Playing())
						gVideoManager.Draw();

					gForceRedraw = true;
				}
				else
					gView->BlackScreen();
			}
				 
			::EndPaint(hWnd, &ps);
		    break;

		case WM_ACTIVATE:
			if (wParam)
				activate = true;
			hDC = ::GetDC(NULL);

			if (activate)
			{
				if (not gInFront)
					gForceRedraw = true;
					 
				gInFront = true;

				if (gInBackground)
					PutInForeground();
				
				::SetFocus(hwndApp); 

				if (gView->BitDepth() <= 8)
				{
					//
					gPaletteManager.Activate(hDC);
    				
   					// make sure the palette is set correctly for the offscreen world
					gPaletteManager.ResetPalette();
   				
    				gView->Draw();		// blast the screen on and reset the palette	
				}

    			// make sure we have a cursor
    			gCursorManager.ChangeCursor(NO_CURSOR);
    			gCursorManager.CheckCursor();
    			
    			StartTimer();
			}
			else	// deactivate
			{ 
				gInFront = false;

				if (not gInBackground)
					PutInBackground();

				if (gView->BitDepth() <= 8)
				{
					gPaletteManager.Deactivate(hDC);
				}
 
				// set the cursor to something normal
				gCursorManager.ChangeCursor(ARROW_CURSOR); 

				StopTimer();
			}	
			
			::ReleaseDC(NULL, hDC);
			break;

		case WM_TIMER:
			// do our idle time processing
			if (wParam == FIVEL_TIMER)
			{
				if (gInFront)
				{
					TInterpreter::GetInstance()->Idle();
					gVideoManager.Idle();
					gAudioManager.Idle();
				}
			}
			break;
           
		case WM_MOUSEMOVE:
			cursorPos.Set(LOWORD(lParam), HIWORD(lParam));
			gCursorManager.CheckCursor(cursorPos); 
			break;

        case WM_LBUTTONDOWN: 
            cursorPos.Set(LOWORD(lParam), HIWORD(lParam));

			if (gInputManager.InInput())
			{
				// If a text entry box has grabbed our input, and we get
				// a mouse event, act like the user pressed return.
				// (This keeps users from flailing around quite so much
				// on our initial login screens.)
				gInputManager.KeyDown(VK_RETURN);
			}
            else if (not TInterpreter::GetInstance()->Napping())
            {  
                if (theZone = gTouchZoneManager.GetTouchZone(cursorPos))  
					theZone->DoCallback();
            }
            break;   
        
        case WM_SYSCHAR:
        	// alt-key processing
        	if ((char) wParam == '.')
        	{
				gDebugLog.Log("Hit Alt-period");
	        	if (TInterpreter::GetInstance()->Napping())
				{
					gDebugLog.Log("Escape from Nap");
					TInterpreter::GetInstance()->KillNap();
				}
				if (gVideoManager.Playing())
				{
					gDebugLog.Log("Fast forward through video");
					gVideoManager.Kill();
				}
				if (gAudioManager.Playing())
				{
					gDebugLog.Log("Fast forward through audio");
					gAudioManager.Kill(0, false);
				}
            }
            else if (__iscsym((char) wParam))  
            {
            	// first see if this is a keybind then go on and
            	//	do default actions
            	
				if ((theKey = gCommandKeyManager.GetCommandKey((char) wParam)) != NULL)
            	{
                   if (gVideoManager.Playing())
                    	gVideoManager.Kill();
                    	
                    if (gAudioManager.Playing())
                    	gAudioManager.Kill(0, false); 
                    
					TCallback *callback = theKey->GetCallback();
					gDebugLog.Log("Key '%c' hit, running callback: %s",
								  (char) wParam,
								  callback->PrintableRepresentation().c_str());
                    callback->Run();
            	}
            	// q - quit
            	else if ((char) wParam == 'q')	
                { 
                	TInterpreter::GetInstance()->Pause();		// no more command execution
                	
		            ::PostQuitMessage(0);
                }
                // f - fast forward through video and audio
                else if ((char) wParam == 'f')
                {
					gDebugLog.Log("Fast forward through video and audio");
					if (gVideoManager.Playing())                  
						gVideoManager.Kill();
						
					if (gAudioManager.Playing())
						gAudioManager.Kill(0, false);
                }
                // p - pause or resume audio and video 
                else if ((char) wParam == 'p')  
                {
                	if (gVideoManager.Paused())
                		gVideoManager.Resume();
                	else
                    	gVideoManager.Pause(0);
                    	
                    if (gAudioManager.Paused())
                    	gAudioManager.Resume();
                    else
                    	gAudioManager.Pause(0);
                }
            }
            else 
            	return DefWindowProc(hWnd, message, wParam, lParam);
            	
            break;
                         
        case WM_CHAR:
        	// the escape key is our general quit what you are doing key
        	if (wParam == VK_ESCAPE)
        	{
				gDebugLog.Log("Escape key");
				if ((theKey = gCommandKeyManager.GetCommandKey((char) 0x1B)) != NULL)
        		{
                   if (gVideoManager.Playing())
                    	gVideoManager.Kill();
                    	
                    if (gAudioManager.Playing())
                    	gAudioManager.Kill(0, false); 
                    	
                    theKey->GetCallback()->Run();
                }
                else 	// do normal escape key actions
                {
        			if (TInterpreter::GetInstance()->Napping())
					{
						gDebugLog.Log("Escape from Nap");
						TInterpreter::GetInstance()->KillNap();
					}
					if (gVideoManager.Playing())
					{
						gDebugLog.Log("Fast forward through video");
						gVideoManager.Kill();
					}
					if (gAudioManager.Playing())
					{
						gDebugLog.Log("Fast forward through audio");
						gAudioManager.Kill(0, false);
					}
				}
			}
			else if (gInputManager.InInput())
			{
				gInputManager.KeyDown((char) wParam);
			}
            else
            {
	        	// pass it to touch zones to see if a touchzone should be hit 
	        	theZone = gTouchZoneManager.GetTouchZone(wParam);
	                  
	            if (theZone != NULL)
					theZone->DoCallback(); 
			}
            break;

		case WM_DESTROY:
			::PostQuitMessage(0);
			break;

		case WM_SYSCOMMAND:
            if (wParam == SC_SCREENSAVE)
				lRet = 0;			// don't let the screen saver come up
            else
                lRet = DefWindowProc(hWnd, message, wParam, lParam);
            break;

		case WM_POWER: //prevent the system from suspending
        	if (wParam == PWR_SUSPENDREQUEST) 
        		lRet = PWR_FAIL;
        	else
        		lRet = DefWindowProc(hWnd, message, wParam, lParam);
        	break;
         
        default:              /* Passes it on if unproccessed   */
            lRet = DefWindowProc(hWnd, message, wParam, lParam);
			break;
   }

   // cbo_test - try to improve performance
	if (gInFront)
	{
		//TInterpreter::GetInstance()->Idle();
		gVideoManager.Idle();
		gAudioManager.Idle();
	}

   return (lRet);
}

//
// Child window to put up the black background.
//

LRESULT CALLBACK ChildWndProc (HWND hwnd, UINT message, UINT wParam, LPARAM lParam)
	throw ()
{
	CATCH_ALL_EXCEPTIONS_AND_RETURN(RealChildWndProc(hwnd, message,
													 wParam, lParam));
	return 0;
}

LRESULT RealChildWndProc (HWND hwnd, UINT message, UINT wParam, LPARAM lParam)
{
	long		retVal = 0;
	bool		activate = false;
	
	switch (message)
	{
		case WM_CREATE:
			break; 
			
        case WM_DESTROY:          /* message: window being destroyed */ 
            PostQuitMessage(0);
            break;

		case WM_MOUSEMOVE:
       		DefWindowProc(hwnd, message, wParam, lParam);
	       	break;

		case WM_ACTIVATE:
			if (wParam)
				activate = true;

			if (activate)
			{
				// put ourselves just behind the main window again
				//SetWindowPos(hBackgroundWnd, hWnd, 0, 0, gHorizRes, gVertRes, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE); 
				//ShowWindow(hBackgroundWnd, SW_SHOW);
			}
			else
			{
				// hide this window
				//ShowWindow(hBackgroundWnd, SW_HIDE);
				//SetWindowPos(hBackgroundWnd, hwndApp, 0, 0, 0, 0, SWP_HIDEWINDOW);
			}
			break;

	    default:                  /* Passes it on if unproccessed    */
	        return (DefWindowProc(hwnd, message, wParam, lParam));
	}  
	
	return (retVal);
}

//
//	CleanUp - Clean everything up.
//
void CleanUp()
{
	gDebugLog.Log("CleanUp: tossing everything"); 
	DumpStats();

	//if (gVideoManager.Playing())
		gVideoManager.Kill();
			
	//if (gAudioManager.Playing())
		gAudioManager.Kill(0, true);

	// Shut down our interpreter.
	if (gWin5LInterpreter)
	{
		delete gWin5LInterpreter;
		gWin5LInterpreter = NULL;
	}

	// Clean up our other resources.
	gVariableManager.RemoveAll();
	gTouchZoneManager.RemoveAll();
	gCommandKeyManager.RemoveAll();	
	gPictureManager.RemoveAll();
	gPaletteManager.RemoveAll();
	gFontManager.RemoveAllButDefaultFont();
	
	DumpStats();
}

//
//	ShutDown
//
void ShutDown(bool Toss /* = true */)
{    
	gDebugLog.Log("ShutDown");

	if (Toss)
		CleanUp(); 
	
    gView->BlackScreen();
        
    DeInitInstance();

    exit(0);
}

// Mesage handler for about box.
LRESULT CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
	throw ()
{
	CATCH_ALL_EXCEPTIONS_AND_RETURN(RealAbout(hDlg, message, wParam, lParam));
	return 0;
}

LRESULT RealAbout(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
		case WM_INITDIALOG:
			// set the text field to our version number
			SetDlgItemText(hDlg, IDC_VERSION_LINE, VERSION_STRING);
				return true;

		case WM_COMMAND:
			if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL) 
			{
				EndDialog(hDlg, LOWORD(wParam));
				return true;
			}
			break;
	}
    return false;
}

//
//	CheckSystem - Make sure this system meets our minimum requirements.
//
bool CheckSystem(void)
{
	gSysInfo.Init();

	if (not gSysInfo.IsWin32())
	{
		//gLog.Error("This application requires at least Win95.");
		AlertMsg("This application requires at least Win95", true);
		return (false);
	}

	return (true);
}

//
//	StartTimer - Start our execution timer.
//
void StartTimer(void)
{
	gTimer = ::SetTimer(hwndApp, FIVEL_TIMER, FIVEL_TIMER_MSECS, NULL);
}

//
//	StopTimer - Kill our execution timer.
//
void StopTimer(void)
{
	if (gTimer != 0)
		::KillTimer(hwndApp, gTimer);
	gTimer = 0;
} 

void DumpStats(void)
{
	MEMORYSTATUS	memStat;

	::GlobalMemoryStatus(&memStat);
	gDebugLog.Log("Percent of memory in use <%ld>", memStat.dwMemoryLoad);
	gDebugLog.Log("Bytes of physical memory <%ld>", memStat.dwTotalPhys);
	gDebugLog.Log("Free physical memory <%ld>", memStat.dwAvailPhys);
	gDebugLog.Log("Free virtual memory <%ld>", memStat.dwAvailVirtual);
}

void ReDoScript(TString &inCardName)
{
	if (gVideoManager.Playing())
		gVideoManager.Kill();

	if (gAudioManager.Playing())
		gAudioManager.Kill();

	gTouchZoneManager.RemoveAll();

	try
	{
		TInterpreter::GetInstance()->ReloadScript(inCardName);
	}
	catch (...)
	{
		ShutDown(false);
	}
}

//
//	SwitchScripts - Switch to an entirely different script file.
//                  This function really isn't necessary for recent
//                  5L programs, and should probably go away.
//
void SwitchScripts(int32 inScript)
{ 
	TString		curPalName;
	LPalette	*curPal = NULL;
	bool		reloadPal = FALSE;
	
	if ((inScript > 0) and (gConfigManager.SwitchScripts(inScript)))
	{ 	
		// save the current palette
		curPal = gPaletteManager.GetCurrentPalette();
		if (curPal != NULL)
		{
			curPalName = curPal->GetName();
			reloadPal = true;
			curPal = NULL;
		}

		gCursorManager.ChangeCursor(NO_CURSOR);

		// Delete everything, including our interpreter.
		CleanUp();
		
		gDebugLog.Log("SwitchScript: start script <%s>",
					  gConfigManager.CurScript());

		SetGlobals();

		// Get the current palette back into the resource tree.
		if (reloadPal)
        	curPal = gPaletteManager.GetPalette(curPalName);
        
        if (curPal == NULL)
        	gVariableManager.SetString("_graphpal", "NULL");
        else 
        {
			gPaletteManager.SetPalette(curPal, true);
			gDebugLog.Log("_graphpal kept as <%s> for new script", curPalName.GetString());
        }

		// now try to start the new script
		try
		{
			gWin5LInterpreter =
				new TWin5LInterpreter(gConfigManager.CurScript());
			TInterpreter::GetInstance()->JumpToCardByName("start"); 
		}
		catch (...)
		{
    		ShutDown(true);
		}
	}
	else
		ShutDown(true);
}

//
//	SetGlobals - Set the "global" variables that the engine maintains. These should
//		not be confused with "special" variables that the variable manager intercepts
//		and maintains.
//
void SetGlobals(void)
{
	TString DLSuser;
	int index;

	// "global" variables maintained by engine
    gVariableManager.SetString("_lpstat", "1");
    gVariableManager.SetString("_lpactive", "0");
    gVariableManager.SetString("_movieplaying", "0");
    gVariableManager.SetString("_error", "0");
    gVariableManager.SetString("_graphpal", "NULL"); 
    gVariableManager.SetString("_faded", "0");
    gVariableManager.SetString("_locked", "0");
	gVariableManager.SetLong("_resx", gHorizRes);
	gVariableManager.SetLong("_resy", gVertRes);
	gVariableManager.SetLong("_bitdepth", gView->BitDepth());

	gVariableManager.SetLong("_QuickTimeVideoPreload", 2000);
	gVariableManager.SetLong("_QuickTimeVideoCycles", 50);
	gVariableManager.SetLong("_QuickTimeAudioPreload", 500);
	gVariableManager.SetLong("_QuickTimeAudioCycles", 20);

	// Global variables to supports DLS/5L integration
	DLSuser = gConfigManager.DLSUser();
	if (DLSuser.Equal("DefaultUser"))
		gVariableManager.SetString("_DLSenabled", "0");
	else
	{
		gVariableManager.SetString("_DLSenabled", "1");
		index = DLSuser.Find(' ');
		gVariableManager.SetString("_DLSFirstName", DLSuser.Mid(0, index));
		gVariableManager.SetString("_DLSLastName", DLSuser.Mid(index+1));
	}

	// set the engine build variables
	gVariableManager.SetString("_enginebuildstr", VERSION_STRING);

	int32		buildNum;

	buildNum = 10000 * VERSION_MAJOR_NUM;
	buildNum += (100 * VERSION_MINOR_NUM);
	buildNum += VERSION_REV_BIG;
	gVariableManager.SetLong("_enginebuild", buildNum);
}	

//
//	PutInBackground -
//
void PutInBackground(void)
{
	SetWindowPos(hwndApp, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE);

	ShowWindow(hwndApp, SW_MINIMIZE);
	ShowWindow(hBackgroundWnd, SW_HIDE);

	gInBackground = true;

	gDebugLog.Log("PutInBackground");
}

//
//	PutInForeground
//
void PutInForeground(void)
{
	ShowWindow(hBackgroundWnd, SW_SHOW);
	ShowWindow(hwndApp, SW_MAXIMIZE);

	SetWindowPos(hwndApp, /*HWND_TOP*/ HWND_TOPMOST, 
		gWinRect.left, gWinRect.top, gWinRect.right, gWinRect.bottom, 0);
//	SetWindowPos(hBackgroundWnd, hwndApp, 
//		gBackgroundWinRect.left, gBackgroundWinRect.top, 
//		gBackgroundWinRect.right, gBackgroundWinRect.bottom, 0);

	gInBackground = false;

	gDebugLog.Log("PutInForeground");
}

static TString ReadSpecialVariable_system()
{
	return gSysInfo.ShortString();
}

static TString ReadSpecialVariable_curcard()
{
	return TInterpreter::GetInstance()->CurCardName();
}

static TString ReadSpecialVariable_prevcard()
{
	return TInterpreter::GetInstance()->PrevCardName();
}

static TString ReadSpecialVariable_eof()
{
	if (gFileManager.CurFileOpen())
	{
		if (gFileManager.CurFileAtEOF())
        		return "1";
        	else
        		return "0";
	}
	else
	{
		gDebugLog.Log("Trying to read _EOF and no file open!");
		return "0";
	}
}

/*
 $Log$
 Revision 1.13.2.2  2003/10/06 20:16:28  emk
 3.4.5 - Ripped out old QuickTime layer and replaced with TQTMovie wrapper.
 (Various parts of the new layer include forward ports from
 FiveL_3_2_0_5_TQTMovie and back ports from Tamale.)  This engine is
 completely untested and almost certainly has bugs and incomplete error
 handling.

 Revision 1.13.2.1  2002/10/11 18:03:30  emk
 3.4.3 - 11 Oct 2002 - emk

 Douglas--I don't have the scripts required to test this properly, so
 you'll have to arrange for both the smoke testing (i.e., does it work
 at all?) and the regular testing (i,e., does everything work right?).
 If there are problems, I'll build a 3.4.4 on Monday.

   * Removed code to clip cursor into box.  (Backported from 3.5.)
     This *should* fix a bug which caused the cursor to be locked at
     0,0 after startup on some machines.

 Revision 1.13  2002/07/26 20:00:27  zeb
 3.3.21 - 26 July 2002 - zeb

   * Added FileSystem::ExistenceCheck, which we use to check for the
     existence of various files during the startup process (bug #937).

 Revision 1.12  2002/07/25 22:25:36  emk
   * Made new CryptStream auto_ptr code work under Windows.
   * PURIFY: Fixed memory leak in TBTree::Add of duplicate node.  We now
     notify the user if there are duplicate cards, macros, etc.
   * PURIFY: Fixed memory leak in TBTree destructor.
   * PURIFY: Fixed memory leak in ConfigManager destructor.
   * PURIFY: Fixed memory leaks when deleting DIBs.
   * PURIFY: Made sure we deleted offscreen GWorld when exiting.
   * PURIFY: Fixed memory leak in LBrowser.
   * PURIFY: Fixed memory leak in LFileBundle.
   * PURIFY: Fixed uninitialized memory reads when View methods were
     called before View::Init.
   * PURIFY: Made View::Draw a no-op before View::Init is called.
     (It seems that Windows causes us to call Draw too early.)

 Revision 1.11  2002/07/24 17:41:14  emk
 3.3.19 - 24 July 2002 - emk

   * Cleaned up Win32 5L.log (bug #1057).
   * We now print the glyph cache size every 100K (bug #969).

 Revision 1.10  2002/07/23 21:53:53  emk
 3.3.17 - 23 July 2002 - emk

   * Fixed RETURN in macros (bug #1053).
   * Fixed typography exception when missing buttpcx graphic (bug #1039).
   * Made Win32 BROWSE return an error if it fails (bug #793).
   * Forward-ported QtComponentVersion to Win32 (bug #1054).
   * Performance tuned Win32 textaa (bug #933).

 Revision 1.9  2002/07/15 15:56:44  zeb
 3.3.13 - 15 July 2002 - zeb, emk
   * Language change: (IF cond true_cmd false_cmd) now takes arbitrary
     expressions for 'cond'.  The following new primitives have
     been added: AND, OR, NOT, contains, =, <>, <, >, <=, >=.
   * Added a new (LOG filename msg) command, which allows the programmer
     to write to "5L", "debug" and "MissingMedia" logs.
   * Major logging improvements: All primitives are now automatically
     logged in a standard format (bug #1003).
   * Adjusting of coordinates using origin is now logged.
   * Callbacks are now logged in a much more useful fashion.
   * Old arithmetic primitives now return a value (add, sub, div).
   * Added MakeQuotedString to TTemplateUtils and wrote a matching test suite.

 Revision 1.8  2002/07/08 16:43:56  emk
 3.3.11 - Bugfixes from 3.2.0.5 through 3.2.0.7.

   * Ported Win32 QuickTime 6/VP3 bugfix forward from 3.2.0.x.
   * Ported Win32 QuickTime 6 gamma bugfix forward from 3.2.0.x.
   * Ported Win32 line drawing bugfix forward from 3.2.0.x.
   * Fixed Win32 (touch ...) command to highlight touchzones more like the
     Macintosh.  (It now redraws the unhighlighted graphic at the end of the
     highlight sequence.)

 Revision 1.7  2002/06/20 16:32:54  emk
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

 Revision 1.6.6.6  2002/06/12 19:03:03  emk
 3.3.4.5 - Moved Do* commands from Card.{h,cpp} to TWinPrimitives.{h,cpp},
 and broke the remaining dependencies between these primitive commands and
 the current 5L interpreter.  The TInterpreter and TPrimitives interfaces
 are now quite mature.

 *** Please beat very, very hard on this build.  I don't anticipate
 further changes to the Windows engine for a while. ***

 REMOVED COMMANDS: kill (use still), loadpick (use loadpic)
 NEEDS TESTING: origin w/macros, other uses of origin.  5L now
   sets the origin to 0,0 whenever it begins a new card, which
   should produce behavior identical to the old system, unless
   I've overlooked something.
 NEEDS TESTING: make sure all the commands are available, and
   have the right names.  I've checked this a dozen times
   by eye, but I might have overlooked something.

 The only remaining dependencies between the interpreter and the rest of 5L
 are in the Header and TStyleSheet classes.  I'm postponing this last bit
 of cleanup until after 3.4.  Up next: Repeat the 3.3.4.{1-5} changes for
 the Macintosh.

 Revision 1.6.6.5  2002/06/11 18:03:59  emk
 Fixed a bug where 5L deleted the default font when switching scripts,
 causing INPUT to crash when passed a non-existant header name.

 Revision 1.6.6.4  2002/06/05 20:42:38  emk
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

 Revision 1.6.6.3  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.6.6.2  2002/06/05 08:50:52  emk
 A small detour - Moved responsibility for script, palette and data directories
 from Config.{h,cpp} to FileSystem.{h,cpp}.

 Revision 1.6.6.1  2002/06/05 07:05:30  emk
 Began isolating the 5L-language-specific code in Win5L:

   * Created a TInterpreter class, which will eventually become the
     interface to all language-related features.
   * Moved ssharp's developer preference support out of Config.{h,cpp}
     (which are tighly tied to the language) and into TDeveloperPrefs.{h,cpp},
     where they will be isolated and easy to port to other platforms.

 Revision 1.6  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.5.2.2  2002/05/01 03:27:07  emk
 3.3.2.6 - First Windows engine with (textaa ...) command.

 - Implemented a primitive, slow Image::DrawPixMap command that uses
 ::GetPixel and ::SetPixel to do alpha blending (shudder).  Strangely
 enough, it's about as fast as the somewhat optimized Mac routines.
 Anyone got a good GDI book?

 - Fixed several assertion failures.

 Known problems:

 - Occasional assertion failure on exit.  The reference-counting on
 TIndexFile claims it's getting dereferenced too many times.  This is
 an old bug; all the TBTree and TBNode classes are pretty dodgy.

 - Assertion failure on "Special Variables" screen in 5Ltest.  This is
 caused by overlong lines.

 Revision 1.5.2.1  2002/04/30 07:57:31  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.5  2002/04/09 14:07:43  emk
 Merged code from 3.2.0.3 to handle mouse-downs during text input by treating
 them as though the user pressed RETURN.  This should keep our users from
 getting quite so lost on the Genetics sign-on screen.

 Revision 1.2.2.4  2002/07/03 11:44:39  emk
 3.2.0.6 - All known, fixable 3.2.0.x Windows bugs should be fixed.  Please
 test this engine carefully, especially line drawing (including diagonal)
 and (touch ...) callbacks.

     Bug #958 workaround: Since VP 3.2.1.3 doesn't support GDI
     under the QT 6 Public Preview, 5L no longer forces QuickTime
     to use GDI.  See the bug report for details.  5L scripts
     can tell whether QuickTime is using DirectDraw (the most
     common alternative to GDI) by checking the variable
     _QuickTimeHasDDObject.

     Bug #980 revisted: Our fix to touchzone handling yesterday broke
     support for the second touchzone command.  This has been fixed.

     Bug #986 fixed: QuickTime no longer performs gamma correction
     when importing images, so we no longer see nasty color mismatches.

     Bug #822 fixed: When drawing lines with thickness > 1, we now
     decompose the lines into single-pixel lines to work around a bug
     in certain Win98 and WinME systems which caused lines to begin
     one pixel too soon.

 Revision 1.2.2.3  2002/04/09 13:53:45  emk
 Mouse-click during text entry now works the same as typing RETURN.

 This fixes a common usability problem with our program's login screens--
 people don't read the "Press ENTER or TAB to continue" message, and
 click around blindy.  This is a very tiny UI tweak.

 This code is tagged in CVS as FiveL_3_2_0_3.

 Revision 1.4  2002/03/05 10:25:41  tvw
 Added new option to 5L.prefs to optionally allow multiple
 instances of 5L to run.

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

 Revision 1.9  2000/08/08 19:03:40  chuck
 no message

 Revision 1.8  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.7  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.6  2000/03/01 15:46:55  chuck
 no message

 Revision 1.5  2000/02/02 15:15:32  chuck
 no message

 Revision 1.4  1999/11/16 13:46:32  chuck
 no message

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:26:23  chuck
 Initial revision

*/
