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

#include "LUtil.h"
#include "Index.h"
#include "Header.h"
#include "Macro.h"
#include "Graphics.h"
#include "Variable.h"
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

#if defined USE_BUNDLE
	#include "LFileBundle.h"
#else
	#include "LFiles.h"
#endif

#define MAX_LOADSTRING 100

// TAKE_OVER_SCREEN - define it if you want to take over the screen.
//		The only reason we wouldn't do this is for the debug version.
//
#ifndef _DEBUG
#define TAKE_OVER_SCREEN
#endif

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

View				*gView = NULL;
CardManager         gCardManager;
MacroManager        gMacroManager;
HeaderManager       gHeaderManager;
VariableManager     gVariableManager;
LTouchZoneManager	gTouchZoneManager;     
SysInfo				gSysInfo; 
TLogger				gLog;
TLogger				gMissingMediaLog;
#ifdef DEBUG
TLogger				gDebugLog;
#endif   
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
GWorldPtr			gDummyGWorldPtr = NULL;
IndexFileManager	gIndexFileManager;
LHttp				gHttpTool;
LBrowser			gBrowserTool;

// The one and only CLimitSingleInstance object
SingleInstance g_SingleInstanceObj(TEXT("{03C53E8D-4B46-428c-B812-DBAC5F1A6378}"));

// Foward declarations of functions included in this code module:
bool				InitInstance(HINSTANCE, int);
void				DeInitInstance(void);
bool				InitApplication(HINSTANCE hInstance);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK	ChildWndProc(HWND, UINT, UINT, LPARAM);
LRESULT CALLBACK	About(HWND, UINT, WPARAM, LPARAM);
void				StartTimer(void);
void				StopTimer(void);
bool				CheckSystem(void);
#ifdef DEBUG
void				DumpStats(void);
#endif


int APIENTRY WinMain(HINSTANCE hInstance,
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

	// Initialize global strings
	LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadString(hInstance, IDC_FIVEL, szWindowClass, MAX_LOADSTRING);
	LoadString(hInstance, IDC_FIVEL_CHILD, szChildClass, MAX_LOADSTRING);

	// Initialize the application.
	if (not InitApplication(hInstance))
		return (false);

	// Make system checks.
	if (not CheckSystem())
		return (false);

	// Process the command line and configuration file.
	if (not gConfigManager.Init(lpCmdLine))
		return (false);	

	// Initialize the global log file.
	gLog.Init(gConfigManager.InstallPath(), SHORT_NAME, true, true);
	gLog.Log("%s", VERSION_STRING);

	// Initialize the missing media file.
	gMissingMediaLog.Init(gConfigManager.InstallPath(), "MissingMedia", false, true);

#ifdef DEBUG
	// Initialize the debug log.
	gDebugLog.Init(gConfigManager.InstallPath(), "Debug");
	gDebugLog.Log("%s", VERSION_STRING);
#endif

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

	// read in the script 
	if (not gIndexFileManager.NewIndex(gConfigManager.CurScript()))
	{
 		DeInitInstance();
 		
    	return (false);
    }

	// initialize the URL checker
	gHttpTool.Init();

	// initialize the browser launcher
	gBrowserTool.Init();
 
#ifdef DEBUG
	gDebugLog.Log("Bit Depth: %d", gView->BitDepth());
	gDebugLog.Log("Resolution: %d x %d", gHorizRes, gVertRes);
#endif

	// jump to the start card
	gCardManager.JumpToCardByName("start"); 
            
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
#ifdef DEBUG
	wcex.hIcon			= LoadIcon(hInstance, (LPCTSTR)IDI_FIVEL);
#else
	wcex.hIcon			= NULL;
#endif
	wcex.hCursor		= NULL;
	wcex.hbrBackground	= NULL;
#ifdef DEBUG
	wcex.lpszMenuName	= (LPCSTR)IDC_FIVEL;
#else
	wcex.lpszMenuName	= NULL;
#endif
	wcex.lpszClassName	= szWindowClass;
#ifdef DEBUG
	wcex.hIconSm		= LoadIcon(wcex.hInstance, (LPCTSTR)IDI_SMALL);
#else
	wcex.hIconSm		= NULL;
#endif

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
    if (g_SingleInstanceObj.IsAnotherInstanceRunning())
		return FALSE;
	
	HWND        hWnd;
#ifdef TAKE_OVER_SCREEN
    POINT		cursorPos;
#endif
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

	// Initialize QuickTime - use the kInitializeQTMLUseGDIFlag as we 
	//		have seen flashes on startup of certain movies on certain
	//		platforms (Dell laptops and Win98)
	//
	if (::InitializeQTML(kInitializeQTMLUseGDIFlag) != noErr)
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

	::EnterMovies();				// Initialize QuickTime
		
	//Create and show app window:    

#ifdef DEBUG 
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
#else
	win_style = WS_POPUP;
	::AdjustWindowRect(&Rectgl, WS_POPUP, false);
#endif	
 
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

#ifdef TAKE_OVER_SCREEN
	SetWindowPos(hWnd, /*HWND_TOP*/ HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE);
#else
	SetWindowPos(hWnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOSIZE);
#endif
	    
    gCursorManager.ChangeCursor(NO_CURSOR);

// black full-screen background. Doesn't look good with blueramp.
#ifdef TAKE_OVER_SCREEN
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
#endif

    ::ShowWindow(hWnd, SW_SHOW);
    ::UpdateWindow(hWnd);

#ifdef TAKE_OVER_SCREEN	
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
	
	// now gScreenRect has the screen coordinates of our virtual screen
	gCursorManager.ClipCursor(&gScreenRect);

#endif
    
    hwndApp = hWnd; 

	gPaletteManager.Init();

	gGrafPtr = (CGrafPtr) ::CreatePortAssociation (hwndApp, NULL, NULL); // Register window with QTML
	
	// set up a dummy GWorld that we can use when prerolling movies
	Rect dummyRect = {0, 0, 1, 1};
	OSErr err = ::NewGWorld(&gDummyGWorldPtr, 8, &dummyRect, NULL, NULL, 0);
	
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

	::DestroyPortAssociation (gGrafPtr);      // Unregister window with QTML

	::ExitMovies();                               // Terminate QuickTime
	::TerminateQTML();                            // Terminate QTML
}

//
//  WndProc - 
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
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

#ifdef TAKE_OVER_SCREEN
				// go back to restricting cursor movement
				gCursorManager.ClipCursor(&gScreenRect);
#endif
    			
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

#ifdef TAKE_OVER_SCREEN 
				// stop restricting cursor movement
				gCursorManager.ClipCursor(NULL);
#endif				
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
					gCardManager.Idle();
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

            if (not gCardManager.Napping())
            {  
                if (theZone = gTouchZoneManager.GetTouchZone(cursorPos))  
					theZone->DoCommand();
            }
            break;   
        
        case WM_SYSCHAR:
        	// alt-key processing
        	if ((char) wParam == '.')
        	{
#ifdef _DEBUG
				gDebugLog.Log("Hit Alt-period");
#endif
	        	if (gCardManager.Napping())
				{
#ifdef _DEBUG
					gDebugLog.Log("Escape from Nap");
#endif
					gCardManager.KillNap();
				}
				if (gVideoManager.Playing())
				{
#ifdef _DEBUG
					gDebugLog.Log("Fast forward through video");
#endif
					gVideoManager.Kill();
				}
				if (gAudioManager.Playing())
				{
#ifdef _DEBUG
					gDebugLog.Log("Fast forward through audio");
#endif
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
                    	
                    gCardManager.JumpToCard(theKey->GetCard());
            	}
            	// q - quit
            	else if ((char) wParam == 'q')	
                { 
                	gCardManager.Pause();		// no more command execution
                	
		            ::PostQuitMessage(0);
                }
                // f - fast forward through video and audio
                else if ((char) wParam == 'f')
                {
#ifdef _DEBUG
					gDebugLog.Log("Fast forward through video and audio");
#endif 
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
#ifdef _DEBUG
				gDebugLog.Log("Escape key");
#endif
				if ((theKey = gCommandKeyManager.GetCommandKey((char) 0x1B)) != NULL)
        		{
                   if (gVideoManager.Playing())
                    	gVideoManager.Kill();
                    	
                    if (gAudioManager.Playing())
                    	gAudioManager.Kill(0, false); 
                    	
                    gCardManager.JumpToCard(theKey->GetCard());
                }
                else 	// do normal escape key actions
                {
        			if (gCardManager.Napping())
					{
#ifdef _DEBUG
						gDebugLog.Log("Escape from Nap");
#endif
						gCardManager.KillNap();
					}
					if (gVideoManager.Playing())
					{
#ifdef _DEBUG
						gDebugLog.Log("Fast forward through video");
#endif
						gVideoManager.Kill();
					}
					if (gAudioManager.Playing())
					{
#ifdef _DEBUG
						gDebugLog.Log("Fast forward through audio");
#endif
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
					theZone->DoCommand(); 
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
		//gCardManager.Idle();
		gVideoManager.Idle();
		gAudioManager.Idle();
	}

   return (lRet);
}

//
// Child window to put up the black background.
//
LRESULT CALLBACK ChildWndProc (HWND hwnd, UINT message, UINT wParam, LPARAM lParam)
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
void CleanUp(void)
{
#ifdef DEBUG
	gDebugLog.Log("CleanUp: tossing everything"); 
	DumpStats();
#endif

	//if (gVideoManager.Playing())
		gVideoManager.Kill();
			
	//if (gAudioManager.Playing())
		gAudioManager.Kill(0, true);
			
	gCardManager.Pause();		
	gCardManager.RemoveAll();
	gMacroManager.RemoveAll();
	gHeaderManager.RemoveAll();
	gVariableManager.RemoveAll();
	gTouchZoneManager.RemoveAll();
	gCommandKeyManager.RemoveAll();	
	gPictureManager.RemoveAll();
	gPaletteManager.RemoveAll();
	gFontManager.RemoveAll();
	gIndexFileManager.RemoveAll();
	
#ifdef DEBUG
	DumpStats();
#endif
}

//
//	ShutDown
//
void ShutDown(bool Toss /* = true */)
{    
#ifdef _DEBUG
	gDebugLog.Log("ShutDown");
#endif

	if (Toss)
		CleanUp(); 
	
    gView->BlackScreen();
        
    DeInitInstance();

    exit(0);
}

// Mesage handler for about box.
LRESULT CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
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

#ifdef DEBUG
void DumpStats(void)
{
	MEMORYSTATUS	memStat;

	::GlobalMemoryStatus(&memStat);
	gDebugLog.Log("Percent of memory in use <%ld>", memStat.dwMemoryLoad);
	gDebugLog.Log("Bytes of physical memory <%ld>", memStat.dwTotalPhys);
	gDebugLog.Log("Free physical memory <%ld>", memStat.dwAvailPhys);
	gDebugLog.Log("Free virtual memory <%ld>", memStat.dwAvailVirtual);
}
#endif

#ifdef DEBUG
void ReDoScript(TString &inCardName)
{
	if (gVideoManager.Playing())
		gVideoManager.Kill();

	if (gAudioManager.Playing())
		gAudioManager.Kill();

	gCardManager.Pause();
	gCardManager.RemoveAll();
	gMacroManager.RemoveAll();
	gHeaderManager.RemoveAll();
	gTouchZoneManager.RemoveAll();
	gIndexFileManager.RemoveAll();

	// NOTE - if we implement loadscript then we will have to open up
	//	all files here that were open before

	// now try to open up the same script file
	if (gIndexFileManager.NewIndex(gConfigManager.CurScript()))
	{
		// Fix Key bindings
		gCommandKeyManager.RebuildKeyBindings();

		gCardManager.JumpToCardByName(inCardName.GetString());
	}
	else
		ShutDown(false);
}
#endif

//
//	SwitchScripts - 
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
		
		CleanUp();
		
#ifdef _DEBUG
		gDebugLog.Log("SwitchScript: start script <%s>", gConfigManager.CurScript());
#endif

		SetGlobals();

		// Get the current palette back into the resource tree.
		if (reloadPal)
        	curPal = gPaletteManager.GetPalette(curPalName);
        
        if (curPal == NULL)
        	gVariableManager.SetString("_graphpal", "NULL");
        else 
        {
			gPaletteManager.SetPalette(curPal, true);

#ifdef _DEBUG
			gDebugLog.Log("_graphpal kept as <%s> for new script", curPalName.GetString());
#endif
        }

		// now try to start the new script
		if (gIndexFileManager.NewIndex(gConfigManager.CurScript()))
			gCardManager.JumpToCardByName("start"); 
		else
    		ShutDown(true);
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

#ifdef _DEBUG	
	gVariableManager.SetString("_debug", "1");
#else
	gVariableManager.SetString("_debug", "0");	
#endif

	// set the engine build variables
	gVariableManager.SetString("_enginebuildstr", VERSION_STRING);

	int32		buildNum;

	buildNum = 10000 * MAJOR_NUM;
	buildNum += (100 * MINOR_NUM);
	buildNum += BUILD_NUM;
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

	gCursorManager.UnClipCursor();	// remove cursor restraints

	gInBackground = true;

	gLog.Log("PutInBackground");
}

//
//	PutInForeground
//
void PutInForeground(void)
{
	ShowWindow(hBackgroundWnd, SW_SHOW);
	ShowWindow(hwndApp, SW_MAXIMIZE);

	gCursorManager.ReClipCursor();

	SetWindowPos(hwndApp, /*HWND_TOP*/ HWND_TOPMOST, 
		gWinRect.left, gWinRect.top, gWinRect.right, gWinRect.bottom, 0);
//	SetWindowPos(hBackgroundWnd, hwndApp, 
//		gBackgroundWinRect.left, gBackgroundWinRect.top, 
//		gBackgroundWinRect.right, gBackgroundWinRect.bottom, 0);

	gInBackground = false;

	gLog.Log("PutInForeground");
}


/*
 $Log$
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
