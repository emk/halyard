//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
//
//	Menu.c
//
//	Handles menu stuff.
//
//	History:
//
//	950305 jb: Written
//
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


//  __#Defines________________________________________________________________________
//  __#Headers________________________________________________________________________
#include "Mac5L.h"
#include "CMenuUtil.h"

//  __#Protos_________________________________________________________________________
//  __ Macros_________________________________________________________________________
//  __ Enums__________________________________________________________________________
//  __ Typedefs_______________________________________________________________________
//  __ Static Protos__________________________________________________________________
//  __ Extern Globals_________________________________________________________________
//  __ Functions______________________________________________________________________



//____ StartupHideMenuBar __________________________________________________________________________
//
//	Call this very early in your startup cycle; it saves off important information
//	about the menubar.
//
//						LMenuBar(ResIDT inMBARid);
CMenuUtil::CMenuUtil()
{
	GDHandle		mainDev;

	mSavedGrayRgn 	= ::GetGrayRgn(); 		// Store the old gray region
	mMenuBarHeight 	= ::GetMBarHeight(); 	// Store the menu bar height

	//determine rectangle bounding the menubar so we can tell if
	//user has mouse-downed in it later
	mainDev = GetMainDevice();
	mMenuBarRect = (**mainDev).gdRect;
	mMenuBarRect.bottom = mMenuBarHeight;
	
	mMenuBarHidden = false;

}//StartupHideMenuBar


//____ ShutdownHideMenuBar __________________________________________________________________________
//
//
//
CMenuUtil::~CMenuUtil(void)
{
	ShowMenuBar();
}//ShutdownHideMenuBar


//____ ToggleMenuBar __________________________________________________________________________
//
//	Description
//
//	Returns	void
//
void CMenuUtil::ToggleMenuBar(void)
{
	//user is actually changing state of menubar.
	if (mMenuBarHidden)
	{
		mUserWantsMenuBar = TRUE;
		ShowMenuBar();
	}
	else
	{
		mUserWantsMenuBar = FALSE;
		HideMenuBar();
	}
	
}//ToggleMenuBar


//____ HideMenuBar __________________________________________________________________________
//
//
//
void CMenuUtil::HideMenuBar(void)
{
	GDHandle		mainDisplay;
	Rect			mainDisplayBounds;
	RgnHandle		workRgn;
	GrafPtr			windowPort;
	GrafPtr			oldPort;
	CGrafPtr		windowCPort;
	WindowPtr		frontWindow;
	
	// Make sure the menu bar is not already hidden
	if (mMenuBarHidden)
	{
		return;
	}
		
	// Get the rectangle for the main display
	mainDisplay = GetMainDevice();
	mainDisplayBounds = (*mainDisplay)->gdRect;
	
	// Calculate the new regions
	mNewGrayRgn 	= NewRgn();
	mCoveredRgn 	= NewRgn();
	workRgn 		= NewRgn();
	
	// Build region for entire screen
	RectRgn(workRgn, &mainDisplayBounds);
	
	// Gets the new gray rgn (entire desktop)
	UnionRgn(workRgn, mSavedGrayRgn, mNewGrayRgn);
	
	// ...and the difference (should be the menubar)
	DiffRgn(mNewGrayRgn, mSavedGrayRgn, mCoveredRgn);

	DisposeRgn(workRgn);
	
	// Set the new gray region
	LMSetGrayRgn(mNewGrayRgn);
	
	// Get the window manager port clip region
	GetPort(&oldPort);
	GetWMgrPort(&windowPort);
//	windowPort = UScreenPort::GetScreenPort();
	SetPort(windowPort);
	SetClip(mNewGrayRgn);
	
	GetCWMgrPort(&windowCPort);
	SetPort((GrafPtr)windowCPort);
	SetClip(mNewGrayRgn);
	
	// Force an update of the desktop
	PaintOne(nil, mNewGrayRgn);
	
	// Update the windows
	frontWindow = FrontWindow();
	PaintOne((WindowRef)frontWindow, mCoveredRgn);
	PaintBehind((WindowRef)frontWindow, mCoveredRgn);
	CalcVis((WindowRef)frontWindow);
	CalcVisBehind((WindowRef)frontWindow, mCoveredRgn);
	
	// Mark the menu bar as hidden
	LMSetMBarHeight(0);
	mMenuBarHidden = true;
	
	// Restore the port
	SetPort(oldPort);
	
}//HideMenuBar


//____ ShowMenuBar __________________________________________________________________________
//
//
//
void CMenuUtil::ShowMenuBar(void)
{
WindowPtr	frontWindow;
GrafPtr		windowPort;
GrafPtr		oldPort;
CGrafPtr	windowCPort;
	
	// Make sure the menu bar is hidden
	if (!mMenuBarHidden)
	{
		return;
	}
	
	// Reset the menu bar height
	LMSetMBarHeight(mMenuBarHeight);
	
	// Restore the gray region
	LMSetGrayRgn(mSavedGrayRgn);
	
	// Recalculate the windows
	frontWindow = FrontWindow();
	CalcVis((WindowRef)frontWindow);
	CalcVisBehind((WindowRef)frontWindow, mNewGrayRgn);
	
	// Reset the window manager port's clipping
	GetPort(&oldPort);
	GetWMgrPort(&windowPort);
	SetPort(windowPort);
	SetClip(mNewGrayRgn);
	GetCWMgrPort(&windowCPort);
	SetPort((GrafPtr)windowCPort);
	SetClip(mNewGrayRgn);
	
	// Erase the menu region
	FillRgn(mCoveredRgn, &qd.white);
	SetPort(oldPort);
	
	// Redraw the menu bar
	HiliteMenu(0);
	DrawMenuBar();
	
	// Mark that the menu bar is not hidden
	mMenuBarHidden = false;
}//ShowMenuBar


#ifdef NOTYET
//____ CheckMenuHide __________________________________________________________________________
//
//	This unhides or hides them menubar in certain situations:
//
//	o	Mousedown in region where menubar would be - we show menu bar
//	o	we're no longer the Foreground application - we show menu bar
//	o	Mousedown _not_ in menubar region - we may hide the menubar
//
//
//	Call the when there's a context switch, or there's a mouse-down;
//	Furthermore, call this after handling mouse-menu action to re-hide
//	the menu bar;
//
//	Pass in mouseDowns in global coordinates if there was a mousedown,
//	otherwise, pass NULL
//
void CMenuUtil::CheckMenuHide(Point *mouseWhere)
{
	if ((gInFront == FALSE) || (PtInRect(*mouseWhere, &mMenuBarRect)))
	{
		ShowMenuBar();
	}
	else
	{
		if (!mUserWantsMenuBar)
			HideMenuBar();
		else
			ShowMenuBar();
	}
}//CheckMenuHide
#endif
