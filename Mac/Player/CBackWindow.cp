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
//	CBackWindow.cp
//
//

#include "debug.h"
#include "Mac5L.h"
#include "gamma.h"

#include "CBackWindow.h"
#include "CPlayerView.h"

CBackWindow::CBackWindow()
	:LWindow()
{
	mFadeAfterDraw = false;
	
}

CBackWindow::CBackWindow(LStream  *inStream) : LWindow(inStream)
{

}

// ---------------------------------------------------------------------------
//	¥ CreateWindow											 [static] [public]
// ---------------------------------------------------------------------------
//	Return a newly created Window object initialized from a PPob resource
CBackWindow*
CBackWindow::CreateWindow(
	ResIDT			inWindowID,
	LCommander*		inSuperCommander)
{
	SetDefaultCommander(inSuperCommander);
	SetDefaultAttachable(nil);
	
	CBackWindow	*theWindow = static_cast<CBackWindow*>
						(UReanimator::ReadObjects(ResType_PPob, inWindowID));
	ThrowIfNil_(theWindow);
	
	try {
		theWindow->FinishCreate();
		if (theWindow->HasAttribute(windAttr_ShowNew)) {
			theWindow->Show();
		}
	}
	
	catch (...) {						// FinishCreate failed. Window is
		delete theWindow;				//   in an inconsistent state.
		throw;							//   Delete it and rethrow
	}
	
	return theWindow;
}

//
//	DrawSelf - put up our back ground
//
void
CBackWindow::DrawSelf()
{
	Rect	theFrame;
	
	CalcLocalFrameRect(theFrame);
	
	//ApplyForeAndBackColors();
	::RGBBackColor(&Color_Black);
	::RGBForeColor(&Color_Black);
	::PenMode(patCopy);
	::PaintRect(&theFrame);	
		
	//::EraseRect(&theFrame);
	ValidPortRect(&theFrame);
	
	if (mFadeAfterDraw)
	{
		DoGFade(true, 5, false);
		mFadeAfterDraw = false;
	}
}

/* 
$Log$
Revision 1.1  1999/10/21 17:19:17  chuck
Initial revisioni

*/