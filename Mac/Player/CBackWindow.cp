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

#include "THeader.h"

#include "gamma.h"

#include "CBackWindow.h"
#include "CPlayerView.h"

USING_NAMESPACE_FIVEL

CBackWindow::CBackWindow()
	: PP::LWindow()
{
	mFadeAfterDraw = false;
	
}

CBackWindow::CBackWindow(PP::LStream  *inStream) : PP::LWindow(inStream)
{

}

// ---------------------------------------------------------------------------
//	¥ CreateWindow											 [static] [public]
// ---------------------------------------------------------------------------
//	Return a newly created Window object initialized from a PPob resource
CBackWindow* 
CBackWindow::CreateWindow(PP::ResIDT inWindowID, PP::LCommander* inSuperCommander)
{
	SetDefaultCommander(inSuperCommander);
	SetDefaultAttachable(nil);
	
	CBackWindow	*theWindow = static_cast<CBackWindow*>
						(PP::UReanimator::ReadObjects(PP::ResType_PPob, inWindowID));
	ThrowIfNil_(theWindow);
	
	try {
		theWindow->FinishCreate();
		if (theWindow->HasAttribute(PP::windAttr_ShowNew)) {
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
	::RGBBackColor(&PP::Color_Black);
	::RGBForeColor(&PP::Color_Black);
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
Revision 1.3.12.1  2003/02/25 14:57:32  emk
Whitespace fixes and removal of a few bogus #includes.

Revision 1.3  2002/03/04 15:41:16  hamon
Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.

Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.

Changes by Elizabeth and Eric, okayed by Eric.

Revision 1.2  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.1  1999/10/21 17:19:17  chuck
Initial revisioni

*/