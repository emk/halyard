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
//	CBackWindow.h
//
//

#pragma once

BEGIN_NAMESPACE_FIVEL

class CBackWindow : public PP::LWindow
{
	public:
		enum { class_ID = FOUR_CHAR_CODE('PlBk') };
		
					CBackWindow();
					CBackWindow(PP::LStream *inStream);
	static CBackWindow		*CreateWindow(PP::ResIDT	inWindowID, PP::LCommander*	inSuperCommander)	;
				
	void 			DrawSelf();
	void			FadeAfterDraw(void) { mFadeAfterDraw = true; }	
	
	protected:
		bool		mFadeAfterDraw;
		
};

END_NAMESPACE_FIVEL

/* 
$Log$
Revision 1.3  2002/03/04 15:41:17  hamon
Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.

Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.

Changes by Elizabeth and Eric, okayed by Eric.

Revision 1.2  1999/10/22 20:42:27  chuck
New cursor management

Revision 1.1  1999/10/21 17:19:17  chuck
Initial revisioni

*/