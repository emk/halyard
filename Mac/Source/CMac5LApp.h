// ===========================================================================
//	CMac5LApp.h					©1994 Metrowerks Inc. All rights reserved.
// ===========================================================================

#pragma once

#include "CPalette.h"

class	CBackWindow;

class	CMac5LApp : public LApplication
{
	public:
							CMac5LApp();
		virtual 			~CMac5LApp();
	
		void				QuitScript(void);
		void				StartUp(void);
		
#ifdef DEBUG_5L
		void				ReDoScript(char *curCard);
		bool				OpenScriptAgain(FSSpec *scriptSpec, char *jumpCard);
#endif

//		void				SetPalette(PaletteHandle inPalHand, bool inActivateNow);
		
		void				NewColorTable(CPalette *inPal, bool inGraphics);
		void				DoNewPalette(CTabHandle inCTab);
		void				RestorePalette(void);
		CTabHandle			GetCTab(void) { return (mGraphicsPal->GetCTab()); }
		void				CheckPalette(void);
		bool				HaveNewPal(void)
					{ return (mHaveNewPal); }
		
		Boolean				AttemptQuitSelf(Int32 inSaveOption);
		virtual void		HandleAppleEvent(
								const AppleEvent	&inAppleEvent,
								AppleEvent			&outAEReply,
								AEDesc				&outResult,
								Int32				inAENumber);
		bool				OpenScript(FSSpec *scriptSpec);
		void				DoExit(int16 inSide);
									
	protected:
		CBackWindow			*mDisplayWindow;
		void				DoAEOpenDoc(
								const AppleEvent 	&inAppleEvent,
								AppleEvent			&outARReply,
								long 				inAENumber);
		
	private:
		Rect				mScreenRect;
		int					mBitDepth;
		bool				mScriptRunning;
		bool				mHaveNewPal;
		CPalette			*mGraphicsPal;
		CPalette			*mMoviePal;
		PaletteHandle		mCurPal;
	
		bool				GetScriptFile(FSSpec *scriptSpec);
};

// global application object
extern CMac5LApp *gTheApp;
extern Handle clickSound;