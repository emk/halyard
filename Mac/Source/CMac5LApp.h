// ===========================================================================
//	CMac5LApp.h					©1994 Metrowerks Inc. All rights reserved.
// ===========================================================================

#pragma once

#include "THeader.h"
#include "TLogger.h"
#include "CPalette.h"

BEGIN_NAMESPACE_FIVEL

class	CBackWindow;

class	CMac5LApp : public PP::LApplication
{

	public:
							CMac5LApp();
		virtual 			~CMac5LApp();
	
		void				QuitScript(void);
		void				StartUp(void);

		//////////
		// Open a script file.  Scripts are represented without a directory or extension.
		// For example ":Scripts:sample.scr" would be represented as "sample".  Return
		// true if no errors occur.
		//
		bool				OpenScript(const TString &inScriptName);	
#ifdef DEBUG
		void				ReDoScript(const char *curCard);
		bool				OpenScriptAgain(const TString &inScriptName, const char *jumpCard);
		void				ReDoReDoScript(void);

		Boolean				ObeyCommand(PP::CommandT inCommand, void *ioParam);
		void				FindCommandStatus(
								PP::CommandT 		inCommand,
								Boolean&			outEnabled,
								Boolean&			outUsesMark,
								UInt16&				outMark,
								Str255				outName);
#endif
		void				CheckMemory(void);
		void				MaxMemory(void);
		void				DumpMemory(void);
		

//		void				SetPalette(PaletteHandle inPalHand, bool inActivateNow);
		
//		void				NewColorTable(CPalette *inPal, bool inGraphics);
		
//		void				DoNewPalette(CTabHandle inCTab);
//		void				RestorePalette(void);
//		CTabHandle			GetCTab(void) { return (mGraphicsPal->GetCTab()); }
//		void				CheckPalette(void);
//		bool				HaveNewPal(void)
//					{ return (mHaveNewPal); }
		
		Boolean				AttemptQuitSelf(SInt32 inSaveOption);
		virtual void		HandleAppleEvent(
								const AppleEvent	&inAppleEvent,
								AppleEvent			&outAEReply,
								AEDesc				&outResult,
								SInt32				inAENumber);
		
		void				DoExit(int16 inSide);
		
		virtual void		EventResume		(const EventRecord& inMacEvent);
		virtual void		EventSuspend	(const EventRecord& inMacEvent);							
	
	protected:
		CBackWindow			*mDisplayWindow;
		
		void				CleanUp(void);
		void				SetGlobals(void);
		void				DoAEOpenDoc(
								const AppleEvent 	&inAppleEvent,
								AppleEvent			&outARReply,
								long 				inAENumber);
		
	private:
		Rect				mScreenRect;
		int					mBitDepth;
		bool				mScriptRunning;
//		bool				mHaveNewPal;
//		CPalette			*mGraphicsPal;
//		CPalette			*mMoviePal;
//		PaletteHandle		mCurPal;
		TString				mReDoCard;		// card to jump to when redo redoscript
		bool				mReDoReDo;		// waiting for a redo redoscript?
	
		bool				GetScriptFile(FSSpec *scriptSpec);
};

// global application object
extern CMac5LApp *gTheApp;

extern Handle clickSound;
extern WindowPtr gWindow;
extern int FiveLmain();

END_NAMESPACE_FIVEL
